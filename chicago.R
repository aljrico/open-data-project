# Functions to deal with Chicago's crime data
# Authors: Cristian Estany <cresbabellpuig@gmail.com>
#          Alfredo Hernández <aldomann.designs@gmail.com>
#          Alejandro Jiménez <aljrico@gmail.com>

# Libraries ------------------------------------------------

library(tidyverse)
library(data.table)
library(lubridate)
library(ggmap)
library(data.table)
library(dplyr)
library(forcats)

# Download raw data
crimes.file = "data/Crimes_-_2001_to_present.csv"
if (!file.exists(crimes.file)) {
	# SRC: https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2
	download.file("https://data.cityofchicago.org/api/views/ydr8-5enu/rows.csv", destfile = crimes.file)
}

# Read interesting columns and save into a CSV
crimes.file.clean = "data/Crimes_-_2001_to_present_clean.csv"
if (file.exists(crimes.file) & !file.exists(crimes.file.clean)) {
	chicago.df.clean <- fread(crimes.file, sep = ",", header= TRUE, select = c(3,6,9,18,20,21))
	write_csv(chicago.df.clean, crimes.file.clean)
}

# Read cleaned data
chicago.df <- fread(file="data/Crimes_-_2001_to_present_clean.csv", sep = ",", header = TRUE)

# Clean formats and column names
chicago.df <- chicago.df %>%
	rename(Primary.Type = `Primary Type`) %>%
	mutate(Date = mdy_hms(Date)) %>%
	filter(is.na(Longitude) != TRUE) %>%
	mutate(Primary.Type = ifelse(grepl("NON", Primary.Type), "NON-CRIMINAL", Primary.Type))

# Merge crime categories
sort(table(chicago.df$Primary.Type)) # A lot of categores with low incidence.

all.categories <- factor(unique(chicago.df$Primary.Type))
chicago.df <- chicago.df %>% mutate(Category = ifelse(Primary.Type == "THEFT" | Primary.Type == "BURGLARY" | Primary.Type == "LIQUOR LAW VIOLATION" | Primary.Type == "MOTOR VEHICLE THEFT" | Primary.Type == "ARSON" | Primary.Type == "CRIMINAL DAMAGE", "PROPERTY CRIME", ifelse(Primary.Type == "BATTERY" | Primary.Type == "ROBBERY" | Primary.Type=="ASSAULT" | Primary.Type =="CRIM SEXUAL ASSAULT" | Primary.Type == "SEX OFFENSE" | Primary.Type == "STALKING" | Primary.Type == "KIDNAPPING" | Primary.Type == "HOMICIDE" | Primary.Type == "INTIMIDATION" | Primary.Type == "HUMAN TRAFFICKING", "VIOLENT CRIMES", "QUALITY OF LIFE CRIMES")))


# Filter an arbitrary range of years
chicago.df <- chicago.df %>%
	filter(year(Date) %in% 2001:2017)

# Smaller data frame for testing purposes

chicago.df.small <- data.table(chicago.df)
chicago.df.small <- chicago.df.small[sample(.N, 20000)]

# Maps -----------------------------------------------------

# Simple map using a sample
chicago <- get_map(location = 'chicago', zoom = 11)

ggmap(chicago) +
	geom_point(data = chicago.df.small, aes(x = Longitude, y = Latitude, colour = Primary.Type)) +
	labs(x = "Longitude", y = "Latitude")

# Random plots ---------------------------------------------

# Histogram
ggplot(chicago.df %>% filter(Primary.Type == "CRIM SEXUAL ASSAULT" | Primary.Type == "STALKING" | Primary.Type == "SEX OFFENSES")) +
	geom_histogram(aes(year(Date)), binwidth = 0.5)

ggplot(chicago.df %>% filter(Primary.Type == "NARCOTICS")) +
	geom_histogram(aes(year(Date)), binwidth = 0.5)

# Histogram per type
ggplot(chicago.df) +
	geom_histogram(aes(year(Date)), binwidth = 0.5) +
	facet_wrap(~ Primary.Type)

# Histotgram per type (loop)
for(var in unique(chicago.df$Primary.Type)){
	dev.new()
	print(ggplot(chicago.df[chicago.df$Primary.Type==var,]) +	geom_histogram(aes(year(Date)), binwidth = 0.5) + ggtitle(var))
}




# Testing lubdridate
ggplot(chicago.df.small, aes(Date, Longitude)) +
	geom_point()



chicago.df$Primary.Type[chicago.df$Primary.Type == "NON-CRIMINAL (SUBJECT SPECIFIED)"] <- "NON - CRIMINAL"


ggmap(chicago) +
	geom_point(data = chicago.df %>% filter(Primary.Type == "HOMICIDE"), aes(x = Longitude, y = Latitude, colour = as.factor(year(Date)))) +
	labs(x = "Longitude", y = "Latitude")


ggmap(chicago, extent = "device") +
	geom_density2d(data = chicago.df %>% filter(Primary.Type == "BURGLARY"), aes(x = Longitude, y = Latitude), size = 0.1, color = "blue") +
	stat_density2d(data = chicago.df %>% filter(Primary.Type == "BURGLARY"), aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01,	 bins = 16, geom = "polygon") + 
	#scale_fill_gradient(low = "dark blue" ,high = "white") +
	scale_alpha(range = c(0.05, 0.5), guide = FALSE)
