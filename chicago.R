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

# Read and clean raw data ----------------------------------

# All data (a few rows, for testing purposes)
# chicago.df <- read.csv(file="data/Crimes_-_2001_to_present_clean.csv", nrows = 200)

# Read interesting columns and save into a CSV
# chicago.df.clean <- fread("data/Crimes_-_2001_to_present.csv", sep = ",", header= TRUE, select = c(3,6,8,9,10,20,21))
# write_csv(chicago.df.clean, "data/Crimes_-_2001_to_present_clean.csv")

# Read cleaned data
chicago.df <- fread(file="data/Crimes_-_2001_to_present_clean.csv", sep = ",", header = TRUE)

# Clean formats and column names
chicago.df <- chicago.df %>%
	rename(Primary.Type = `Primary Type` , Location.Description = `Location Description`) %>%
	mutate(Date = mdy_hms(Date)) %>%
	filter(is.na(Longitude) != TRUE)

# NA test (only 1.3% of the reports are not geolocated)
# summary(chicago.df$Latitude)

# table(chicago.df$Primary.Type) # A lot of categores with low incidence.

# Filter an arbitrary range of years
chicago.df <- chicago.df %>%
	filter(year(Date) %in% 2001:2017)

# Smaller data frame for testing purposes

chicago.df.small <- data.table(chicago.df)
chicago.df.small <- chicago.df.small[sample(.N, 2000)]

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

ggplot(chicago.df %>% filter(Primary.Type == "DOMESTIC VIOLENCE")) +
	geom_histogram(aes(year(Date)), binwidth = 0.5)

# Testing lubdridate
ggplot(chicago.df.small, aes(Date, Longitude)) +
	geom_point()



chicago.df$Primary.Type[chicago.df$Primary.Type == "NON-CRIMINAL (SUBJECT SPECIFIED)"] <- "NON - CRIMINAL"


ggmap(chicago) +
	geom_point(data = chicago.df %>% filter(Primary.Type == "HOMICIDE"), aes(x = Longitude, y = Latitude, colour = as.factor(year(Date)))) +
	labs(x = "Longitude", y = "Latitude")


ggmap(chicago, extent = "device") +
	geom_density2d(data = chicago.df %>% filter(Primary.Type == "HOMICIDE"), aes(x = Longitude, y = Latitude), size = 0.1, color = "blue") +
	stat_density2d(data = chicago.df %>% filter(Primary.Type == "HOMICIDE"), aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01,	 bins = 16, geom = "polygon") + 
	#scale_fill_gradient(low = "dark blue" ,high = "white") +
	scale_alpha(range = c(0.05, 0.5), guide = FALSE)


ggmap(chicago, extent = "device") +
	geom_density2d(data = chicago.df %>% filter(Primary.Type == "BATTERY"), aes(x = Longitude, y = Latitude), size = 0.1, color = "blue") +
	stat_density2d(data = chicago.df %>% filter(Primary.Type == "BATTERY"), aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01,	 bins = 16, geom = "polygon") + 
	#scale_fill_gradient(low = "dark blue" ,high = "white") +
	scale_alpha(range = c(0.05, 0.5), guide = FALSE)

chicago.df$Primary.Type[chicago.df$Primary.Type == "THEFT"] <- "NON - VIOLENT"
chicago.df$Primary.Type[chicago.df$Primary.Type == "BURGLARY"] <- "NON - VIOLENT"
chicago.df$Primary.Type[chicago.df$Primary.Type == "GAMBLING"] <- "NON - VIOLENT"
chicago.df$Primary.Type[chicago.df$Primary.Type == "LIQUOR LAW VIOLATION"] <- "NON - VIOLENT"
chicago.df$Primary.Type[chicago.df$Primary.Type == "PROSITUTION"] <- "NON - VIOLENT"
chicago.df$Primary.Type[chicago.df$Primary.Type == "NON-CRIMINAL"] <- "NON - VIOLENT"
chicago.df$Primary.Type[chicago.df$Primary.Type == "RITUALISM"] <- "NON - VIOLENT"
chicago.df$Primary.Type[chicago.df$Primary.Type == "MOTOR VEHICLE THEFT"] <- "NON - VIOLENT"
chicago.df$Primary.Type[chicago.df$Primary.Type == "CONCEALED CARRY LICENSE VIOLATION"] <- "NON - VIOLENT"
chicago.df$Primary.Type[chicago.df$Primary.Type == "PUBLIC INDECENCY"] <- "NON - VIOLENT"


ggmap(chicago, extent = "device") +
	geom_density2d(data = chicago.df.small, aes(x = Longitude, y = Latitude), size = 0.1, color = "blue") +
	stat_density2d(data = chicago.df.small, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01,	 bins = 16, geom = "polygon") + 
	#scale_fill_gradient(low = "dark blue" ,high = "white") +
	scale_alpha(range = c(0.05, 0.5), guide = FALSE)
