# Functions to deal with Chicago's crime data
# Authors: Cristian Estany <cresbabellpuig@gmail.com>
#          Alfredo Hernández <aldomann.designs@gmail.com>
#          Alejandro Jiménez <aljrico@gmail.com>

# Libraries ------------------------------------------------

library(tidyverse)
library(plyr)
library(data.table)
library(lubridate)
library(ggmap)

# Read and clean raw data ----------------------------------

# Download raw data
crimes.file = "data/Crimes_-_2001_to_present.csv"
if (!file.exists(crimes.file)) {
	# SRC: https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2
	download.file("https://data.cityofchicago.org/api/views/ydr8-5enu/rows.csv", destfile = crimes.file)
}

# Read interesting columns and save into a CSV
crimes.file.clean = "data/Crimes_-_2001_to_present_clean.csv"
if (file.exists(crimes.file) & !file.exists(crimes.file.clean)) {
	chicago.df <- fread(crimes.file, sep = ",", header= TRUE, select = c(3,6,9,18,20,21))%>%
		# Clean data names and
		dplyr::rename(PrimaryType = `Primary Type`) %>%
		filter(is.na(Longitude) != TRUE) %>%
		mutate(PrimaryType = ifelse(grepl("NON", PrimaryType), "NON-CRIMINAL", PrimaryType)) %>%
		# Create meta crime category
		mutate(Category = PrimaryType) %>%
		mutate(Category = plyr::mapvalues(Category,
																			dummy <- c("ARSON", "BURGLARY", "CRIMINAL DAMAGE", "CRIMINAL TRESPASS",
																								 "MOTOR VEHICLE THEFT", "THEFT"),
																			c(rep("PROPERTY CRIME", length(dummy))))) %>%
		mutate(Category = plyr::mapvalues(Category,
																			dummy <- c("ASSAULT", "BATTERY", "CRIM SEXUAL ASSAULT", "DOMESTIC VIOLENCE",
																								 "HOMICIDE", "HUMAN TRAFFICKING", "INTIMIDATION", "KIDNAPPING",
																								 "ROBBERY", "SEX OFFENSE", "STALKING", "WEAPONS VIOLATION"),
																			c(rep("VIOLENT CRIME", length(dummy))))) %>%
		mutate(Category = plyr::mapvalues(Category,
																			dummy <- c("CONCEALED CARRY LICENSE VIOLATION", "DECEPTIVE PRACTICE", "GAMBLING",
																								 "INTERFERENCE WITH PUBLIC OFFICER", "LIQUOR LAW VIOLATION", "NARCOTICS",
																								 "NON-CRIMINAL", "OBSCENITY", "OFFENSE INVOLVING CHILDREN",
																								 "OTHER NARCOTIC VIOLATION",  "OTHER OFFENSE", "PROSTITUTION",
																								 "PUBLIC INDECENCY", "PUBLIC PEACE VIOLATION", "RITUALISM"),
																			c(rep("QUALITY OF LIFE CRIME", length(dummy))))) %>%
		dplyr::select(PrimaryType, Category, Date, Year, Arrest, Longitude, Latitude)

	write_csv(chicago.df, crimes.file.clean)
	rm(chicago.df)
}

rm(crimes.file, crimes.file.clean)

# NA test (only 1.3% of the reports are not geolocated)
# summary(chicago.df$Latitude)

# Read already cleaned data --------------------------------

# Read cleaned data
chicago.df <- fread(file="data/Crimes_-_2001_to_present_clean.csv", sep = ",", header = TRUE)

# Use proper lubridate format
chicago.df <- chicago.df %>%
	mutate(Date = mdy_hms(Date))

# Filter an arbitrary range of years
# chicago.df <- chicago.df %>%
	# filter(Year %in% 2010:2017)

# Smaller data frame for testing purposes
chicago.df.small <- chicago.df %>%
	sample_n(500000)

# Summarised data frame
chicago.by.cat <- chicago.df.small %>%
	group_by(Category, Year, Month = month(Date)) %>%
	dplyr::summarise(N = n()) %>%
	mutate(Date = ymd(paste(Year, Month, 1))) %>%
	ungroup() %>%
	select(-c(Year, Month))

# Chicago Tribune-like plot
ggplot(chicago.by.cat) +
	geom_line(aes(x = Date, y = N, colour = Category))

# Load map data --------------------------------------------

chicago <- get_map(location = "Chicago, Illinois", zoom = 11, source = "google")

chicago.map <- ggmap(chicago, base_layer = ggplot(
	aes(x = Longitude, y = Latitude),
	data = chicago.df))

# Maps -----------------------------------------------------

ggmap(chicago) +
	geom_point(data = chicago.df.small, aes(x = Longitude, y = Latitude, colour = Category)) +
	labs(x = "Longitude", y = "Latitude") +
	facet_wrap(~ Year)

# Random plots ---------------------------------------------

# Histograms
ggplot(chicago.df %>% filter(PrimaryType == "THEFT" | PrimaryType == "BURGLARY")) +
	geom_bar(aes(wday(Date, label = T)))

ggplot(chicago.df %>% filter(PrimaryType == "THEFT" | PrimaryType == "BURGLARY")) +
	geom_bar(aes(wday(Date, label = T)))

# Testing lubdridate
ggplot(chicago.df.small, aes(Date, Longitude)) +
	geom_point()

# Heatmaps -------------------------------------------------

ggmap(chicago, base_layer = ggplot(aes(x = Longitude, y = Latitude), data = chicago.df)) +
	geom_density2d(size = 0.3) +
	stat_density2d(aes(fill = ..level.., alpha = ..level..), size = 0.01,
								 bins = 16, geom = "polygon") +
	scale_fill_gradient(low = "green", high = "red") +
	scale_alpha(range = c(0, 0.3), guide = FALSE)

+
	facet_wrap(~ Year)
