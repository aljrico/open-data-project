# Functions to deal with Chicago's crime data
# Authors: Cristian Estany <cresbabellpuig@gmail.com>
#          Alfredo Hernández <aldomann.designs@gmail.com>
#          Alejandro Jiménez <aljrico@gmail.com>

# Libraries ------------------------------------------------

library(tidyverse)
library(data.table)
library(lubridate)
library(ggmap)

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
	filter(is.na(Longitude) != TRUE) %>%
	mutate(Primary.Type = ifelse(grepl("NON", Primary.Type), "NON-CRIMINAL", Primary.Type))

unique(chicago.df$Primary.Type)

# NA test (only 1.3% of the reports are not geolocated)
# summary(chicago.df$Latitude)

# Filter an arbitrary range of years
chicago.df <- chicago.df %>%
	filter(year(Date) %in% 2001:2017)

# Smaller data frame for testing purposes
chicago.df.small <- chicago.df[1:2000,]

# Load map data --------------------------------------------

chicago <- get_map(location = "Chicago, Illinois", zoom = 11, source = "google")

chicago.map <- ggmap(chicago, base_layer = ggplot(
	aes(x = Longitude, y = Latitude),
	data = chicago.df.small))

# Maps -----------------------------------------------------

ggmap(chicago) +
	geom_point(data = chicago.df.small, aes(x = Longitude, y = Latitude, colour = Primary.Type)) +
	labs(x = "Longitude", y = "Latitude")

# Random plots ---------------------------------------------

# Histograms
ggplot(chicago.df %>% filter(Primary.Type == "THEFT" | Primary.Type == "BURGLARY")) +
	geom_bar(aes(wday(Date, label = T))) +
	facet_wrap(~ year(Date))

ggplot(chicago.df %>% filter(Primary.Type == "THEFT" | Primary.Type == "BURGLARY")) +
	geom_bar(aes(wday(Date, label = T)))

# Testing lubdridate
ggplot(chicago.df.small, aes(Date, Longitude)) +
	geom_point()

# Heatmaps -------------------------------------------------

chicago.map +
	geom_density2d(size = 0.3) +
	stat_density2d(aes(fill = ..level.., alpha = ..level..), size = 0.01,
								 bins = 16, geom = "polygon") +
	scale_fill_gradient(low = "green", high = "red") +
	scale_alpha(range = c(0, 0.3), guide = FALSE)
	# facet_wrap(~ day(chicago.df.small$Date))
