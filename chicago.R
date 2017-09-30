# Functions to deal with Chicago's crime data
# Authors: Cristian Estany <cresbabellpuig@gmail.com>
#          Alfredo Hernández <aldomann.designs@gmail.com>
#          Alejandro Jiménez <aljrico@gmail.com>

# Source base code -----------------------------------------
source("read_ucr.R")

# Libraries ------------------------------------------------

library(lubridate)
library(ggmap)

# Read and organise UCR data -------------------------------

chicago.df <- read_ucr_db("data/Crimes_-_2001_to_present_clean.csv")

# Use proper lubridate format
chicago.df <- chicago.df %>%
	mutate(Date = mdy_hms(Date))

# Filter an arbitrary range of years
chicago.df <- chicago.df %>%
	filter(Year %in% 2001:2016)

# Smaller data frame for testing purposes
chicago.df.small <- chicago.df %>%
	sample_n(50000)

# Summarised data frame for time series
chicago.by.cat <- chicago.df %>%
	group_by(Category, Year, Month = month(Date)) %>%
	dplyr::summarise(N = n()) %>%
	mutate(Date = ymd(paste(Year, Month, 1))) %>%
	ungroup() %>%
	select(-c(Year, Month))

# Load ggmap data ------------------------------------------

chicago <- get_map(location = "Chicago, Illinois", zoom = 11, source = "google")

# ggmap object with aes() properties
# chicago.map <- ggmap(chicago, base_layer = ggplot(
# 	aes(x = Longitude, y = Latitude),
# 	data = chicago.df))

# Data visualisation ---------------------------------------

# Chicago Tribune-like plot
ggplot(chicago.by.cat) +
	geom_line(aes(x = Date, y = N, colour = Category))

# Heatmaps
ggmap(chicago, base_layer = ggplot(aes(x = Longitude, y = Latitude), data = chicago.df.small)) +
	geom_density2d(size = 0.3) +
	stat_density2d(aes(fill = ..level.., alpha = ..level..), size = 0.01,
								 bins = 16, geom = "polygon") +
	scale_fill_gradient(low = "green", high = "red") +
	scale_alpha(range = c(0, 0.3), guide = FALSE) +
	facet_wrap(~ Category)

