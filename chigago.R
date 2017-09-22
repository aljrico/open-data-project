# Functions to deal with Chicago's crime data
# Authors: Cristian Estany <cresbabellpuig@gmail.com>
#          Alfredo Hernández <aldomann.designs@gmail.com>
#          Alejandro Jiménez <aljrico@gmail.com>

# Libraries ------------------------------------------------

library(tidyverse)
library(data.table)

# Read and clean raw data ----------------------------------

# All data (a few rows, for testing purposes)
# chicago.df <- read.csv(file="data/Crimes_-_2001_to_present_clean.csv", nrows = 200)

# Read interesting columns and save into a CSV
# chicago.df.clean <- fread("data/Crimes_-_2001_to_present.csv", sep = ",", header= TRUE, select = c(3,6,8,9,10,20,21))
# write_csv(chicago.df.clean, "data/Crimes_-_2001_to_present_clean.csv")

# Read cleaned data
chicago.df <- fread(file="data/Crimes_-_2001_to_present_clean.csv", sep = ",", header = TRUE) %>%
	rename(Primary.Type = `Primary Type` , Location.Description = `Location Description`)

# Working example ------------------------------------------

ggplot(chicago.df, aes(x = Longitude, y = Latitude)) +
	geom_point(aes(colour = Primary.Type))
