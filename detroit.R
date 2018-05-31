# Functions to deal with Detroit's crime data
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
library(dgof)

# Mining Data ------------------------------------------------

# Download raw data
detroit.crimes.file = "data/detroit_Crimes_-_2001_to_present.csv"
if (!file.exists(detroit.crimes.file)) {
	# SRC: https://data.detroitmi.gov/Public-Safety/DPD-Reported-Major-Crimes-2011-2014/75yu-k3gj
	download.file("https://data.detroitmi.gov/api/views/75yu-k3gj/rows.csv", destfile = detroit.crimes.file)
}

# Read interesting columns and save into a CSV
detroit.crimes.file.clean = "data/detroit_Crimes_-_2001_to_present_clean.csv"
if (file.exists(detroit.crimes.file) & !file.exists(detroit.crimes.file.clean)) {
	detroit.df.clean <- fread(detroit.crimes.file, sep = ",", header= TRUE)
	write_csv(detroit.df.clean, detroit.crimes.file.clean)
}
rm(detroit.crimes.file, detroit.crimes.file.clean)

# Read cleaned data
detroit.df <- fread(file="data/detroit_Crimes_-_2001_to_present_clean.csv", sep = ",", header = TRUE, select = c(2, 5,9,11))

# Merge crime types in more general categories
detroit.df <- detroit.df %>% mutate(Category = ifelse(CATEGORY == "LARCENY" | CATEGORY == "BURGLARY" | CATEGORY == "LIQUOR LAW VIOLATION" | CATEGORY == "STOLEN VEHICLE" | CATEGORY == "ARSON" | CATEGORY == "CRIMINAL DAMAGE", "PROPERTY CRIMES", ifelse(CATEGORY == "BATTERY" | CATEGORY == "ROBBERY" | CATEGORY=="ASSAULT" | CATEGORY =="AGGRAVATED ASSAULT" | CATEGORY == "SEX OFFENSE" | CATEGORY == "STALKING" | CATEGORY == "KIDNAPPING" | CATEGORY == "HOMICIDE" | CATEGORY == "INTIMIDATION" | CATEGORY == "HUMAN TRAFFICKING", "VIOLENT CRIMES", "QUALITY OF LIFE CRIMES")))

count(detroit.df[detroit.df$Category == 'PROPERTY CRIMES',])
