# Functions to deal with Chicago's crime data
# Authors: Cristian Estany <cresbabellpuig@gmail.com>
#          Alfredo Hernández <aldomann.designs@gmail.com>
#          Alejandro Jiménez <aljrico@gmail.com>

# Libraries ------------------------------------------------

library(tidyverse)

# Read raw data --------------------------------------------

chicago.df <- read.csv(file="data/Crimes_-_2001_to_present.csv",nrows=200)

