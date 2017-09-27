# Functions to deal with Chicago's crime data
# Authors: Cristian Estany <cresbabellpuig@gmail.com>
#          Alfredo Hernández <aldomann.designs@gmail.com>
#          Alejandro Jiménez <aljrico@gmail.com>

# Libraries ------------------------------------------------
library(censusapi)

# API authentication ---------------------------------------

# write.csv(apis, file = "api-list.txt")
# View(apis)
# Add key to .Renviron
# Sys.setenv(CENSUS_KEY="4bb25fa0e03987fb845d77f69f545f133b2c2131")
# Reload .Renviron
# readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
# Sys.getenv("CENSUS_KEY")

CENSUS_KEY="4bb25fa0e03987fb845d77f69f545f133b2c2131"

# apis <- listCensusApis()

local.apis <- read.csv("api-list.txt")

# Testing --------------------------------------------------


