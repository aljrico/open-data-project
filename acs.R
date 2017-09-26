# Functions to deal with Chicago's crime data
# Authors: Cristian Estany <cresbabellpuig@gmail.com>
#          Alfredo Hernández <aldomann.designs@gmail.com>
#          Alejandro Jiménez <aljrico@gmail.com>

# Libraries ------------------------------------------------
library(acs)

# API authentication ---------------------------------------

# api.key.install("4bb25fa0e03987fb845d77f69f545f133b2c2131")


# Tests ----------------------------------------------------
lots.o.data=acs.fetch(endyear = 2016, geo=geo.make(state="WA", county=c(33,35,53,61), tract="*"), table.number="B05006")

illinois=geo.make(state="IL")

chic.il = geo.make(state = "IL", county=c(41.7,42,-87.4,87.8))
