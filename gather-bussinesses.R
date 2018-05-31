
# Gathering data from Legally Operating Businesses ------------------------

library('tidyverse')
library('data.table')
library('ggmap')

url <- 'https://data.cityofnewyork.us/api/views/w7w3-xahh/rows.csv?accessType=DOWNLOAD'

# Download raw data
bussiness.file = "data/legally-operating-businesses.csv"
if (!file.exists(bussiness.file)) {
	download.file(url, destfile = bussiness.file)
}
rm(url)

# Read raw data
buss.raw <- fread(bussiness.file, sep = ",", header= TRUE)

# Delete NAs
buss <- buss.raw[complete.cases(buss.raw[ , 16:17]),]
rm(buss.raw)

# Simple map
nycmap <- get_map(location = 'New York City', zoom = 11)

# Awesome Heatmap
ggmap(nycmap, extent = "device") +
	geom_density2d(data = buss, aes(x = Longitude, y = Latitude), size = 0.5, color = "grey") +
	stat_density2d(data = buss, aes(x = Longitude, y = Latitude, alpha = ..level..), size = 0.01,	 bins = 16, geom = "polygon", fill='red') + 
	scale_alpha(range = c(0.001, 0.8), guide = FALSE) +
	labs(x=NULL, y=NULL, title="Legally Operating Bussinesses\n") +
	theme(strip.background=element_rect(fill="white", color="white")) +
	theme(strip.text=element_text(face="bold", hjust=0))
