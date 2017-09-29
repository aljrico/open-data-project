# Functions to test the quality of the sampled data
# Authors: Cristian Estany <cresbabellpuig@gmail.com>
#          Alfredo Hernández <aldomann.designs@gmail.com>
#          Alejandro Jiménez <aljrico@gmail.com>

# Libraries ------------------------------------------------

library(tidyverse)
library(spatstat)

# Full data K tests ----------------------------------------

# Read cleaned data
source("read_iucr.R")

ct <- read_iucr_db("data/Crimes_-_2001_to_present_clean.csv") %>%
	filter(Category == "PROPERTY CRIME") %>%
	# filter(Category == "VIOLENT CRIME") %>%
	# filter(Category == "QUALITY OF LIFE CRIME") %>%
	select(Longitude, Latitude)

ct <- as.matrix(ct)
ct <- as.ppp(ct, c(-87.8,-87.4,41.7,42))

# ct.K <- Kest(ct.pp, correction = "border")
ct.K <- Kest(ct.pp, correction = "iso")

# write.csv(ct.K, "k-tests/k-6m-qual-iso.csv")

# Quickly plot K function
# plot(ct.K)

# Read files -----------------------------------------------

# Property of life crimes
K.prop <- read.csv("k-tests/k-6m-prop-bord.csv") %>%
	dplyr::select(r, theo, border) %>%
	mutate(iso = read.csv("k-tests/k-6m-prop-iso.csv")$iso)

# Violent of life crimes
K.viol <- read.csv("k-tests/k-6m-viol-bord.csv") %>%
	dplyr::select(r, theo, border) %>%
	mutate(iso = read.csv("k-tests/k-6m-viol-iso.csv")$iso)

# Quality of life crimes
K.qual <- read.csv("k-tests/k-6m-qual-bord.csv") %>%
	dplyr::select(r, theo, border) %>%
	mutate(iso = read.csv("k-tests/k-6m-qual-iso.csv")$iso)

# Sample K tests -------------------------------------------

ct <- chicago.df %>%
	sample_n(500000) %>%
	filter(Category == "PROPERTY CRIME") %>%
	# filter(Category == "VIOLENT CRIME") %>%
	# filter(Category == "QUALITY OF LIFE CRIME") %>%
	select(Longitude, Latitude)


# Plots ----------------------------------------------------

# Plot K functions
plot_k_function <- function(df, my.title){
	ggplot(df) +
		geom_line(aes(x = r, y = theo), colour = "black", linetype = "solid") +
		geom_line(aes(x = r, y = iso), colour = "red", linetype = "longdash") +
		geom_line(aes(x = r, y = border), colour = "blue", linetype = "longdash") +
		labs(title = paste("K function for", my.title), x = "r", y = "K(r)")
}

plot_k_function(K.prop, "property crimes")
plot_k_function(K.viol, "violent crimes")
plot_k_function(K.qual, "quality of life crimes")

# Histograms -----------------------------------------------

library(reshape2)

a <- data.frame(label = c("a", "b", "c"), y1= c(1, 2, 3), y2 = c(4,5,6))
b <- melt(a)

ggplot(b) +
	geom_bar(aes(x = label, y= value, fill = variable), stat="identity", width=.5, position = "dodge")

