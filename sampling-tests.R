# Functions to test the quality of the sampled data
# Authors: Cristian Estany <cresbabellpuig@gmail.com>
#          Alfredo Hernández <aldomann.designs@gmail.com>
#          Alejandro Jiménez <aljrico@gmail.com>

# Source base code -----------------------------------------
source("read_ucr.R")

# Libraries ------------------------------------------------

library(tidyverse)
library(spatstat)

# K tests functions ----------------------------------------
ct.src <- read_iucr_db("data/Crimes_-_2001_to_present_clean.csv")

get_k_function <- function(cat, sample = F, corr = "iso"){
	ct <- ct.src %>%
		filter(Category == toupper(cat)) %>%
		select(Longitude, Latitude)

	if(sample != F){
		ct <- ct %>%
			sample_n(sample)
	}

	ct <- as.matrix(ct)
	ct <- as.ppp(ct, c(-87.8,-87.4,41.7,42))

	ct.K <- Kest(ct, correction = corr)

	return(ct.K)
}

# Example of full K funct calculation
# write.csv(get_k_function("quality of life crime"), "k-tests/k-6m-qual-iso.csv")

# Plot K functions
plot_k_function <- function(df, my.title){
	ggplot(df) +
		geom_line(aes(x = r, y = theo), colour = "black", linetype = "solid") +
		geom_line(aes(x = r, y = iso), colour = "red", linetype = "longdash") +
		geom_line(aes(x = r, y = border), colour = "blue", linetype = "longdash") +
		labs(title = paste("K function for", my.title), x = "r", y = "K(r)")
}

# Read files -----------------------------------------------

# Property of life crimes (previously calculated)
K.prop <- read.csv("k-tests/k-6m-prop-bord.csv") %>%
	dplyr::select(r, theo, border) %>%
	mutate(iso = read.csv("k-tests/k-6m-prop-iso.csv")$iso)

# Violent of life crimes (previously calculated)
K.viol <- read.csv("k-tests/k-6m-viol-bord.csv") %>%
	dplyr::select(r, theo, border) %>%
	mutate(iso = read.csv("k-tests/k-6m-viol-iso.csv")$iso)

# Quality of life crimes (previously calculated)
K.qual <- read.csv("k-tests/k-6m-qual-bord.csv") %>%
	dplyr::select(r, theo, border) %>%
	mutate(iso = read.csv("k-tests/k-6m-qual-iso.csv")$iso)

# Sample K tests -------------------------------------------
samp.size = 50000

K.prop.samp <- get_k_function("property crime", samp.size)
K.viol.samp <- get_k_function("violent crime", samp.size)
K.qual.samp <- get_k_function("quality of life crime", samp.size)

plot_k_function(K.prop, "property crimes")
plot_k_function(K.viol, "violent crimes")
plot_k_function(K.qual, "quality of life crimes")

# Histograms -----------------------------------------------

library(reshape2)

a <- data.frame(label = c("a", "b", "c"), y1= c(1, 2, 3), y2 = c(4,5,6))
b <- melt(a)

ggplot(b) +
	geom_bar(aes(x = label, y= value, fill = variable), stat="identity", width=.5, position = "dodge")

