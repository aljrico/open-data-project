# Functions to test the quality of the sampled data
# Authors: Cristian Estany <cresbabellpuig@gmail.com>
#          Alfredo Hernández <aldomann.designs@gmail.com>
#          Alejandro Jiménez <aljrico@gmail.com>

# Libraries ------------------------------------------------

library(tidyverse)
library(spatstat)

# Full data K tests ----------------------------------------

ct <- chicago.df %>%
	filter(Category == "PROPERTY CRIME") %>%
	# filter(Category == "VIOLENT CRIME") %>%
	# filter(Category == "QUALITY OF LIFE CRIME") %>%
	select(Longitude, Latitude)

ct <- as.matrix(ct)
ct <- as.ppp(ct, c(-87.8,-87.4,41.7,42))

# ct.K <- Kest(ct.pp, correction = "border")
ct.K <- Kest(ct.pp, correction = "iso")

# write.csv(ct.K, "kest6mqual-rip.csv")

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
plot_k_function <- function(df){
	ggplot(df) +
		geom_line(aes(x = r, y = theo), colour = "black", linetype = "solid") +
		geom_line(aes(x = r, y = iso), colour = "red", linetype = "longdash") +
		geom_line(aes(x = r, y = border), colour = "blue", linetype = "longdash") +
		labs(title = "K function", x = "r", y = "K(r)")
}

plot_k_function(K.prop)
plot_k_function(K.viol)
plot_k_function(K.qual)

# Histograms -----------------------------------------------


