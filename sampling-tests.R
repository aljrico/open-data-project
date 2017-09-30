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
ct.src <- read_ucr_db("data/Crimes_-_2001_to_present_clean.csv")

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
		geom_line(aes(x = r, y = theo), colour = "black", linetype = "longdash") +
		geom_line(aes(x = r, y = iso), colour = "red", linetype = "solid") +
		labs(title = paste("K function for", my.title), x = "r", y = "K(r)")
}

plot_k_function_comp <- function(df, df.samp, my.title){
	ggplot() +
		geom_line(data = df.samp,
							aes(x = r, y = theo, linetype = "theo", colour = "sample")) +
		geom_line(data = df.samp,
							aes(x = r, y = iso, linetype = "iso", colour = "sample")) +
		geom_line(data = df,
							aes(x = r, y = theo.full, linetype = "theo", colour = "full")) +
		geom_line(data = df,
							aes(x = r, y = iso.full, linetype = "iso", colour = "full")) +
		scale_linetype_manual(values = c("theo" = "longdash", "iso" = "solid")) +
		labs(title = paste("K function for", my.title), x = "r", y = "K(r)")
}

# Read files -----------------------------------------------

# Property of life crimes (previously calculated)
K.prop <- read.csv("k-tests/k-6m-prop-iso.csv") %>%
	dplyr::select(r, theo.full = theo, iso.full = iso)

# Violent of life crimes (previously calculated)
K.viol <- read.csv("k-tests/k-6m-viol-iso.csv") %>%
	dplyr::select(r, theo.full = theo, iso.full = iso)

# Quality of life crimes (previously calculated)
K.qual <- read.csv("k-tests/k-6m-qual-iso.csv") %>%
	dplyr::select(r, theo.full = theo, iso.full = iso)

# plot_k_function(K.prop.samp, "property crimes")
# plot_k_function(K.viol, "violent crimes")
# plot_k_function(K.qual, "quality of life crimes")

# Sample K tests -------------------------------------------
samp.size = 500000

K.prop.samp <- get_k_function("property crime", samp.size)
K.viol.samp <- get_k_function("violent crime", samp.size)
K.qual.samp <- get_k_function("quality of life crime", samp.size)

plot_k_function_comp(K.prop, K.prop.samp, "property crimes")
plot_k_function_comp(K.viol, K.viol.samp, "violent crimes")
plot_k_function_comp(K.qual, K.qual.samp, "quality of life crimes")

# Histograms -----------------------------------------------

library(reshape2)

b <- melt(a)

ggplot(b) +
	geom_bar(aes(x = label, y= value, fill = variable), stat="identity", width=.5, position = "dodge")

#Prepare dummy dataframes to perform tests
test1 <- chicago.df %>%
	group_by(Primary.Type) %>%
	summarise(N=n())
test1 <- as.data.frame(test1)

#Normalizing the values of the dummy dataframes
sum <- sum(test1$N)
for(i in 1:length(test1$Primary.Type)){
	test1[i,2] <- test1[i,2]/sum
}
test2 <- chicago.df.small %>%
	group_by(Primary.Type) %>%
	summarise(N=n())
test2 <- as.data.frame(test2)
sum <- sum(test2$N)
for(i in 1:length(test2$Primary.Type)){
	test2[i,2] <- test2[i,2]/sum
}

# Merge dummy dataframes into one test sample.
test.sample <- merge(test1, test2, by='Primary.Type', all.x = TRUE)
test.sample[is.na(test.sample)] <- 0

# Melt the test sample. This way is ready for a more straight-forward ggplot
library(reshape)
mdata <- melt(test.sample, id=c("Primary.Type"))
colnames(mdata)[2] <-'samplenosample'

# Barplot of the distribution of both original and sampled data
ggplot(mdata, aes(x = Primary.Type, y = value, fill = samplenosample)) +
	geom_bar( alpha = 1, position ='dodge', stat = 'identity', width=.5) +
	coord_flip() +
	labs(title = "Original Data vs Sampled Data")

# Measure the difference of the distributions.
test.sample$diff <- abs((test.sample$N.x - test.sample$N.y)*test.sample$N.x)
ggplot(test.sample, aes(x= Primary.Type, y = diff)) +
	geom_bar(stat='identity', width=.5) +
	coord_flip()

# Are these two samples following theh same distribution? Kolmogorov-Smirnov test
ks.test(test.sample$N.x, test.sample$N.y)


