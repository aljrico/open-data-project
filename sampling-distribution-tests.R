# Functions to test the quality of the sampled data
# Authors: Cristian Estany <cresbabellpuig@gmail.com>
#          Alfredo Hernández <aldomann.designs@gmail.com>
#          Alejandro Jiménez <aljrico@gmail.com>

# Source base code -----------------------------------------
source("read_ucr.R")

# Libraries ------------------------------------------------

library(tidyverse)
library(spatstat)
library(reshape2)


# Histograms -----------------------------------------------

#Prepare dummy dataframes to perform tests
chicago.df <- read_ucr_db("data/Crimes_-_2001_to_present_clean.csv")
chicago.df.small <- sample_n(chicago.df, 200000)

test1 <- chicago.df %>% 
	dplyr::group_by(PrimaryType) %>% 
	dplyr::summarise(N=n())
test1 <- as.data.frame(test1)

#Normalizing the values of the dummy dataframes
sum <- sum(test1$N)
for(i in 1:length(test1$PrimaryType)){
	test1[i,2] <- test1[i,2]/sum
}
test2 <- chicago.df.small %>% 
	dplyr::group_by(PrimaryType) %>% 
	dplyr::summarise(N=n())
test2 <- as.data.frame(test2)
sum <- sum(test2$N)
for(i in 1:length(test2$PrimaryType)){
	test2[i,2] <- test2[i,2]/sum
}

# Merge dummy dataframes into one test sample. 
test.sample <- merge(test1, test2, by='PrimaryType', all.x = TRUE)
test.sample[is.na(test.sample)] <- 0

# Melt the test sample. This way is ready for a more straight-forward ggplot
library(reshape)
mdata <- melt(test.sample, id=c("PrimaryType"))
colnames(mdata)[2] <-'samplenosample'

# Barplot of the distribution of both original and sampled data
ggplot(mdata, aes(x = PrimaryType, y = value, fill = samplenosample)) +
	geom_bar( alpha = 1, position ='dodge', stat = 'identity', width=.5) +
	coord_flip() +
	labs(title = "Original Data vs Sampled Data")

# Measure the difference of the distributions.
test.sample$diff <- abs((test.sample$N.x - test.sample$N.y)*test.sample$N.x)
ggplot(test.sample, aes(x= PrimaryType, y = diff)) + 
	geom_bar(stat='identity', width=.5) +
	coord_flip()

# Are these two samples following theh same distribution? Kolmogorov-Smirnov test
ks.test(test.sample$N.x, test.sample$N.y)


