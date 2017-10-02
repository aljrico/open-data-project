# Functions to deal with Chicago's crime data
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
crimes.file = "data/Crimes_-_2001_to_present.csv"
if (!file.exists(crimes.file)) {
	# SRC: https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2
	download.file("https://data.cityofchicago.org/api/views/ydr8-5enu/rows.csv", destfile = crimes.file)
}

# Read interesting columns and save into a CSV
crimes.file.clean = "data/Crimes_-_2001_to_present_clean.csv"
if (file.exists(crimes.file) & !file.exists(crimes.file.clean)) {
	chicago.df.clean <- fread(crimes.file, sep = ",", header= TRUE, select = c(3,6,9,18,20,21))
	write_csv(chicago.df.clean, crimes.file.clean)
}
rm(crimes.file, crimes.file.clean)

# Read cleaned data
chicago.df <- fread(file="data/Crimes_-_2001_to_present_clean.csv", sep = ",", header = TRUE)

# Clean formats and column names
chicago.df <- chicago.df %>%
	dplyr::rename(Primary.Type = `PrimaryType`) %>%
	mutate(Date = mdy_hms(Date)) %>%
	filter(is.na(Longitude) != TRUE) %>%
	mutate(Primary.Type = ifelse(grepl("NON", Primary.Type), "NON-CRIMINAL", Primary.Type))

# Exploratory Analysis: We can see the incidence of every crime type.
sort(table(chicago.df$Primary.Type)) 

# Merge crime types in more general categories
chicago.df <- chicago.df %>% mutate(Category = ifelse(Primary.Type == "THEFT" | Primary.Type == "BURGLARY" | Primary.Type == "LIQUOR LAW VIOLATION" | Primary.Type == "MOTOR VEHICLE THEFT" | Primary.Type == "ARSON" | Primary.Type == "CRIMINAL DAMAGE", "PROPERTY CRIMES", ifelse(Primary.Type == "BATTERY" | Primary.Type == "ROBBERY" | Primary.Type=="ASSAULT" | Primary.Type =="CRIM SEXUAL ASSAULT" | Primary.Type == "SEX OFFENSE" | Primary.Type == "STALKING" | Primary.Type == "KIDNAPPING" | Primary.Type == "HOMICIDE" | Primary.Type == "INTIMIDATION" | Primary.Type == "HUMAN TRAFFICKING", "VIOLENT CRIMES", "QUALITY OF LIFE CRIMES")))


# Filter an arbitrary range of years
chicago.df <- chicago.df %>%
	filter(year(Date) %in% 2001:2016)

# Sumarized data frame per crime typology
mcrimes <- chicago.df %>% 
	group_by(arrested = Category, year = year(Date), month = month(Date)) %>% 
	summarise(N=n())
mcrimes$date <- ymd(paste(mcrimes$year, mcrimes$month, 1))
mcrimes <- mcrimes[c(1,4,5)]

# Sumarized data frame per arrest
marrested <- chicago.df %>% 
	filter(Arrest == 'true') %>% 
	group_by(Category, year = year(Date), month = month(Date)) %>% 
	summarise(Narrested=n())

mnonarrested <- chicago.df %>% 
	filter(Arrest =='false') %>% 
	group_by(Category, year=year(Date), month = month(Date)) %>% 
	summarise(Nnonarrested=n())

marrest <- merge(marrested, mnonarrested, all.x = TRUE)
marrest$date <- ymd(paste(marrest$year, marrest$month, 1))
marrest$clearance <- marrest$Narrested/(marrest$Nnonarrested + marrest $Narrested)

# Sampling ------------------------------------------------

# Smaller data frame for testing purposes
samp.size <- 200000
chicago.df.small <- data.table(chicago.df)
chicago.df.small <- as.data.frame(chicago.df.small[sample(.N, samp.size)])

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

# Maps -----------------------------------------------------

# Simple map using a sample
chicago <- get_map(location = 'chicago', zoom = 11)

ggmap(chicago) +
	geom_point(data = chicago.df.small, aes(x = Longitude, y = Latitude, colour = Primary.Type)) +
	labs(x = "Longitude", y = "Latitude")

# Awesosme heatmap
ggmap(chicago, extent = "device") +
	geom_density2d(data = chicago.df.small, aes(x = Longitude, y = Latitude), size = 0.5, color = "grey") +
	stat_density2d(data = chicago.df.small, aes(x = Longitude, y = Latitude, fill = Category, alpha = ..level..), size = 0.01,	 bins = 16, geom = "polygon") + 
	#scale_fill_gradient(low = "dark blue" ,high = "white") +
	scale_alpha(range = c(0.05, 0.5), guide = FALSE) +
	labs(x=NULL, y=NULL, title="Crime Distribution Heatmap\n") +
	theme(panel.margin.y=unit(0.5, "cm")) +
	theme(strip.background=element_rect(fill="white", color="white")) +
	theme(strip.text=element_text(face="bold", hjust=0)) +
	facet_wrap(~ Category)

# Point map
ggmap(chicago) +
	geom_point(data = chicago.df.small, aes(x = Longitude, y = Latitude, colour = as.factor(year(Date)))) +
	labs(x = "Longitude", y = "Latitude")+
	facet_wrap(~ Category)

# Random plots ---------------------------------------------

# Histogram
ggplot(chicago.df %>% filter(Primary.Type == "CRIM SEXUAL ASSAULT" | Primary.Type == "STALKING" | Primary.Type == "SEX OFFENSES")) +
	geom_histogram(aes(year(Date)), binwidth = 0.5)

ggplot(chicago.df) +
	geom_histogram(aes(year(Date)), binwidth = 0.5)

# Historical histogram per category
ggplot(chicago.df) +
	geom_histogram(aes(year(Date)), binwidth = 0.5) +
	facet_wrap(~ Category)

# Historical plot
ggplot(data=mcrimes, aes(y= N, x=date, color=Category)) +
	theme_bw() +
	geom_line(size=1, lineed="round") +
	theme(axis.text.x = element_text(face = "bold")) +
	theme(axis.text.y = element_text(face = "bold"))

# Monthly histogram per category
ggplot(chicago.df) +
	geom_histogram(aes(month(Date)), binwidth = 0.5) +
	facet_wrap(~ Category)

# Daily histogram per category
ggplot(chicago.df) +
	geom_histogram(aes(day(Date)), binwidth = 0.5) +
	facet_wrap(~ Category)

# Hourly histogram per category
ggplot(chicago.df) +
	geom_histogram(aes(hour(Date)), binwidth = 0.5) +
	facet_wrap(~ Category)

# My old lady goes to church histogram per category
ggplot(chicago.df %>%  filter(weekdays(Date) == "Sunday")) +
	geom_histogram(aes(hour(Date)), binwidth = 0.5) +
	facet_wrap(~ Category)

# Hourly histogram per category and day
ggplot(chicago.df) +
	geom_histogram(aes(hour(Date)), binwidth = 0.5) +
	facet_grid(weekdays(Date) ~ Category)

# Histogram about Arrest
ggplot(chicago.df, aes(x = year(Date), fill = Arrest)) + 
	geom_histogram(binwidth = 0.5, position='fill')

# Plot about clearance rate
ggplot(marrest, aes(y = clearance,x = date, color = Category)) + 
	geom_point()

# Sunday Truce ---------------------------------------------

# Creating a new dataframe summarizing crimes by its weekday
struce <- chicago.df %>% 
	group_by(Category, wday = weekdays(Date)) %>% 
	summarise(N=n())

weekday <- data.frame()

i=1
for( day in unique(struce$wday)){
weekday[i, 'mean'] <- mean(struce$N[struce$wday == day])
weekday[i, 'dev'] <- sd(struce$N[struce$wday ==day])/sqrt(length(struce$N[struce$wday == day]))
weekday[i, 'name'] <- day
i = i+1
}

weekday$name <- factor(weekday$name, levels= c("Monday", 
 "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

weekday[order(weekday$name), ]

ggplot(struce, aes(x = wday, y = N)) +
	geom_boxplot()

ggplot(weekday, aes(x = name, y = mean, ymax = mean + dev, ymin = mean - dev)) +
	geom_point() +
	geom_errorbar()

for (wday in unique(struce$wday)){
t.test(struce$N[struce$wday == 'Sunday'], struce$N[struce$wday == wday])
}