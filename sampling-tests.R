# Functions to deal with Chicago's crime data
# Authors: Cristian Estany <cresbabellpuig@gmail.com>
#          Alfredo Hernández <aldomann.designs@gmail.com>
#          Alejandro Jiménez <aljrico@gmail.com>

# Libraries ------------------------------------------------

library(tidyverse)
library(spatstat)

# K tests --------------------------------------------------

# xy <- matrix(runif(1000), ncol=2)
# pp <- as.ppp(xy, c(0,1,0,1))
# plot(Kest(pp))

ct <- chicago.df %>%
	# sample_n(500000) %>%
	# filter(Category == "QUALITY OF LIFE CRIME") %>%
	# filter(Category == "VIOLENT CRIME") %>%
	filter(Category == "PROPERTY CRIME") %>%
	select(Longitude, Latitude)

ct <- as.matrix(ct)
ct.pp <- as.ppp(ct, c(-87.8,-87.4,41.7,42))
# ct.pp <- as.ppp(ct, c(-91.7,-87.5,36.7,42.1))

system.time(ct.K <- Kest(ct.pp, correction = "Ripley"))
# system.time(ct.K <- Kest(ct.pp))

write.csv(ct.K, "kest6mqual-rip.csv")
write.csv(ct.K, "kest6mviol-rip.csv")
write.csv(ct.K, "kest6mprop-rip.csv")

# Plot K test function (Poisson)
plot(ct.K)
# ggplot(ct.K) +
# geom_line(aes(x = r, y = iso)) +
# geom_line(aes(x = r, y = theo), colour = "red", linetype = "longdash") +
# labs(title = "K function", x = "r", y = "K(r)")
