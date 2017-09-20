# Base code for the Open Data project
# Authors: Cristian Estany <cresbabellpuig@gmail.com>
#          Alfredo Hernández <aldomann.designs@gmail.com>
#          Alejandro Jiménez <aljrico@gmail.com>

library(tidyverse)
library(tidytext) # Loads tidytetx
library(stringi)

# Sentiment analysis test ----------------------------------

# get_sentiments("afinn")

test.text <- "A wonderful serenity has taken possession of my entire soul, like these sweet mornings of spring which I enjoy with my whole heart. I am alone, and feel the charm of existence in this spot, which was created for the bliss of souls like mine. I am so happy, my dear friend, so absorbed in the exquisite sense of mere tranquil existence, that I neglect my talents."

test.text <- data.frame(word = unlist(stri_extract_all_words(stri_trans_tolower(test.text))))

test.text %>%
	inner_join(get_sentiments("afinn"))
