# Base code for the Open Data project
# Authors: Cristian Estany <cresbabellpuig@gmail.com>
#          Alfredo Hernández <aldomann.designs@gmail.com>
#          Alejandro Jiménez <aljrico@gmail.com>

library(tidyverse)
library(tidytext) # Loads tidytetx

# Sentiment analysis test ----------------------------------

# The AFINN lexicon assigns words with a score that runs between -5 and 5, with negative scores indicating negative sentiment and positive scores indicating positive sentiment
get_sentiments("afinn")

# test Crs
