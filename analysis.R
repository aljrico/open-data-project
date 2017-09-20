# Base code for the 'Open Data project'
# Authors: Cristian Estany <cresbabellpuig@gmail.com>
#          Alfredo Hernández <aldomann.designs@gmail.com>
#          Alejandro Jiménez <aljrico@gmail.com>
#------------------------------------------------------------------

# Activate Packages
library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(tidyverse)
library(tidytext)
library(stringi)
	
# Declare Twitter API Credentials
api_key <- "PWjDRNdHLndOJewEKjNDDSBBI" # From dev.twitter.com
api_secret <- "vxfnBjGre7hKmexEvxnM6B3I9TX9dSFhWbvvCEXqb2QYfV03jI" # From dev.twitter.com
token <- "2340512892-AdOJv1ygQ7XLqbofSAKX6sI7f9y2fgQbqvMBYTX" # From dev.twitter.com
token_secret <- "sWzarXl0W8FewiSC51OMCwqnGZ6L2EQivKfUv4uy7Cpil" # From dev.twitter.com

# Create Twitter Connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)

# Query for tweets
tweets <- searchTwitter("#Interstellar", n=10000, lang="en", since="2014-08-20")
tweets.df <- twListToDF(tweets)


# Sentiment analysis
for(i in 1:length(tweets.df$text)){
	test.text <- tweets.df[i,1]

	test.text <- data.frame(word = unlist(stri_extract_all_words(stri_trans_tolower(test.text))))

	result <- test.text %>%
		inner_join(get_sentiments("afinn"))

	tweets.df$mark[i] <- sum(result$score)
}

#----------------------------------------------------------------------



## ## MISC ## ##

## Sentiment analysis example using get_sentiments("afinn").

test.text <- "A wonderful serenity has taken possession of my entire soul, like these sweet mornings of spring which I enjoy with my whole heart. I am alone, and feel the charm of existence in this spot, which was created for the bliss of souls like mine. I am so happy, my dear friend, so absorbed in the exquisite sense of mere tranquil existence, that I neglect my talents."

test.text <- data.frame(word = unlist(stri_extract_all_words(stri_trans_tolower(test.text))))

test.text %>%
	inner_join(get_sentiments("afinn"))
