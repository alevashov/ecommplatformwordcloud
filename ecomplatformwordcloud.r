#getting tweets part
# loading packages
library(twitteR)
library(ROAuth)


### fetching tweets ###

### this part needs to be loaded once, seeting up certificate
download.file(url = "http://curl.haxx.se/ca/cacert.pem",
              destfile = "cacert.pem")
setup_twitter_oauth('YOUR CONSUMER KEY', # api key
                    'YOUR CONSUMER SECRIT', # api secret
                    'YOUR ACCESS TOKEN', # access token
                    'YOUR ACCESS TOKEN SECRET' # access token secret
)
### end of the part that needs to be loaded once

# defining parameters 
term <- 'prestashop'
#can be more than one stopwords below, they used to exclude the terms from word cloud
mystopwords <- c('prestashop')

conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")


df_tweets <- twListToDF(searchTwitter(term, n = 2000, lang = 'en')) %>%
        # converting some symbols
        dmap_at('text', conv_fun)

#save to CSV file
filename <- paste0(term,'.csv')
write.csv(df_tweets, filename)


# visualisation 
# based on https://sites.google.com/site/miningtwitter/questions/talking-about/wordclouds/wordcloud1 

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

tmp <- read.csv(filename, stringsAsFactors = FALSE)

mach_text <- Corpus(VectorSource(tmp$text))

# create a corpus
mach_corpus = Corpus(VectorSource(mach_text))

# create document term matrix applying some transformations
tdm = TermDocumentMatrix(mach_corpus,
                         control = list(removePunctuation = TRUE,
                                        stopwords = c(mystopwords, "https", 'nhttps',stopwords("english")),
                                        removeNumbers = TRUE, tolower = TRUE))

# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# save the image in png format
imagename <- paste0(term,'cloud','.png')

png(imagename, width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, min.freq=3, max.word=100, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()
