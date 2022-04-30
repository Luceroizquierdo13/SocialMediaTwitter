install.packages("twitteR")
install.packages("RCurl")
install.packages("tm")
install.packages("wordcloud")
install.packages("SnowballC")

library("twitteR")
library("RCurl")
library("tm")
library("wordcloud")
library("SnowballC")
library("dendextend") # color dendrogram


consumer_key <- 'm3ITApJG637fiWCz9IVnf9Zmz'
consumer_secret <- 'muPPtAzkkerETj0dPd8Rj52R7uqscfKujLtWaMkc49mzXap5Lh'
access_token <- '453157501-hWWsz4BzpfLDRICpaTaakQcvHQ3SBMZIM43PC0d5'
access_token_secret <- 'KRX6rb9t5gOTqwz9zVydz9kuflCTePhmqjnJ9vOeqlRhC'

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_token_secret)

starbucks_tweets <- searchTwitter("starbucks", n=500, lang = "en")


# save text
starbucks_tweets_text <- sapply(starbucks_tweets, function(x) x$getText() )
starbucks_tweets_text

# The below would be the same
# Analyze review
docs <- Corpus(VectorSource(starbucks_tweets_text))


starbucks_tweets_text2 <- sapply(starbucks_tweets_text,function(row) iconv(row,"latin1","ASCII",""))
docs <- Corpus(VectorSource(starbucks_tweets_text2))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("https", "tco")) 
docs <- tm_map(docs, removeWords, c("tco")) 
docs <- tm_map(docs, removeWords, c("t.co")) 

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Text stemming
docs <- tm_map(docs, stemDocument)

# Word counts
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))
#ggsave("wordcloud.png", plot = pic)

# association
findFreqTerms(dtm) # get frequency
# association of words
findAssocs(dtm,'union',0.2)

# draw dendrogram
dtm_top <- removeSparseTerms(dtm,sparse=0.90) # distance between .89 and .9
#df <- as.data.frame(inspect(dtm_top))
df <- as.matrix(dtm_top)
dscale <- scale(df)
distance <- dist(dscale, method="euclidean") # distance matrix
fit2 <- hclust(distance, method="complete")
#fit <- color_branches(fit, k = 3)
#fit <- color_labels(fit, k = 3)
plot(fit2,main = 'Starbucks Dendrogram', ylab = 'Height')
#ggsave("dendogram.png", plot = dendogram)

#barplot
# Plot frequent terms
freq <- rowSums(df)
freq <- subset(freq, freq>=50)
barplot<-barplot(freq, las=2#, col = rainbow(25)
        )

#ggsave("barplot.png", plot = barplot)
