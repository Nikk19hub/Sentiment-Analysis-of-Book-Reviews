library(tm)
library(wordcloud)
library(syuzhet)


reviews <- read.csv(file.choose(),header = T)

str(reviews)


corpus <- iconv(reviews$text)
corpus <- Corpus(VectorSource(corpus))

inspect(corpus[1:5])

corpus <- tm_map(corpus, tolower)

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeNumbers)

corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, removeWords, c("book","read","life"))

corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus[1:5])

reviews_final <- corpus

tdm <- TermDocumentMatrix(reviews_final)
tdm <- as.matrix(tdm)
tdm[1:10, 1:5]

w <- rowSums(tdm)
w <- subset(w, w>=300)
barplot(w, las = 2,col = "blue")

w <- sort(rowSums(tdm),decreasing = T)
set.seed(8000)
wordcloud(words = names(w),
          freq = w,
          max.words = 50,
          random.order = T,
          min.freq = 5,
          colors = brewer.pal(25,"Dark2"),
          scale = c(3,0.3))

sentiment_data <- iconv(reviews$text)
s <- get_nrc_sentiment(sentiment_data)
s[1:10,]

s$score <- s$positve - s$negative
s[1:10,]

write.csv(x = s,file ="C:/Users/admin/Desktop/Final_score2.csv")


review_score <- colSums(s[,])
print(review_score)

barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment')









