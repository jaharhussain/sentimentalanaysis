library(tm)
library(wordcloud)
library(syuzhet)

#import data in R
reviews <-read.csv(file.choose(),header=T)

#check the structure of the file
str(reviews)
#creating corpus 
#this function used the base package function iconv to translate value labels into  a specified
corpus <-iconv(reviews$text)
corpus <-Corpus(VectorSource(corpus))

#to see the corpus 
inspect(corpus[1:5])

#cleaning corpus
corpus <-tm_map(corpus,tolower)

inspect(corpus[1:5])

corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])

corpus <-tm_map(corpus,removeNumbers)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeWords,stopwords("english"))
inspect(corpus[1:5])
corpus(tm_map(corpus,stripWhitespace))
inspect(corpus[1:5])

reviews_final<- corpus


#create term document
tdm<-TermDocumentMatrix(reviews_final)
tdm<-as.matrix(tdm)
tdm[1:10,1:5]


#bar plot of the words
w<-rowSums(tdm)
w<-subset(w,w>=25)
barplot(w,las=2,col="blue")

corplus<-tm_map(corpus,removeWords,c("buy","calls","button"))

#creat word cloud
w<-sort(rowSums(tdm),decreasing = T)
set.seed(2000)
wordcloud(words = names(w),
          freq = w,
          max.words = 50,
          random.order = T,
          min.freq = 5,
          colors = brewer.pal(25,"Dark2"),
          scale=c(3,0,3))

#obtain sebtiment scores
sentiment_data<- iconv(reviews$text)
s<-get_nrc_sentiment(sentiment_data)
s[1:10]

#calculate review wise score
s$score<-s$positive-s$negative
s[1:10]
#write score into a csv file
write.csv(x-s.file-"C:\Users\Altaf\Desktop\scores.csv")
#check product sentiment
#check overall sentiment of the product
review_score<-colSums(s[,])
print(review_score)

#plot product sentiment 
#bar plot
barplot(colSums(s),
        las=2,
        col= rainbow(10),
        ylab='count',
        main = 'sentiment')