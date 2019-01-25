require(quanteda)
require(RColorBrewer)
require(dplyr)
require(ggplot2)
require(pROC)
yelp<-read.csv("C:/Users/petlogic/Desktop/data mining/project/yelp.csv", stringsAsFactors = FALSE)
table(yelp$category)
theme_set(theme_bw())
ggplot(aes(x=category), data = yelp) + geom_bar(fill="blue", width = 0.5)
set.seed(2012)
yelp<-yelp[sample(nrow(yelp)),]
?corpus
comm.corpus<-corpus(yelp$text)
docvars(comm.corpus)<-yelp$category
?dfm
bad.plot <-corpus_subset(comm.corpus, docvar1=="bad")
bad.plot<-dfm(bad.plot, tolower = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_numbers = TRUE, remove=stopwords("SMART"))
bad.col<-brewer.pal(10, "BrBG")
textplot_wordcloud(bad.plot, min.freq = 16, color = bad.col)  
title("Bad Review Wordcloud", col.main = "grey14")

good.plot <-corpus_subset(comm.corpus, docvar1=="good")
good.plot<-dfm(good.plot, tolower = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_numbers = TRUE, remove=stopwords("SMART"))
good.col<-brewer.pal(10, "BrBG")
textplot_wordcloud(good.plot, min.freq = 16, color = good.col)  
title("Good Review Wordcloud", col.main = "grey14")

features_bad <- textstat_frequency(bad.plot, n = 100)
features_bad$feature <- with(features_bad, reorder(feature, -frequency))


ggplot(features_bad, aes(x = feature, y = frequency)) +
  geom_point(color="red") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Frequency plot of most frequently occuring words in bad reviews")

features_good <- textstat_frequency(good.plot, n = 100)
features_good$feature <- with(features_good, reorder(feature, -frequency))


ggplot(features_good, aes(x = feature, y = frequency)) +
  geom_point(color="red") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Frequency plot of most frequently occuring words in good reviews")


#separating Train and test data
yelp.train<-yelp[1:8500,]
yelp.test<-yelp[8500:nrow(yelp),]

text.dfm <- dfm(comm.corpus, tolower = TRUE)  #generating document freq matrix
text.dfm <- dfm_trim(text.dfm, min_count = 5, min_docfreq = 3)  
text.dfm <- dfm_weight(text.dfm) 

#trining and testing data of dfm 
text.dfm.train<-text.dfm[1:8500,]

text.dfm.test<-text.dfm[8500:nrow(yelp),]

nb.classifier<-textmodel_nb(text.dfm.train,yelp.train[,1])
nb.classifier

pred<-predict(nb.classifier,text.dfm.test)


table1<-table(predicted=pred$nb.predicted,actual=yelp.test[,1])
View(table1)

acc_nb=mean(pred$nb.predicted==yelp.test[,1])*100

prednum<-ifelse(pred$nb.predicted=="bad",1,2)

auc<-roc(as.factor(yelp.test[,1]),prednum)
plot(auc)
auc$auc
