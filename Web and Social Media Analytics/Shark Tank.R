setwd("D:/PGP BABI/Web and social media analytics/Group Assignment")
mydata = read.csv("Shark+Tank+Companies.csv", stringsAsFactors=FALSE)

library(tm)
library(SnowballC)
library(wordcloud)

table(mydata$deal)

mydatacorpus = Corpus(VectorSource(mydata$description))

wordcloud(mydatacorpus,colors=rainbow(7),max.words=50)

#Convert to lower-case

mydatacorpus = tm_map(mydatacorpus, tolower)

#remove Punctuation

mydatacorpus = tm_map(mydatacorpus, removePunctuation)

#Remove stopwords

mydatacorpus = tm_map(mydatacorpus, removeWords, c(stopwords("english")))

#Stem document 

mydatacorpus = tm_map(mydatacorpus, stemDocument)

# Remove extra whitespaces
mydatacorpus = tm_map(mydatacorpus, stripWhitespace)

wordcloud(mydatacorpus)


##Step1 Create DTM
mydataDTM = DocumentTermMatrix(mydatacorpus)
mydatasparse = removeSparseTerms(mydataDTM, 0.995)
SharkSparse = as.data.frame(as.matrix(mydatasparse))

#Make all variable names R-friendly
colnames(SharkSparse) = make.names(colnames(SharkSparse))

##Step ii. Use "Deal" as dependent variable

SharkSparse$DV = mydata$deal
SharkSparse$DV=as.factor(SharkSparse$DV)

#Step iii. Build a CART model

library(rpart)
library(rpart.plot)

mydataCART = rpart(DV ~ ., data=SharkSparse, method="class")

prp(mydataCART,extra=2)

##CART prediction:

CARTpredict=predict(mydataCART, data= SharkSparse, type="class")
CARTpredict

table(SharkSparse$DV, CARTpredict)

##CART accuracy
CARTaccuracy = (213+112)/495
CARTaccuracy ##65.66 % is the CART model accuracy


##Step iv Build Logistic Regression model

mydataLogit=glm(DV~.,data=SharkSparse,family="binomial")

mydataPredict=predict(mydataLogit,data=SharkSparse,type="response")
table(SharkSparse$DV,mydataPredict>0.5)

#accuracy of logistic regression
Logitaccuracy = (135+115)/495
Logitaccuracy  ##50.5% is the logit accuracy

##Step v. Build Random Forest Model
library(randomForest)

RF=randomForest(DV~.,data=SharkSparse)

varImpPlot(RF)


##Step 2 Ratio Variable 

SharkSparse$ratio = mydata$askedFor/mydata$valuation


## Ratio on CART
mydataCARTratio = rpart(DV ~ ., data=SharkSparse, method="class")
prp(mydataCARTratio, extra=2)

##CART prediction with ratio
CARTratioPredict = predict(mydataCARTratio, data=SharkSparse, type="class")

table(SharkSparse$DV, CARTratioPredict)

##CART accuracy with ratio
CARTratioaccuracy = (171+156)/495
CARTratioaccuracy  ##66 % is the accuracy


##Ratio on Logistic Regression
mydatalogitRatio = glm(DV~., data = SharkSparse, family = "binomial")

##Logistic prediction with ratio:
LogitRatiopredict = predict(mydatalogitRatio, data=SharkSparse)
table(SharkSparse$DV, LogitRatiopredict>= 0.5)

##Accuracy for logistic ratio 
LogitRatioAccuracy = (244+251)/495
LogitRatioAccuracy
