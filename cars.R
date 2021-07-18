setwd("D:/PGP BABI/Machine Learning/Group Assignment")
mydata=read.csv("Cars.csv",header=T)
str(mydata)

##converting as factors
mydata$Engineer=as.factor(mydata$Engineer)
mydata$MBA=as.factor(mydata$MBA)
mydata$license=as.factor(mydata$license)

library(DMwR)
mydata=knnImputation(mydata)

library(car)
mydata$Usage=ifelse(mydata$Transport=="Car",1,0)
table(mydata$Usage)
View(mydata)
mydata$Usage=as.factor(mydata$Usage)
mydata$Gender=ifelse(mydata$Gender=="Male",1,0)
table(mydata$Gender)
mydata$Gender=as.factor(mydata$Gender)


##Partition Data
library(caret)
set.seed(1234)
pd<-sample(2,nrow(mydata),replace=TRUE, prob=c(0.7,0.3))

train=mydata[pd==1,]
val=mydata[pd==2,]

prop.table(table(train$Usage))
prop.table(table(val$Usage))

View(train)
train=train[,c(1:8,10)]
val=val[,c(1:8,10)]
names(train)

#Normalize?
normalize<-function(x){
  +return((x-min(x))/(max(x)-min(x)))}

train=transform(train, Work.Exp=ave(train$Work.Exp,FUN = normalize))
train=transform(train, Salary=ave(train$Salary,FUN = normalize))
train<-transform(train, Distance=ave(train$Distance,FUN = normalize))
train<-transform(train, Age=ave(train$Age,FUN = normalize))

val<-transform(val, Work.Exp=ave(val$Work.Exp,FUN = normalize))
val<-transform(val, Salary=ave(val$Salary,FUN = normalize))
val<-transform(val, Distance=ave(val$Distance,FUN = normalize))
val<-transform(val, Age=ave(val$Age,FUN = normalize))


####KNN and Naive Bayes

train.2fact=train[,c(1,5,9)]

val.2fact<-val[,c(1,5,9)]

train.2fact$Usage<-as.factor(train.2fact$Usage)
val.2fact$Usage<-as.factor(val.2fact$Usage)



####Naive Bayes

NB.1<-naiveBayes(x=train.2fact[-3], y=train.2fact$Usage)
#pedict
y_pred<-predict(NB.1,newdata=val.2fact[-3])

#Confusion matrix

cm.NB.1=table(val.2fact[,3],y_pred)
cm.NB.1

View(train.2fact)
# Visualising the Training set results
library(ElemStatLearn)
set = train.2fact
X1 = seq(min(set[, 1])-0.1 , max(set[, 1])+0.1 , by = 0.01)
X2 = seq(min(set[, 2]) -0.1, max(set[, 2])+0.1 , by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'Work.Exp')
y_grid = predict(NB.1, newdata = grid_set)
plot(set[, -3],
     main = 'Naive Bayes (Train set)',
     xlab = 'Age', ylab = 'Work.Exp',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == "1", 'blue', 'red'))
points(set, pch = 21, bg = ifelse(set[, 3] == "1", 'green', 'black'))


#Now for test
library(ElemStatLearn)
set = val.2fact
X1 = seq(min(set[, 1])-0.1 , max(set[, 1])+0.1 , by = 0.01)
X2 = seq(min(set[, 2]) -0.1, max(set[, 2])+0.1 , by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'Work.Exp')
y_grid = predict(NB.1, newdata = grid_set)
plot(set[, -3],
     main = 'Naive Bayes (Val set)',
     xlab = 'Age', ylab = 'Work.Exp',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == "1", 'blue', 'red'))
points(set, pch = 21, bg = ifelse(set[, 3] == "1", 'green', 'black'))



#Knn frames

y_pred<-knn(train=train.2fact[,-3],test=val.2fact[-3], cl=train.2fact[,3],k=3)
cm.knn<-table(val.2fact[,3],y_pred)
cm.knn

#Visualizing Training set
set = train.2fact
X1 = seq(min(set[, 1]) , max(set[, 1]) , by = 0.01)
X2 = seq(min(set[, 2]), max(set[, 2]) , by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'Work.Exp')
y_grid = knn(train=train.2fact[,-3],test=grid_set[-3], cl=train.2fact[,3],k=3)
plot(set[, -3],
     main = 'Knn (Train set)',
     xlab = 'Age' , ylab = 'Work.Exp',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == "1", 'blue', 'red'))
points(set, pch = 21, bg = ifelse(set[, 3] == "1", 'green', 'black'))



#Now for test
set = val.2fact
X1 = seq(min(set[, 1]) , max(set[, 1]) , by = 0.01)
X2 = seq(min(set[, 2]), max(set[, 2]) , by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'Work.Exp')
y_grid = knn(train=train.2fact[,-3],test=grid_set[-3], cl=train.2fact[,3],k=3)
plot(set[, -3],
     main = 'Knn (Val set)',
     xlab = 'Age' , ylab = 'Work.Exp',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == "1", 'blue', 'red'))
points(set, pch = 21, bg = ifelse(set[, 3] == "1", 'green', 'black'))




##########################
####Full Model

####KNN

train.Count<-train[,c(1:9)]
val.Count<-val[,c(1:9)]

#knn3
y_pred.3<-knn(train=train.Count[,-9],test=val.Count[-9], cl=train.Count[,9],k=3)
tab.knn.3<-table(val.Count[,9],y_pred.3)
tab.knn.3

accuracy.knn.3<-sum(diag(tab.knn.3))/sum(tab.knn.3)
accuracy.knn.3

####Naive Bayes
train.Count$Usage<-as.factor(train.Count$Usage)
val.Count$Usage<-as.factor(val.Count$Usage)

NB<-naiveBayes(x=train.Count[-9], y=train.Count$Usage)
#pedict
y_pred<-predict(NB,newdata=val.Count[-9])


#Confusion matrix

cm.NB=table(val.Count[,9],y_pred)
cm.NB
accuracy.NB<-sum(diag(cm.NB))/sum(cm.NB)
accuracy.NB

View(train)
unknown=read.csv("unknown.csv",header=T)
unknown$license<-as.factor(unknown$license)
unknown$Engineer<-as.factor(unknown$Engineer)
unknown$MBA<-as.factor(unknown$MBA)
attach(train)
regressors<-c("Age","Work.Exp","Salary","Distance","license","Engineer","MBA","Gender")
unknown$predictcaruse<-predict.train(object = train.Count,unknown[,regressors],type = "raw")
print(carunknown)


