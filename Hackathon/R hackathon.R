bank = read.csv("Training Data.csv", stringsAsFactors = FALSE, header = T)
bankTest= read.csv("Test Data.csv", stringsAsFactors = FALSE, header = T)


bank = kNN(bank, variable = c('age',"marital","job","education","contact","credit.history",'credit.score', "poutcome","balance"), k= 5)
bankTest= kNN(bankTest, variable = c('age',"marital","job","education","contact","credit.history",'credit.score', "poutcome","balance"), k= 5)

table(is.na(bankTest))

anova_mod = rpart(age ~ . -Y, data=bank[!is.na(bank$age), ], method="anova", na.action=na.omit)  # since ptratio is numeric.
predicted_age=predict(anova_mod, bank[is.na(bank$age), ])
bank$age=ifelse(bank$age=="NA",predict(anova_mod, bank[is.na(bank$age), ]), bank$age)

anova_modT <- rpart(age ~ ., data=bankTest[!is.na(bankTest$age), ], method="anova", na.action=na.omit)  # since ptratio is numeric.
predicted_age=predict(anova_mod, bank[is.na(bank$age), ])
bankTest$age=ifelse(bankTest$age=="NA",predict(anova_modT, bankTest[is.na(bankTest$age), ]), bankTest$age)

View(bankTest)
View(bank)

bank$job_unk <- ifelse(bank$job == "unknown", 1, 0)
bank$edu_unk <- ifelse(bank$education == "unknown", 1, 0)
bank$cont_unk <- ifelse(bank$contact == "unknown", 1, 0)
bank$pout_unk <- ifelse(bank$poutcome == "unknown", 1, 0)

bankTest$job_unk <- ifelse(bankTest$job == "unknown", 1, 0)
bankTest$edu_unk <- ifelse(bankTest$education == "unknown", 1, 0)
bankTest$cont_unk <- ifelse(bankTest$contact == "unknown", 1, 0)
bankTest$pout_unk <- ifelse(bankTest$poutcome == "unknown", 1, 0)



bank$job <- as.numeric(as.factor(bank$job))
bank$marital <- as.numeric(as.factor(bank$marital))
bank$education <- as.numeric(as.factor(bank$education))
bank$default<- ifelse(bank$default == "yes", 1, 0)
bank$housing <- ifelse(bank$housing== "yes", 1, 0)
bank$loan<- ifelse(bank$loan== "yes", 1, 0)
bank$month <- as.numeric(as.factor(bank$month))
bank$contact <- as.numeric(as.factor(bank$contact))
bank$poutcome <- as.numeric(as.factor(bank$poutcome))
bank$credit.history <- as.numeric(as.factor(bank$credit.history))
 
View(bank)


bankTest$job <- as.numeric(as.factor(bankTest$job))
bankTest$marital <- as.numeric(as.factor(bankTest$marital))
bankTest$education <- as.numeric(as.factor(bankTest$education))
bankTest$default<- ifelse(bankTest$default == "yes", 1, 0)
bankTest$housing <- ifelse(bankTest$housing== "yes", 1, 0)
bankTest$loan<- ifelse(bankTest$loan== "yes", 1, 0)
bankTest$month <- as.numeric(as.factor(bankTest$month))
bankTest$contact <- as.numeric(as.factor(bankTest$contact))
bankTest$poutcome <- as.numeric(as.factor(bankTest$poutcome))
bankTest$credit.history <- as.numeric(as.factor(bankTest$credit.history))


model_std <- glm(Y ~ ., family = binomial(link = "logit"),  data = bank)
summary(model_std)


predictions <- predict.glm(model_std, newdata= bankTest, type= "response")
predictions[predictions > 0.5] <- 1
predictions[predictions <= 0.5] <- 0

library(gmodels)
install.packages("gmodels")
install.packages("TSA")
install.packages("FitaR")
residual.analysis <- function(model, std = TRUE){
  library(TSA)
  library(FitAR)
  if (std == TRUE){
    res.model = rstandard(model)
  }else{
    res.model = residuals(model)
  }
  par(mfrow=c(2,2))
  plot(res.model,type='o',ylab='Standardised residuals', main="Time series plot of standardised residuals")
  abline(h=0)
  hist(res.model,main="Histogram of standardised residuals")
  qqnorm(res.model,main="QQ plot of standardised residuals")
  qqline(res.model, col = 2)
  acf(res.model,main="ACF of standardised residuals")
  print(shapiro.test(res.model))
