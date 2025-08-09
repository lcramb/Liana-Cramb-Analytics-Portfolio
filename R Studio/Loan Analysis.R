# Install Required Packages if not installed yet
install.packages("randomForest")
install.packages("ROCR")

# WEEK 8 ZOOM Class 
#par(mfrow=c(1,1))
#source("/var/www/html/jlee141/econdata/R/func_lib.R")
# For desktop version of R
source("https://bigblue.depaul.edu/jlee141/econdata/R/func_lib.R")

loan <- read.csv("https://bigblue.depaul.edu/jlee141/econdata/eco520/loan_default.csv")
summary(loan)
str(loan)

# Create Clean Data 
indata <- loan

# Split Train and Test set
set.seed(12345)
# train_idx <- sample(1000,800)
train_idx <- sample(nrow(indata), round(0.8*nrow(indata)))
train <- indata[train_idx,]
test <- indata[-train_idx,]
testy <- test$Default

# Linear Prob Model (LPM)
lp0 <- lm(Default~Credit_score, data=train)
summary(lp0)
yhat_lp0 <- predict(lp0, newdata=test)

plot(test$Credit_score,yhat_lp0, main="LPM" )
abline(h=c(0,1),col="red")
conf_table(yhat_lp0,testy,"LPM")

dec1 <- ifelse(yhat_lp0 > 0.5,1,0)

table(testy,dec1)
auc_plot(yhat_lp0,testy,"LPM")


# Estimate model with the rest of independent variables 
lp1 <- lm(Default~ . ,data=train)
yhat_lp1 <- predict(lp1, newdata=test)
plot(test$Credit_score,yhat_lp1 )
abline(h=c(0,1),col="red")
conf_table(yhat_lp1,testy,"LPM")
dec2 <- ifelse(yhat_lp1 > 0.5,1,0)
table(testy,dec2)
auc_plot(yhat_lp1,testy,"LPM")

# Logistic Regression Model
logit1 <- glm(Default~ . , data=train, family=binomial(link=logit))
yhat_lg1 <- predict(logit1,newdata=test, type="response")

plot(test$Credit_score,yhat_lg1, main="Logistic",col="blue")
conf_table(yhat_lg1,testy,"Log")
dec4 <- ifelse(yhat_lg1 > 0.5,1,0)
table(testy,dec4)
auc_plot(yhat_lg1,testy,"Logit")

# Random Forest Model
# install.packages("randomForest")
library(randomForest)
train1 <- train
train1$Default <- as.factor(train1$Default)

rf0 <- randomForest(Default~Credit_score, data=train1,mtry=4,ntree=500 )
summary(rf0)
yhat_rf0 <- predict(rf0,newdata=test,type="prob")
yhat_rf0 <- yhat_rf0[,2]

plot(test$Credit_score,yhat_rf0, main="RandomForest",col="blue")
conf_table(yhat_rf0,testy,"Log")
dec5 <- ifelse(yhat_rf0 > 0.5,1,0)
table(testy,dec5)
auc_plot(yhat_rf0,testy,"Random Forest")


model <- "Default ~ Checking_amount + Term + Credit_score + Personal_loan + 
Home_loan + Education_loan + Emp_status + Amount + Saving_amount + Age "

# Further Tests on mtry 
oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(formula(model),data=train1, mtry=i,ntree=100)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values

# Find the good number of trees 
rf_tree <- randomForest(formula=Default~.,data=train1, mtry=3,ntree=800)
Trees= rep(1:nrow(rf_tree$err.rate)) 
Error.rate = rf_tree$err.rate[,"OOB"]
plot(Trees,Error.rate, col="red") 

rf1 <- randomForest(Default~ ., data=train1,mtry=7,ntree=200 )
str(rf1)
yhat_rf1 <- predict(rf1,newdata=test,type="prob")
yhat_rf1 <- yhat_rf1[,2]
plot(test$Credit_score,yhat_rf1, main="RandomForest",col="blue")

conf_table(yhat_rf1,testy,"RF")
dec6 <- ifelse(yhat_rf1 > 0.5,1,0)
table(testy,dec6)
auc_plot(yhat_rf1,testy,"Random Forest")

par(mfrow=c(2,2))
auc_plot(yhat_lp1,testy,"LPM")
auc_plot(yhat_lg1,testy,"Logit")
auc_plot(yhat_rf0,testy,"Random Forest")
auc_plot(yhat_rf1,testy,"Random Forest")
