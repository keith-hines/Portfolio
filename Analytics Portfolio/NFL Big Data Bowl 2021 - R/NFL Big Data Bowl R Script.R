# NFL Big Data Bowl
# Our goal is to better understand what leads to punt efficiency, leading our staff to find the most efficient punters out of college and put them in the best situations to eliminate return yards


# First, we will upload our data

## Clearing our environment
rm(list = ls())

## Reading in the data (that we have already merged and cleaned up a bit in excel)
library(readr)
punt <- read.csv("puntdata.csv")
View(punt)


# Now, we can start cleaning

## Eliminating Unneeded rows containing different types of special teams plays
punt <- punt[!grepl("Field Goal",punt$specialTeamsPlayType),]
punt <- punt[!grepl("Extra Point", punt$specialTeamsPlayType),]
head(punt)

## Eliminating rows that had penalties
punt <- punt[!grepl("Non-Special Teams Result", punt$specialTeamsResult),]

# Creating a net punt column (an important statistic for judging punter effectiveness)

## First, we have to change the NA results in return yardage to = 0
punt$kickReturnYardage[is.na(punt$kickReturnYardage)] = 0

## Now, we can create the column by subtracting return yardage from length
punt$netpunt <- punt$kickLength - punt$kickReturnYardage

## We can also create additonal categories to convert our categorical values for the sake of the regression
punt$blocked <- ifelse(punt$specialTeamsResult == 'Blocked Punt', 1, 0)
punt$snapOK <- ifelse(punt$snapDetail == 'OK', 1, 0)
punt$normalKickType <- ifelse(punt$kickType == 'N', 1, 0)

# Creating a new binary column for "explosive return"
punt$explosive <- ifelse(punt$kickReturnYardage >= 15, 1, 0)

## Splitting into test and train
set.seed(32)
trn <- runif(nrow(punt)) < .7
train <- punt[trn==TRUE,]
test <- punt[trn==FALSE,]

###############################################Linear#################################################################

#*********************************
#******* LINEAR REGRESSION *******
#*********************************

#linear regression with all predictors
full_ols <- lm(kickReturnYardage ~ kickLength + snapTime + operationTime + hangTime + blocked + snapOK + normalKickType, data = punt)
summary(full_ols)
confint(full_ols)

#correlation of powerful predictors
cor(punt$kickLength,punt$kickReturnYardage, use = "complete.obs")
cor(punt$hangTime,punt$kickReturnYardage, use = "complete.obs")
cor(punt$operationTime,punt$kickReturnYardage, use = "complete.obs")
cor(punt$kickReturnYardage,punt$snapOK, use = "complete.obs")

#ols with best predictors
punt_ols <- lm(kickReturnYardage ~ kickLength + hangTime + normalKickType, data = punt)
summary(punt_ols)
confint(punt_ols)

#predictions and residuals
preds <- predict(punt_ols)
eps <- punt$kickReturnYardage - preds
par(mfrow=c(1,2))
hist(eps)
qqnorm(eps)
qqline(eps)

#validation of predictions
plot(predict(punt_ols), resid(punt_ols))
qqnorm(resid(punt_ols))

library(MASS)
truehist(resid(punt_ols), col="lightblue")

#model did not perform well in-sample, even before using training and test split. consider ways to transform the data:

#transform kick return yards less than 0 to be 0
sum(punt$kickReturnYardage < 0)
min(punt$kickReturnYardage)
punt_pos <- punt
punt_pos$kickReturnYardage <- pmax(punt$kickReturnYardage,0)
View(punt_pos)

#re-run ols without negative kick return yards
punt_pos_ols <- lm(kickReturnYardage ~ kickLength + hangTime + normalKickType, data = punt_pos)
summary(punt_pos_ols)
plot(predict(punt_pos_ols), resid(punt_pos_ols))
qqnorm(resid(punt_pos_ols))

preds_pos <- predict(punt_pos_ols)
eps_pos <- punt$kickReturnYardage - preds_pos
par(mfrow=c(1,2))
hist(eps_pos)
qqnorm(eps_pos)
qqline(eps_pos)

#residuals still do not fit well. now that there are no negative kick return yards, we can use log transformation:
hist(log(punt_pos$kickReturnYardage))
#histogram of log(return yards) is much more normally distributed

#linear regression to predict log(kickReturnYardage). need to add 1 to all values so there won't be log(0)
punt_pos$kickReturnYardage <- punt_pos$kickReturnYardage + 1
View(punt_pos)
punt_pos$kickReturnYardage <- log(punt_pos$kickReturnYardage)
View(punt_pos)
punt_log_ols <- lm(kickReturnYardage ~ kickLength + hangTime + normalKickType, data = punt_pos)
summary(punt_log_ols)
plot(predict(punt_log_ols), resid(punt_log_ols))
qqnorm(resid(punt_log_ols))
qqline(punt_pos$kickReturnYardage, col="light blue", lwd=3)
#residuals are slightly better

#Use training and test sets to build predictive model for log(return yards)
set.seed(27)
trn <- runif(nrow(punt_pos)) < .7
punt_log_train <- punt_pos[trn==TRUE,]
punt_log_test <- punt_pos[trn==FALSE,]

punt_log_ols1 <- lm(kickReturnYardage ~ kickLength + hangTime + normalKickType, data = punt_log_train)
summary(punt_log_ols1)
#R2 improved slightly


#measure RMSE of the residuals
Y <- punt_log_train$kickReturnYardage
Y.tst <- punt_log_test$kickReturnYardage

do.RMSE.trn <- function(yhat)  sqrt( mean( (Y-yhat)^2 ) )
do.RMSE.tst <- function(yhat)  sqrt( mean( (Y.tst-yhat)^2 ) )
RMSE.trn_OLS <- do.RMSE.trn(predict(punt_log_ols1, data = punt_log_train))
RMSE.tst_OLS <- do.RMSE.tst(predict(punt_log_ols1, data = punt_log_test))
RMSE.trn_OLS; RMSE.tst_OLS
#very similar RMSE for train and test. Represents square root of the variance of the residuals

#validate performance of predictions
#in sample
preds_log <- predict(punt_log_ols1)
eps_log <- punt_log_train$kickReturnYardage - preds_log
par(mfrow=c(1,2))
hist(eps_log)
qqnorm(eps_log)
qqline(eps_log)
#out of sample
preds_log <- predict(punt_log_ols1)
eps_log <- punt_log_test$kickReturnYardage - preds_log
par(mfrow=c(1,2))
hist(eps_log)
qqnorm(eps_log)
qqline(eps_log)

#*********************************
#******* Ridge REGRESSION ********
#*********************************

#Ridge with log kick return yards
library(glmnet)
X_train <- data.matrix(punt_log_train[,c(5,8:10,15:17)])
X_train[is.na(X_train)] = 0
Y_train <- punt_log_train[,6]
X_test <- data.matrix(punt_log_test[,c(5,8:10,15:17)])
X_test[is.na(X_test)] = 0
Y_test <- punt_log_test[,6]
head(Y_train)

ridge_mod <- glmnet(X_train, Y_train, family = "gaussian", alpha = 0, standardize = TRUE, nlambda=5)

plot(ridge_mod, lwd=3)
plot(ridge_mod, lwd=3, xvar = "lambda")

coef(ridge_mod)

# calculate the training error
mse_train <- colMeans((replicate(5, Y_train)-predict(ridge_mod,X_train))^2)
plot(mse_train,type = "o", lwd=3,col="light blue",xlab="model complexity", ylim=c(.5,1.5))
mse_train

cor(predict(ridge_mod,X_test)[,5],Y_test)^2

#Cross validation
cv_ridge <- cv.glmnet(X_train, Y_train, alpha = 0, family="gaussian",k=10) # default is LOOCV
plot(cv_ridge, xvar = "lambda")

lambda_ridge <- cv_ridge$lambda.min
lambda_ridge

ridge_best <- glmnet(X_train, Y_train, family = "gaussian", alpha = 0, lambda = lambda_ridge, standardize = TRUE)
cor(predict(ridge_best,X_test),Y_test)^2


################################################ Classification ##########################################################################################################

## Logistic Regression
explosiveglm <- glm(explosive ~ snapTime + snapOK + normalKickType + operationTime + kickLength + hangTime, family="binomial", data=punt)
summary(explosiveglm)
yhat.glm <- predict(explosiveglm, test, type="response")
glm.roc <- roc(test$explosive, yhat.glm, direction="<")
glm.roc

## Classification Tree
library(rpart)
library(pROC)
form1 <- formula(explosive ~ snapTime + snapOK + normalKickType + operationTime + kickLength + hangTime)
t1 <- rpart(form1, data=train, cp=.001, method="class")
plot(t1,uniform=T,compress=T,margin=.05,branch=0.3)
text(t1, cex=.7, col="blue",use.n=TRUE)
plotcp(t1)

CP <- printcp(t1)
cp <- CP[,1][CP[,4] == min(CP[,4])]
cp
test$yhat.t1 <- predict(t1, test, type="prob")[,2]
tree.roc <- roc(test$explosive, test$yhat.t1, direction="<")
tree.roc

## GAM
library(mgcv)
gamexplosive <- gam(explosive ~ snapTime + snapOK + normalKickType + operationTime + s(kickLength,bs='cr') + s(hangTime,bs='cr'), family=binomial, data=punt)
summary(gamexplosive)
yhat_gam <- predict(gamexplosive,newdata = test, type = "response")
gam.roc <- roc(test$explosive,yhat_gam, direction="<")
gam.roc

## Neural Net
library(nnet)
net <- nnet(form1, data = train, size = 7, maxit = 500, decay=0.002)
yhat.net <- predict(net, test)
test$yhat.net <- yhat.net
par(mfrow=c(1,1))
net.roc <- roc(test$explosive, test$yhat.net, direction="<")
net.roc


# Plotting the ROC's to compare models
plot(tree.roc, lwd=3, col = "dark green")
lines(glm.roc, lwd=3, col = "dark blue")
lines(gam.roc, lwd=3, col = "light blue")
lines(net.roc, lwd=3, col = "brown")
legend("bottomright",title="ROC Curves",c("tree","glm","gam","nnet"), fill=c("dark green","dark blue","light blue", "brown"))
