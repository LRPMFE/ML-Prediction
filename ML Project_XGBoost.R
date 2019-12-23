library(data.table) 
library(foreign) 
library(randomForest)
library(lfe)
library(stargazer)
library(ggplot2)
#w <- data.table(read.dta("D:\\data\\res9506withRet.dta"))
w <- data.table(read.dta("D:\\data\\Ret_5_day_eps(2).dta"))
w <- data.table(read.dta("D:\\data\\fundamental_Ret_5_day_eps(1).dta"))
w <- data.table(read.csv("D:\\data\\fundamental_Ret_5_day_eps(3).csv"))
w <- data.table(read.csv("D:\\data\\wohewozuihoudejuejiang.csv"))
w <- w[!is.na(ifPositiveDrift_Lag)]
w <- w[!is.na(Ret5DayBefore)]
w <- w[!is.na(Ret5DayAfter)]
w <- w[!is.na(Ret30DayAfter)]
w <- w[!is.na(Ret60DayAfter)]
w <- w[!is.na(Past30dayVol)]
w <- w[!is.na(PastMktVol3Month)]
w <- w[!is.na(Past10dayVol)]
w <- w[!is.na(fqtr)]
w <- w[!is.na(epsfxq)]
w <- w[!is.na(LaggedME)]
w <- w[!is.na(ChangeInCashInvestment)]
w <- w[!is.na(SharesRepurchased)]
w <- w[!is.na(LTDebtDue1Year)]
w <- w[!is.na(Amortization)]
w <- w[!is.na(Ret1DayBefore)]
w <- w[!is.na(Past200SK)]
#w <- w[!is.na(Past200SK.y)]
w <- w[!is.na(Past200Kur)]
setkey(w, gvkey, datadate)
setorder(w, datadate, gvkey)

w[, PR := (log(99) - log(LaggedME))/(log(99))]
w[, signs := as.numeric(Ret60DayAfter > 0)]



# train model on data 1995-1998
y_data_train <- as.vector(w[fyearq <= 2015, signs])
#x_data_train <- as.matrix(w[fyearq <= 2015, c(4, 10, 12:17)])
#x_data_train <- as.matrix(w[fyearq <= 2015, c(5, 10, 12:22)])
#x_data_train <- as.matrix(w[fyearq <= 2015, c(9, 13, 15:28)])
x_data_train <- as.matrix(w[fyearq <= 2015, c(10, 14, 16:27, 30)])

y_data_train <- as.vector(w[fyearq <= 2015, signs])
#x_data_train <- as.matrix(w[fyearq <= 2015, c(5, 10, 12:22)])
x_data_train <- as.matrix(w[fyearq <= 2015, c(9, 13, 15:28)])

# test data is data 1999 
y_data_test <- as.vector(w[fyearq > 2015, signs])
#x_data_test <- as.matrix(w[fyearq > 2015, c(5, 10, 12:22)])
#x_data_test <- as.matrix(w[fyearq > 2015, c(9, 13, 15:28)])
x_data_test <- as.matrix(w[fyearq > 2015, c(10, 14, 16:27, 30)])

# set maximum terminal nodes equal to 30 for a relatively parsimonious trees
rfLC <- randomForest(x_data_train, y_data_train, ntree = 1000, maxnodes = 8)

# check prediction fit in-sample
RF_pred_in_sample <- predict(rfLC, x_data_train)
# with time clustering
RF_test_in_sample = felm(y_data_train~RF_pred_in_sample | 0 | 0 | w[fyearq <= 2015, fyearq]) 
# Output regressions as text, report t-stats
stargazer(RF_test_in_sample, type = 'text', report = 'vc*t') 

# check prediction fit out-of-sample
RF_pred_out_of_sample <- predict(rfLC, x_data_test)
# with time clustering
RF_test_out_of_sample = felm(y_data_test~RF_pred_out_of_sample | 0 | 0 | w[fyearq > 2015, fyearq]) 
RF_test_out_of_sample <- lm(y_data_test~RF_pred_out_of_sample)
# Output regressions as text, report t-stats
stargazer(RF_test_out_of_sample, type = 'text', report = 'vc*t')
stargazer(RF_test_in_sample, RF_test_out_of_sample, type = 'text', report = 'vc*t') 

# compare to linear regression
x <- x_data_train
# with time clustering
LR_test_in_sample = felm(signs~x | fyearq + gvkey | 0 | fyearq + gvkey, data = w1) 
#LR_test_in_sample <- lm(signs~., data = ww1)
# Output regressions as text, report t-stats
stargazer(LR_test_in_sample, type = 'text', report = 'vc*t')

summary(LR_test_in_sample)
x_lm <- x_data_test
co <- LR_test_in_sample$coefficients
co[is.na(co)] <- 0
LR_pred_out_of_sample <- x_data_test%*%co
LR_test_out_of_sample <- lm(y_data_test~LR_pred_out_of_sample)
summary(LR_test_out_of_sample)



# Fama-MacBeth regressions using out-of-sample predicted values
# Random Forest first
port_ret = NULL
row_counter_end = 0
for (i in 1999:1999)
{
  year_length <- w[fyear == i,AnnualRet]
  year_length <- length(year_length)
  row_counter_start = row_counter_end + 1
  row_counter_end = row_counter_end + year_length
  x_temp <- RF_pred_out_of_sample[row_counter_start:row_counter_end]
  y_temp <- y_data_test[row_counter_start:row_counter_end]
  fit_yr <- lm(y_temp ~ x_temp)
  summary(fit_yr)
  temp <- coefficients(fit_yr)
  port_ret = rbind(port_ret,temp[2])
}
# recall, scale depends on magnitude of x-variable, so only fair to compare Sharpe ratios and t-stats
fm_RF_output = list(SR_Return = mean(port_ret)/sqrt(var(port_ret)), 
                    tstat_MeanRet = sqrt(1+1999-1999)*mean(port_ret)/sqrt(var(port_ret)))
fm_RF_output




library(xgboost)

#(a) {eta = 0.1, maxdepth = 1}

# Next, let's see how the XGBoost method does in the same exercise

params <- list(booster = "gbtree", objective = "binary:logistic",
               eta = 0.1, gamma = 0.1, max_depth = 10)

xgb_train <- xgb.DMatrix(data = x_data_train, label = y_data_train)

# use xgb.cv to find the best nround (number of trees) for this model.
xgbcv <- xgb.cv(params = params, data = xgb_train, nfold = 20,
                nrounds = 200, showsd = T, print_every_n = 20) 

# pick out the lowest cv mean rmse
cv_nrounds = which.min(xgbcv$evaluation_log$test_error_mean)

# with optimal nrounds in hand, run the prediction for out of sample
xgb_optb <- xgboost(params = params, data = xgb_train, nround = cv_nrounds)
xgb_test <- xgb.DMatrix(data = x_data_test, label = y_data_test)
xgb_pred <- predict(xgb_optb, xgb_test)

# precision decision 
sum((xgb_pred > 0.5) == y_data_test)/sum(length(xgb_pred))

# pick the best criterion

b <- c()
c <- seq(0.3, 0.7, 0.01)
for(i in c){
  b <- c(b, sum((xgb_pred > i) == y_data_test)/sum(length(xgb_pred)))
}
print(b)
index <- which(b == max(b))
print(c[index])
print(b[index])

yre <- as.factor(as.numeric(xgb_pred > 0.42))


confusionMatrix(as.factor(y_data_test), yre)

xgb_test_out_of_sample <- lm(y_data_test~xgb_pred)

summary(xgb_test_out_of_sample)

# plot importance of features in order to understand model
boost_out <- xgb.importance(feature_names = colnames(x_data_test), model = xgb_optb)
xgb.plot.importance(importance_matrix = boost_out[1:11])


glm_simple_roc <- simple_roc(y_data_test, xgb_pred)
TPR <- glm_simple_roc$TPR
FPR <- glm_simple_roc$FPR

# plot the corresponding ROC curve
q <- qplot(FPR,TPR,xlab="FPR",ylab="TPR",col=I("blue"),
           main="ROC Curve for XGBoost Model",size=I(0.75))
# add straight 45 degree line from 0 to 1
q + geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), size=I(1.0)) + theme_bw()
cat('Comment: The model performS better than a random guess.')

deciles <- ntile(xgb_pred, n = 10)
br <- quantile(xgb_pred,probs=c(seq(from=0,to=1,by=0.1)))
deciles <- cut(xgb_pred,breaks=br,include.lowest=TRUE)
deciles <- as.numeric(deciles)
df <- data.table(deciles=deciles,xgb_pred=xgb_pred,y_data_test=y_data_test)
lift1 <- df[, lapply(.SD, mean), by = deciles] 
lift1 <- lift1[, .(deciles, y_data_test)] 
lift1[, `:=`(mean_response, y_data_test/mean(y_data_test))] 
setkey(lift1, deciles) 
lift1


w1 <- w[fyearq <= 2015]
w2 <- w[fyearq > 2015]

#ww1 <- w1[, c(4, 16, 18, 19, 20)]

ww1 <- w1[, c(10, 14, 16:27, 30, 31)]
ww2 <- w2[, c(10, 14, 16:27, 30, 31)]

#(i)
out <- glm(signs~., data = ww1, family = 'binomial')
coeff <- out$coefficients
coeff
cat('Comment: The numbers make sense because they are significantly different from zeros.')

#(ii)
test_stat <- out$null.deviance - out$deviance
k <- out$df.null - out$df.residual
pvalue_chisq <- 1 - pchisq(test_stat, df = k)
cat('Comment: The model is better because the p-value is p_value =', pvalue_chisq)

#(iii)
library(dplyr) 
phat <- predict(out, type = "response")
#phat <- predict(out, data = ww2, type = 'response')
deciles <- ntile(phat, n = 10)
#br <- quantile(phat,probs=c(seq(from=0,to=1,by=0.33)))
#deciles <- cut(phat,breaks=br,include.lowest=TRUE)
#deciles <- as.numeric(deciles)
df <- data.table(deciles=deciles,phat=phat,signs=ww1$signs)
lift <- aggregate(df,by=list(deciles),FUN="mean",data=df) # find mean default for each decile
lift <- lift[,c(2,4)]
lift[,3] <- lift[,2]/mean(ww1$signs)
names(lift) <- c("decile","Mean Response","Lift Factor")
lift

# compute a ROC curve
# define a function that creates the true and false positive rates
simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}

glm_simple_roc <- simple_roc(ww1$signs==1, phat)
TPR <- glm_simple_roc$TPR
FPR <- glm_simple_roc$FPR

# plot the corresponding ROC curve
q <- qplot(FPR,TPR,xlab="FPR",ylab="TPR",col=I("blue"),
           main="ROC Curve for Logistic Regression Default Model",size=I(0.75))
# add straight 45 degree line from 0 to 1
q + geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), size=I(1.0)) + theme_bw()
cat('Comment: The model performS better than a random guess.')


insam <- as.matrix(cbind(rep(1, length(ww1$signs)), ww1[, 1:15]))
a <- as.vector(out$coefficients)
infi <- exp(insam%*%a)/(1 + exp(insam%*%a))

sum((infi > 0.5) == ww1$signs)/length(ww1$signs)

outsam <- as.matrix(cbind(rep(1, length(ww2$signs)), ww2[, 1:15]))
a <- as.vector(out$coefficients)
outfi <- exp(outsam%*%a)/(1 + exp(outsam%*%a))
sum((outfi > 0.5) == ww2$signs)/length(ww2$signs)



library(caret)



b <- c()
c <- seq(0.2, 0.8, 0.005)
for(i in c){
  b <- c(b, sum((outfi > i) == ww2$signs)/length(ww2$signs))
}
print(b)
index <- which(b == max(b))
print(c[index])





#w[,ExRet:= AnnualRet - exp(lnRf)]
port_ret <- c()

for(i in 1995:2006){
  temp <- w[fyear == i,]
  fit_yr <- lm(temp$AnnualRet ~ temp$no, data=temp)
  temp <- coefficients(fit_yr)
  port_ret = rbind(port_ret,temp[2])
}

fm_output = list(MeanReturn = mean(port_ret), 
                 StdReturn = sqrt(var(port_ret)), 
                 SR_Return = mean(port_ret)/sqrt(var(port_ret)), 
                 tstat_MeanRet = sqrt(1+2006-1995)*mean(port_ret)/sqrt(var(port_ret)))
fm_output 
