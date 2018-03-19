library(ggplot2)
library(TTR)
library(gbm)
data<- read.csv("data.csv", header = TRUE, sep = ",")
data$days<-weekdays(as.Date(data$TransactionDate))
data$weeks<-ifelse(data$days =="Saturday" | data$days =="Sunday","Weekend","Weekday")
data$month<-as.numeric(format(as.Date(data$TransactionDate), "%m"))
data$year<-as.numeric(format(as.Date(data$TransactionDate), "%Y"))
##set.seed(150)
data$ma <-EMA(data$Amount,14)
data <-data[-(1:14),]

data$days<-as.factor(data$days)
data$weeks<-as.factor(data$weeks)
data$month<-as.factor(data$month)
data$year<-as.factor(data$year)

train<-data[(1:815),]
val <- data[(816:829),]
sales <-val$Amount
val$Amount <-NULL
Dates<-as.Date(as.character(val$TransactionDate))

formula<-Amount~(days+weeks+month+year)*ma
fit <-gbm(formula,data=train,n.trees = 1000)
model<-predict(fit,newdata = val,n.trees = 1000)
df<-data.frame(Dates, sales,(floor(model)))
colnames(df)[3]<-"model"
df
err<-floor(sqrt(mean(sales-model)^2))