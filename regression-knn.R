library(MASS)
data(Boston)
df = Boston
head(df)

#a
median = median(df$crim)
median
crime_med = as.numeric(df$crim > median) #1 is crime rate above median
crime_med
df = df[, names(df) != "crim"]  # Remove the crime column
df$crime_med = crime_med  # Add 'crime_med ' as a new column
df$crime_med = as.factor(df$crime_med)
str(df)
df = na.omit(df)
sum(is.na(df))
head(df)
tail(df)


library(caret) 
s <- createDataPartition(y = df$crime_med,p = 0.7,list = FALSE)
train_df <- df[s,] # 70% training
test_df<- df[-s,] # 30% testing
summary(train_df$crime_med)
summary(test_df$crime_med)

#d
lda.fit=lda(crime_med ~ .,data=train_df)
lda.fit
lda.pred=predict(lda.fit, test_df) #don't put "data =" before test.2005
lda.class=lda.pred$class
lda.class
lda.class[1:20]
conf = table(lda.class,test_df$crime_med)
conf
mean(lda.class!=test_df$crime_med) #error rate

#f
glm.fit=glm(crime_med ~., data = train_df,family=binomial) 
summary(glm.fit)
glm.probs = predict(glm.fit, newdata = test_df, type = "response")
glm.probs[1:10]
n=length(glm.probs)
glm.pred=rep(0,n)
glm.pred
glm.pred[glm.probs>.5]= 1
glm.pred
table(predict=glm.pred,actual=test_df$crime_med) #classifcation matrix
mean(glm.pred!=test_df$crime_med)

#g
set.seed(12345)
library(caret)
library(FNN)

head(train_df)
train.knn = train_df[ , - 14] #removing response 
test.knn = test_df[ , -14] #removing response 

log(dim(train_df)[1]) #5.9 so we'll consider 5 and 7 
tct <- trainControl(method="repeatedcv")
knn.pred <- train(crime_med~ .,data=train_df, 
                  method = "knn", trControl = tct, preProcess = c("center","scale"),tuneLength = 15)
knn.pred
plot(knn.pred)

knn.pred=knn(train.knn,test.knn,train_df$crime_med,k=5)
conf=table(knn.pred,test_df$crime_med)
mean(knn.pred != test_df$crime_med)

knn.pred=knn(train.knn,test.knn,train_df$crime_med,k=1)
conf=table(knn.pred,test_df$crime_med)
mean(knn.pred != test_df$crime_med)

knn.pred=knn(train.knn,test.knn,train_df$crime_med,k=7)
conf=table(knn.pred,test_df$crime_med)
mean(knn.pred != test_df$crime_med)

knn.pred=knn(train.knn,test.knn,train_df$crime_med,k=25)
conf=table(knn.pred,test_df$crime_med)
mean(knn.pred != test_df$crime_med)

knn.pred=knn(train.knn,test.knn,train_df$crime_med,k=33)
conf=table(knn.pred,test_df$crime_med)
mean(knn.pred != test_df$crime_med)




