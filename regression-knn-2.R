setwd("/Users/halla.d/Library/Mobile Documents/com~apple~CloudDocs/Desktop/STA401")
auto = read.csv("Auto.csv")

#a
median = median(auto$mpg)
median
mpg01 = as.numeric(auto$mpg > median)
mpg01
df = auto[, names(auto) != "mpg"]  # Remove the 'mpg' column
df$mpg01 = mpg01  # Add 'mpg01' as a new column
df$mpg01 = as.factor(df$mpg01)
df$origin = as.factor(df$origin)
df$cylinders = as.factor(df$cylinders)
df = na.omit(df)
sum(is.na(df))
df$horsepower = as.integer(df$horsepower)
head(df)
tail(df)

#b
str(df)
plot(df)
par(mfrow = c(2, 4)) 
boxplot(df$cylinders ~ df$mpg01, 
        ylab = "Number of Cylinders", 
        xlab = "mpg01",
        main = "Boxplot of #Cylinders vs mpg01")

boxplot(df$displacement ~ df$mpg01, 
        ylab = "Displacement", 
        xlab = "mpg01",
        main = "Boxplot of dislpacement vs mpg01")

boxplot(df$year ~ df$mpg01, 
        ylab = "Year", 
        xlab = "mpg01",
        main = "Boxplot of Year vs mpg01")

boxplot(df$weight ~ df$mpg01, 
        ylab = "weight", 
        xlab = "mpg01",
        main = "Boxplot of Weight vs mpg01")

boxplot(df$horsepower ~ df$mpg01, 
        ylab = "horsepower", 
        xlab = "mpg01",
        main = "Boxplot of horsepower vs mpg01")
boxplot(df$acceleration ~ df$mpg01, 
        ylab = "acceleration", 
        xlab = "mpg01",
        main = "Boxplot of acceleration vs mpg01")
boxplot(df$origin ~ df$mpg01, 
        ylab = "origin", 
        xlab = "mpg01",
        main = "Boxplot of origin vs mpg01")

#c
library(caret) 
s <- createDataPartition(y = df$mpg01,p = 0.7,list = FALSE)
train_df <- df[s,] # 70% training
test_df<- df[-s,] # 30% testing
summary(train_df$mpg01)
131/(131+144)
summary(test_df$mpg01)
56/(56+61)

#d
library(MASS)
lda.fit=lda(mpg01~ horsepower + weight + displacement + year + acceleration + cylinders,data=train_df)
lda.fit
lda.pred=predict(lda.fit, test_df) #don't put "data =" before test.2005
lda.class=lda.pred$class
lda.class
lda.class[1:20]
conf = table(lda.class,test_df$mpg01)
conf
mean(lda.class!=test_df$mpg01) #error rate

#e 
qda.fit = qda(mpg01~ horsepower + weight + displacement + year + acceleration,data=train_df)
qda.pred=predict(qda.fit, test_df) #don't put "data =" before test.2005
qda.class=qda.pred$class
qda.class[1:20]
conf = table(qda.class,test_df$mpg01)
conf
mean(qda.class!=test_df$mpg01) #error rate

#f
glm.fit=glm(mpg01~ horsepower + weight + displacement + year + acceleration,data=train_df,family=binomial) 
summary(glm.fit)
glm.probs = predict(glm.fit, newdata = test_df, type = "response")
glm.probs[1:10]
n=length(glm.probs)
glm.pred=rep(0,n)
glm.pred
glm.pred[glm.probs>.5]= 1
glm.pred
table(predict=glm.pred,actual=test_df$mpg01) #classifcation matrix
mean(glm.pred!=test_df$mpg01)

#g
set.seed(12345)
library(caret)
library(FNN)

train.knn = train_df[ , - (8:9)] #removing response and name
test.knn = test_df[ , - (8:9)] #removing response and name

log(dim(train_df)[1]) #5.6 so we'll consider 5 and 7 
tct <- trainControl(method="repeatedcv")
knn.pred <- train(mpg01~ horsepower + weight + displacement + year + acceleration,data=train_df, 
                  method = "knn", trControl = tct, preProcess = c("center","scale"),tuneLength = 15)
knn.pred
plot(knn.pred)

knn.pred=knn(train.knn,test.knn,train_df$mpg01,k=5)
conf=table(knn.pred,test_df$mpg01)
mean(knn.pred != test_df$mpg01)

knn.pred=knn(train.knn,test.knn,train_df$mpg01,k=1)
conf=table(knn.pred,test_df$mpg01)
mean(knn.pred != test_df$mpg01)

knn.pred=knn(train.knn,test.knn,train_df$mpg01,k=7)
conf=table(knn.pred,test_df$mpg01)
mean(knn.pred != test_df$mpg01)

knn.pred=knn(train.knn,test.knn,train_df$mpg01,k=25)
conf=table(knn.pred,test_df$mpg01)
mean(knn.pred != test_df$mpg01)

knn.pred=knn(train.knn,test.knn,train_df$mpg01,k=33)
conf=table(knn.pred,test_df$mpg01)
mean(knn.pred != test_df$mpg01)

