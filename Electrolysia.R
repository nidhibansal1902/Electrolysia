#Libraries

library(dplyr)
library(ggplot2)
library(caTools)
library(xgboost)
library(readr)
library(stringr)
library(caret)


train_data<-read.csv("train.csv")
test_data<-read.csv("test.csv")

train_data$datetime<-as.numeric(train_data$datetime)
test_data$datetime<-as.numeric(test_data$datetime)

test_data$electricity_consumption<-''

colSums(train_data=='')
colSums(is.na(train_data))

colSums(test_data=='')
colSums(is.na(test_data))

boxplot(train_data$var1)
boxplot(train_data$windspeed)  ##Outlier is there
boxplot(train_data$pressure)
boxplot(train_data$electricity_consumption)

histogram((train_data$var1))
histogram((train_data$windspeed))

#Linear Reg
reg<-lm(electricity_consumption ~., data=train_data)
test_data$electricity_consumption<-predict(reg,test_data[,-8])
summary(reg)

ggplot() +
  geom_point(aes(x = train_data$windspeed, y = train_data$electricity_consumption),
             colour = 'red') +
  geom_line(aes(x = train_data$windspeed, y = predict(reg, newdata = train_data[,-8])),
            colour = 'blue') +
  ggtitle('Windspeed vs Elec consumption (Training set)') +
  xlab("Windspeed") +
  ylab('Elec Consumption')

electricity_consumption<-train_data[,8 ]
train_data<-train_data[,-8]
#One hot encoding
dummies <- dummyVars(~ var2, data =train_data)
train_data_ohe <- as.data.frame(predict(dummies, newdata = train_data))
train_data_combined <- cbind(train_data[,-7],train_data_ohe)
train_data_combined<-cbind(train_data_combined,electricity_consumption)


dummies_test <- dummyVars(~ var2, data =test_data)
test_data_ohe <- as.data.frame(predict(dummies_test, newdata = test_data))
test_data_combined <- cbind(test_data[,-7],test_data_ohe)

train_data_combined<-train_data_combined[,-1]
test_data_combined<-test_data_combined[,-1]

#temp vector
vec_train=c(552)
i=552
while (i < 26496) {
  
  i = i+552
  vec_train<-cbind(vec_train,i)
}

vec_test=c(192)
j=192
k=1
while (j < 8568) {
  
  if(k>=12)
    k=0
  k=k+1
  if((k==1)|(k==2)|(k==4)|(k==6)|(k==7)|(k==9)|(k==11)){
    j = j+192}
  else if((k==3)|(k==5)|(k==10)|(k==12)){
    j = j+168}
  else if(k==8){
    j=j+120}
  if(j==5688)
    j=j+24
  
  vec_test<-cbind(vec_test,j)
}
vec_test<-cbind(1, vec_test)
#XGBoost
set.seed(100)
temp=2
for(vec in vec_train){
  print( "vec")
  print( vec)
model_xgb1 <- xgboost(data.matrix(train_data_combined[1:vec,-9]),
                      label=data.matrix(train_data_combined[1:vec,9]),
                      cv=5,
                      silent=1,
                      objective="reg:linear",
                      nrounds=500,
                      max.depth=10,
                      eta=0.1,
                      colsample_bytree=0.5,
                      seed=235,
                      metric="rmse",
                      importance=1)
print("vec_test ")
print( vec_test[temp])
    test_xgb1 <- predict(model_xgb1,data.matrix(test_data_combined[vec_test[temp-1]+1:vec_test[temp],]))
    test_data[vec_test[temp-1]+1:vec_test[temp],8]<-round(test_xgb1)
    temp=temp+1
}

solution <- data.frame('ID' = test_data[1:8568,1], 'electricity_consumption'=test_data[1:8568,8])

head(solution)

# Write it to file
write.csv(solution, 'Submission.csv', row.names = F)


