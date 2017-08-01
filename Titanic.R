# No of people who survived are 342 and who didnt survive are 549


# Lower Class =491
# Middle Class=184
# Upper Class=216
 
# Male=577 Female=314

#10-50 is the range in which most of the people survived

#Sibsp
# It tells about the number of people who were related 
# For eg. 608 people were there who didn't have a relative or a spouse/husband'
 # 0-608
 # 1-209
# tells us that majority of people are single and unrelated to any person on the titanic boat


#parch
#again will tell us about the relation. 

#A,B and C if they are decks 
# People living on B,C,D,E,F   have survived more 
# Not every person seem to have a cabin number 

# parch and sibsp are given because it is common thinking that people who are alone are more likely to survive

library(readr)
train <- read_csv("D:/UIC/1st Semester/Kaggle/train.csv")
View(train)



colSums(is.na(train))
attach(train)
detach(train)
hist(Age)
plot(density(train$Age,na.rm = T))



# Load file

#missing values can be imputed using how many children were there 
#using the mice function 
library(mice)
tempData <- mice(train,m=5,maxit=50,meth='pmm',seed=500)
require(gridExtra)
library(gridExtra)

#checking the distribution of Age data

train<-complete(tempData,1)
plot(density(train$Age))

require(ggplot2)
train<-as.data.frame(unclass(train))



#build a logistic regression model to predict the output
train.model<-glm(Survived~Sex+Pclass*Age+SibSp+Parch+Embarked,family ="binomial",data=train)
summary(train.model)

#predicting with missing values

library(readr)
test <- read_csv("D:/UIC/1st Semester/Kaggle/test.csv")
View(test)
str(test)

test.model<-predict(train.model,newdata = test,type = 'response')
summary(test.model)
str(test.model)
class(test.model)
pred=prediction(test.model,train.model)

install.packages("ROCR")
library('ROCR')
perfspec <- performance(prediction.obj = test.model, measure="spec", x.measure="cutoff")
performance(prediction.obj = )
plot(perfspec)
par(new=TRUE)
perfsens <- performance(prediction.obj = pred, measure="sens", x.measure="cutoff")
plot(perfsens)







