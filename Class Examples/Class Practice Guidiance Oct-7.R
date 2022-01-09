setwd("C:/Users/lit/Desktop")
model.data<-read.csv("class practice oct-7.csv",stringsAsFactors = TRUE)

# let algorithm pick up the model
algo.model<-step(lm(y~.^2,data=model.data))
summary(algo.model)

# EDA
library(car)
scatterplot(data=model.data,y~x1)
scatterplot(data=model.data,y~x2)
scatterplot(data=model.data,y~x3)
scatterplot(data=model.data,y~x5)
scatterplot(data=model.data,y~x1|x4)
scatterplot(data=model.data,y~x2|x4)
scatterplot(data=model.data,y~x3|x4)
scatterplot(data=model.data,y~x5|x4)

my.model<-lm(data=model.data,y~x1+x2+x3+x4+x5+x1:x4+x2:x4)
summary(my.model)
# doing some adjustment after obtaining the fit
my.model<-lm(data=model.data,y~x1+x2+x3+x1:x4+x2:x4)

# algo model:y ~ x1 + x2 + x3 + x4 + x5 + x1:x4 + x1:x5 + x2:x4
# my model: y~x1+x2+x3+x1:x4+x2:x4

# use train/valid to test
row_ind<-sample(1:nrow(model.data),600)
train<-model.data[row_ind,]
valid<-model.data[-row_ind,]

# fit with train and test with valid:
# my model:
my.model.with.train<-lm(data=train,y~x1+x2+x3+x1:x4+x2:x4)
algo.model.with.train<-lm(data=train, y ~ x1 + x2 + x3 + x4 + x5 + x1:x4 + x1:x5 + x2:x4)

sqrt(mean((predict(my.model.with.train,newdata=valid)-valid$y)^2))
sqrt(mean((predict(algo.model.with.train,newdata=valid)-valid$y)^2))

