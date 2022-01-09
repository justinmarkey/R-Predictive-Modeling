# Step 1: Data Partition
# first 150 are training data
# the remaining 50 are validation data
setwd("C:/Users/lit/Desktop")
dt<-read.csv("class pratice sep-27.csv",stringsAsFactors = TRUE)
train<-dt[1:150,]
valid<-dt[-(1:150),]
summary(train)
summary(valid)

# Step 2: Build Model
# Model 1: Science = Math
# Model 2: Science = Math + Gender + Program
# Model 3: Scinece = Math + Gender + Program + Gender*Math + Program*Math
# The following partial models should also be considered
#          Science = Math + Gender*Math + Program + Gender  (a)                         
#          Science = Math + Gender + Program + Program*Math (b)
#          Science = Math + Gender*Math + Program*Math      (c)
#          Science = Math + Gender*Math + Program           (d)
#          Science = Math + Program*Math + Gender           (e)
m1<-lm(data=train,science~math)
m2<-lm(data=train,science~math+gender+prog)
m3<-lm(data=train,science~math*gender+math*prog)
m3a<-lm(data=train,science~math*gender+prog)
m3b<-lm(data=train,science~math*prog+gender)
m3c<-lm(data=train,science~math+gender:math+prog:math)
m3d<-lm(data=train,science~math+gender:math+prog)
m3e<-lm(data=train,science~math+prog:math+gender)

summary(m3e)

# Step 3: Evaluate the Model Performance
# Identify the best model that 1) provides explanations and/or 2) is most accurate
# explanation power
summary(m1)$adj.r.square; 
summary(m2)$adj.r.square; 
summary(m3)$adj.r.square
summary(m3a)$adj.r.square; 
summary(m3b)$adj.r.square; 
summary(m3c)$adj.r.square; 
summary(m3d)$adj.r.square; 
summary(m3e)$adj.r.square; # best explanation

# prediction power
sqrt(mean((valid$math-predict(m1,newdata=valid))^2))
sqrt(mean((valid$math-predict(m2,newdata=valid))^2))
sqrt(mean((valid$math-predict(m3,newdata=valid))^2))

sqrt(mean((valid$math-predict(m3a,newdata=valid))^2))
sqrt(mean((valid$math-predict(m3b,newdata=valid))^2))
sqrt(mean((valid$math-predict(m3c,newdata=valid))^2))
sqrt(mean((valid$math-predict(m3d,newdata=valid))^2))
sqrt(mean((valid$math-predict(m3e,newdata=valid))^2))

