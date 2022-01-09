setwd("C:/Users/lit/Desktop")

dt <- read.csv("2014_Financial_Data.csv")

library(rpart)
library(rattle)
library(ROSE)

# 1
model.data<-dt[,-1]
# 2
default_tree <- rpart(data=model.data,
                      Class~.,
                      method="class")
# 3
summary(default_tree)
default_tree$variable.importance
# 4
complicated_tree <- rpart(data=model.data,
                      Class~.,
                      method="class",
                      control=rpart.control(cp=0.001))
plotcp(complicated_tree)
# 5
cptable<-as.data.frame(complicated_tree$cptable)
optimal.cp<-cptable$CP[which.min(cptable$xerror)]
pruned_tree<-prune(complicated_tree,cp=optimal.cp)
fancyRpartPlot(pruned_tree)
# 6
actual_y<-dt$Class
pred_y <- predict(pruned_tree)[,"1"]
roc.curve(actual_y, pred_y)
# 7
cutoff<-0.7
mean(ifelse((actual_y==1 & pred_y>cutoff)|(actual_y==0 & pred_y<cutoff),1,0)) # accuracy
sum(ifelse(actual_y==1 & pred_y>cutoff,1,0))/sum(actual_y)# sensitivity
sum(ifelse(actual_y==0 & pred_y<cutoff,1,0))/(length(actual_y)-sum(actual_y)) # specificity
