setwd("C:/Users/kyole/Desktop/BUS 347")

adult.dt<-read.csv("adult.csv")

adult.dt$WORKCLASS<-with(adult.dt,factor(WORKCLASS))
adult.dt$EDUCATION<-with(adult.dt,factor(EDUCATION))
adult.dt$OCCUPATION<-with(adult.dt,factor(OCCUPATION))
adult.dt$RELATIONSHIP<-with(adult.dt,factor(RELATIONSHIP))
adult.dt$RACE<-with(adult.dt,factor(RACE))
adult.dt$SEX<-with(adult.dt,factor(SEX))
adult.dt$ABOVE50K<-with(adult.dt,factor(ABOVE50K))

train.ind<-sample(1:nrow(adult.dt),30000)
train<-adult.dt[train.ind,]
valid<-adult.dt[-train.ind,]

# Build model with train
library(rpart)
default_tree<-rpart(ABOVE50K~.,
                    data=train,
                    method="class")
summary(default_tree)
default_tree

# retrieve the information we need
# CP table
default_tree$cptable
# variable importance
default_tree$variable.importance
# tree plot
library(rattle)
fancyRpartPlot(default_tree)

# Let's perform the pruning process mannually
# first, create a more complicated tree
comp_tree<-rpart(ABOVE50K~.,
                 data=train,
                 method="class",
                 control=rpart.control(cp=0.00005))
# fancyRpartPlot(comp_tree)
comp_tree$cptable
plotcp(comp_tree)
# prune the tree by finding the min of xerror
cp<-data.frame(comp_tree$cptable)
min(cp$xerror)
optimal_cp<-cp[which.min(cp$xerror),"CP"]
pruned_tree<-rpart(ABOVE50K~.,
                   data=train,
                   method="class",
                   control=rpart.control(cp=optimal_cp))

## Random Forest
library(randomForest)
default_forest<-randomForest(ABOVE50K~.,
                             data=train)

# check the random forest output
summary(default_forest)
default_forest

# retrieve detailed information of the rf
default_forest$importance
varImpPlot(default_forest)

# convergence of the algorithm
plot(default_forest)

# modify the parameter of the random forest
customized_forest<-randomForest(
  ABOVE50K~.,
  data=train,
  ntree=100,
  mtry=4,
)
customized_forest

# Explore the optimal ntree and mtry in the algorithm by the for loop
ntree_vector<-1:500
mtry_vector<-c(2,3,4,5,6,7)
output_performance<-matrix(NA,
                           nrow=length(ntree_vector),
                           ncol=length(mtry_vector))
for(i in 1:length(ntree_vector)){
  for(j in 1:length(mtry_vector)){
    # build each forest using the corresponding mtry and ntree
    single_rf<-randomForest(
      data=train,
      ABOVE50K~.,
      ntree=ntree_vector[i],
      mtry=mtry_vector[j])
    # extract the performance
    output_performance[i,j]<-single_rf$err.rate[ntree_vector[i],1]
    # use the cat function to track process
    cat("i=", i , " ", "j=", j, "\n")
  }
}
# conclusion: ntree=200 & mtry=2 gives the highest performance (oob.err)


