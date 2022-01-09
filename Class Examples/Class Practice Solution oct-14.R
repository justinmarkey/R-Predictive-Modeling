setwd("C:/Users/lit/Desktop")

# Import Data
stock_data<-read.csv("class practice oct-13.csv")
model.data<-stock_data[,-which(colnames(stock_data) == "DATE")]

####################
## Model Building ##
####################
# Fit Model with Stepwise Selection
full.mod<-lm(Y_IBM~., data=model.data)
stepwise.mod<-step(full.mod, direction="backward", trace=FALSE)
summary(stepwise.mod)

#####################
## Model Diagnosis ##
#####################
# 1) Check multicollinearity (before running regression)
cor(model.data[,-which(colnames(model.data) == "Y_IBM")])
# 1) Check multicollinearity (after running regression)
library(car)
vif(stepwise.mod)
# Conclusion: This model has multicollinearity issue.


# 2) Check heteroscadasticity
plot(stepwise.mod$residuals)
ncvTest(stepwise.mod)
# Conclusion: The variance is not constant, which violates the assumption.


# 3) Check normal distribution
qqnorm(stepwise.mod$residuals)
qqline(stepwise.mod$residuals)
hist(stepwise.mod$residuals)

shapiro.test(stepwise.mod$residuals[1:5000])
shapiro.test(stepwise.mod$residuals[5001:10000])
shapiro.test(stepwise.mod$residuals[-(1:10000)])
# Conclusion: The distribution of residual is not normal, which also violates the assumption


# 4) Check Serial Correlation
library(lmtest)
dwtest(stepwise.mod)
# Conclusion: large p-value, so serial correlation is not a concern. Assumption is satistified.

