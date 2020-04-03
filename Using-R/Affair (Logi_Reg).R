
affair <- read.csv(file.choose()) # Choose the claimants Data set
str(affair)
summary(affair)
View(affair)
attach(affair)
table(affair$affairs) ## 0.625
#sum(is.na(affair))
#affair <- na.omit(affair) # Omitting NA values from the Data 
# na.omit => will omit the rows which has atleast 1 NA value
dim(affair)

install.packages("caTools")
library(caTools)

table(affair$affairs) ## 451/601 = 0.75
split=sample.split(affair$affairs,SplitRatio = 0.75)
split

## Creating Training and Testing Setz
affairtrain=subset(affair,split==TRUE)
affairtest=subset(affair,split==FALSE)
View(affairtrain)
dim(affairtrain)

nrow(affairtrain) ## 450
nrow(affairtest)  ## 151

## Logistic regression model
install.packages("AER")
library(AER)
??AER

install.packages("plyr")
library(plyr)

affairtrain$affairs[affairtrain$affairs > 0] <- 1
affairtrain$affairs[affairtrain$affairs == 0] <- 0

affairtrain$gender <- as.factor(revalue(affairtrain$gender,c("male"=1,"female"=0)))
affairtrain$children <- as.factor(revalue(affairtrain$children,c("yes"=1,"no"=0)))

## Building the logistic model
affairlogi <- glm(affairs~.,data=affairtrain,family = binomial)

summary(affairlogi)


## Making prediction on Training data set
predicttrain = predict(affairlogi,type="response")
summary(predicttrain)

#datatframe <- as.data.frame(claimants)
#datatframe
#View(datatframe)

?tapply
tapply(predicttrain,affairtrain$affairs,mean)

## "tapply" function computes the average prediction for each of the true outcomes
##        0         1 
##  0.2149776 0.3512282 

# Confusion matrix for threshold of 0.5
table(affairtrain$affairs,predicttrain > 0.5)

##  FALSE TRUE
# 0   324  14
# 1   87   25

## Sensityvity
## 25/25+87 = 0.228 

## Specificity
## 324/324+14 = 0.95

# Confusion matrix for threshold of 0.7
table(affairtrain$affairs,predicttrain > 0.7)

## FALSE TRUE
# 0   335   3
# 1   109   3

## Sensitivity
## 3/3+109 = 0.02

## Specificity
## 335/335+3 = 0.991

## By increasing the threshold value the sensitivity decreases and specificity increasing

#### ROC CURVE can help us to decide which value of yhe threshold is best

install.packages("ROCR")
library(ROCR)

?prediction
ROCRpred <- prediction(predicttrain,affairtrain$affairs)

## Performance function
ROCRpperf <- performance(ROCRpred,"tpr","fpr")


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
plot(ROCRpperf,colorize=TRUE)

plot(ROCRpperf,colorize=TRUE,print.cutoffs.at=seq(0,1,by=0.1),text.adj=c(-0.2,1.7))

