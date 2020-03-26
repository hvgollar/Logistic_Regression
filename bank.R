attach(bank)
library(bank)
summary(bank)
#job dummy variables
bank$job_ad<- ifelse(bank$job=="admin",1,0)
bank$job_ukn <- ifelse(bank$job_=='unknown',1,0)
bank$job_uemp <- ifelse(bank$job=="unemployed",1,0)
bank$job_mgmt <- ifelse(bank$job=='management',1,0)
bank$job_hm <- ifelse(bank$job=='housemaid',1,0)
bank$job_entr <- ifelse(bank$job=='entrepreneur',1,0)
bank$job_st <- ifelse(bank$job=='student',1,0)
bank$job_blu<- ifelse(bank$job=='blue-collar',1,0)
bank$job_self <- ifelse(bank$job=='self-employed',1,0)
bank$job_rtd <- ifelse(bank$job=="retired",1,0)
bank$job_tech <- ifelse(bank$job=='technician',1,0)
bank$job_ser <- ifelse(bank$job=='services',1,0)
View(bank)

#marital dummy variables
bank$marti_1 <- ifelse(bank$marital=="single",1,0)
bank$marti_2<- ifelse(bank$marital=='married',1,0)
bank$marti <- ifelse(bank$marital=='divorced',1,0)

#default dummy variables
bank$defu_dum <- ifelse(bank$default=='yes',1,0)

#Education dummy variables 
bank$edu_ukn <- ifelse(bank$education=='unknown',1,0)
bank$edu_sec <- ifelse(bank$education=="secondary",1,0)
bank$edu_prim <- ifelse(bank$education=='primary',1,0)
bank$edu_teri<- ifelse(bank$education=='tertiary',1,0)

#housing dummy varibales 
bank$housin_dum <- ifelse(bank$housing=='yes',1,0)

#loan dummy variables
bank$loan_dum <- ifelse(bank$loan=='yes',1,0)

#contact dummy variables
bank$contact_ukn <- ifelse(bank$contact=='unknown',1,0)
bank$contact_tel <- ifelse(bank$contact=="telephone",1,0)
bank$contact_cel <- ifelse(bank$contact=='cellular',1,0)

#housing dummy varibales bank$edu_ykn <- ifelse(bank$education=='unknown',1,0)
bank$month1 <- ifelse(bank$month=="jan",1,0)
bank$month2 <- ifelse(bank$month=='feb',1,0)
bank$month3<- ifelse(bank$month=='mar',1,0)
bank$month4 <- ifelse(bank$month=="apr",1,0)
bank$month5<- ifelse(bank$month=='may',1,0)
bank$month6<- ifelse(bank$month=='jun',1,0)
bank$month7<- ifelse(bank$month=="july",1,0)
bank$month8<- ifelse(bank$month=='aug',1,0)
bank$month9<- ifelse(bank$month=='sep',1,0)
bank$month10<- ifelse(bank$month=="oct",1,0)
bank$month11<- ifelse(bank$month=='nov',1,0)
bank$month12<- ifelse(bank$month=='dec',1,0)

#contact witnh client
bank$pdays[bank$pdays > 0]<- 1
bank$pdays[bank$pdays <= 0] <- 0

#poutcome dummy variables 
bank$pout_ukn<- ifelse(bank$poutcome=='unknown',1,0)
bank$pout_fail<- ifelse(bank$poutcome=="failure",1,0)
bank$pout_oth<- ifelse(bank$poutcome=='other',1,0)
bank$pout_succ<- ifelse(bank$poutcome=='success',1,0)

#y dummy variables
bank$y_dum <- ifelse(bank$y=='yes',1,0)

banks <- bank[ ,-c(2,3,4,5,7,8,9,11,16,17) ]

View(banks)
summary(banks)
#model building
model <- glm(y_dum~.,family ='binomial',data = banks)
summary(model)

ban <- banks[,-c(8,9,22,27,32,39,48)]
View(ban)
model1 <-glm(y_dum~.,family ='binomial',data =ban)
summary(model1)
bank1 <- ban[,-c(1,6,7,8,18,20,21,34,37,39)]
View(bank1)
model2 <-glm(y_dum~.,family ='binomial',data =bank1)
summary(model2)
bank2 <- bank1[,-c(5,31)]
model3<-glm(y_dum~.,family ='binomial',data =bank2)
summary(model3)
bank3<- bank2[,-c(9,11)]
model4<-glm(y_dum~.,family ='binomial',data =bank3)
summary(model4)
prob <- predict(model4,bank3,type="response")
confusion<-table(prob>0.5,bank3$y_dum )
confusion
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
library(ROCR)               
rocrpred<-prediction(prob,bank3$y_dum)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

