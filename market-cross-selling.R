#Clean the environment
rm(list=ls(all=T))

#set current working directory
setwd('D:/INSOFE/Insofe_ project/Target_Marketing_and_cross_selling_Data')

#loading required libraries
library(XLConnect)
library(dplyr)
library(compare)
library(ggplot2)
library(car)
library(MASS)
library(vegan)
library(dummies)
library(e1071)
library(C50)
#library(corrplot)
#Loading dataset
df = readWorksheet(loadWorkbook("Target Marketing and cross selling - Data.xls"), sheet = "Sheet1", header = TRUE)


#reanmeing column names
colnames(df) <- tolower(gsub("\\.","_",colnames(df)))
str(df)
df = df %>% mutate(late_service = as.numeric(as.Date(complete_date,format = "%y-%m-%d") - as.Date(call_date,format = "%y-%m-%d")))
df = df %>% mutate(setup_call_diff = as.numeric(as.Date(call_date,format = "%y-%m-%d") - as.Date(setup_date,format = "%y-%m-%d")))


#Changing data type
df$customer_id = as.factor(df$customer_id)
df$customer_type = as.factor(df$customer_type)
#removing unwanted indepentent attributes
df = df[,c("customer_id","customer_type","rev_code","call_date","complete_date","schedule_date","dispatch_date","late_service","setup_call_diff")]

#reordering data
cutomertype <- df %>% group_by(customer_type) %>% arrange(desc(customer_type))
customerID <- data.frame(cutomertype %>% group_by(customer_id) %>% arrange(desc(customer_id)))

#creating dulicate of call date to calculate number of days between successive calls
secondcall <- customerID$call_date
customerID <- cbind(customerID,secondcall)

#arrangind data by calls in descending order
customerID = data.frame(customerID %>% group_by(customer_id) %>% arrange(customer_id,desc(call_date)))

# calculating number of days between calls
days = as.numeric(as.Date(customerID$secondcall[2:5284],format = "%y-%m-%d") - as.Date(customerID$call_date[1:5283],format = "%y-%m-%d"))
days = data.frame(days)
nrow(days)
days = rbind(c(0),days)

class(days)
customerID = cbind(customerID,days)
rm(secondcall)
#removing duplicate second call column
customerID <- customerID[,-10]
# converting negative values into pisitive if any
customerID$days = as.numeric(lapply(customerID$days, abs))
customerID$setup_call_diff = as.numeric(lapply(customerID$setup_call_diff, abs))

#Removing Noise from data
customerID$customer_id == 'C000197'
customerID$customer_id == 'C000436'
customerID <- customerID[-c(982,983,2177,2178),]

# calculating delays value 
customerID %>% filter(dispatch_date > schedule_date)
delay = data.frame()
delay = customerID[,"dispatch_date"] > customerID[,"schedule_date"]
delay = as.character(delay)
k = 1

# convertinf false and true into 0 and 1
while (k< 5281){
  if(delay[k] == "FALSE") delay[k] = 0
  else delay[k] = 1
  k = k+1
}
delay = as.factor(delay)
customerID = cbind(customerID,delay)
customerID$customer_id = as.character(customerID$customer_id)
rm(cutomertype,days,delay,k)
# removing first transaction of each customer
raw_data2 = data.frame()
y = customerID %>% group_by(customer_id) %>% summarise(n = n())
y %>% filter(n==2)
y$n[1]
i = 1
k = 1
while(i< 5281){
  for(j in c(1:1056)){
    if(customerID$customer_id[i]== y$customer_id[j]){
      s = y$n[j]-1
      raw_data2 = rbind(raw_data2,customerID[i,])
      break}
  }
  i = i+y$n[k]
  k = k+1
}
customerID = anti_join(customerID,raw_data2)
rm(raw_data2,i,j,k,s,y)
#calculating dependent variable bases on mean value by customer type
churn = c(rep(1,4224))
customerID = cbind(customerID,churn)

cust1 = customerID %>% filter(customer_type=="CustType01")
cust2 = customerID %>% filter(customer_type=="CustType02")
cust3 = customerID %>% filter(customer_type=="CustType03")
customerID %>% group_by(customer_type) %>% summarize(n = n(),mean(days))
Days1 = cust1$days
cust1$churn = lapply(Days1,function(x){
  if(x>c(151)) x = 1
  else x= 0
})

Days2 = cust2$days
cust2$churn = lapply(Days2,function(x){
  if(x>c(140)) x = 1
  else x= 0
})

Days3 = cust3$days
cust3$churn = lapply(Days3,function(x){
  if(x>c(120)) x = 1
  else x= 0
})

train_data = rbind(cust1,cust2,cust3)
train_data$churn = as.factor(as.character(train_data$churn))
str(train_data)
rm(cust3,cust2,cust1,churn,Days3,Days2,Days1)
# updating churn 
j= 1
while (j< 4225){
  if(train_data$delay[j] == 1)
    train_data$churn[j] = 1
  else train_data$churn[j] = train_data$churn[j]
  j = j+1
}
#train_data %>% summarize(n = n(),mean(late_serive))
#train_data %>% group_by(customer_type) %>% summarise(n = n(),mean(setup_call_diff))

#updating churn variable in reference to late service
j= 1
while (j< 4225){
  if(train_data$late_service[j] > 1)
    train_data$churn[j] = 1
  else train_data$churn[j] = train_data$churn[j]
  j = j+1
}

# updating churn column in reference to setup call diff
j= 1
while (j< 4225){
  if(train_data$setup_call_diff[j] > 365)
    train_data$churn[j] = 1
  else train_data$churn[j] = train_data$churn[j]
  j = j+1
}


train_1= train_data %>% filter(churn == 1) %>%mutate(service_type = 0)
train_2 = train_data %>% filter(churn == 0) %>%mutate(service_type = rev_code)
train_data = rbind(train_1,train_2)
train_data$service_type = as.factor(train_data$service_type)
rm(train_1,train_2,j)




# keeping last transaction of each customer as test data
test_data = data.frame()
y = train_data %>% group_by(customer_id) %>% summarise(n = n())
y = y%>% arrange(desc(customer_id))
backuptrain = train_data
train_data = data.frame(train_data %>% group_by(customer_id) %>% arrange(desc(customer_id)))
y$n[1]
i = 1
k = 1

while(i< 4225){
  for(j in c(1:1056)){
    if(train_data$customer_id[i]== y$customer_id[j]){
      s = y$n[j]-1
      p = s+i
      test_data = rbind(test_data,train_data[p,])
      break}
  }
  i = i+y$n[k]
  k = k+1
}
train_data = anti_join(train_data,test_data)
rm(i,j,k,p,s,y,customerID)
#train_data <- train_data %>% group_by(customer_id) %>% arrange(desc(customer_id))
#test_data <- test_data %>% group_by(customer_id) %>% arrange(desc(customer_id))

model = glm(churn ~ .-customer_id-complete_date-schedule_date-call_date-dispatch_date-service_type,data = train_data,family = binomial)
summary(model)
#alias(glm(churn ~ .,data = train_data,family = binomial))
#as.formula(
#  paste(
#    paste(deparse(churn ~ train), collapse=""), 
#    paste(attributes(alias(model)$Complete)$dimnames[[1]], collapse="-"),
#    sep="-"
#  )
#)
#stepAIC(model, direction = "both")
vif(model)


#names(model)
prob<-predict(model, type="response")
train_pred_class <- ifelse(prob> 0.5, 1, 0)
#table(train_data$churn,pred_class)
# Test results 
fitted.results <- predict(model,test_data,type='response')
test_pred_class <- ifelse(fitted.results > 0.5,1,0)
#table(test_data$churn,fitted.class)


library(ROCR)
p <- predict(model,test_data, type="response")
pr <- prediction(p, test_data$churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(a=0, b= 1)
rm(p,pr,prf,prob)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Error Metric
conf.mat = table(train_data$churn,train_pred_class)
train_accuracy = sum(diag(conf.mat))/sum(conf.mat)
train_recall = conf.mat[2,2]/sum(conf.mat[2,])

# Error Metric
conf.mat1 = table(test_data$churn,test_pred_class)
test_accuracy = sum(diag(conf.mat1))/sum(conf.mat1)
test_recall = conf.mat1[2,2]/sum(conf.mat1[2,])


cat("Recall in Training", train_recall, '\n',
    "Recall in Testing", test_recall, '\n',
    "Accuracy in Training", train_accuracy, '\n',
    "Accuracy in Testing", test_accuracy, '\n'
)

#Recall in Training 0.9517544 
#Recall in Testing 0.9786325 
#Accuracy in Training 0.9305556 
#Accuracy in Testing 0.8560606

rm(conf.mat,conf.mat1,fitted.results,test_pred_class,train_pred_class,train_recall,train_accuracy,test_recall,test_accuracy)


# Decision Trees using C5.0 (For Classification Problem)
dtC50= C5.0(churn ~ days+delay+late_service+customer_type, data = train_data, rules=TRUE)
summary(dtC50)
C5imp(dtC50, pct=TRUE)
plot(dtC50)


conf.mat1_c50_train = table(train_data$churn,predict(dtC50, newdata=train_data, type="class"))
accuracy_C50_train = sum(diag(conf.mat1_c50_train))/sum(conf.mat1_c50_train)
recall_c50_train = conf.mat1_c50_train[2,2]/sum(conf.mat1_c50_train[2,])

conf.mat1_c50_test = table(test_data$churn,predict(dtC50, newdata=test_data, type="class"))
accuracy_C50_test = sum(diag(conf.mat1_c50_test))/sum(conf.mat1_c50_test)
recall_c50_test = conf.mat1_c50_test[2,2]/sum(conf.mat1_c50_test[2,])
rm(conf.mat1_c50_test,conf.mat1_c50_train,conf.mat1,conf.mat)

cat("Recall in Training", recall_c50_train, '\n',
    "Recall in Testing", recall_c50_test, '\n',
    "Accuracy in Training", accuracy_C50_train, '\n',
    "Accuracy in Testing", accuracy_C50_test, '\n'
    )
#Recall in Training 0.8078947 
#Recall in Testing 0.7136752 
#Accuracy in Training 0.8431187 
#Accuracy in Testing 0.8503788

rm(accuracy_C50_test,accuracy_C50_train,recall_c50_test,recall_c50_train)


#####################################################################

trainc50 = data.frame(backuptrain %>% group_by(customer_id) %>% arrange(desc(customer_id)))
testc50 = data.frame()
y = trainc50 %>% group_by(customer_id) %>% summarise(n = n())
y = y%>% arrange(desc(customer_id))
trainc50$service_type = as.factor(trainc50$service_type)
trainc50$call_date = as.factor(trainc50$call_date)
trainc50$complete_date = as.factor(trainc50$complete_date)
trainc50$schedule_date = as.factor(trainc50$schedule_date)
trainc50$dispatch_date = as.factor(trainc50$dispatch_date)

y$n[1]
i = 1
k = 1

while(i< 4225){
  for(j in c(1:1056)){
    if(trainc50$customer_id[i]== y$customer_id[j]){
      s = y$n[j]-1
      p = s+i
      testc50 = rbind(testc50,trainc50[p,])
      break}
  }
  i = i+y$n[k]
  k = k+1
}
trainc50 = anti_join(trainc50,testc50)
rm(i,j,k,p,s)
# Decision Trees using C5.0 (For Classification Problem)
names(trainc50)

servicedtC50= C5.0(service_type ~ delay+customer_type+rev_code+late_service+days, data = trainc50, rules=TRUE)
summary(servicedtC50)
C5imp(servicedtC50, pct=TRUE)


conf.mat1_c50_train_service = table(trainc50$service_type,predict(servicedtC50, newdata=trainc50, type="class"))
accuracy_C50_train_service = sum(diag(conf.mat1_c50_train_service))/sum(conf.mat1_c50_train_service)
recall_c50_train_service = conf.mat1_c50_train_service[2,2]/sum(conf.mat1_c50_train_service[2,])

conf.mat1_c50_test_service = table(testc50$service_type,predict(servicedtC50, newdata=testc50, type="class"))
accuracy_C50_test_service = sum(diag(conf.mat1_c50_test_service))/sum(conf.mat1_c50_test_service)
recall_c50_test_service = conf.mat1_c50_test_service[2,2]/sum(conf.mat1_c50_test_service[2,])

cat("Recall in Training", accuracy_C50_train_service, '\n',
    "Recall in Testing", recall_c50_test_service, '\n',
    "Accuracy in Training", accuracy_C50_train_service, '\n',
    "Accuracy in Testing", accuracy_C50_test_service, '\n')

#Recall in Training 0.8440657 
#Recall in Testing 0.9140811 
#Accuracy in Training 0.8440657 
#Accuracy in Testing 0.8721591     
rm(conf.mat1_c50_test_service,conf.mat1_c50_train_service,accuracy_C50_test_service,accuracy_C50_train_service,recall_c50_test_service,recall_c50_train_service)    
############################################ SVM #######################################

trainsvm = data.frame(backuptrain %>% group_by(customer_id) %>% arrange(desc(customer_id)))
trainsvm$service_type = as.factor(trainsvm$service_type)
trainsvm = trainsvm[,-c(4,5,6,7)]
# Build the classification model.
ind_Num_Attr = c("late_service",'setup_call_diff','days')
cat_attr = setdiff(names(trainsvm),ind_Num_Attr)
ind_Cat_Attr = setdiff(cat_attr, "service_type")

# Standardizing the numeric data
cla_Data = decostand(trainsvm[,ind_Num_Attr], "range") 
rm(ind_Num_Attr)

# Convert all categorical attributes to numeric 
# 1. Using dummy function, convert education and family categorical attributes into numeric attributes 
custtype = dummy(trainsvm$customer_type)
revcode = dummy(trainsvm$rev_code)
delays = dummy(trainsvm$delay)
churns = dummy(trainsvm$churn)
customer_id = trainsvm$customer_id
cla_Data = cbind(cla_Data,customer_id,custtype, revcode, delays,churns)
rm(churns,delays,custtype,revcode)
#ind_Cat_Attr = setdiff(ind_Cat_Attr, c("dispatch_date", "call_date",'schedule_date','complete_date','customer_id'))


# 2. Using as.numeric function, convert remaining categorical attributes into numeric attributes 
#cla_Data = cbind(cla_Data, sapply(data[,ind_Cat_Attr], as.numeric))
rm(ind_Cat_Attr)
ind_Attr = names(cla_Data)

# Append the Target attribute 
cla_Data = cbind(cla_Data, service=trainsvm[,"service_type"]) 
trainsvm = cla_Data
y = trainsvm %>% group_by(customer_id) %>% summarise(n = n())
y = y%>% arrange(desc(customer_id))


testsvm= data.frame()
y$n[1]
i = 1
k = 1

while(i< 4225){
  for(j in c(1:1056)){
    if(trainsvm$customer_id[i]== y$customer_id[j]){
      s = y$n[j]-1
      p = s+i
      testsvm = rbind(testsvm,trainsvm[p,])
      break}
  }
  i = i+y$n[k]
  k = k+1
}
rm(i,j,k,p,s)
ind_Attr = ind_Attr[-4]
ind_Attr[]
ind_Attr1 = ind_Attr[-c(12,13)]
trainsvm = anti_join(trainsvm,testsvm)
trainsvm = trainsvm[,-4]
testsvm = testsvm [,-4]
model_svm = svm(x = trainsvm[,ind_Attr1], 
            y = trainsvm$service, 
            type = "C-classification", 
            kernel = "linear", cost = 0.8, gamma = 1) 

# Look at the model summary
summary(model_svm)

model_svm$index

#plot(cmdscale(dist(trainsvm[,ind_Attr])),
    #col = as.integer(trainsvm$service),
     #pch = c("o","+")[1:nrow(trainsvm) %in% model_svm$index + 1])

# Predict on train data  
pred_Train_svm  =  predict(model_svm, trainsvm[,ind_Attr1])  

# Build confusion matrix and find accuracy   
cm_Train = table(trainsvm$service, pred_Train_svm)
accu_Train_svm= sum(diag(cm_Train))/sum(cm_Train)
recall_Train_svm = cm_Train[2,2]/sum(cm_Train[2,])
accu_Train_svm
recall_Train_svm
# Predict on test data
pred_Test_svm = predict(model_svm, testsvm[,ind_Attr1]) 

# Build confusion matrix and find accuracy   
cm_Test = table(testsvm$service, pred_Test_svm)
accu_Test_svm= sum(diag(cm_Test))/sum(cm_Test)
recall_Test_svm = cm_Test[2,2]/sum(cm_Test[2,])
accu_Test_svm
recall_Test_svm
# accu_Train_svm 0.9318182
# accu_Test_svm  0.84375
# recall_Train_svm 0.886
# recall_Test_svm 0.813

rm(accu_Test_svm,accu_Train_svm,ind_Attr1,ind_Attr,pred_Test_svm,pred_Train_svm,cat_attr,cm_Test,cm_Train,customer_id)
rm(trainsvm,testsvm,cla_Data)





################################################## VISUALISATION ############################################################################

### This code is just for visualization###################
df1 =df

#reanmeing column names
colnames(df1) <- tolower(gsub("\\.","_",colnames(df1)))
str(df1)
df1 = df1 %>% mutate(late_service = as.numeric(as.Date(complete_date,format = "%y-%m-%d") - as.Date(call_date,format = "%y-%m-%d")))
df1 = df1 %>% mutate(setup_call_diff = as.numeric(as.Date(call_date,format = "%y-%m-%d") - as.Date(setup_date,format = "%y-%m-%d")))


#df$call_date - df$complete_date
#arranging data bi customer type and customer id
df1$customer_id = as.factor(df1$customer_id)
df1$customer_type = as.factor(df1$customer_type)
#removing unwanted indepentent attributes
df1 = df1[,c("customer_id","state","customer_type","rev_code","call_date","complete_date","schedule_date","dispatch_date","late_service","setup_call_diff")]

cutomertype1 <- df1 %>% group_by(customer_type) %>% arrange(desc(customer_type))
customerID1 <- data.frame(cutomertype1 %>% group_by(customer_id) %>% arrange(desc(customer_id)))
#creating dulicate of call date to calculate number of days between successive calls
secondcall1 <- customerID1$call_date
customerID1 <- cbind(customerID1,secondcall1)
#arrangind data by calls in descending order
customerID1 = data.frame(customerID1 %>% group_by(customer_id) %>% arrange(customer_id,desc(call_date)))
# calculating number of days between calls
days1 = as.numeric(as.Date(customerID1$secondcall1[2:5284],format = "%y-%m-%d") - as.Date(customerID1$call_date[1:5283],format = "%y-%m-%d"))


days1 = data.frame(days1)
nrow(days1)
days1 = rbind(c(0),days1)

class(days1)
customerID1 = cbind(customerID1,days1)
rm(secondcall1)
#removing duplicate second call column
customerID1 <- customerID1[,-11]
# converting negative values into pisitive if any
customerID1$days1 = as.numeric(lapply(customerID1$days1, abs))
customerID1$setup_call_diff = as.numeric(lapply(customerID1$setup_call_diff, abs))
customerID1$customer_id == 'C000197'
customerID1$customer_id == 'C000436'

customerID1 <- customerID1[-c(982,983,2177,2178),]
# calculating delays value 
customerID1 %>% filter(dispatch_date > schedule_date)
delay1 = data.frame()
delay1 = customerID1[,"dispatch_date"] > customerID1[,"schedule_date"]
delay1 = as.character(delay1)
k = 1

# convertinf false and true into 0 and 1
while (k< 5281){
  if(delay1[k] == "FALSE") delay1[k] = 0
  else delay1[k] = 1
  k = k+1
}
delay1 = as.factor(delay1)
customerID1 = cbind(customerID1,delay1)
customerID1$customer_id = as.character(customerID1$customer_id)

raw_data21 = data.frame()
y = customerID1 %>% group_by(customer_id) %>% summarise(n = n())
y %>% filter(n==2)
y$n[1]
i = 1
k = 1
while(i< 5281){
  for(j in c(1:1056)){
    if(customerID1$customer_id[i]== y$customer_id[j]){
      s = y$n[j]-1
      raw_data21 = rbind(raw_data21,customerID1[i,])
      break}
  }
  i = i+y$n[k]
  k = k+1
}
customerID1 = anti_join(customerID1,raw_data21)
rm(raw_data2,i,j,k,s,y)

rm(cutomertype1,df1,days1,delay1,k,raw_data21)
#calculating dependent variable bases on mean value by customer type
churn1 = c(rep(1,4224))
customerID1 = cbind(customerID1,churn1)

cust1 = customerID1 %>% filter(customer_type=="CustType01")
cust2 = customerID1 %>% filter(customer_type=="CustType02")
cust3 = customerID1 %>% filter(customer_type=="CustType03")
customerID1 %>% group_by(customer_type) %>% summarize(n = n(),mean(days1))
Days1 = cust1$days1
cust1$churn1 = lapply(Days1,function(x){
  if(x>c(151)) x = 1
  else x= 0
})

Days2 = cust2$days1
cust2$churn1 = lapply(Days2,function(x){
  if(x>c(139)) x = 1
  else x= 0
})

Days3 = cust3$days1
cust3$churn1 = lapply(Days3,function(x){
  if(x>c(120)) x = 1
  else x= 0
})

train_data1 = rbind(cust1,cust2,cust3)
train_data1$churn1 = as.factor(as.character(train_data1$churn1))
str(train_data1)
rm(cust3,cust2,cust1,churn1,Days3,Days2,Days1)
# updating churn 
j= 1
while (j< 4225){
  if(train_data1$delay1[j] == 1)
    train_data1$churn1[j] = 1
  else train_data1$churn1[j] = train_data1$churn1[j]
  j = j+1
}
rm(customerID1,raw_data21,y,churn1,j)


train_1= train_data1 %>% filter(churn1 == 1) %>%mutate(service_type = 0)
train_2 = train_data1 %>% filter(churn1 == 0) %>%mutate(service_type = rev_code)
train_data1 = rbind(train_1,train_2)
train_data1$service_type = as.factor(train_data1$service_type)
rm(train_1,train_2)

#work only on numeric data
#corrplot(cor(df), order="hclust",tl.col="black",tl.cex=.75)
#factanal(df,8)

ggplot(data = df, aes(x = State,fill = Customer.Type)) + geom_bar() +
  labs(x = "States", y = "Number of customers transactions")


ggplot(data = backuptrain, aes(x = customer_type,fill = churn)) + geom_bar() +
  labs(x = "customer type", y = "Total number of customers")


ggplot(data = train_data1, aes(x = state,fill = churn1)) + geom_bar() +
  labs(x = "States", y = "Total number of customers")

type2cust =train_data1 %>% filter(customer_type == "CustType02")

ggplot(data = type2cust, aes(x = state,fill = churn1)) + geom_bar() +
  labs(x = "States", y = "customer type 2 count")
#removing churn customers 
service_data <- train_data1 %>% filter(service_type != 0)

ggplot(data = service_data, aes(x = state,fill = service_type)) + geom_bar() +
  labs(x = "States", y = "Numer of customers")

########################################################################################################################

