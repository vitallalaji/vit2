data<-read.csv(file.choose())
str(data)
View(data)
data<-data[-c(1,2)]
table(data$delivery_status)
#library(anytime)
#data$orderdate<-anydate(data$orderdate)
#data$shipment_date<-as.Date(data$shipment_date)
data$expected_date<-as.Date(data$expected_date)
data$orderdate<-as.Date(data$orderdate)
data$shipment_date<-as.Date(data$shipment_date)

library(ggplot2)
ggplot(data, aes(x = price_of_the_product, fill = factor(delivery_status))) +
  geom_histogram() +
  xlab("price of the product") +
  ylab("Total Count") +
  labs(fill = "delivery_status") 
table(data$delivery_status)


ggplot(data, aes(x = tracking, fill = factor(delivery_status))) +
  geom_bar(width = 0.5) +
  xlab("tracking") +
  ylab("Total Count") +
  labs(fill = "delivery_status") 

ggplot(data, aes(x = payment_method, fill = delivery_status)) +
  geom_bar(width = 0.5) +
  xlab("payment method") +
  ylab("Total Count") +
  labs(fill = "delivery_status") 

ggplot(data,aes(x=distance_in_kms,y=delivery_status,fill=delivery_status))+
  geom_point()+
  xlab("distance of kms")+
  ylab("total counts")+
  labs(fill="delivery status")
  


ggplot(data, aes(x=distance_in_kms, y=delivery_status)) + 
  geom_area(aes(col=delivery_status))
?geom_jitter
unique(data$distance_in_kms)
dup.names <-data[which(duplicated((data$distance_in_kms))), "distance_in_kms"]

# random forest

train<-data[1:7000,]
test<-data[7001:10000,]



rf.label <- as.factor(train$delivery_status)

set.seed(1234)
library(randomForest)
rf.1 <- randomForest(delivery_status~.,data=train ,mtry=4,importance = TRUE, ntree = 1000)
rf.1
importance(rf.1)
varImpPlot(rf.1)
summary(rf.1)
traindf<-predict(rf.1,train)
table(traindf,train$delivery_status)
mean(traindf==train$delivery_status)
testdf<-predict(rf.1,test,type="class")
t<-table(testdf,test$delivery_status)
t
sum(diag(t))/sum(t)

#adaboost

set.seed(111)
dat = circle_data(n = 500)
train_index = sample(1:500, 400)

ada = adaboost(train[,-3], train[3,], tree_depth = 2,
               n_rounds = 200, verbose = TRUE)
print(ada)
yhat_ada = predict(ada, dat$X[-train_index,])

adaboost<-boosting(delivery_status~., data=train, boos=TRUE, mfinal=20,coeflearn='Breiman')
summary(adaboost)
adaboost$trees
adaboost$weights
adaboost$importance
errorevol(adaboost,train)
v<-predict(adaboost,train)
mean(v==train$delivery_status)
t1<-adaboost$trees[[1]]
library(tree)
plot(t1)
text(t1,pretty=0)
z<-predict(adaboost,test)
y<-table(z,test$delivery_status)
mean(test$delivery_status==z)

#