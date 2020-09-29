#=======================================================================
#
# Project 4 
#
#=======================================================================
#calling all libraries that we are going to use
library(readr)
library(ggplot2) 

#setting up working directory
setwd("C:/Users/ahmasiri/Desktop/PGP DSBA/Data/Project 4 - Cars Case Study")
#reading data from csv file to Cars variable and view it
Cars <- read.csv("Cars-dataset.csv")
attach(Cars)



#Prepare the data for analysis
Cars$Engineer = as.factor(Engineer)
Cars$MBA = as.factor(MBA)
Cars$license = as.factor(license)

#dealing with NA values in MBA variable
Cars[is.na(Cars)] <- 1


#check if ther is any NA value in dataset
anyNA(Cars)


# EDA 

#Retrieve the dimension of an object.
dim(Cars)

#Get the names of an object.
names(Cars)

#Display the internal structure of an dataset.
str(Cars)

#Returns the first 10 rows of the dataset.
head(Cars, 10)

#Returns the last 10 rows of the dataset.
tail(Cars, 10)

#Return a summary of the dataset variables.
summary(Cars)





#graph for all variable variables
# Quantitative
par(mfrow=c(2,2))
boxplot(Age, main = "Age")
boxplot(Work.Exp, main = "Work.Exp")
hist(Age, main = "Age")
hist(Work.Exp, main = "Work.Exp")

par(mfrow=c(2,2))
boxplot(Salary,main = "Salary(K)")
boxplot(Distance, main = "Distance")
hist(Salary, main = "Salary(K)")
hist(Distance, main = "Distance")

# catagorical
par(mfrow=c(2,2))
ggplot(Cars) + geom_bar(aes(x = Gender))
ggplot(Cars) + geom_bar(aes(x = Engineer))
ggplot(Cars) + geom_bar(aes(x = MBA))
ggplot(Cars) + geom_bar(aes(x = license))
ggplot(Cars) + geom_bar(aes(x = Transport))


#Bi-Variate Analysis
#kernel density plots
ggplot(Cars,
       aes(x = Age, #quantitative variable
           fill = factor(Transport, #defining x axis a categorical
                         levels = c("2Wheeler", "Car", "Public Transport"),
                         labels = c("2Wheeler", "Car", "Public Transport")))) +
  geom_density(alpha = 0.8) + #setting transparency of graph to keep overlaps visible
  labs(fill = "Transport", # setting title of legend
       x = "Age",
       title = "Employee Age by Transport")
#jitter and box plots
ggplot(Cars,
       aes(x = factor(Transport, #defining x axis a categorical
                      labels = c("2Wheeler", "Car", "Public Transport")),
           y = Age,
           color = Transport)) + #specifying that coloring is to be based on drive type
  geom_boxplot(size=1, #makes the lines thicker
               outlier.shape = 1, #specifies circles for outliers
               outlier.color = "black", #makes outliers black
               outlier.size = 3) + #increases the size of the outlier symbol
  geom_jitter(alpha = 0.5, #setting transparency of graph
              width=.2) + #decreases the amount of jitter (.4 is the default)
  labs(title = "Employee Age by Transport Type",
       x = "",
       y = "Age") +
  theme_minimal() + #setting minimal theme (no background color)
  theme(legend.position = "none") + #hiding legend
  coord_flip() #x and y axes are reversed


#kernel density plots
ggplot(Cars,
       aes(x = Salary, #quantitative variable
           fill = factor(Transport, #defining x axis a categorical
                         levels = c("2Wheeler", "Car", "Public Transport"),
                         labels = c("2Wheeler", "Car", "Public Transport")))) +
  geom_density(alpha = .8) + #setting transparency of graph to keep overlaps visible
  labs(fill = "Transport", # setting title of legend
       x = "Salary",
       title = "Employee Salary by Transport")

#jitter and box plots
ggplot(Cars,
       aes(x = factor(Transport, #defining x axis a categorical
                      labels = c("2Wheeler", "Car", "Public Transport")),
           y = Salary,
           color = Transport)) + #specifying that coloring is to be based on drive type
  geom_boxplot(size=1, #makes the lines thicker
               outlier.shape = 1, #specifies circles for outliers
               outlier.color = "black", #makes outliers black
               outlier.size = 3) + #increases the size of the outlier symbol
  geom_jitter(alpha = 0.5, #setting transparency of graph
              width=.2) + #decreases the amount of jitter (.4 is the default)
  labs(title = "Employee Salary by Transport Type",
       x = "",
       y = "Salary (k)") +
  theme_minimal() + #setting minimal theme (no background color)
  theme(legend.position = "none") + #hiding legend
  coord_flip() #x and y axes are reversed


#kernel density plots
ggplot(Cars,
       aes(x = Work.Exp, #quantitative variable
           fill = factor(Transport, #defining x axis a categorical
                         levels = c("2Wheeler", "Car", "Public Transport"),
                         labels = c("2Wheeler", "Car", "Public Transport")))) +
  geom_density(alpha = .8) + #setting transparency of graph to keep overlaps visible
  labs(fill = "Transport", # setting title of legend
       x = "Work.Exp",
       title = "Employee Work.Exp by Transport")
#jitter and box plots
ggplot(Cars,
       aes(x = factor(Transport, #defining x axis a categorical
                      labels = c("2Wheeler", "Car", "Public Transport")),
           y = Work.Exp,
           color = Transport)) + #specifying that coloring is to be based on drive type
  geom_boxplot(size=1, #makes the lines thicker
               outlier.shape = 1, #specifies circles for outliers
               outlier.color = "black", #makes outliers black
               outlier.size = 3) + #increases the size of the outlier symbol
  geom_jitter(alpha = 0.5, #setting transparency of graph
              width=.2) + #decreases the amount of jitter (.4 is the default)
  labs(title = "Employee Work.Exp by Transport Type",
       x = "",
       y = "Work.Exp") +
  theme_minimal() + #setting minimal theme (no background color)
  theme(legend.position = "none") + #hiding legend
  coord_flip() #x and y axes are reversed


#kernel density plots
ggplot(Cars,
       aes(x = Distance, #quantitative variable
           fill = factor(Transport, #defining x axis a categorical
                         levels = c("2Wheeler", "Car", "Public Transport"),
                         labels = c("2Wheeler", "Car", "Public Transport")))) +
  geom_density(alpha = .8) + #setting transparency of graph to keep overlaps visible
  labs(fill = "Transport", # setting title of legend
       x = "Distance",
       title = "Employee Distance by Transport")
#jitter and box plots
ggplot(Cars,
       aes(x = factor(Transport, #defining x axis a categorical
                      labels = c("2Wheeler", "Car", "Public Transport")),
           y = Distance,
           color = Transport)) + #specifying that coloring is to be based on drive type
  geom_boxplot(size=1, #makes the lines thicker
               outlier.shape = 1, #specifies circles for outliers
               outlier.color = "black", #makes outliers black
               outlier.size = 3) + #increases the size of the outlier symbol
  geom_jitter(alpha = 0.5, #setting transparency of graph
              width=.2) + #decreases the amount of jitter (.4 is the default)
  labs(title = "Employee Distance by Transport Type",
       x = "",
       y = "Distance") +
  theme_minimal() + #setting minimal theme (no background color)
  theme(legend.position = "none") + #hiding legend
  coord_flip() #x and y axes are reversed




# stacked bar chart
ggplot(Cars,
       aes(x = Engineer,
           fill = factor(Transport, #defining x axis a categorical
                         levels = c("2Wheeler", "Car", "Public Transport"),
                         labels = c("2Wheeler", "Car", "Public Transport")))) +
  labs(fill = "Transport", # setting title of legend
       x = "Engineer",
       title = "Employee Engineer by Transport") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked

# stacked bar chart
ggplot(Cars,
       aes(x = MBA,
           fill = factor(Transport, #defining x axis a categorical
                         levels = c("2Wheeler", "Car", "Public Transport"),
                         labels = c("2Wheeler", "Car", "Public Transport")))) +
  labs(fill = "Transport", # setting title of legend
       x = "MBA",
       title = "Employee MBA by Transport") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked


# stacked bar chart
ggplot(Cars,
       aes(x = license,
           fill = factor(Transport, #defining x axis a categorical
                         levels = c("2Wheeler", "Car", "Public Transport"),
                         labels = c("2Wheeler", "Car", "Public Transport")))) +
  labs(fill = "Transport", # setting title of legend
       x = "license",
       title = "Employee license by Transport") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked






# we are convering dependent varible to 1 and 0 where 1 indicate Cars and 0 indicates others
Cars$BiTransport = ifelse(Transport == "Car",1,0)
Cars$BiTransport = as.factor(Cars$BiTransport)
summary(Cars)

library(caTools)

# SEPERATE DATE TO BE TOW PARS ONE FOR TRAIN AND OTHER FOR TEST 
set.seed(300)
spl = sample.split(Cars$BiTransport, SplitRatio=0.75)
train = subset(Cars, spl ==T)
test = subset(Cars, spl==F)



library(class)

#KNN
set.seed(1)
knnmod <- caret::train(Transport ~ .,
                       method     = "knn",
                       tuneGrid   = expand.grid(k = 2:51),
                       metric     = "Accuracy",
                       preProcess = c("scale"),
                       data       = train)
knnmod

na.omit(train)

model_knn=knn(train[,c(3,4,8)],test[,c(3,4,8)],train$Transport,k=19)

caret::confusionMatrix(test$Transport,model_knn,positive="Car")

lrmod <- caret::train(BiTransport ~ Engineer+MBA+license,
                      method     = "glm",
                      metric     = "Sensitivity",
                      data       = train)

lrpred<-predict(lrmod,newdata=test)
lrpred

Name = c("Naive_Bayes", "KNN", "Logistic_Regression")
Accuracy = c(0.93,0.94,0.92)
Sensitivity=c(0.98,1,0.87)
Specificity=c(0.90,0.91,0.97)
output = data.frame(Name,Accuracy,Sensitivity,Specificity)
output




library(e1071) # to build a naive bayes model
library(ROCR)

#naiveBayes
model<-naiveBayes(BiTransport~.,data=train)
model

# generating the probabilities in prediction
ypred<-predict(model, newdata = test, type="raw")
plot(test$BiTransport,ypred[,2])

# generating the class in prediction
pred<-predict(model,newdata=test)



p_test<-prediction(ypred[,2], test$BiTransport)
perf<-performance(p_test,"tpr", "fpr")
plot(perf,colorize = TRUE)

cutoffs <-
  data.frame(
    cut = perf@alpha.values[[1]],
    fpr = perf@x.values[[1]],
    tpr = perf@y.values[[1]]
  )

head(cutoffs)

cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(subset(cutoffs, fpr < 0.1))

class_prediction_with_new_cutoff = ifelse(ypred[, 2] >= 0.0129, 1, 0)
new_confusion_matrix = table(test$BiTransport, class_prediction_with_new_cutoff)


new_accuracy = sum(diag(new_confusion_matrix)) / sum(new_confusion_matrix)
new_accuracy

new_sensitivity = new_confusion_matrix[2,2] / sum(new_confusion_matrix[2, ])
new_sensitivity

new_specificity = new_confusion_matrix[1,1] / sum(new_confusion_matrix[1, ])
new_specificity

AUC_NB=performance(p_test,"auc")@y.values
AUC_NB

ks_nb = max(attr(perf,'y.values')[[1]] - attr(perf,'x.values')[[1]])
ks_nb

GINI_NB=2*AUC_NB[[1]]-1
GINI_NB




library(MASS)
library(caret)

# Logistic Regression
## Check split consistency
prop.table(table(train$BiTransport))
prop.table(table(test$BiTransport))
prop.table(table(Cars$BiTransport))
LRmodel = glm(BiTransport~ ., data = train, family= binomial)
summary(LRmodel)

log_model = stepAIC(LRmodel, direction = "both",k=5) #loosely speaking K=5, represents (P < 0.02)
summary(log_model)
varImp(log_model)
# convert to data frame
l = data.frame(varImp(log_model))
l <- cbind(newColName = rownames(l), l)
rownames(l) <- 1:nrow(l)

# soritng the imprtance of varaible 
l[with(l, order(-Overall)), ]

exp(0.0296)
P = 0.49

# prediction on test dataset
predTrain = predict(log_model, newdata= train, type="response")
tb = table(predTrain>0.50,train$BiTransport)
tb
print('accuracy is ')
sum(diag(tb))/sum(tb)

# prediction on test dataset
predTest = predict(log_model, newdata= test, type="response")
tb = table(predTest >0.50,test$BiTransport)
tb
print('accuracy is ')
sum(diag(tb))/sum(tb)

#par(mfrow=c(1,2))
p0 <- prediction(predTrain,train$BiTransport)
p1 <- performance(p0, "tpr", "fpr")
plot(p1, main = "ROC Curve" ,colorize = TRUE) ## logistic regression model
AUC  <- as.numeric(performance(p0, "auc")@y.values) ## AUC  = 0.9083176
gini <- 2*AUC - 1                                   ## gini = 0.8166352
KS   <- max(p1@y.values[[1]] - p1@x.values[[1]])    ## KS   = 0.6511416
print('AUC')
AUC
print('KS')
KS
p0 = prediction(predTrain,train$BiTransport) 
p1 = performance(p0,"tpr","fpr")
plot(p1, main = "ROC Curve" ,colorize = TRUE)
str(p1)

cutoffs <-
  data.frame(
    cut = p1@alpha.values[[1]],
    fpr = p1@x.values[[1]],
    tpr = p1@y.values[[1]]
  )

head(cutoffs)
View(cutoffs)
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(subset(cutoffs, fpr < 0.2))
class_prediction_with_new_cutoff = ifelse(predTrain>= 0.24, 1, 0)
new_confusion_matrix = table(train$BiTransport,class_prediction_with_new_cutoff )
new_confusion_matrix
new_accuracy = sum(diag(new_confusion_matrix)) / sum(new_confusion_matrix)
new_accuracy
new_sensitivity = new_confusion_matrix[2,2] / sum(new_confusion_matrix[2, ])
new_sensitivity
new_specificity = new_confusion_matrix[1,1] / sum(new_confusion_matrix[1, ])
new_specificity
class_prediction_with_new_cutoff = ifelse(predTest>= 0.24, 1, 0)
new_confusion_matrix = table(test$BiTransport ,class_prediction_with_new_cutoff)
new_confusion_matrix
new_accuracy = sum(diag(new_confusion_matrix)) / sum(new_confusion_matrix)
new_accuracy
new_sensitivity = new_confusion_matrix[2,2] / sum(new_confusion_matrix[2, ])
new_sensitivity
new_specificity = new_confusion_matrix[1,1] / sum(new_confusion_matrix[1, ])
new_specificity



library(class)
library(e1071)
library(gbm)          # basic implementation using AdaBoost
library(xgboost)      # a faster implementation of a gbm#loading a few libraries
library(caret)        # an aggregator package for performing many machine learning models
library(ipred)
library(rpart)
library(gbm)

attach(Cars)

#logistic regression
german_logistic <- glm(Transport~., data=train, family=binomial(link="logit"))
test$log.pred<-predict(german_logistic, test, type="response")
table(test$Transport,test$log.pred>0.5)

#knn
#knn compare
knn_fit<- knn(train = train[,c(3,4,8)], test = test[,c(3,4,8)], cl= train[,8],k = 3,prob=TRUE)
table(test[,9],knn_fit)

#naive bayes
nb_gd<-naiveBayes(x=train[,c(3,4,8)], y=as.factor(train[,9]))
pred_nb<-predict(nb_gd,newdata = test[,c(3,4,8)])
table(test[,9],pred_nb)

## Bagging
Cars.bagging <- bagging(Transport ~.,
                        data=train,
                        control=rpart.control(maxdepth=5, minsplit=4))

test$pred.Transport <- predict(Cars.bagging, test)
table(test$Transport,test$pred.Transport)

#Boosting
gbm.fit <- gbm(
  formula = Transport ~ .,
  data = train,
  n.trees = 10000, #these are the number of stumps
  interaction.depth = 1,#number of splits it has to perform on a tree (starting from a single node)
  shrinkage = 0.001,#shrinkage is used for reducing, or shrinking the impact of each additional fitted base-learner(tree)
  cv.folds = 5,#cross validation folds
  n.cores = NULL, # will use all cores by default
  verbose = FALSE#after every tree/stump it is going to show the error and how it is changing
)  
test$pred.Transport <- predict(gbm.fit, test,type="response" )
#we have to put type="response" just like in logistic regression else we will have log odds
table(test$Transport,head(test$pred.Transport,105))



