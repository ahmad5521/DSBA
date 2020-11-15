
seed = 134

set.seed(seed)
index = sample(1:nrow(IDataset),0.80*nrow(IDataset))
IDataset.train1 = IDataset[index,]
IDataset.test1 = IDataset[-index,]


library(randomForest)

seed=1000

set.seed(seed)




#auc -> 0.1714639
#Income+Count_3_6_months_late+Count_6_12_months_late+Count_more_than_12_months_late    
#IDataset.train1[, c(2,3,4,5)]


#auc -> 0.0501028
#Income+Count_3_6_months_late+Count_6_12_months_late+Count_more_than_12_months_late+risk_score    
#IDataset.train1[, c(2,3,4,5,9)]


#auc -> 0.03329917
#age_in_years+Income+Count_3_6_months_late+Count_6_12_months_late+Count_more_than_12_months_late+risk_score   
#IDataset.train1[, c(14, 2,3,4,5,9]

#auc -> 0.05851644
#age_in_years+Income+Count_3_6_months_late+Count_6_12_months_late+Count_more_than_12_months_late    
#IDataset.train1[, c(14, 2,3,4,5)]


#auc -> 0.03141114
#age_in_years+Income+Count_3_6_months_late+Count_6_12_months_late+Count_more_than_12_months_late+sourcing_channel+risk_score    
#c(14, 2,3,4,5,11,9)


#auc -> 0.0266754
#age_in_years+Income+Count_3_6_months_late+Count_6_12_months_late+Count_more_than_12_months_late+sourcing_channel+premium+risk_score    
#c(14, 2,3,4,5,11,9,13)



#auc -> 0.01334393
#All

#build our random forest
rndFor = randomForest(default ~ Income+Count_3_6_months_late+Count_6_12_months_late+Count_more_than_12_months_late+risk_score, data = IDataset.train1 , 
                      ntree=501, mtry = 3, nodesize = 10,
                      importance=TRUE)

#The error rate plot w.r.t number of trees reveals that anything more than, say 51
# trees is really not that valuable.
rndFor$err.rate

plot(rndFor, main="")

legend("topright", c("OOB", "1", "0"), text.col=1:6, lty=1:3, col=1:3)

title(main="Error Rates Random Forest IDataset.train ")

set.seed(seed)


#Now we will "tune" the Random Forest by trying different m values.
#We will stick with 51 trees (odd number of trees are preferable). 
#The returned forest, "tRndFor" is the one corresponding to the best m
tRndFor = tuneRF(x = IDataset.train1[, c(2,3,4,5,9)], 
                 y= IDataset.train1 $default,
                 mtryStart = 3, 
                 ntreeTry = 251, 
                 stepFactor = 1.5, 
                 improve = 0.0001, 
                 trace=TRUE, 
                 plot = TRUE,
                 doBest = TRUE,
                 nodesize = 10, 
                 importance=TRUE
)

#List the importance of the variables. Larger the MeanDecrease values
#the more important the variable.
importance(tRndFor)


#Lets make predictions on the training data and measure the prediction error rate.
IDataset.train1 $predict.class = predict(tRndFor, IDataset.train1 , type="class")

IDataset.train1 $prob1 = predict(tRndFor, IDataset.train1 , type="prob")[,"0"]

tb.train=table(IDataset.train1 $default, IDataset.train1 $predict.class)


print('accuracy is ')
sum(diag(tb.train))/sum(tb.train)

library(ROCR)
pred_ROCR <- prediction(IDataset.train1 $prob1, IDataset.train1 $default)
roc_ROCR <- performance(pred_ROCR, measure = "tpr", x.measure = "fpr")
plot(roc_ROCR, main = "ROC curve", colorize = T)
abline(a = 0, b = 1)

auc_ROCR <- performance(pred_ROCR, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]


print('AUC is ')
auc_ROCR




trainsensitivity2 = tb.train[2,2] / sum(tb.train[2, ])
trainsensitivity2

trainspecificity2 = tb.train[1,1] / sum(tb.train[1, ])
trainspecificity2

#Now using the tuned Random Forest from the previous step, 
#and redo our errors and top decile calculations for the previously identified threshold. 

IDataset.test1$predict.class = predict(tRndFor, IDataset.test1 , type="class")

IDataset.test1$prob1 = predict(tRndFor, IDataset.test1 , type="prob")[,"0"]

tb.test=table(IDataset.test1 $default, IDataset.test1 $predict.class)

print('accuracy is ')
sum(diag(tb.test))/sum(tb.test)




testsensitivity2 = tb.test[2,2] / sum(tb.test[2, ])
testsensitivity2

testspecificity2 = tb.test[1,1] / sum(tb.test[1, ])
testspecificity2


#result
#train accuracy = 95
#train sensitivity = 99
#train specificity = 20


#test accuracy = 94
#train sensitivity = 99
#train specificity = 11




##########################################
#with smoote
##########################################

# library(DMwR)

# set.seed(seed)
# index = sample(1:nrow(IDataset),0.80*nrow(IDataset))
# IDataset.train1 = IDataset[index,]
# IDataset.test1 = IDataset[-index,]


# ## Smote : Synthetic Minority Oversampling Technique To Handle default Imbalancy In Binary Classification
# IDataset.train.balanced.data1 <- SMOTE(default ~., as.data.frame(IDataset.train1[, c(2,3,4,5,9,16)]), perc.over = 4800, k = 5, perc.under = 1000)

# as.data.frame(table(IDataset.train.balanced.data1$default))



# ## Smote : Synthetic Minority Oversampling Technique To Handle default Imbalancy In Binary Classification
# IDataset.test.balanced.data1 <- SMOTE(default ~., as.data.frame(IDataset.test1[, c(2,3,4,5,9,16)]), perc.over = 4800, k = 5, perc.under = 1000)

# as.data.frame(table(IDataset.test.balanced.data1$default))





# library(randomForest)

# seed=1000

# set.seed(seed)



# #build our random forest
# rndFor = randomForest(default ~ Income+Count_3_6_months_late+Count_6_12_months_late+Count_more_than_12_months_late+risk_score, data = IDataset.train.balanced.data1 , 
#                       ntree=501, mtry = 3, nodesize = 10,
#                       importance=TRUE)

# #The error rate plot w.r.t number of trees reveals that anything more than, say 51
# # trees is really not that valuable.
# rndFor$err.rate

# plot(rndFor, main="")

# legend("topright", c("OOB", "1", "0"), text.col=1:6, lty=1:3, col=1:3)

# title(main="Error Rates Random Forest IDataset.train ")

# set.seed(seed)

# head(IDataset.train.balanced.data1)
# names(IDataset.train.balanced.data1)
# #Now we will "tune" the Random Forest by trying different m values.
# #We will stick with 51 trees (odd number of trees are preferable). 
# #The returned forest, "tRndFor" is the one corresponding to the best m
# tRndFor = tuneRF(x = IDataset.train.balanced.data1[, -6], 
#                  y= IDataset.train.balanced.data1 $default,
#                  mtryStart = 3, 
#                  ntreeTry = 251, 
#                  stepFactor = 1.5, 
#                  improve = 0.0001, 
#                  trace=TRUE, 
#                  plot = TRUE,
#                  doBest = TRUE,
#                  nodesize = 10, 
#                  importance=TRUE
# )

# #List the importance of the variables. Larger the MeanDecrease values
# #the more important the variable.
# importance(tRndFor)


# #Lets make predictions on the training data and measure the prediction error rate.
# IDataset.train.balanced.data1 $predict.class = predict(tRndFor, IDataset.train.balanced.data1 , type="class")

# IDataset.train.balanced.data1 $prob1 = predict(tRndFor, IDataset.train.balanced.data1 , type="prob")[,"0"]

# tbl=table(IDataset.train.balanced.data1 $default, IDataset.train.balanced.data1 $predict.class)


# print('accuracy is ')
# sum(diag(tbl))/sum(tbl)

# library(ROCR)
# pred_ROCR <- prediction(IDataset.train.balanced.data1 $prob1, IDataset.train.balanced.data1 $default)
# roc_ROCR <- performance(pred_ROCR, measure = "tpr", x.measure = "fpr")
# plot(roc_ROCR, main = "ROC curve", colorize = T)
# abline(a = 0, b = 1)

# auc_ROCR <- performance(pred_ROCR, measure = "auc")
# auc_ROCR <- auc_ROCR@y.values[[1]]


# print('AUC is ')
# auc_ROCR

# #Now using the tuned Random Forest from the previous step, 
# #and redo our errors and top decile calculations for the previously identified threshold. 

# IDataset.test.balanced.data1$predict.class = predict(tRndFor, IDataset.test.balanced.data1 , type="class")

# IDataset.test.balanced.data1$prob1 = predict(tRndFor, IDataset.test.balanced.data1 , type="prob")[,"0"]

# tbl=table(IDataset.test.balanced.data1 $default, IDataset.test.balanced.data1 $predict.class)

# print('accuracy is ')
# sum(diag(tbl))/sum(tbl)

# pred_ROCR <- prediction(IDataset.test.balanced.data1 $prob1, IDataset.test.balanced.data1 $default)
# roc_ROCR <- performance(pred_ROCR, measure = "tpr", x.measure = "fpr")
# plot(roc_ROCR, main = "ROC curve", colorize = T)
# abline(a = 0, b = 1)

# auc_ROCR <- performance(pred_ROCR, measure = "auc")
# auc_ROCR <- auc_ROCR@y.values[[1]]


# print('AUC is ')
# auc_ROCR





# #result
# #train accuracy = 0.93
# #test accuracy = 0.94


