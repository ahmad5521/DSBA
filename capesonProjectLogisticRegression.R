## Split the data into train & test dataset. Split80:20
seed = 101

set.seed(seed)
index = sample(1:nrow(IDataset),0.80*nrow(IDataset))
IDataset.train2 = IDataset[index,]
IDataset.test2 = IDataset[-index,]





## Let's check the count of unique value in the target variable
as.data.frame(table(IDataset.train2$default))

## Let's check the count of unique value in the target variable
as.data.frame(table(IDataset.test2$default))





# let us build the model with all varaibles

LRmodel = glm(default~ Income+Count_3_6_months_late+Count_6_12_months_late+Count_more_than_12_months_late+risk_score , data = IDataset.train2, family= binomial)
summary(LRmodel)

# Using stepwise algorithm for removing insignificant variables

library(MASS)
log_model = stepAIC(LRmodel, direction = "both",k=5) 
summary(log_model)


# lets see important variables

library(caret)
varImp(log_model)

# convert to data frame
l = data.frame(varImp(log_model))
l <- cbind(newColName = rownames(l), l)
rownames(l) <- 1:nrow(l)

# soritng the imprtance of varaible 
l[with(l, order(-Overall)), ]

# lets see model performances on train data
# prediction on test dataset
IDataset.train2$prob1 = predict(log_model, newdata= IDataset.train2, type="response")
tb.train2 = table(IDataset.train2$prob1>0.50, IDataset.train2$default)
print('accuracy is ')
sum(diag(tb.train2))/sum(tb.train2)




trainsensitivity2 = tb.train2[2,2] / sum(tb.train2[2, ])
trainsensitivity2

trainspecificity2 = tb.train2[1,1] / sum(tb.train2[1, ])
trainspecificity2

# let us check accuracy on test data set
# prediction on test dataset
IDataset.test2$prob1 = predict(log_model, newdata= IDataset.test2, type="response")
tb.test2 = table(IDataset.test2$prob1>0.50, IDataset.test2$default)
print('accuracy is ')
sum(diag(tb.test2))/sum(tb.test2)





testsensitivity2 = tb.test2[2,2] / sum(tb.test2[2, ])
testsensitivity2

testspecificity2 = tb.test2[1,1] / sum(tb.test2[1, ])
testspecificity2














#result
#train accuracy = 94
#train sensitivity = 94
#train specificity = 57


#test accuracy = 93
#train sensitivity = 94
#train specificity = 51




