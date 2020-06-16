#=======================================================================
#
# Project 3 
#
#=======================================================================
#calling all libraries that we are going to use
library(readxl) # read xlsx
library(DataExplorer) # visual exploration of data
library(ggplot2)# data visualisation
library(gridExtra) # arrange multiple grid-based plots on a page
library(factoextra) # extract and visualize the results of multivariate data analysis
library(NbClust) # to find optimal number of clusters
library(caTools)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(cluster)
library(randomForest)
library(ROCR)
library(ineq) 
library(InformationValue)
library(rattle)
require(caTools)

#setting up working directory
setwd("C:/Users/ahmasiri/Desktop/PGP DSBA/Data/Project 3 - Thera Bank Case Study")


#reading data from csv file to Thera_Bank_Personal_Loan_Modelling_Dataset variable and view it
Thera_Bank_Personal_Loan_Modelling_Dataset <- read_excel("Thera Bank_Personal_Loan_Modelling-dataset.xlsx", sheet = 2)

# fix spaces in column names
spaceless <- function(x) {colnames(x) <- gsub(" ", "", colnames(x));x}
Thera_Bank_Personal_Loan_Modelling_Dataset <- spaceless(Thera_Bank_Personal_Loan_Modelling_Dataset)

# renaming some columen
names(Thera_Bank_Personal_Loan_Modelling_Dataset)[2] <- "Age"
names(Thera_Bank_Personal_Loan_Modelling_Dataset)[3] <- "Experiance"
names(Thera_Bank_Personal_Loan_Modelling_Dataset)[4] <- "Income"

attach(Thera_Bank_Personal_Loan_Modelling_Dataset)

View(Thera_Bank_Personal_Loan_Modelling_Dataset)

#check if ther is any NA value in dataset
anyNA(Thera_Bank_Personal_Loan_Modelling_Dataset)
anyNA(ID)               
anyNA(Age)              
anyNA(Experiance)
anyNA(Income)           
anyNA(ZIPCode)          
anyNA(Familymembers)
anyNA(CCAvg)            
anyNA(Education)        
anyNA(Mortgage)
anyNA(PersonalLoan)     
anyNA(SecuritiesAccount)
anyNA(CDAccount)
anyNA(Online)           
anyNA(CreditCard)

#dealing with NA values
Thera_Bank_Personal_Loan_Modelling_Dataset[is.na(Thera_Bank_Personal_Loan_Modelling_Dataset)] <- 0

#Converting variable tipes from int to logic and factor
Thera_Bank_Personal_Loan_Modelling_Dataset$ZIPCode = as.factor(Thera_Bank_Personal_Loan_Modelling_Dataset$ZIPCode)
Thera_Bank_Personal_Loan_Modelling_Dataset$Education = as.factor(Thera_Bank_Personal_Loan_Modelling_Dataset$Education)
Thera_Bank_Personal_Loan_Modelling_Dataset$PersonalLoan = as.factor(Thera_Bank_Personal_Loan_Modelling_Dataset$PersonalLoan)

Thera_Bank_Personal_Loan_Modelling_Dataset$SecuritiesAccount = as.factor(Thera_Bank_Personal_Loan_Modelling_Dataset$SecuritiesAccount)
Thera_Bank_Personal_Loan_Modelling_Dataset$CDAccount = as.factor(Thera_Bank_Personal_Loan_Modelling_Dataset$CDAccount)
Thera_Bank_Personal_Loan_Modelling_Dataset$Online = as.factor(Thera_Bank_Personal_Loan_Modelling_Dataset$Online)
Thera_Bank_Personal_Loan_Modelling_Dataset$CreditCard = as.factor(Thera_Bank_Personal_Loan_Modelling_Dataset$CreditCard)

#attaching varioable names
attach(Thera_Bank_Personal_Loan_Modelling_Dataset)

#Return a summary of the dataset variables.
summary(Thera_Bank_Personal_Loan_Modelling_Dataset)

#Retrieve the dimension of an object.
dim(Thera_Bank_Personal_Loan_Modelling_Dataset)

#Get the names of an object.
names(Thera_Bank_Personal_Loan_Modelling_Dataset)

#Display the internal structure of an dataset.
str(Thera_Bank_Personal_Loan_Modelling_Dataset)

#Returns the first 10 rows of the dataset.
head(Thera_Bank_Personal_Loan_Modelling_Dataset, 10)

#Returns the last 10 rows of the dataset.
tail(Thera_Bank_Personal_Loan_Modelling_Dataset, 10)


# univariate Analysis

#graph for all variables
# Quantitative
par(mfrow=c(2,3))
boxplot(Age, main = "Age")
boxplot(Experiance, main = "Experiance")
boxplot(Income, main = "Income")
hist(Age, main = "Age")
hist(Experiance, main = "Experiance")
hist(Income, main = "Income")

par(mfrow=c(2,3))
boxplot(Familymembers,main = "Familymembers")
boxplot(CCAvg, main = "CCAvg")
boxplot(Mortgage,main = "Mortgage")
hist(Familymembers, main = "Familymembers")
hist(CCAvg, main = "CCAvg")
hist(Mortgage, main = "Mortgage")

# catagorical
par(mfrow=c(3,2))
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset) + geom_bar(aes(x = ZIPCode))
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset) + geom_bar(aes(x = Education))
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset) + geom_bar(aes(x = PersonalLoan))
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset) + geom_bar(aes(x = SecuritiesAccount))
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset) + geom_bar(aes(x = CDAccount))
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset) + geom_bar(aes(x = Online))
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset) + geom_bar(aes(x = CreditCard))

#Bi-Variate Analysis
#jitter and box plots
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset,
       aes(x = factor(Education, #defining x axis a categorical
                      labels = c("1", "2", "3")),
           y = Income,
           color = Education)) + #specifying that coloring is to be based on drive type
  geom_boxplot(size=1, #makes the lines thicker
               outlier.shape = 1, #specifies circles for outliers
               outlier.color = "black", #makes outliers black
               outlier.size = 3) + #increases the size of the outlier symbol
  geom_jitter(alpha = 0.5, #setting transparency of graph
              width=.2) + #decreases the amount of jitter (.4 is the default)
  labs(title = "Costomer Income by Education",
       x = "",
       y = "Income (k)") +
  theme_minimal() + #setting minimal theme (no background color)
  theme(legend.position = "none") + #hiding legend
  coord_flip() #x and y axes are reversed

#scatter plot
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset,aes(x = Income, y = Age)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Income(K)", #specifying the labels of axes and title of plot
       y = "Age",
       title = "Income(K) vs Age",
       subtitle = "Relation between Customer Income and Age") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables


#scatter plot
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset,aes(x = Income, y = Experiance)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Income(K)", #specifying the labels of axes and title of plot
       y = "Experiance",
       title = "Income(K) vs Age",
       subtitle = "Relation between Customer Income and Experiance") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables

#scatter plot
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset,aes(x = Income, y = CCAvg)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Income(K)", #specifying the labels of axes and title of plot
       y = "CCAvg",
       title = "Income(K) vs CCAvg",
       subtitle = "Relation between Customer Income and CCAvg") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables

#scatter plot
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset,aes(x = Income, y = Mortgage)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Income(K)", #specifying the labels of axes and title of plot
       y = "Mortgage",
       title = "Income(K) vs Mortgage",
       subtitle = "Relation between Customer Income and Mortgage") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables

# stacked bar chart
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset,
       aes(x = CreditCard,
           fill = factor(PersonalLoan,
                         levels = c("0", "1"),
                         labels = c("0", "1")))) +
  labs(fill = "PersonalLoan", # setting title of legend
       x = "CreditCard",
       title = "CreditCard by PersonalLoan") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked

# stacked bar chart
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset,
       aes(x = SecuritiesAccount,
           fill = factor(PersonalLoan,
                         levels = c("0", "1"),
                         labels = c("0", "1")))) +
  labs(fill = "PersonalLoan", # setting title of legend
       x = "SecuritiesAccount",
       title = "SecuritiesAccount by PersonalLoan") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked



#=======================================================================
#2.1 Apply Clustering algorithm < type, rationale>
Thera_Bank_Personal_Loan_Modelling_Dataset.scaled = scale(
  Thera_Bank_Personal_Loan_Modelling_Dataset %>% select(2, 3, 4, 6, 7))

#Calculate Euclidean Distance between data points
eucDistMatrix <- dist(x=Thera_Bank_Personal_Loan_Modelling_Dataset.scaled, method = "euclidean")
print(eucDistMatrix, digits = 3)

# Create dissimilarity matric using hclust() and agglomeration method = Ward's Method 
h_cluster <- hclust(eucDistMatrix, method = "ward.D2" )

# Plot the dendrogram
plot(h_cluster, hang = -1)

# ** Dendrogram indicates 2/3 clusters of colleges in our data

# Plot rectangles for possible clusters
rect.hclust(h_cluster, k = 2, border = 2:5) 
rect.hclust(h_cluster, k = 3, border = 2:5) 
rect.hclust(h_cluster, k = 4, border = 2:5) 

# Find optimal number of clusters by creating different dendrograms 
# by varying agglomeration method
h_cluster_euc_comp <- hclust(eucDistMatrix, method = 'complete')
plot(h_cluster_euc_comp, 
     hang = -1, col = 'green')

h_cluster_euc_avg <- hclust(eucDistMatrix, method = 'average')
plot(h_cluster_euc_avg, 
     hang = -1, col = 'red')

manhDistMatrix <- dist(x=clg_data[, -1], method = "manhattan")
h_cluster_manh_comp <- hclust(manhDistMatrix, method = 'complete')
plot(h_cluster_manh_comp, labels = as.character(clg_data$Engg_College), 
     hang = -1, col = 'blue')


# ** All the dendograms indicate a presence of 3 major clusters

# Add cluster membership to original dataset
cluster_name <- cutree(h_cluster, k = 3)
clg_data_hclusters <- cbind(clg_data,cluster_name)


# Visualise the clusters in two dimensions
h_clust_viz_3 <- fviz_cluster(list(data = clg_data_hclusters[, -c(1,7)], 
                                   cluster = clg_data_hclusters[, 7])) + 
  ggtitle("hierarchical 3")
h_clust_viz_3


# Number of members in each cluster
View(clg_data_hclusters[order(clg_data_hclusters$cluster_name),])
table(clg_data_hclusters$cluster_name)

# Observe the differences between identified clusters
aggr_mean <- aggregate(clg_data[, -1], list(cluster_name), mean)


# Create cluster profiles
hcluster.profile <- data.frame(Cluster = aggr_mean[, 1],
                               Number_of_Colleges = 
                                 as.vector(table(cluster_name)),
                               aggr_mean[, -1])
View(hcluster.profile)
hcluster.profile


# Insights ----------------------------------------------------------------

# ** Cluster 1 has 6 colleges that have high mean ratings for all performance
# ** metrics and they charge medium high fees. These are probably the Tier 1 
# ** engineering colleges
# **
# ** Cluster 2 colleges have medium quality teaching, average placements, 
# ** internships and infrastructure but farily high fees. These  are the
# ** Tier 2 engineering colleges
# ** 
# ** Cluster 3 colleges fare badly in all the areas as compared to their
# ** counterparts, These can be called Tier 3 engineering colleges.







#=========================kmeans=========================
Thera_Bank_Personal_Loan_Modelling_Dataset.scaled = scale(
  Thera_Bank_Personal_Loan_Modelling_Dataset %>% select(2, 3, 4, 6, 7))
seed = 1000
#finding best k
set.seed(seed)
nc = NbClust(Thera_Bank_Personal_Loan_Modelling_Dataset %>% select(2, 3, 4, 6, 7)
             , min.nc = 2, max.nc = 5, method = "kmeans")
#it shows that the best value of k is 3
clus = kmeans(x=Thera_Bank_Personal_Loan_Modelling_Dataset.scaled, centers = 3, nstart = 5)
clusplot(Thera_Bank_Personal_Loan_Modelling_Dataset.scaled,clus$cluster, color = T, shode = T, label = 1, lines = 1)



#3.1 Applying CART <plot the tree>
#===========================CART======================================
set.seed(1000) # To ensure reproducibility
split <- sample.split(Thera_Bank_Personal_Loan_Modelling_Dataset[-1], SplitRatio = 0.7)
train <- subset(Thera_Bank_Personal_Loan_Modelling_Dataset, split == TRUE)
test <- subset( Thera_Bank_Personal_Loan_Modelling_Dataset, split == FALSE)
nrow(train)
nrow(test)
# Check that the distribution of the dependent variable is similar in train and test sets
prop.table(table(Thera_Bank_Personal_Loan_Modelling_Dataset$PersonalLoan))
prop.table(table(train$PersonalLoan))
prop.table(table(test$PersonalLoan))
## Build a CART model on the train dataset
# We will use the "rpart" and the "rattle" libraries to build decision trees.
# Setting the control parameters (to control the growth of the tree)
# Set the control parameters very low to let the tree grow deep
r.ctrl = rpart.control(minsplit = 50, minbucket = 10, cp = 0, xval = 10)
# Building the CART model
# formula - response variable~predictor variables  
# data - dataset
# method - "class" - for classification, "anova" for regression
# control - tree control parameters
cart_model1 <- rpart(formula = PersonalLoan~., data = train, method = "class", control = r.ctrl)
cart_model1
#Clearly we need to prune this tree
## Model Tuning
#The cost complexity table can be obtained using the printcp or plotcp functions
printcp(cart_model1)
plotcp(cart_model1)
# The unncessarily complex tree above can be pruned using a cost complexity threshold. 
# Using a complexity threshold of 0.07 gives us a relatively simpler tree.
cart_model2 = prune(cart_model1, cp= 0.07 ,"CP")
printcp(cart_model2)
cart_model2
#Displaying the decision tree
prp(cart_model2)
#Let us check the variable importance
cart_model1$variable.importance
# Predicting on the train dataset
train_predict.class_CART <- predict(cart_model2, train, type="class") # Predicted Classes
train_predict.score_CART <- predict(cart_model2, train) # Predicted Probabilities
# Create confusion matrix for train data predictions
tab.train_CART = table(train$PersonalLoan, train_predict.class_CART)
tab.train_CART
# Accuracy on train data
accuracy.train_CART = sum(diag(tab.train_CART)) / sum(tab.train_CART)
accuracy.train_CART
# CART Model (cart_model2) has 98% accuracy on train data. 
## Model Evaluation
# Predicting on the test dataset
test_predict.class_CART <- predict(cart_model2, test, type="class") # Predicted Classes
test_predict.score_CART <- predict(cart_model2, test) # Predicted Probabilities
# Create confusion matrix for test data predictions
tab.test_CART = table(test$PersonalLoan, test_predict.class_CART)
tab.test_CART
# Accuracy on test data
accuracy.test_CART = sum(diag(tab.test_CART)) / sum(tab.test_CART)
accuracy.test_CART
#The CART model accuracy on test data is 97% 


fancyRpartPlot(cart_model2)



#3.1 Applying Random forest
#===========================Random forest================================
set.seed(1000) # To ensure reproducibility

rf_model1 = randomForest(
  PersonalLoan ~ .,
  data = select(train, -1,-5),
  ntree = 51,
  mtry = 6,
  nodesize = 10,
  importance = TRUE
)

#Plot the model to determine the optimum number of trees
plot(rf_model1)

#The plot reveals that anything more than, say 50 trees, is really not that valuable.

# List the importance of the variables. 
# Larger the MeanDecrease values, the more important the variable. 
# Look at the help files to get a better sense of how these are computed.

print(rf_model1$importance)


# Let us tune the randomforest model by trying diferent m values
# Tune the RF model to find out the best mtry
# We will take ntree = 51 (odd number of trees are preferred)
# The returned forest, rf_model2 is the one corresponding to the best m

## Tune the Random Forest Model
# Check the column number of the response variable
names(train)

set.seed(500) # To ensure reproducibility

rf_model2 = tuneRF(x = select(train, -1,-5,-10), # matrix or data frame of predictor/independent variables
                   y = train$PersonalLoan, # response vector (factor for classification, numeric for regression)
                   mtrystart = 5, # starting value of mtry
                   stepfactor=1.5, # at each iteration, mtry is inflated (or deflated) by this value
                   ntree=51, # number of trees built for each mtry value
                   improve=0.0001, # the (relative) improvement in OOB error must be by this much for the search to continue
                   nodesize=10, # Minimum size of terminal nodes
                   trace=TRUE, # prints the progress of the search
                   plot=TRUE, # to get the plot of the OOB error as function of mtr
                   doBest=TRUE, # return a forest using the optimal mtry found
                   importance=TRUE # 
)

# The optimal number of mtry is 6.
# tuneRF returns rf_model2. It is the random forest of 51 trees built with m = 6


## Model Validation
# Predicting on the train dataset
train_predict.class_RF <- predict(rf_model2, train, type="class") # Predicted Classes
train_predict.score_RF <- predict(rf_model2, train) # Predicted Probabilities

# Create confusion matrix for train data predictions
tab.train_RF = table(train$PersonalLoan, train_predict.class_RF)
tab.train_RF

# Accuracy on train data
accuracy.train_RF = sum(diag(tab.train_RF)) / sum(tab.train_RF)
accuracy.train_RF

# RandomForest mode (rf_model2) has 99% accuracy on train data. 

## Model Evaluation
# Predicting on the test dataset
test_predict.class_RF <- predict(rf_model2, test, type="class") # Predicted Classes
test_predict.score_RF <- predict(rf_model2, test) # Predicted Probabilities

# Create confusion matrix for test data predictions
tab.test_RF = table(test$PersonalLoan, test_predict.class_RF)
tab.test_RF

# Accuracy on test data
accuracy.test_RF = sum(diag(tab.test_RF)) / sum(tab.test_RF)
accuracy.test_RF

# The model has good performance on test data too.
# An accuracy of 99% on train data and 97% on test data indicates that this is a good model, 
# neither overfit nor underfit.




## Comparing Models
Model_Name = c("CART", "Random Forest")
Train_Accuracy_perc = c( accuracy.train_CART*100, accuracy.train_RF*100)
Test_Accuracy_perc = c( accuracy.test_CART*100, accuracy.test_RF*100)
output = data.frame(Model_Name,Train_Accuracy_perc,Test_Accuracy_perc)
output


#Random Forest model is a good predictor model for employee PersonalLoan prediction.
# We will use rf_model2 as our final model.


varImpPlot(rf_model2, sort = TRUE)





# Confusion Matrix
#=======================================================================

# We will compare all the 4 models that we created earlier - rf_model1, rf_model2, cart_model1, cart_model2
# Predict PersonalLoan class and probability for all 4 models
# Predict on test data using cart_model1
cart_model1_predict_class = predict(cart_model1, test, type = 'class')
cart_model1_predict_score = predict(cart_model1, test, type = 'prob')

# Predict on test data using cart_model2
cart_model2_predict_class = predict(cart_model2, test, type = 'class')
cart_model2_predict_score = predict(cart_model2, test, type = 'prob')

# Predict on test data using rf_model1
rf_model1_predict_class = predict(rf_model1, test, type = 'class')
rf_model1_predict_score = predict(rf_model1, test, type = 'prob')

# Predict on test data using rf_model2
rf_model2_predict_class = predict(rf_model2, test, type = 'class')
rf_model2_predict_score = predict(rf_model2, test, type = 'prob')


# Create Confusion Matrix for all the four models
conf_mat_cart_model1 = table(test$PersonalLoan, cart_model1_predict_class)
conf_mat_cart_model1

conf_mat_cart_model2 = table(test$PersonalLoan, cart_model2_predict_class)
conf_mat_cart_model2

conf_mat_rf_model1 = table(test$PersonalLoan, rf_model1_predict_class)
conf_mat_rf_model1

conf_mat_rf_model2 = table(test$PersonalLoan, rf_model2_predict_class)
conf_mat_rf_model2

# Accuracy

# Accuracy of models on test data
accuracy_cart_model1 = sum(diag(conf_mat_cart_model1)) / sum(conf_mat_cart_model1)

accuracy_cart_model2 = sum(diag(conf_mat_cart_model2)) / sum(conf_mat_cart_model2)

accuracy_rf_model1 = sum(diag(conf_mat_rf_model1)) / sum(conf_mat_rf_model1)

accuracy_rf_model2 = sum(diag(conf_mat_rf_model2)) / sum(conf_mat_rf_model2)

# Sensitivity / Recall

# Sensitivity of models on test data
sensitivity_cart_model1 = conf_mat_cart_model1[2,2] / sum(conf_mat_cart_model1['1',])

sensitivity_cart_model2 = conf_mat_cart_model2[2,2] / sum(conf_mat_cart_model2['1',])

sensitivity_rf_model1 = conf_mat_rf_model1[2,2] / sum(conf_mat_rf_model1['1',])

sensitivity_rf_model2 = conf_mat_rf_model2[2,2] / sum(conf_mat_rf_model2['1',])

# Specificity

# Specificity of models on test data
specificity_cart_model1 = conf_mat_cart_model1[1,1] / sum(conf_mat_cart_model1['0',])

specificity_cart_model2 = conf_mat_cart_model2[1,1] / sum(conf_mat_cart_model2['0',])

specificity_rf_model1 = conf_mat_rf_model1[1,1] / sum(conf_mat_rf_model1['0',])

specificity_rf_model2 = conf_mat_rf_model2[1,1] / sum(conf_mat_rf_model2['0',])

# Precision

# Precision of models on test data
precision_cart_model1 = conf_mat_cart_model1[2,2] / sum(conf_mat_cart_model1[,'1'])

precision_cart_model2 = conf_mat_cart_model2[2,2] / sum(conf_mat_cart_model2[,'1'])

precision_rf_model1 = conf_mat_rf_model1[2,2] / sum(conf_mat_rf_model1[,'1'])

precision_rf_model2 = conf_mat_rf_model2[2,2] / sum(conf_mat_rf_model2[,'1'])

# KS

# Using library ROCR functions prediction and performance
pred_cart_model1 = prediction(cart_model1_predict_score[, 2], test$PersonalLoan) 
perf_cart_model1 = performance(pred_cart_model1,"tpr","fpr")
ks_cart_model1 = max(attr(perf_cart_model1,'y.values')[[1]] - attr(perf_cart_model1,'x.values')[[1]])

pred_cart_model2 = prediction(cart_model2_predict_score[, 2], test$PersonalLoan) 
perf_cart_model2 = performance(pred_cart_model2,"tpr","fpr")
ks_cart_model2 = max(attr(perf_cart_model2,'y.values')[[1]] - attr(perf_cart_model2,'x.values')[[1]])

pred_rf_model1 = prediction(rf_model1_predict_score[, 2], test$PersonalLoan) 
perf_rf_model1 = performance(pred_rf_model1,"tpr","fpr")
ks_rf_model1 = max(attr(perf_rf_model1,'y.values')[[1]] - attr(perf_rf_model1,'x.values')[[1]])

pred_rf_model2 = prediction(rf_model2_predict_score[, 2], test$PersonalLoan) 
perf_rf_model2 = performance(pred_rf_model2,"tpr","fpr")
ks_rf_model2 = max(attr(perf_rf_model2,'y.values')[[1]] - attr(perf_rf_model2,'x.values')[[1]])


# AUC

# Using library ROCR
auc_cart_model1 = performance(pred_cart_model1, measure = "auc")
auc_cart_model1 = auc_cart_model1@y.values[[1]]

auc_cart_model2 = performance(pred_cart_model2, measure = "auc")
auc_cart_model2 = auc_cart_model2@y.values[[1]]

auc_rf_model1 = performance(pred_rf_model1, measure = "auc")
auc_rf_model1 = auc_rf_model1@y.values[[1]]

auc_rf_model2 = performance(pred_rf_model2, measure = "auc")
auc_rf_model2 = auc_rf_model2@y.values[[1]]

# Gini
# Using library ineq 
gini_cart_model1 = ineq(cart_model1_predict_score[, 2],"gini")

gini_cart_model2 = ineq(cart_model2_predict_score[, 2],"gini")

gini_rf_model1 = ineq(rf_model1_predict_score[, 2],"gini")

gini_rf_model2 = ineq(rf_model2_predict_score[, 2],"gini")

# Concordance - Discordance


concordance_cart_model1 = Concordance(actuals = ifelse(test$PersonalLoan == '1', 1,0), predictedScores = ifelse(cart_model1_predict_class == '1', 1,0))

concordance_cart_model2 = Concordance(actuals = ifelse(test$PersonalLoan == '1', 1,0), predictedScores = ifelse(cart_model2_predict_class == '1', 1,0))

concordance_rf_model1 = Concordance(actuals = ifelse(test$PersonalLoan == '1', 1,0), predictedScores = ifelse(rf_model1_predict_class == '1', 1,0))

concordance_rf_model2 = Concordance(actuals = ifelse(test$PersonalLoan == '1', 1,0), predictedScores = ifelse(rf_model2_predict_class == '1', 1,0))

# Comparing models


cart_model1_metrics = c(accuracy_cart_model1, sensitivity_cart_model1, specificity_cart_model1, precision_cart_model1, ks_cart_model1, auc_cart_model1, gini_cart_model1, concordance_cart_model1$Concordance)

cart_model2_metrics = c(accuracy_cart_model2, sensitivity_cart_model2, specificity_cart_model2, precision_cart_model2, ks_cart_model2, auc_cart_model2, gini_cart_model2, concordance_cart_model2$Concordance)

rf_model1_metrics = c(accuracy_rf_model1, sensitivity_rf_model1, specificity_rf_model1, precision_rf_model1, ks_rf_model1, auc_rf_model1, gini_rf_model1, concordance_rf_model1$Concordance)

rf_model2_metrics = c(accuracy_rf_model2, sensitivity_rf_model2, specificity_rf_model2, precision_rf_model2, ks_rf_model2, auc_rf_model2, gini_rf_model2, concordance_rf_model2$Concordance)

comparison_table = data.frame(cart_model1_metrics, cart_model2_metrics, rf_model1_metrics, rf_model2_metrics)

rownames(comparison_table) = c("Accuracy", "Sensitivity", "Specificity", "Precision", "KS", "Auc", "Gini", "Concordance")

comparison_table



#=======================================================================
#
# T H E - E N D
#
#=======================================================================

