#=======================================================================
#
# Project 2 - Cold Storage
#
#=======================================================================
#calling all libraries that we are going to use
library("readxl")


#=======================================================================
# problem 1
#=======================================================================

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
Thera_Bank_Personal_Loan_Modelling_Dataset[is.na(Thera_Bank_Personal_Loan_Modelling_Dataset)] <- 0
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

#objects in the dataset can be accessed by simply giving their names
attach(Thera_Bank_Personal_Loan_Modelling_Dataset)

#get Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# univariate Analysis




#ZIPCode
#Mode
getmode(ZIPCode)




#=======================================================================
#
# T H E - E N D
#
#=======================================================================

