## Aansh Sardana
## 
## HarvardX: PH125.9x - Capstone Project
# Banking Marketing strategy by Data Analysis Project 

#################################################
# Banking Marketing strategy by Data Analysis Project 
################################################



## 2.Data Analysis:


### 2.1.Starting Analysis


rm(list = ls())
options(warn=-1)

#let's install needed packages and download dataset from my github repository

if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("Matrix", repos = "http://cran.us.r-project.org")
if (!require(devtools)) install.packages("devtools")
if(!require(DataExplorer)) install.packages("DataExplorer")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(class)) install.packages("class", repos = "http://cran.us.r-project.org")
if(!require(gmodels)) install.packages("gmodels", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")

#inititalising the libraries

library(readr)
library(tidyverse)
library(GGally)
library(devtools)
library(glmnet)
library(Matrix)
library(ggplot2)
library(DataExplorer)
library(corrplot)
library(caret)
library(randomForest)
library(class)
library(gmodels)
library(dplyr)
library(psych)

#set the seed to 1
set.seed(1)


#Loading the dataset:

#reading data from my github profile


dt.df <- read.csv("https://raw.github.com/aansh-01/create_your_own/master/bank-full.csv", header=TRUE, sep=";")




##Viewing the names of the column in dataset

names(dt.df)

#details of coloumns of the dataset:
str(dt.df)

#dataset Summary analysis:

summary(dt.df)



#2.2. Preparation of Data 

##Checking for any missing velue : ##

sum(is.na(dt.df))
#There are no missing values in our dataset.


#From starting analysis we conclude that there are many variables with class=int; hence, we need to convert them into numeric class

#Converting int values to numeric class:#

dt.df$pdays <- as.numeric(dt.df$pdays)
dt.df$duration <- as.numeric(dt.df$duration)
dt.df$age <- as.numeric(dt.df$age)
dt.df$campaign <- as.numeric(dt.df$campaign)
dt.df$previous <- as.numeric(dt.df$previous)





##Ordered the levels of month:

dt.df$month<- factor(dt.df$month, ordered = TRUE, levels = c("mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))





#As the target variable is a categorical variable which can have two possible values either yes or no. So we convert it into numerical value 1 and 0 respectively

##Converting the target variable into 1 for yes and 0 for no

table(dt.df$y)



dt.df <- dt.df %>%
  mutate(y = ifelse(y=="yes", 1, 0))

dt.df$y <- as.factor(dt.df$y)
table(dt.df$y)




### 2.3.Descriptive Analysis

##Creating the histogram of given input variables##


plot_histogram(dt.df[,-21],ggtheme = theme_linedraw(base_size = 15, base_family = "serif"))




mtab <- table(dt.df$default, dt.df$y)
ptt <- as.data.frame(prop.table(mtab, 2))
colnames(ptt) <-  c("default", "y", "perc")
ggplot(data = ptt, aes(x = default, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +
  labs(title = "Default Distribution", y = "Percent", x = "Default")

##Therefore,  People who are in default are higher in number##



mtab <- table(dt.df$education, dt.df$y)
ptt <- as.data.frame(prop.table(mtab, 2))
colnames(ptt) <-  c("education", "y", "perc")
ggplot(data = ptt, aes(x = education, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = "Distribution according to education", y = "Percent", x = "Education")

##Therefore, customers who sign up for bank deposits, proportionally, have achieved a higher level of education, than those who didn't sign up.##



mtab <- table(dt.df$month, dt.df$y)
ptt <- as.data.frame(prop.table(mtab, 2))
colnames(ptt) <-  c("month", "y", "perc")
ggplot(data = ptt, aes(x = month, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +
  labs(title = "Distribution according to month", y = "Percent", x = "Month")

##Therefore , the month of May is when the highest number of calls were placed for marketing deposit. And the following months of April, September, and October is the time when a higher proportion of people subscribed for term deposits.##

mtab <- table(dt.df$marital, dt.df$y)
ptt <- as.data.frame(prop.table(mtab, 2))
colnames(ptt) <-  c("marital", "y", "perc")
ggplot(data = ptt, aes(x = marital, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) + 
  labs(title = "Distribution according to marital status", y = "Percent", x = "Marital")


##Therefore , With respect to Marital Status there is not an observed large difference in the proportion of people subscribed to term deposits and people without term deposits.##



mtab <- table(dt.df$job, dt.df$y)
ptt <- as.data.frame(prop.table(mtab, 2))
colnames(ptt) <-  c("job", "y", "perc")
ggplot(data = ptt, aes(x = job, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = "Distribution according to jobs", y = "Percent", x = "Jobs")

##Therefore , We see there are higher proportions for customers signing up for the term deposits who have the jobs of admin, retired, and students.##

mtab <- table(dt.df$day_of_week, dt.df$y)
ptt <- as.data.frame(prop.table(mtab, 2))
colnames(ptt) <-  c("day_of_week", "y", "perc")
ggplot(data = ptt, aes(x = day_of_week, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +
  labs(title = "Distribution on the basis of day of week", y = "Percent", x = "Day_of_week")

##Therefore , Campaigns that were performed midweek, on Tuesdays, Wednesdays, and Thursdays had a slightly higher proportion of people who subscribed for bank deposit..##


myboxplot<-ggplot(dt.df, aes(factor(y), age)) + geom_boxplot(aes(fill = factor(y)))
myboxplot
##Therefore , the age range for successful conversion has a slightly lower median, but higher quartile ranges.##




mtab <- table(dt.df$housing, dt.df$y)
ptt <- as.data.frame(prop.table(mtab, 2))
colnames(ptt) <-  c("housing", "y", "perc")
ggplot(data = ptt, aes(x = housing, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +
  labs(title = "Distribution on the basis of housing", y = "Percent", x = "Housing")

##Therefore , we see that a higher proportion of people who have subscribed for bank deposit are home owners versus ones that don't own their own houses.##





mtab <- table(dt.df$loan, dt.df$y)
ptt <- as.data.frame(prop.table(mtab, 2))
colnames(ptt) <-  c("loan", "y", "perc")
ggplot(data = ptt, aes(x = loan, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +
  labs(title = "Distribution on the basis of Loan", y = "Percent", x = "Loan")

##Therefore , we see the proportion of people who have subscribed and not subscribed to a term deposit is the same for categories of the Loan.##





mtab <- table(dt.df$contact, dt.df$y)
ptt <- as.data.frame(prop.table(mtab, 2))
colnames(ptt) <-  c("contact", "y", "perc")
ggplot(data = ptt, aes(x = contact, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +
  labs(title = "Distribution on the basis of Contacts", y = "Percent", x = "Contact")

##Therefore , Customers who have cell phones have a more direct way of communicating, and signed up for term deposits more than those who only had a landline telephone.##








mtab <- table(dt.df$poutcome, dt.df$y)
ptt <- as.data.frame(prop.table(mtab, 2))
colnames(ptt) <-  c("poutcome", "y", "perc")
ggplot(data = ptt, aes(x = poutcome, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +
  labs(title = "Distribution on the basis of outcome of previous marketing campaign", y = "Percent", x = "Outcome of previous marketing campaign")
##Therefore , Potential customers who successfully connected and responded in previous campaigns had a higher proportion of signing up for the term deposit.##






myboxplot <- ggplot(dt.df, aes(factor(y), duration)) + geom_boxplot(aes(fill = factor(y)))
myboxplot
##Therefore , longer the phone conversation the greater the conversion rate is for the potential customer to sign up for the term deposit. There are higher median and quartile ranges.##


df_corll <- select_if(dt.df, is.numeric) %>% cor()
corrplot(df_corll, method = "number")

##Therefore , We see our target variable has a high positive correlation with duration and if the customer was involved and connected in a previous campaign, while there's negative correlation with number of employees,number of days from last contact ,Euribor 3 month rate and employee variation rate.##

##*




## 3.Data Modeling and Results:


### 3.1.Data Preparation


##Missing values for duration were filtered out because if duration = 0 then y = "no" . Thus, it doesn't make sense to have 0 second duration.##
##I have also filtered out education illiterate, and default yes because they only have 1 observation each. We can't predict these situations if they happen to be in the test data but not the train data.##


dt.df <- dt.df %>%
  filter(duration != 0, education != "illiterate", default != "yes") %>%
  mutate(y = ifelse(y==1, 1, 0))


##Splitting the data into training and test datasets:#


#setting seed to 123
set.seed(123)
trainIndex <- createDataPartition(dt.df$y,
                                  p = 0.8, # training contains 80% of data
                                  list = FALSE)
dfTrainSet <- dt.df[ trainIndex,]
dfTestSet  <- dt.df[-trainIndex,]



dim(dfTrainSet)



dim(dfTestSet)

#Above output shows that the train dataset has 32931 rows and 21 columns and the test dataset has 8232 rows and 21 columns.
#The number of columns remains the same because the dataset was split vertically.



### 3.2.Data Modeling using Random Forest:


# Random forest is a supervised learning algorithm which is used for both classification as well as regression. It creates decision trees on data samples and then gets the prediction from each of them and finally selects the best solution by means of voting.

# The data set was categorized into training and testing data with 80%:20% split ratio respectively. 
# A seed value was set using set.seed() function to regenerate the split data. 

# A random forest model was built using training data using random forest algorithm. 
# We use 10 predictors for each split and grow 200 trees fully without pruning. 
# A subset of predictors is randomly chosen without replacement at each split which helps in reducing the 
# variance of the model overall. 

# This model gives an Out-Of-Bag error rate of 8.7%. The model also outputs a confusion matrix but maintains a high accuracy in predicting the response variable i.e. term deposit(Yes/No) field.


set.seed(123)

# random forest
model_rndfor <- randomForest(as.factor(y)~.,
                             data = dfTrainSet,
                             ntree = 200,
                             mtry=10,
                             importance = TRUE)

print(model_rndfor)



rf_pred_prob <- predict(model_rndfor,newdata = dfTestSet)



head(rf_pred_prob)


##Model evaluation:##


# putting "rf_pred_prob" in a data frame
outcome_RF_test <- data.frame(dfTestSet$y)

# merging "model_rndfor" and "outcome_RF_test" 
comparison_df_RF <- data.frame(rf_pred_prob, outcome_RF_test)

# specifying column names for "comparison_df_RF"
names(comparison_df_RF) <- c("RF_Predicted_y", "RF_Observed_y")

comparison_df_RF$RF_Predicted_y <- as.factor(comparison_df_RF$RF_Predicted_y)
comparison_df_RF$RF_Observed_y <- as.factor(comparison_df_RF$RF_Observed_y)

# inspect "comparison_df_RF" 
head(comparison_df_RF)


str(comparison_df_RF)


confusionMatrix(comparison_df_RF$RF_Observed_y,comparison_df_RF$RF_Predicted_y)



# Total RF observation -  8232 

# 7050 cases out of 8232 have been accurately predicted (TN->True Negatives) as negative class (0) which constitutes 85%.

# 510 out of 8232 observations were accurately predicted (TP-> True Positives) as positive class (1) which constitutes 6%. 

# Thus a total of 510 out of 8232 predictions where TP i.e, True Positive in nature.

# There were 520 cases of False Positives (FP) meaning 520 cases out of 8232 were actually negative but got predicted as positive.

# There were 366 cases of False Negatives (FN) meaning 366 cases out of 8232 were actually positive in nature but got predicted as negative.

# Accuracy of the model is the correctly classified positive and negative cases divided by all ther cases.The total accuracy of the model is 91.96%, which means the model prediction is very accurate.




##variable importance plot:

varImpPlot(model_rndfor)

##By setting the importance argument on, we obtained the variable importance plot as above using varImpPlot() function and we can see that duration is highly significant in our data set.## 


##Plotting a graph for error rate with number of trees

plot(model_rndfor)%>%
  legend("right", legend=c("OOB Error", "FPR", "FNR"),
         col=c("black", "red", "green"), lty=1:3, cex=0.8)



##We can see that the False Negative Rate is higher compared to other error rate and False Positive Rate is lowest. The error rate starts dropping for at ntree~ 20. This says that our model is predicting 'Yes' cases more accurately than 'No' cases which is also shown confusion matrix




## 3.2.Data Modeling using KNN
##Making a copy of our data set for our k-NN classification. 


data_FOR_knn <- dt.df

str(data_FOR_knn)



# We must use numeric variables onlyBecause k-NN algorithm involves determining distances between datapoints. This is applicable only to independent variables. The target variable for k-NN classification should remain a factor variable.
# Firstly , we will scale the data just in case our features are on different metrics. For example, if we had "duration" as a variable, it would be on a much larger scale than "age", which could be problematic given the k-NN relies on distances. Note that we are using the 'scale' function here, which means we are scaling to a z-score metric.
# 
# The variables "age", "duration", "campaign", "pdays", "previous", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m"  and "nr.employed" are interger variables, that can be scalled.



data_FOR_knn[, c("age", "duration", "campaign", "pdays", "previous", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m","nr.employed")] <- scale(data_FOR_knn[, c("age", "duration", "campaign", "pdays", "previous", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m","nr.employed")])

head(data_FOR_knn)





str(data_FOR_knn)


##We can see that the variables "job", "marital", "education", "default", "housing", "loan", "contact", "month", "day_of_week" and "poutcome" are factor variables that have two or more levels.##


## The dummy code variables that have two levels, but are not numeric. ##

data_FOR_knn$contact <- dummy.code(data_FOR_knn$contact)



##dummy code variables that have three or more levels.##

default <- as.data.frame(dummy.code(data_FOR_knn$default))
housing <- as.data.frame(dummy.code(data_FOR_knn$housing))
day_of_week <- as.data.frame(dummy.code(data_FOR_knn$day_of_week))
education <- as.data.frame(dummy.code(data_FOR_knn$education))
loan <- as.data.frame(dummy.code(data_FOR_knn$loan))
month <- as.data.frame(dummy.code(data_FOR_knn$month))
job <- as.data.frame(dummy.code(data_FOR_knn$job))
marital <- as.data.frame(dummy.code(data_FOR_knn$marital))
poutcome <- as.data.frame(dummy.code(data_FOR_knn$poutcome))



##Renamming "unknown" columns.##



marital <- rename(marital, unknown_marital = unknown)
education <- rename(education , unknown_education  = unknown)
default <- rename(default , unknown_default  = unknown)

default <- rename(default , yes_default  = yes)
default <- rename(default , no_default  = no)

housing <- rename(housing , yes_housing  = yes)
housing <- rename(housing , no_housing  = no)

loan <- rename(loan , yes_loan  = yes)
loan <- rename(loan , no_loan  = no)


job <- rename(job, unknown_job = unknown)

housing <- rename(housing , unknown_housing  = unknown)
loan <- rename(loan , unknown_loan  = unknown)





##Combinning new dummy variables with original data set..##

data_FOR_knn <- cbind(data_FOR_knn, job, marital, education, default, housing, loan, month, day_of_week,poutcome)




str(data_FOR_knn)



##Removing original variables that had to be dummy coded.##

data_FOR_knn <- data_FOR_knn %>% select(-one_of(c("job", "marital", "education", "default", "housing", "loan", "month", "day_of_week", "poutcome")))

head(data_FOR_knn)

#We are now ready for k-NN classification. We partition 80% of the data into the training set and the remaining 20% into the test set.

##Splitting the dataset into Test and Train:##

set.seed(1234) # set the seed to make the partition reproducible

# 80% of the sample size
sample_size <- floor(0.8 * nrow(data_FOR_knn))


train_index <- sample(seq_len(nrow(data_FOR_knn)), size = sample_size)

# putting outcome in its own object
outcome_OF_knn <- data_FOR_knn %>% select(y)

# removing original variable from the data set
data_FOR_knn <- data_FOR_knn %>% select(-y)



# creating test and training sets that contain all of the predictors
knn_data_train <- data_FOR_knn[train_index, ]
knn_data_test <- data_FOR_knn[-train_index, ]

# Splitting outcome variable into training and test sets using the same partition as above.
outcome_OF_knn_train <- outcome_OF_knn[train_index, ]
outcome_OF_knn_test <- outcome_OF_knn[-train_index, ]




##We will run our k-NN classification on our data using 'class' package,. We have to decide on the number of neighbors (k).This is an iterative exercise as we need to keep changing the value of k to dtermine the optimum performance. In our case, we started with k=10 till k=20, and finally got an optimum performance at k=17.


model_knn <- knn(train = knn_data_train, test = knn_data_test, cl = outcome_OF_knn_train, k=17)


##Evaluating model:##


# putting "outcome_OF_knn_test" in a data frame
outcome_OF_knn_test <- data.frame(outcome_OF_knn_test)

# merging "model_knn" and "outcome_OF_knn_test" 
knn_comparison_df <- data.frame(model_knn, outcome_OF_knn_test)

# specifying column names for "knn_comparison_df"
names(knn_comparison_df) <- c("KNN_Predicted_y", "KNN_Observed_y")

knn_comparison_df$KNN_Predicted_y <- as.factor(knn_comparison_df$KNN_Predicted_y)
knn_comparison_df$KNN_Observed_y <- as.factor(knn_comparison_df$KNN_Observed_y)

# inspecting "knn_comparison_df" 
head(knn_comparison_df)



# Finally we will compare our predicted values of deposit to our actual values.
# The confusion matrix will give an indication of how well our model predicted the actual values.
# The confusion matrix output also shows overall model statistics and statistics by class


confusionMatrix(knn_comparison_df$KNN_Observed_y,knn_comparison_df$KNN_Predicted_y)



# The K-nn test data consisted of 8233 observations. 
# 7045 cases have been accurately predicted (TN->True Negatives) as negative class (0) which constitutes 87%. 
# Also, 383 out of 8233 observations were accurately predicted (TP-> True Positives) as positive class (1) which constitutes 4%. 
# Thus a total of 383 out of 8233 predictions where TP i.e, True Positive in nature.

# There were 591 cases of False Positives (FP) meaning 591 cases out of 8238 were actually negative but got predicted as positive.

# There were 214 cases of False Negatives (FN) meaning 214 cases were actually positive in nature but got predicted as negative.

# Accuracy of the model is the correctly classified positive and negative cases divided by all ther cases.The total accuracy of the model is 90.22%, which means the model prediction is very accurate.

