##OBJECTIVE
#Design the best classifier using ctree function based on one of the given dataset according to the selected quality measure.
#In each experiment, a different classifier is designed and it's quality measure is tested using precision.

##DEFINITION
#A classifier is interesting if the precision is greater than or equal to 75%. We could also consider accuracy
#the more precise the classifier is, the better it is.

##POTENTIAL PRACTICAL APPLICATION
#Predicting the age of abalone from physical measurements from younger to older. The age of abalone is determined by cutting the shell through the cone, staining it, 
#and counting the number of rings through a microscope -- a boring and time-consuming task. Other measurements, which are easier to obtain, are used to predict the age.


#library loading
library(caret)
library(party)
library(rpart)
library(e1071)
library(car)

#The fixed random seed makes the results repeatable
set.seed(1234)

#Reading the data
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data', 'abalone.data')
abalone = read.csv("abalone.data", header = FALSE, col.names = c('sex', 'length', 'diameter', 'height', 'whole_weight','shucked_weight', 'viscera_weight','shell_weight',"rings") )

summary(abalone)
str(abalone)
View(abalone)

#To find the unique values of the classes
unique(abalone$rings)

#The classes from 1 to 29 will be grouped in to three classes
#Age1 - 1 to 10 rings
#Age2 - 11 to 20 rings
#Age3 - 21 to 29 rings

#New column for age class
age_class <- recode(abalone$rings,"c('1','2','3','4','5','6','7','8','9','10')='Age1';c('11','12','13','14','15','16','17','18','19','20')='Age2';
                      c('21','22','23','24','25','26','27','28','29')='Age3'")

#Modifying the dataset according to the new classes
abalone1 <- cbind(abalone, age_class)
abalone2 <- subset(abalone1, select = -c(rings) )

#Final dataset after grouping in to 3 classes
View(abalone2)
unique(abalone2$age_class)

idTrainData <- unlist(createDataPartition(abalone2$age_class,p=0.7))
idTrainData

trainabalone <- abalone2[idTrainData,]
trainabalone

testabalone <- abalone2[-idTrainData,]
testabalone

#To check if both collections have representative examples of the amount of each class.
table(trainabalone$age_class)
table(testabalone$age_class)

#Defining formula
myFormula <- age_class ~ sex+ length + diameter + height + whole_weight + shucked_weight + viscera_weight + shell_weight


###EXPERIMENT 1 ###
##Classification of the training data using Decision Trees without data preprocessing

abalone_ctree <- ctree(myFormula, data=trainabalone)


plot(abalone_ctree, uniform=TRUE,     main="Abalone")

#Visual representation of the cross-validation results 
trainPred = predict(abalone_ctree,newdata=trainabalone)
testPred = predict(abalone_ctree,newdata=testabalone)

##CALCULATION RESULTS
# Accuracy = a + d / a+b+c+d
print(mean(testPred == testabalone$age_class))
#[1] 0.7515974

#Confusion Matrix
confusionMatrix(testabalone$age_class, testPred)
#Prediction Age1 Age2 Age3
#Age1  720   99    0
#Age2  202  221    0
#Age3    2    8    0

#Kappa : 0.4206  

#Precision = a/a+b
print(sum(testPred == testabalone$age_class & testPred == 'Age1') / sum(testPred == 'Age1'))
#0.7792208

#The decision tree with no data preprocessing has precision = 77.92 % and accuracy = 75.15 %, 
#therefore acceptable. We can adjust the parameters to get a better result.


### EXPERIMENT 2 ###
##Classification of the training data using Decision Trees with data preprocessing [testtype = c("Bonferroni")]
myParam = ctree_control(minbucket = 5,mincriterion = 0.8,teststat = c("quad"),testtype = c("Bonferroni"))

abalone_ctree2 <- ctree(age_class~., data=trainabalone,controls = myParam )

plot(abalone_ctree2, uniform=TRUE,     main="Abalone")

#Visual representation of the cross-validation results 
trainPred = predict(abalone_ctree2,newdata=trainabalone)
testPred = predict(abalone_ctree2,newdata=testabalone)

##CALCULATION RESULTS
# Accuracy = a + d / a+b+c+d
print(mean(testPred == testabalone$age_class))
#[1] 0.75

#Confusion Matrix
confusionMatrix(testabalone$age_class, testPred)
#Prediction Age1 Age2 Age3
#Age1  715  104    0
#Age2  199  224    0
#Age3    2    8    0

#Kappa : 0.4196  

#Precision = a/a+b
print(sum(testPred == testabalone$age_class & testPred == 'Age1') / sum(testPred == 'Age1'))
#0.7792208

#The decision tree with data preprocessing (minbucket = 5,mincriterion = 0.8,teststat = c("quad"),testtype = c("Bonferroni"))
#has precision = 77.92 % and accuracy = 75 %.There is no big difference from previous experiment and so
#we can adjust the parameters to get a better result.

###EXPERIMENT 3 ###
##Classification of the training data using Decision Trees with data preprocessing [testtype = c("Univariate")]
myParam = ctree_control(minbucket = 5,mincriterion = 0.9,teststat = c("quad"),testtype = c("Univariate"))

abalone_ctree3 <- ctree(age_class~., data=trainabalone,controls = myParam )

plot(abalone_ctree3, uniform=TRUE,     main="Abalone")

#Visual representation of the cross-validation results 
trainPred = predict(abalone_ctree3,newdata=trainabalone)
testPred = predict(abalone_ctree3,newdata=testabalone)

##CALCULATION RESULTS
# Accuracy = a + d / a+b+c+d
print(mean(testPred == testabalone$age_class))
#[1] 0.7579872

#Confusion Matrix
confusionMatrix(testabalone$age_class, testPred)
#Prediction Age1 Age2 Age3
#Age1  729   90    0
#Age2  202  220    1
#Age3    1    9    0

#Kappa : 0.4332  

#Precision = a/a+b
print(sum(testPred == testabalone$age_class & testPred == 'Age1') / sum(testPred == 'Age1'))
#0.7821888

#The decision tree with data preprocessing (minbucket = 5,mincriterion = 0.9,teststat = c("quad"),testtype = c("Univariate"))
#has precision = 78.2 % and accuracy = 75.79 %.This result seems to be better than previous two. Yet,
#we can adjust the parameters to see if we can get a better result.


###EXPERIMENT 4
##Classification of the training data using Decision Trees with data preprocessing [testtype = c("Teststatistic")]
myParam = ctree_control(minbucket = 5,mincriterion = 0.6,teststat = c("quad"),testtype = c("Teststatistic"))

abalone_ctree4 <- ctree(age_class~., data=trainabalone,controls = myParam )

plot(abalone_ctree4, uniform=TRUE,     main="Abalone")

#Visual representation of the cross-validation results 
trainPred = predict(abalone_ctree4,newdata=trainabalone)
testPred = predict(abalone_ctree4,newdata=testabalone)

##CALCULATION RESULTS
# Accuracy = a + d / a+b+c+d
print(mean(testPred == testabalone$age_class))
#0.75

#Confusion Matrix
confusionMatrix(testabalone$age_class, testPred)
#Prediction Age1 Age2 Age3
#Age1  708  111    0
#Age2  191  231    1
#Age3    2    8    0

#Kappa : 0.425 

#Precision = a/a+b
print(sum(testPred == testabalone$age_class & testPred == 'Age1') / sum(testPred == 'Age1'))
#0.7857936

#The decision tree with data preprocessing (minbucket = 5,mincriterion = 0.6,teststat = c("quad"),testtype = c("Teststatistic"))
#has precision = 78.57 % and accuracy = 75 %. The precision and accuracy seem to be close to  the values in Experiment 3.

######################################################################################################################

#CONCLUSIONS

#Criteria for assessing the quality of the classifier?
#Precision and  accuracy

#Name of the attributes and how many classes are highlighted in the task.
# Different Attributes of Abalone
#Name / Data Type / Measurement Unit / Description
#-----------------------------
#Sex / nominal / -- / M, F, and I (infant)
#Length / continuous / mm / Longest shell measurement
#Diameter / continuous / mm / perpendicular to length
#Height / continuous / mm / with meat in shell
#..
#Rings / integer / 

# The class values are rings which are grouped in to Age1, Age2, Age3.
#Age1 - 1 to 10 rings
#Age2 - 11 to 20 rings
#Age3 - 21 to 29 rings
#The ring value + 1.5 years provides the actual age of abalone
# These values can say if the abalone is an infant or younger or older

##Based on the above experiments, using the ctree function the best classifier is obtained from experiment 3, 
#with precision = 0.782 and accuracy ~ 0.76. Kappa measures were also compared and the best value was 0.4332 
#due to high precision and accuracy.




