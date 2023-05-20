###############################################
##  Tutorial: TOPICS
##    
##    SVM - support vector machines
##     - for iris
##     - for Kaggle Titanic data
##    
##  Dr. A, Gates, 2018
##  
###############################################
## Fun YouTube Resources:
## 
## https://www.youtube.com/watch?v=ueKqDlMxueE   ## SVM
## https://www.youtube.com/watch?v=pS5gXENd3a4   ## SVM
## 
## 
## I will start the examples using the iris dataset
## because it is easy to see and understand
## It also clusters well and has correct labels
## Remember that "real" data is not as easy to 
## work with because it may not cluster, it may have
## incorrect or odd labels or no labels, etc. 

## Next - below, I will use the Kaggle Titanic Datasets
## These will offer a more realiztic view of these
## methods and will also include the required cleaning
## and prep. 



########################
## libraries
## NOTE: Always install.packages("ThePackName") if needed
## for each library included.
#install.packages("e1071")
library(e1071)  # for machine learning methods
#install.packages("mlr")
library(mlr)
# install.packages("caret")
library(caret)
#install.packages("naivebayes")
##library(naivebayes)
library(datasets)
library(ggplot2)
library(MASS)  


#######################################
##            IRIS                   ##
#######################################

#### Look at the iris data ####
## 
## Here, we do not need to clean or prep the
## data. However, when using real data, you 
## will spend 80% of your time prepping/cleaning

########### View the Data 
plot(iris)
(head(iris))
(str(iris))
(summary(iris))
(nrow(iris))
## col is color...
plot(iris$Sepal.Length, iris$Petal.Width, col=iris$Species)
plot(iris$Petal.Length,iris$Petal.Width, col=iris$Species)
## using qplot
qplot(iris$Petal.Length, iris$Petal.Width, data=iris, color=iris$Species)


###### Create a Test and Train set ##############
## Random sample without replacement
## sample(x, size, replace = FALSE, prob = NULL)
## Create a random sample of 30 numbers from 1 - 150
samplerownums<- sample(150,40)
(iris_Testset <- iris[samplerownums,])
## Remove and keep the labels
(irisTestLabels <- iris_Testset[,c(5)])
iris_Testset<-iris_Testset[,-c(5)]
(head(iris_Testset))
## For the training data, we want to have/keep the class label
iris_Trainset <- iris[-samplerownums,]
(head(iris_Trainset))

#################  Set up the SVM -----------------
## Soft svm with cost as the penalty for points
## being in the wrong location of the margin
## boundaries
## There are many kernel options...

###################################################
## Polynomial Kernel...
SVM_fit_P <- svm(Species~., data=iris_Trainset, 
               kernel="polynomial", cost=.1, 
               scale=FALSE)
print(SVM_fit_P)
##Prediction --
(pred_P <- predict(SVM_fit_P, iris_Testset, type="class"))
## COnfusion Matrix
(Ptable <- table(pred_P, irisTestLabels))
## We have 4 variables and so need our plot to be more precise
plot(SVM_fit_P, data=iris_Trainset, Petal.Width~Petal.Length, 
     slice=list(Sepal.Width=3, Sepal.Length=4))
## The above places Petal.Width on the x and Petal.Length
## on the y. It also holds Sepal.Width constant at 3 and
## Sepal.Length constant at 4.
## We need to do this because out plot is 2D and so can
## only show 2 dimensions/attributes as variables 

## ------ View/calucalte misclassification
## The Ptable above is the confusion matrix that shows
## what was classified correctly and what was not

## Misclassification Rate for Polynomial
(MR_P <- 1 - sum(diag(Ptable))/sum(Ptable))


###############################
## Linear Kernel...
SVM_fit_L <- svm(Species~., data=iris_Trainset, 
                 kernel="linear", cost=.1, 
                 scale=FALSE)
print(SVM_fit_L)
##Prediction --
(pred_L <- predict(SVM_fit_L, iris_Testset, type="class"))
(L_table<-table(pred_L, irisTestLabels))
plot(SVM_fit_L, data=iris_Trainset, Petal.Width~Petal.Length, 
     slice=list(Sepal.Width=3, Sepal.Length=4))
## Misclassification Rate for Linear
(MR_L <- 1 - sum(diag(L_table))/sum(L_table))

####################################
## Radial Kernel...
SVM_fit_R <- svm(Species~., data=iris_Trainset, 
                 kernel="radial", cost=.1, 
                 scale=FALSE)
print(SVM_fit_R)
##Prediction --
(pred_R <- predict(SVM_fit_R, iris_Testset, type="class"))
(R_table<-table(pred_R, irisTestLabels))
plot(SVM_fit_R, data=iris_Trainset, Petal.Width~Petal.Length, 
     slice=list(Sepal.Width=3, Sepal.Length=4))

## Misclassification Rate for Radial
(MR_R <- 1 - sum(diag(R_table))/sum(R_table))


## So, the polynomial seems to do the best job most of the 
## time. Remember that the sample is random.
## We can also update the cost. See below for how to 
## tune the cost....


####################################################
## SVM EXAMPLE 2 ###################################

## We cannot plot the above because the number of 
## attributes and the label together exceed 3
## Let's re-run the SVM with fewer attributes

Columns <- c("Petal.Length", "Petal.Width", "Species")
samplerownums<- sample(150,40)
iris_Testset_petal <- iris[samplerownums,Columns]
## Remove and keep the labels
(irisTestLabels <- iris_Testset_petal[,c(3)])
(iris_Testset_petal<-iris_Testset_petal[,-c(3)])
(head(iris_Testset_petal))
## For the training data, we want to have/keep the class label
iris_Trainset_petal <- iris[-samplerownums, Columns]
(head(iris_Trainset_petal))
## Set up the SVM again
SVM_fit2 <- svm(Species~., data=iris_Trainset_petal, kernel="linear",
                cost=.1)
print(SVM_fit2)
plot(SVM_fit2, iris_Trainset_petal)
(pred_2 <- predict(SVM_fit2, iris_Testset_petal, type="class"))
(pred_2)
(irisTestLabels)
(table(pred_2, irisTestLabels))

#####  We can "tune" the SVM by altering the cost ####
tuned_cost <- tune(svm,Species~., data=iris_Trainset_petal,
                   kernel="linear", 
                   ranges=list(cost=c(.01,.1,1,10,100,100)))
summary(tuned_cost)  ## This shows that the best cost is .1



#############################################################
## EXAMPLE:  Using SVM on Titanic Dataset from Kaggle
##
## 
#############################################################
setwd("C:/Users/profa/Documents/R/RStudioFolder_1/DrGExamples")
## Get Training Data
TitanicTrainData <- read.csv("Titanic_Training_Data.csv", na.string=c(""))
(head(TitanicTrainData, n=5))
## Get Testing Data

################## NOTE 1 #####################
## The class or label for SVMs must be a factor
############ NOTE 2 ###########################
## I am going to read in and clean
## and prep the testing data
## However, I will not use the testing
## dataset for my SVM as there are no labels
## I will comment out most/all of the Kaggle
## testing data items and will create my own
## testing set. However, I left the code in (and)
## commented out so you can see it
####################################################
filename="Titanic_Testing_Data.csv"
## Note: na.string=c("") will replace empty with NA
TitanicTestData <- read.table(filename, sep=",", header=TRUE, na.string=c(""))
(head(TitanicTestData,n=5))

########### Clean and Prepare ###################
## SVM data must be numeric
## Let's go through each attribute in the dataset
## and decide what to do with it
#################################################
#(head(TitanicTrainData, n=5))
(head(TitanicTestData,n=5))

## We will not need or gain value from the following
## PassengerID, Pclass, Name, Ticket, Cabin, Embarked
SVM_Train_Titanic <- TitanicTrainData[,-c(1,3,4,9,11,12)]
(head(SVM_Train_Titanic))
(str(SVM_Train_Titanic))
#SVM_Test_Titanic <- TitanicTestData[,-c(1,2,3,8,10,11)]
#(head(SVM_Test_Titanic))

## Change the classification, Survived to a Factor
SVM_Train_Titanic$Survived<-as.factor(SVM_Train_Titanic$Survived)
## Let's remove these first
(head(SVM_Train_Titanic, n=5))
(str(SVM_Train_Titanic))

## Next, we want to keep the variable Sex, but we need a good
## way to make it numerical and keep it accurate
## Let's create two new variables in the data frame
## Let's call the first male and the second female
## then, we place a 0 if the person is not that gender and 1 if they are

## Create a copy
NewSVM_Train <- SVM_Train_Titanic 
#NewSVM_Test <- SVM_Test_Titanic
## Add new columns and place 0 in them for now
NewSVM_Train$male <- 0
NewSVM_Train$female <- 0

#NewSVM_Test$male <- 0
#NewSVM_Test$female <- 0

## Place 1's 
NewSVM_Train$male[NewSVM_Train$Sex == "male"] <- 1
NewSVM_Train$female[NewSVM_Train$Sex == "female"] <- 1

#NewSVM_Test$male[NewSVM_Test$Sex == "male"] <- 1
#NewSVM_Test$female[NewSVM_Test$Sex == "female"] <- 1

(head(NewSVM_Train, n=5))
#(head(NewSVM_Test, n=5))

## Next, combine SibSp and Parch together into new
## variable called FamSize
NewSVM_Train$FamSize <- 0
NewSVM_Train$FamSize <- NewSVM_Train$SibSp + NewSVM_Train$Parch
(head(NewSVM_Train, n=5))

#NewSVM_Test$FamSize <- 0
#NewSVM_Test$FamSize <- NewSVM_Test$SibSp + NewSVM_Test$Parch
#(head(NewSVM_Test, n=5))

## Now, get rid of the columns we do not want
NewSVM_Train<-NewSVM_Train[,-c(2,4,5)]
(head(NewSVM_Train, n=5))

#NewSVM_Test<-NewSVM_Test[,-c(1,3,4)]
#(head(NewSVM_Test, n=5))

## Finally - we need to deal with any NA values
#(sum(is.na(NewSVM_Test$Age)))
## Yes - there are many NA values
(sum(is.na(NewSVM_Train$Age)))
(table(NewSVM_Train$Age))
#(table(NewSVM_Test$Age))
## From the above, we can see that we have ages that are not right (<1)
## We are not sure if < 1 means infant or if its an error.
## I will convert these all to NA 
NewSVM_Train$Age[NewSVM_Train$Age<1] <- NA
#NewSVM_Test$Age[NewSVM_Test$Age<1] <- NA
## Because Age has such a sig affect on Survival, we are better to remove
## all rows with incorrect or NA Age.
NewSVM_Train <- NewSVM_Train[complete.cases(NewSVM_Train), ]
#NewSVM_Test[complete.cases(NewSVM_Test), ]


######### !!! Now, because the test set here is not labeled, we ############
#########     cannot see if our model is working well. So, I will ##########
#########     split the Training data into a Train and Test Set   ##########
#########     and will keep the labels on the test set            ##########
(head(NewSVM_Train))
(NumRows <- nrow(NewSVM_Train))
(samplerows <- sample(1:NumRows, 80))
Special_Testset <- NewSVM_Train[c(samplerows),]
(head(Special_Testset))
## Remove and save labels
(Titanic_Test_Labels <- Special_Testset[,c(1)])
#(Titanic_Test_Labels)
Special_Testset_nolabels <- Special_Testset[,-c(1)]
## This is the finalized testset
(head(Special_Testset_nolabels))

## Now we are ready to create the SVM ---------------------
###########################################################
#######            SVM Titanic                  ###########
###########################################################
(head(NewSVM_Train))
(head(NewSVM_Test))

## Kernel Types
################## Polynomial -----------------------

kernelType = "polynomial"

## Cost = .1--------------------------------------
SVM_fit_Titanic_P <- svm(Survived~., data=NewSVM_Train, 
                 kernel=kernelType, cost=.1, 
                 scale=FALSE)
print(SVM_fit_Titanic_P)
##Prediction -- # 
str(Titanic_Test_Labels)
(pred_Titanic_P <- predict(SVM_fit_Titanic_P, Special_Testset_nolabels , type="class"))
(Titanic_table_P<-table(pred_Titanic_P, Titanic_Test_Labels))
##plot(SVM_fit_Titanic, data=NewSVM_Train, female~Survived)
##, slice=list(Sepal.Width=3, Sepal.Length=4))
## Misclassification Rate for ...
(MR_Titanic_P <- 1 - sum(diag(Titanic_table_P))/sum(Titanic_table_P))
## -------------------------------------------------

## At this time - I had c (the cost) set to .1 for all tests.
## Now, I will *tune* c
#####  We can "tune" the SVM by altering the cost ####
tuned_cost_P <- tune(svm,Survived~., data=NewSVM_Train,
                   kernel=kernelType, 
                   ranges=list(cost=c(.001,.01,0.1,1,10,100,100)))
summary(tuned_cost_P)  ## This shows that the best cost is 1
## Because this tuning showed that the best cost is 1, I will update
## the cost ...

## Cost = 1      --------------------------------------
SVM_fit_Titanic_P1 <- svm(Survived~., data=NewSVM_Train, 
                         kernel=kernelType, cost=.01, 
                         scale=FALSE)
print(SVM_fit_Titanic_P1)
##Prediction -- # 
(pred_Titanic_P1 <- predict(SVM_fit_Titanic_P1, Special_Testset_nolabels , type="class"))
(Titanic_table_P1<-table(pred_Titanic_P1, Titanic_Test_Labels))
##plot(SVM_fit_Titanic, data=NewSVM_Train, female~Survived)
##, slice=list(Sepal.Width=3, Sepal.Length=4))
## Misclassification Rate for ...
(MR_Titanic_P1 <- 1 - sum(diag(Titanic_table_P1))/sum(Titanic_table_P1))

## Interestingly, changing the cost to 1 did NOT offer better results.
## For fun, I updated the svm just above for costs between .001 and 100. 
## I found that .01 gave the best results and so I left the cost as = .01
## You can do this for the linear and radial as well. 
## -------------------------------------------------



###################### Linear -------------------------
kernelType = "linear"  
SVM_fit_Titanic_L <- svm(Survived~., data=NewSVM_Train, 
                       kernel=kernelType, cost=.01, 
                       scale=FALSE)
print(SVM_fit_Titanic_L)
##Prediction -- # 
(pred_Titanic_L <- predict(SVM_fit_Titanic_L, Special_Testset_nolabels , type="class"))
(Titanic_table_L<-table(pred_Titanic_L, Titanic_Test_Labels))
##plot(SVM_fit_Titanic, data=NewSVM_Train, female~Survived)
##, slice=list(Sepal.Width=3, Sepal.Length=4))
## Misclassification Rate for ...
(MR_Titanic_L <- 1 - sum(diag(Titanic_table_L))/sum(Titanic_table_L))
## This one works pretty well. Linear sometimes works well
## when the dimension of the data is already high.
## NOTES: Linear gives good results with cost = .1
## I also tried cost = .001, .01, 10, and 100. 
## Interstingly 100 worked well and so did .01

################## Radial ------------------------------------
kernelType = "radial"  ## Gaussian
SVM_fit_Titanic_R <- svm(Survived~., data=NewSVM_Train, 
                       kernel=kernelType, cost=10, 
                       scale=FALSE)
print(SVM_fit_Titanic_R)
##Prediction -- # 
(pred_Titanic_R <- predict(SVM_fit_Titanic_R, Special_Testset_nolabels , type="class"))
(Titanic_tableR<-table(pred_Titanic_R, Titanic_Test_Labels))

## Misclassification Rate for ...
(MR_Titanic_R <- 1 - sum(diag(Titanic_tableR))/sum(Titanic_tableR))
## Radial is excellent when I changed the cost to 10. In fact, it is now the
## best of the three kernel types.

################ TAKE HOME MESSAGE #########################
## SVM is *not* a black box. You must experiment with kernels
## and costs to see which offers the best results. 

############## Visualization ##############################
## Its nice to visualize stuff.

## Let's try to plot the SVM with the best accuracy
## which is the SVM_fit_Titanic_R
(head(NewSVM_Train))
#(mean(NewSVM_Train$Fare))
plot(SVM_fit_Titanic_R , NewSVM_Train, Age ~ Fare, 
     slice = list(male = 0, female = 0, FamSize=1))

plot(SVM_fit_Titanic_R , NewSVM_Train, Age ~ female, 
     slice = list(male = 0, Fare = 0, FamSize=1))

plot(SVM_fit_Titanic_R , NewSVM_Train, FamSize ~ Age, 
     slice = list(male = 0, Fare = 0, female=1))

plot(SVM_fit_Titanic_R , NewSVM_Train, FamSize ~ Age, 
     slice = list(male = 0, Fare = 0, female=0))

