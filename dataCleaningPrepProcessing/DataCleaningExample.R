##############################################################
## A full edxample of Cleaning and EDA of data using R.
## 
## This example will use a wine dataset in here...
##https://drive.google.com/drive/folders/1Cf9Wspt3JEQ2il7KP6VgxMf6klpih7FW?usp=sharing
##
## Do not assume that this data is valid. It is for example only
################################################################
## Gates, 2018
library(ggplot2)
library(dplyr)  ## for using pipe %>%
library(psych)
library(purrr)  ## for keep()
library(tidyr) ## for gather()

## Step 1 - get and bring in the data
setwd("/Users/teejay/dev/courses/machine-learning-in-r/data")
DataFileName="WineDATA_small_dirty_forDataCleaningExample.csv"
## Important: set header as TRUE if the variables are named and set all blanks to NA
## NOTE:  <NA> means "factor" NA and NA without the <> is non-factor (number)
DirtyWine_DF <- read.csv(DataFileName,header=T, na.strings=c("","NA"))



## To remove NAs - there are many choices. We can remove the
## entire row, we can remove a column that appears to both
## have many NAs and that does not matter for our particular 
## research question, we can replace the NA values with mean
## or median. 

## This must be done one variable at a time, as each variable
## will have its own issues and uniquenesses.
## While doing this, we can also replace or move incorrect values.
## So, we will do all of these steps first, and then come
## back to normalization and visualization. 

## Let's look at the types...
str(DirtyWine_DF)

###   FIX  NONFLAVS -----------------------------------
## Here we see that shade is a factor - that is fine.
## However, nonflavs is also afactor, and that is not right.
## Let's get more info...
(table(DirtyWine_DF$nonflavs))
## OK - we see the problem. One of the values is "yes". Let's replace this first with 
## the char 0 - then change to numeric, 
## and then with the mean....
DirtyWine_DF$nonflavs <- as.character(DirtyWine_DF$nonflavs)
DirtyWine_DF$nonflavs[DirtyWine_DF$nonflavs=="yes"] <- NA
DirtyWine_DF$nonflavs <- as.numeric(DirtyWine_DF$nonflavs)
str(DirtyWine_DF)
(table(DirtyWine_DF$nonflavs))
value <- round(mean(DirtyWine_DF$nonflavs, na.rm=TRUE),2)
DirtyWine_DF$nonflavs[is.na(DirtyWine_DF$nonflavs)] <- value
## Check results...
(DirtyWine_DF$nonflavs)
str(DirtyWine_DF)
(table(DirtyWine_DF$nonflavs))

##########   FIX COLOR -----------------------------------------
##
## The color attribute is also set as a factor and has some
## issues
(table(DirtyWine_DF$color))
(sum(is.na(DirtyWine_DF$color)))
## We see that color has a value of "**" which needs to be fixed. It also have one NA
## It is the "**" that is forcing color to be factor, when it should be numerical
DirtyWine_DF$color <- as.character(DirtyWine_DF$color)
DirtyWine_DF$color[DirtyWine_DF$color=="**"] <- NA
DirtyWine_DF$color <- as.numeric(DirtyWine_DF$color)
str(DirtyWine_DF)
(table(DirtyWine_DF$color))
## Now - replace NA with mean
value <- round(mean(DirtyWine_DF$color, na.rm=TRUE),2)
DirtyWine_DF$color[is.na(DirtyWine_DF$color)] <- value

str(DirtyWine_DF)

## UPDATE -------------------
## This is good. We have no more inproper factors. 
## Now we need to work on the rest of the attributes....

## Look at tables of each variable
lapply(DirtyWine_DF,table)
## This is a sort-of visualization as it shows you possible issues with the data
## For example
## for malicacid, there is a 29 which appears to be an outlier. 
## In proline, there is a -1, etc.

## Next - let's look at total NA values for all columns.
sapply(DirtyWine_DF, function(x) sum(is.na(x)))
## Here we see that ash has 8! Interestingly, I will not use ash
## in my research, so I will remove that column.
## Next, datechecked has 3. I do not need the date either, but will
## keep it for this tutorial. 
## All others have between 0 and 3.

#################   ASH ------------------
## Ash has 8 NA values and is not of use for my research interests
## I will remove the entire column.

DirtyWine_DF2 <- DirtyWine_DF[,-4]
(head(DirtyWine_DF2))
## 

#########    SHADE -----------------------------------------
(DirtyWine_DF2$shade)
(table(DirtyWine_DF2$shade))
## Medium is the most common. Let's replace the "??" with medium.
DirtyWine_DF2$shade[DirtyWine_DF2$shade=="??"] <- "medium"
## Check it and remove unused factors with droplevels
DirtyWine_DF2$shade<-droplevels(DirtyWine_DF2$shade)
str(DirtyWine_DF2$shade)
## NOTE: In this case, I have chosen to fill in medium for the one incorrect
## value in shade. Because there is only one and because shade is not critcal for
## prediction, this choice is OK. 
## The ifelse method can also be used...the following line is commented out
## and is just an example of using ifelse
###DirtyWine_DF$shade <- ifelse(DirtyWine_DF$shade =="??", "medium", as.character(DirtyWine_DF$shade) )

############            ALCOHOL ---------------------------------
## Replace all NA values with the mean. 
plot(DirtyWine_DF2$alcohol)
(DirtyWine_DF2$alcohol)
(table(DirtyWine_DF2$alcohol))
## We have one NA. Let's replace it with the mean
DirtyWine_DF2$alcohol[which(is.na(DirtyWine_DF2$alcohol))]<-mean(DirtyWine_DF2$alcohol, na.rm=T)
(DirtyWine_DF2$alcohol)

###############   ALL OTHER NUMERIC ---------------------------------------
## OK - let's do this for malicacid, alcalinty, mag, phenols, falvs, nonflav and proantho...
## To save time, I will replace all NA values for all columns with the mean except for
## column 1, 14, and 15

## determine the column names that contain NA values
(NA_var_names <- names(DirtyWine_DF2)[colSums(is.na(DirtyWine_DF2)) != 0])

## We do not need a for loop here, as I am only going to update phenols.
## However, it is good to see how to do this if you wanted to update many columns
## at once...
plot(DirtyWine_DF2$proline)
(head(DirtyWine_DF2))
for(i in 1:ncol(DirtyWine_DF)){
  if(i>1  && i < 14){   ##  > 1 will remove shade. < 14 will remove date
      #print(DirtyWine_DF[,i])
      DirtyWine_DF2[is.na(DirtyWine_DF2[,i]), i] <- mean(DirtyWine_DF2[,i], na.rm = TRUE)
    
  }
}
(head(DirtyWine_DF2))
str(DirtyWine_DF2)

#### Check all the variables for NA, str, and head....
for(i in names(DirtyWine_DF2)){
  cat("NAME:",i,"\n")
  (str(DirtyWine_DF2[,i]))
  (head(DirtyWine_DF2[,i],n=10))
  print(sum(is.na(DirtyWine_DF2[,i])))
  
}

### CHECK FOR COMPLETE CASES ------------------
Temp3 <- DirtyWine_DF2[complete.cases(DirtyWine_DF2),]
print("complete cases:   ")
(nrow(Temp3))
print("total rows:    ")
(NROW(DirtyWine_DF2))

## To remove all rows with any NAs, you can uncomment the following
##DirtyWine_DF2 <- na.omit(DirtyWine_DF2)


## Fix any date and time formats
## %08d is an integer with 8 values. sprintf is an old C function.
## The as.Date method allows for date formats, such as %m%d%Y
## RE: https://www.statmethods.net/input/dates.html
DirtyWine_DF2$datechecked <- as.Date(sprintf("%08d", DirtyWine_DF2$datechecked), format ="%m%d%Y")
(head(DirtyWine_DF2, n=5))
## These next two lines will show us the the datechecked column has 3 NA values
(nrow(DirtyWine_DF2))
(sum(complete.cases(DirtyWine_DF2)))
## Fixing NA for dates is not a clear process. It is also not necessary. 
## We can leave the NA as long as we remember that this column has NAs
## You can also set hte NA dates to some arbitrary date.I will leave mine as NA
## Note that the date is in column 14.
## However, as I do not want to deal with the date type as I perform other methods
## and visualizations, I will remove it from the DF and store it
DateDF <- DirtyWine_DF2[,14]
DirtyWine_DF2<-DirtyWine_DF2[,-14]
(head(DirtyWine_DF2))

##### BOX PLOT VISUAL EDA and Cleanining Assistance ---------

## As a first step, we can create box plots of the data.
## Let's normalize the data first....

### This function will normalize using Min-Max
Min_Max_function <- function(x){
  return(  (x - min(x)) /(max(x) - min(x))   )
}

TempDF <- as.data.frame(sapply(DirtyWine_DF2[,-1], Min_Max_function))
(head(TempDF))
str(TempDF)

########  Histograms -----------------------
TempDF %>%
  keep(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#######  Boxplots ----------------------------
## Here is another method
require(reshape2)
ggplot(data = melt(TempDF), 
       aes(x=variable, y=value)) +
        geom_boxplot()

## The boxplot visualization suggest some possible outliers

###############    OUTLIERS ------------------------
## NOtice that malicacid, phenols, and proline may have 
### possible outliers or incorrect values. 
## Let's look at the non-normalized data as boxplots for each
boxplot(DirtyWine_DF2$proline)
(table(DirtyWine_DF2$proline))
## One of the values is 12900. We can take a fair guess that this should have
## been 1290. ## Also, the median for proline is
(median(DirtyWine_DF2$proline))
## Let's change the value. 
DirtyWine_DF2$proline[DirtyWine_DF2$proline> 2000] <- 1290
## There is also a -1. This is a problem. We can change to the median
## to 1290, or remove it. 
DirtyWine_DF2$proline[DirtyWine_DF2$proline < 1000] <- round(median(DirtyWine_DF2$proline,na.rm=T))
boxplot(DirtyWine_DF2$proline)
(max(DirtyWine_DF2$proline))
(min(DirtyWine_DF2$proline))
## OK - this is the best we can do
## It is unclear as to whether the max value is an outlier or not, so we cannot alter it

#### Let's look at malicacid next...
boxplot(DirtyWine_DF2$malicacid)
(table(DirtyWine_DF2$malicacid))
## One of the values is 29. We can take a fair guess that this should have
## been 2.9. ## Also, the median for malic is 1.8 and the mean is 2.6
(median(DirtyWine_DF2$malicacid))
(mean(DirtyWine_DF2$malicacid))
## Let's change the value. 
DirtyWine_DF2$malicacid[DirtyWine_DF2$malicacid> 5] <- 2.3
## There is also a -1. This is a problem. We can change to the median
boxplot(DirtyWine_DF2$malicacid)
(max(DirtyWine_DF2$malicacid))
(min(DirtyWine_DF2$malicacid))
boxplot(DirtyWine_DF2$malicacid)


#### Let's look at phenols next...
boxplot(DirtyWine_DF2$phenols)
(table(DirtyWine_DF2$phenols))
(summary(DirtyWine_DF2$phenols))
## This one is OK.


######## At this point, we have done a lot of cleaning  -
## Notice that cleaning happens in stages and as you look 
## at different measures and visualizations, you *discover*
## further items that require cleaning, fixing, or attention
## From here, let's see the current state of the data

lapply(DirtyWine_DF2,table)
lapply(DirtyWine_DF2,summary)
(nrow(DirtyWine_DF2))
(sum(complete.cases(DirtyWine_DF2)))
## Notice that the number of rows and the number of complete cases (rows without)
## NA are the same - this is what we want.

## The types are good. We have one factor and all others are numeric as they should be
## NOTE: When working with trained ML methods, you MUST use a factor as your label
str(DirtyWine_DF2)  

##Get the dimensions of the dataset
dim(DirtyWine_DF2)




# This line of code will convert shade to an ordinal type if you wish.
#DirtyWine_DF2$shade <- factor( DirtyWine_DF2$shade, ordered = TRUE, levels = c("light", "medium", "dark"))
 



## Using Visual EDA to clean data - another look at histograms 
(NumNumericCols <- sum(sapply(DirtyWine_DF2,is.numeric)))
## The following two lines will dynamically create a set of subplots of good size
NCols = as.integer(NumNumericCols/3)
par(mfrow=c(3,NCols))
for(i in names(DirtyWine_DF2)){
  cat("NAME:",i,"\n")
  if(is.numeric(DirtyWine_DF2[,i])){
    hist(DirtyWine_DF2[,i], main=i, xlab=i)
  }
}

## The above is great for investigating the distributions of each attribute
## We see that alcohol looks largely normal, malicacid is very skewed
## Alcalinity is a sort-of normal with a small variance
## Mag is slightly skewed, but you could use normal methods on it.

## Now that the data is clean, we can also look at correlations again:
## pairs.panels(DirtyWine_DF2)  ## This creates a vis that is hard to read
## Let's break it up....
pairs.panels(DirtyWine_DF2[,c(1,2,3,4)])
pairs.panels(DirtyWine_DF2[,c(5,6,7,8)])
pairs.panels(DirtyWine_DF2[,c(9,10,11,12,13)])

## Results:
## As one might expect, there is acorrelation between proanthocyanins and color
## There is an interesting and storng relationship between color and proline
## that is well worth health-related investigation as proline is a key
## component in collagen production.
## Phenols and flavanoids are highly related and it might be interesting to see the
## relationship between shade, color, phenols, and flavs... and alcohol
pairs.panels(DirtyWine_DF2[,c(1,2,6,7,9,10)])

## From the above, we can visually see that the values look good.

## Keep in mind that data cleaning and prep can take a lot of time
## and can be repetative. The above is an example of some of the
## options and methods - but certainly not all of them.

###################   Copy of the Cleaned Data ----------------------
## Let's create a copy of the cleaned data
CleanWineDF <- DirtyWine_DF2

## We can also write the clean data to a file
getwd()  ## This will tell you where you save it
write.csv(CleanWineDF, file = "CLEAN_Wine_Data.csv")

############################################################################
###################  VISUAL EDA - Exploratory Data Analysis ----------------
#############################################################################
## In this next section, we will use different - but basic and static vis 
## options to explore and investigate the data further.

## When visualizing data, a good first step is to look
## at the structure/types of the data
str(CleanWineDF)

## In our case, we have one factor and all the rest
## are numerical.

###########    NORMALIZATION and TRANSFORMATION ############################

## Normalization can often assist in visualization and in the process of 
## variables. There are many types of normalization. Two common forms
## are min-max and standard (z) normal.

##  MIN-MAX
## We have min-max above, but I will copy the function here again.
## You can see that my Min-Max function takes an object
## and first subtracts the min value. By doing this, you are "moving"
## or translating the data so that the min is 0 and all other values
## are relative to 0. Next, you divide by the max-min or the range of 
## the data. This is similar to dividing by the standard deviation when
## using z-normal to standardize or normalize. When you divide each value
## (minus the min) by the range of the data, you get a relative
## fraction that must be <= 1. This then updates all data values to be
## between 0 and 1, but still retain their relative distance from each other.
## EXAMPLE:  Suppose you numbers 2, 5, 10, 20, 100. The min is 2, the range is 98
##  The numbers become:  (2-2)/98, (5-2)/98, (10-2)/98, (20-2)/98, (100-2)/98
## WHich is:   0, .031, .082, .184, 1

#  MIN MAX FUNCTION ----------
Min_Max_function <- function(x){
  return(  (x - min(x)) /(max(x) - min(x))   )
}

## We can test the above example on our function
MyTestSet <- c(2,5,10,20,100)
Min_Max_function(MyTestSet)  ## We see that the results are what is expected.

#  Z-FUNCTION -----------------
## Alternatively, we can use the mean and standard deviation (z) to
## normalize data. Each value is changed to az = (x - mean)/stdev value
## Let's see that on the sample set of numbers as above:
##  2, 5, 10, 20, 100
## Here, rather than subtracting the min, which moves all numbers over so that 
## the new nim is 0, using z subtracts the mean. This forces the center
## of the dataset to be zero (not the min)
## The mean for 2, 5, 10, 20, and 100 is   27.4
## The stdev (which like the range is also a measure of variation) is 41.16
## So, when we standardize 2, we get:
##  (2 - 27.4)/41.16 = -.61 (approx due to rounding)
## NOtice that with standarzing with z rather than normalizing with min-max
## z values can be negative because the 0 is the center of the standard data.
(mean(MyTestSet))
(sd(MyTestSet))

## You can write your own function to perform a z-standardization

z_function <- function(x){
  return(  (x - mean(x)) /(sd(x))   )
}

## You can also use an R library and method
## standardize(x, centerFun = mean, scaleFun = sd)

### Let's use both to confirm that our function works

(MyStdExampleData <- z_function(MyTestSet))
#require(standardize)
(MyStdExampleData2 <- scale(MyTestSet))  
## scale assume you want mean 0 and sd 1

####################    Versions of our Data ----------------------

## This is our clean dataset
(head(CleanWineDF))
## This is a min-max normalized version of the data
## !!!!!! IMPORTANT  - we can only normalize numeric values!!!!!!
## I am using [,-1] to NOT include the first column as its a factor
Temp <- as.data.frame(sapply(CleanWineDF[,-1], Min_Max_function))
(head(MinMaxWineDF <- data.frame(shade=CleanWineDF[,1], Temp)))

## This is a standard normalized version of the data
Temp <- as.data.frame(sapply(CleanWineDF[,-1], z_function))
(head(STN_WineDF <- data.frame(shade=CleanWineDF[,1], Temp)))


#############    TRANSFORMATION -----------------------------
## In addition to normalizing data, we can transform it
## We can make it discrete (binning)
## We can apply functions to it, such as exponential or log, etc.
## The reasons for doing this depend on your goals.

## Let's transform proline to its log
(head(LogProline <- log(CleanWineDF$proline)))


## Let's discretize the alcohol variable
## There are many methods. I generally use "cut". 
BinnedAlcohol <- cut(CleanWineDF$alcohol, breaks=4, labels=c("Low","Med","High","VeryHigh") )
## Create a new DF that has this new binned variable
(head(CleanWineDF_binnedAlc <- data.frame(CleanWineDF,BinnedAlc=BinnedAlcohol)))
barchart(BinnedAlcohol)
ggplot(CleanWineDF_binnedAlc, aes(x = BinnedAlc)) + geom_bar(aes(fill = shade))

#####  Discretization can be very important for various methods and vis

#####################################################################
###########  Special Transformations and Visualizations --------------------

if(!require(rcompanion)){install.packages("rcompanion")}
library(rcompanion)

### Example 1: Transforming Skewed data
NCols = as.integer(NumNumericCols/3)
par(mfrow=c(3,NCols))
for(i in names(CleanWineDF[,-1])){
  cat("NAME:",i,"\n")
  if(is.numeric(CleanWineDF[,i])){
    hist(CleanWineDF[,i], main=i, xlab=i)
  }
}

## From here, we can see that Proline is skewed, as is phenols
par(mfrow=c(2,1))
plotNormalHistogram(CleanWineDF$proline)
plotNormalHistogram(CleanWineDF$phenols)
## A Q-Q plot will show how far from "normal" the data is
par(mfrow=c(2,1))
qqnorm(CleanWineDF$proline)
qqline(CleanWineDF$proline, col="red")
qqnorm(CleanWineDF$phenols)
qqline(CleanWineDF$phenols, col="red")

## We can try a couple of transformations 
## Square root is common for right-skewed data
par(mfrow=c(2,1))
Proline_SQRT <- sqrt(CleanWineDF$proline)
plotNormalHistogram(Proline_SQRT)
Phenols_SQRT <- sqrt(CleanWineDF$phenols)
plotNormalHistogram(Phenols_SQRT)

## Let's try the cubed root
par(mfrow=c(2,1))
Proline_CUBE <- (CleanWineDF$proline)^(1/3)
plotNormalHistogram(Proline_CUBE)
Phenol_CUBE <- (CleanWineDF$phenols)^(1/3)
plotNormalHistogram(Phenol_CUBE)

## Let's look at the log transformation
par(mfrow=c(2,1))
Proline_LOG <- log(CleanWineDF$proline)
plotNormalHistogram(Proline_LOG)
Phenol_LOG <- log(CleanWineDF$phenols)
plotNormalHistogram(Phenol_LOG)

 

###### Looking at qqplot for other variables--------
## We can show that some of our attributes are close to 
## normal....


par(mfrow=c(3,3))
for(i in names(CleanWineDF[,-c(1,3,5)])){
  cat("NAME:",i,"\n")
  if(is.numeric(CleanWineDF[,i])){
    qqnorm(CleanWineDF[,i], main=i, xlab=i)
    qqline(CleanWineDF[,i],col="red")
    
  }
}

### We can see that alcohol is highly normally distributed.



## ggpairs
## 1/2 at a time....
ggpairs(CleanWineDF[,-c(1,2,3,4,5,6,7)])
ggpairs(CleanWineDF[,-c(1,8,9,10,11,12,13)])





