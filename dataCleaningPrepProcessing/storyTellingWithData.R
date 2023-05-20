##############
## TJ
## My story from school data
##############


## Read the dataset
Filename <- '../data/data-storyteller.csv'
school_df <- read.csv(Filename, na.strings = c(""))
## print the dataframe
head(school_df, n=7) 

## check the structure
str(school_df)

## Section has an int data type,it need to be converted to categorical, session are ordered
## school has the correct datatype, factor
## very.Aheag, middling, behind, more.behind, very.behind and completd has the correct data type,interger.

## change section data type to factor
school_df$Section <- as.ordered(school_df$Section)

## Cofirm the data structure
str(school_df$Section)

### check for missing values

## function to check missing values from the dataframe
checkMissingData <- function(TestData){
  for(colname in names(TestData)){
    cat("\n Looking at column...", colname, "\n")
    NAcount <- sum(is.na(TestData[colname]))
    cat("\nThe num of missing values in column ", colname, "is  ", NAcount)
  }
}

##check missing values using the function
checkMissingData(school_df)
## There is no missing values

## summary of the datasets
summary(school_df)
## school D and E has on one record of observation while school C has 3, school B has 12 and school A has 13
## There is no school with student count that are very ahead


## Check the column names
colnames(school_df) 
##  "School","Section","Very.Ahead..5","Middling..0","Behind..1.5","More.Behind..6.10","Very.Behind..11","Completed"

## rename columns
new_col_name <- c('School' , 'Section' , 'Very_Ahead' , 'Middling' , 'Behind' , 'More_Behind' , 'Very_Behind' , 'Completed')
## print the vector
new_col_name

## Assign the vector as the new column name
colnames(school_df) <- new_col_name

## check the new dataframe
head(school_df, n=5)



## check if there is duplicates records
nrow(school_df) # the total number of row in the dataframe
cat ('There are ', nrow(school_df), 'number of rows in the datasets')

nrow(school_df[duplicated(school_df),]) #duplicate check

cat('There are ', nrow(school_df[duplicated(school_df),]), 'number of duplicates record' )

## There is no duplicate records in the datasets


## load library
library(psych)
library(ggplot2)
library(cowplot)


## Check for incorrect records
boxplot(school_df[,-c(1,2)])

## The completed column and more behind sims to have an oulier

## deep dive into completed columns
table(school_df$Completed)

## There is only one record with 27 completed which happen to be unsual

## check the school compared to the completed columns
table(school_df$School, school_df$Completed)

## School E has an unsually number of student with completed compared to other school.

## check the school that is more behind
## cross tab schoo with more behind

table(school_df$School, school_df$More_Behind)
## School A has the highes number of student that are more behind which is abnormal compared to other school.

### Check the data districution of each of the numeric columns

## Schools
table(school_df$School)

## session
hist(table(school_df$Section))
summary(school_df$Section)

d <- ggplot(school_df, aes(Section)) + geom_bar(color='green', fill='blue') + theme_classic() + ggtitle('Frequency distribution of session')
d

## vry ahead
table(school_df$Very_Ahead)

## middling
m <- ggplot(school_df,aes(Middling) ) + geom_histogram(color='green', fill='blue', bins = 20) + theme_classic()
n <- ggplot(school_df,aes(Middling) ) + geom_dotplot(color='green', fill='blue') + theme_classic()
o <- ggplot(school_df,aes(Middling) ) + geom_density(color='green', fill='blue') + theme_classic()
p <- ggplot(school_df,aes(Middling) ) + geom_freqpoly(color='red', fill='blue') + theme_classic()

plot_grid(m,n,o,p, nrow = 2, ncol = 2, labels = 'AUTO')




## Behind
m <- ggplot(school_df,aes(Behind) ) + geom_histogram(color='green', fill='blue', bins = 20) + theme_classic()
n <- ggplot(school_df,aes(Behind) ) + geom_dotplot(color='green', fill='blue') + theme_classic()
o <- ggplot(school_df,aes(Behind) ) + geom_density(color='green', fill='blue') + theme_classic()
p <- ggplot(school_df,aes(Behind) ) + geom_freqpoly(color='red', fill='blue') + theme_classic()

plot_grid(m,n,o,p, nrow = 2, ncol = 2, labels = 'AUTO')


## more behind

m <- ggplot(school_df,aes(More_Behind) ) + geom_histogram(color='green', fill='blue', bins = 20) + theme_classic()
n <- ggplot(school_df,aes(More_Behind) ) + geom_dotplot(color='green', fill='blue') + theme_classic()
o <- ggplot(school_df,aes(More_Behind) ) + geom_density(color='green', fill='blue') + theme_classic()
p <- ggplot(school_df,aes(More_Behind) ) + geom_freqpoly(color='red', fill='blue') + theme_classic()

plot_grid(m,n,o,p, nrow = 2, ncol = 2, labels = 'AUTO')


## very behind
m <- ggplot(school_df,aes(Very_Behind) ) + geom_histogram(color='green', fill='blue', bins = 20) + theme_classic()
n <- ggplot(school_df,aes(Very_Behind) ) + geom_dotplot(color='green', fill='blue') + theme_classic()
o <- ggplot(school_df,aes(Very_Behind) ) + geom_density(color='green', fill='blue') + theme_classic()
p <- ggplot(school_df,aes(Very_Behind) ) + geom_freqpoly(color='red', fill='blue') + theme_classic()

plot_grid(m,n,o,p, nrow = 2, ncol = 2, labels = 'AUTO')

## completed
m <- ggplot(school_df,aes(Completed) ) + geom_histogram(color='green', fill='blue', bins = 20) + theme_classic()
n <- ggplot(school_df,aes(Completed) ) + geom_dotplot(color='green', fill='blue') + theme_classic()
o <- ggplot(school_df,aes(Completed) ) + geom_density(color='green', fill='blue') + theme_classic()
p <- ggplot(school_df,aes(Completed) ) + geom_freqpoly(color='red', fill='blue') + theme_classic()

plot_grid(m,n,o,p, nrow = 2, ncol = 2, labels = 'AUTO')



## school and section

p <- ggplot(school_df,aes(School,Section) ) + geom_count(color='red', fill='blue', size=7) + theme_classic()
p



## school and middling
m <- ggplot(school_df,aes(School, Middling) ) + geom_boxplot(color='green', fill='blue') + theme_classic()
n <- ggplot(school_df,aes(School, Middling) ) + geom_dotplot(color='green', fill='blue') + theme_classic()
o <- ggplot(school_df,aes(School, Middling) ) + geom_violin(color='green', fill='blue') + theme_classic()
p <- ggplot(school_df,aes(School, Middling) ) + geom_col(color='green', fill='blue') + theme_classic()

plot_grid(m,n,o,p, nrow = 2, ncol = 2, labels = 'AUTO')


## School and Behind

m <- ggplot(school_df,aes(School, Behind) ) + geom_boxplot(color='green', fill='blue') + theme_classic()
n <- ggplot(school_df,aes(School, Behind) ) + geom_dotplot(color='green', fill='blue') + theme_classic()
o <- ggplot(school_df,aes(School, Behind) ) + geom_violin(color='green', fill='blue') + theme_classic()
p <- ggplot(school_df,aes(School, Behind) ) + geom_col(color='green', fill='blue') + theme_classic()

plot_grid(m,n,o,p, nrow = 2, ncol = 2, labels = 'AUTO')

## frequency distribution
table(school_df$School, school_df$Behind)


## school and more behind
##label <- c('Boxplot', 'Dotplot','Violine','Column Plot')

m <- ggplot(school_df,aes(School, More_Behind) ) + geom_boxplot(color='green', fill='blue') + theme_classic()
n <- ggplot(school_df,aes(School, More_Behind) ) + geom_dotplot(color='green', fill='blue') + theme_classic()
o <- ggplot(school_df,aes(School, More_Behind) ) + geom_violin(color='green', fill='blue') + theme_classic()
p <- ggplot(school_df,aes(School, More_Behind) ) + geom_col(color='green', fill='blue') + theme_classic()

plot_grid(m,n,o,p, nrow = 2, ncol = 2, labels = 'AUTO')



## school and very behind

m <- ggplot(school_df,aes(School, Very_Behind) ) + geom_boxplot(color='green', fill='blue') + theme_classic()
n <- ggplot(school_df,aes(School, Very_Behind) ) + geom_dotplot(color='green', fill='blue') + theme_classic()
o <- ggplot(school_df,aes(School, Very_Behind) ) + geom_violin(color='green', fill='blue') + theme_classic()
p <- ggplot(school_df,aes(School, Very_Behind) ) + geom_col(color='green', fill='blue') + theme_classic()

plot_grid(m,n,o,p, nrow = 2, ncol = 2, labels = 'AUTO')


## school and completed
m <- ggplot(school_df,aes(School, Completed) ) + geom_boxplot(color='green', fill='blue') + theme_classic()
n <- ggplot(school_df,aes(School, Completed) ) + geom_dotplot(color='green', fill='blue') + theme_classic()
o <- ggplot(school_df,aes(School, Completed) ) + geom_violin(color='green', fill='blue') + theme_classic()
p <- ggplot(school_df,aes(School, Completed) ) + geom_col(color='green', fill='blue') + theme_classic()

plot_grid(m,n,o,p, nrow = 2, ncol = 2, labels = 'AUTO')




## Boxplot
h <- ggplot(school_df, aes(Group = School,x=School,Completed)) + geom_boxplot(color = 'green', fill = 'blue', size = 1) + theme_dark()
i <- ggplot(school_df, aes(Group = School,x=School,Middling)) + geom_boxplot(color = 'green', fill = 'blue', size=1) + theme_dark()
j <- ggplot(school_df, aes(Group = School,x=School,Behind)) + geom_boxplot(color = 'green', fill = 'blue', size=1) + theme_dark()
k <- ggplot(school_df, aes(Group = School,x=School,More_Behind)) + geom_boxplot(color = 'green', fill = 'blue', size=1) + theme_dark()
l <- ggplot(school_df, aes(Group = School,x=School,Very_Behind)) + geom_boxplot(color = 'green', fill = 'blue', size=1) + theme_dark()

plot_grid(h,i,j,k,l, nrow = 2, ncol = 3, labels = 'AUTO')



## Scatter plots
g <-ggplot(school_df, aes(x=Middling,y=Completed)) + geom_point(aes(y=Completed,color=Very_Behind,size= More_Behind)) + ggtitle('Comparing student record across some schools')
g


###
## Quick correlation plots of the datasets
pairs.panels(school_df[,-c(1,3)], gap=0)



## chech school and session records
(table(school_df$School, school_df$Section))



## coorelation test between middling and completed
cor.test(school_df$Middling, school_df$Completed)

## behind and very behind
cor.test(school_df$Behind, school_df$Very_Behind)



###################################################################################################



## aggregation
aggregate(school_df[,-c(1,2)], by=list(school_df[,1]), FUN = sum)

## total student/school
student_total <- rowSums(school_df[,-c(1,2)])
df_new <- data.frame(school_df,student_total)
head(df_new)

## aggregate the column using school to group by
df_agg <- aggregate(df_new[,-c(1,2)], by = list(df_new[,1]), FUN = sum) 
df_agg

## percent completed by school record
per_completed <- (df_agg[,7]/df_agg[,8]) * 100
per_completed 

df_new <- data.frame(df_agg,per_completed )
df_new

## check each school record of student

### School A
table(df_new$Group.1, df_new$Middling)

########################################################################################################################



## Correlation TEST
## people middling and people behind
cor.test(school_df$Middling, school_df$Behind)

## middling and more behind
cor.test(school_df$Middling, school_df$More_Behind)

## middling and very behind
cor.test(school_df$Middling, school_df$Very_Behind)

## middling and completed
cor.test(school_df$Middling, school_df$Completed)

## behind and more behind
cor.test(school_df$Behind, school_df$More_Behind)

## behind and very behind
cor.test(school_df$Behind, school_df$Very_Behind)

## more behind and very behind
cor.test(school_df$More_Behind, school_df$Very_Behind)

## behind and completed
cor.test(school_df$Behind, school_df$Completed)





