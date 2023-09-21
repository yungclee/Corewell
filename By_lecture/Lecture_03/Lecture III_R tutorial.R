# install R packages
# install.packages("reshape2")
# install.packages("ggplot2") #install

# import the package you need 
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)

#===========================================
# import data into R
#===========================================

## by downloaded file (use the file path on your computer)
data=read.csv("C:/Lili/BH/GEM courses/Data/Glucose.csv")
## "=" and "<-" are exchangeable in R programming. 
## the left hand side is the name of the object you wish to save the content on the right hand side
## proper naming-check is enforced by R, some names are consider "reserved word" such as 'if'
## https://www.datamentor.io/r-programming/reserved-words#:~:text=reserved%20at%20the%20R%20command,the%20logical%20constants%20in%20R.

## if you need documentation or check arguments needed for any function, use help() or put '?' 
## infront of the function name will help you get to the documentation page: 
## usage:
# help(read.csv)
# ?read.csv

# View all the data
View(data)

# view the first 6 rows of the data
head(data)

# from the R documentation:
# ## Default S3 method:
# head(x, n = 6L, ...) <- as you can see the default value for display number is 6.
# if you want to display more rows, you can change the argument/parameter
head(data, 3)


# data file is a data frame
class(data)

# dimension of the data
dim(data)

## data type for each variables in the data set
str(data)

#------------------------------------------------------------------------------#
# indexing in R
#------------------------------------------------------------------------------#

# since data is a data.frame 
# you can use square brackets "[]" to subset/indexing by value, name or criteria
## dataframe[row_idx, col_idx]

## indexing row by number 
# get a subset dataset with the first 5 rows
data_sub=data[1:5,] # ":"(colon) is seq, x:x+3 is equivalent of writing c(x, x+1, x+2, x+3)
data_sub


## indexing columns by number 
# get a subset dataset with the first 3 rows
data_sub <- data[,1:3]
data_sub


## extractung specific element in 3rd row and 2nd column
data[3, 2]

## indexing columns by name
data_sub=data[,c("Glucose","Gender")]
data_sub


## '$' dollar sign indexing
#get one column 
glucose=data$Glucose
class(glucose)

gender=data$Gender
class(gender)

#get Glucose for Females
glucoseF=data$Glucose[data$Gender=="F"]

# get a data for Females

dataF=data[data$Gender=="F",]
glucoseF=dataF$Glucose


#------------------------------------------------------------------------------#
# indexing and function usage practice
#------------------------------------------------------------------------------#
##
## [EASY]Q1: please return the `id` for those with Glucose value >= 4.0 
##           (one-liner solution)
##
##
##
## [MEDIUM]Q2: please return the minimal `Weight` for those with Glucose < 3 and Female
##
## HINT: you can chain the condition by using logic operator "&"(and), "|"(or)
##       ex: data[data$Weight > 10 & data$Gender == "M"] this subsets the data with weight > 10 and Male
## HINT: you can use the function min() to return the minimal value in a vector
##        if you need help with function details, use ?min or help(min)
##
##
##
##
## [HARD]Q3: Whats the difference between the mean weight if male and female?
##
## the difference of mean of the two group is calculated by 
## the mean/average value weight for Male subtract by the mean weight for Female
## HINT: mean() is the function in R to get average of a vector
##
##
##
#------------------------------------------------------------------------------#

# create a new variable
data$logGlucose=log(data$Glucose) # log is deafult to be nature log, use log10() if you want 10-based log

data$Glucose5=data$Glucose*5

# cut the continuous Glucose into categorical data

## syntax: ifelse(condition, action if condition met, action if condition is NOT met)
data$Glucose_grp = ifelse (data$Glucose >3.9, "High", "Low")
class(data$Glucose_grp)

data$Glucose_grp = ifelse (data$Glucose >3.9, 1, 0)
class(data$Glucose_grp)

data$Glucose_grp = as.factor(data$Glucose_grp)
class(data$Glucose_grp)

# cut the continuous Glucose into categorical data
data$Glucose_grp <- cut(data$Glucose, 
                        # [arg:breaks] breaks are left exclusive right inclusive (x1, x2]
                        breaks = c(0,3.9,5.6,6), 
                        # [arg:labels] n break points corresponds to n-1 intervals
                        labels=c('Low', 'Normal', 'High'))
class(data$Glucose_grp)




## create a data frame 
data = data.frame(id = 1:40,
                  Glucose = c(2.2, 2.9, 3.3, 3.3, 3.3, 3.4, 3.4, 3.4, 3.6, 3.6, 
                              3.6, 3.6, 3.7, 3.7, 3.8, 3.8, 3.8, 3.9, 4, 4, 4, 
                              4.1, 4.1, 4.1, 4.2, 4.3, 4.4, 4.4, 4.4, 4.5, 4.6,
                              4.7, 4.7, 4.7, 4.8, 4.9, 4.9, 5, 5.1, 6),
                  Gender = c('F', 'F', 'M', 'M', 'M', 'F', 'F', 'F', 'M', 'M',
                             'F', 'M', 'M', 'M', 'F', 'M', 'M', 'M', 'F', 'M',
                             'F', 'F', 'F', 'M', 'M', 'M', 'F', 'M', 'F', 'M',
                             'F', 'F', 'F', 'M', 'M', 'M', 'F', 'F', 'F', 'M'),
                  Weight = c(68.05, 70, 70.45, 75.15, 50.8, 63.5, 61.6, 56.9, 
                             92.5, 81.6, 70.45, 144.5, 84.9, 66.65, 57.2, 54.75,
                             74.2, 70.6, 68, 93.2, 72.4, 45.55, 62.75, 86.2, 50.5, 
                             92.23, 46.9, 84.2, 68.9, 79.6, 69.95, 80.65, 49.3, 
                             74.2, 98, 66, 70, 80.65, 51, 74.2)
                  )



#===========================================
# summary stats for categorical variables
#===========================================

# calculate count for each category

table(data$Gender)
table(data$Glucose_grp)

# calculate proportion for each category
prop.table(table(data$Gender))
prop.table(table(data$Glucose_grp))

#===========================================
# summary stats for continuous variables
#===========================================
summary(data$Glucose) # summary statistics


#============================================================
#  calcium example (two-sample t-test)
#============================================================
#import data called calcium 
cal=read.csv("C:/Lili/BH/GEM courses/Data/calcium.csv")

head(cal,3)

# change group to a factor 
cal$group=as.factor(cal$group)

# summary stats by group
tapply(cal$decbp, cal$group, summary)

t.test(cal$decbp[cal$group==1],cal$decbp[cal$group==2])

attach(cal)

# two-sample t test
t.test(decbp[group==1],decbp[group==2],paired=FALSE, alternative = "two.sided",var.equal = TRUE)

detach(cal)

#===========================================
# bar plot for categorical variables
# https://rpubs.com/techanswers88/after_stat_in_ggplot
#===========================================

# by Gender
ggplot(data = data, aes(x = Gender, y = after_stat(count))) +
  geom_bar(width=0.3) 

# by Glucose
ggplot(data = data, aes(x = Glucose_grp, y = after_stat(count))) +
      geom_bar(width=0.3) 

# make barplot by Gender (optional)
ggplot(data = data, aes(x = Glucose_grp, y = after_stat(count) , fill = Gender)) +
  geom_bar(position = position_dodge()) 

#alternative (optional)
ggplot(data = data, aes(x = Glucose_grp, y = after_stat(count), fill = Gender)) +
  geom_bar(position = position_dodge()) +
  facet_grid(.~ Gender)


#================================================================================================
# box plot for continuous variables
# http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization
#===================================================================================================

# boxplot for Glucose
boxplot(data$Glucose,  outline = TRUE)

# boxplot by Gender
ggplot(data, aes(x=Gender, y=Glucose)) + 
  geom_boxplot(width=0.2)

# boxplot by Gender (add means)
ggplot(data, aes(x=Gender, y=Glucose)) + 
  geom_boxplot(width=0.2) +
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, color="red", fill="red") 


#===================================================================
# histogram + density plot for continuous variables (optional)
#====================================================================
hist(data$Glucose,freq=FALSE)
lines(density(data$Glucose),col=4,lwd=3)

# alternative using ggplot
ggplot(data = data, aes(x = Glucose)) +
  geom_histogram(aes(y = after_stat(density)), bins = 8) +
  geom_density()

