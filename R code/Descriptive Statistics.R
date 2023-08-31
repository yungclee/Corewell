# import R packages
library(ggplot2)

#===========================================
# import data into R
#===========================================

## by downloaded file (use the file path on your computer)
data=read.csv("C://Users/BH244340/Downloads/Glucose.csv")

## by github URL, requires internet connection, this may timeout for online compiler 
data = read.csv(url("https://raw.githubusercontent.com/yungclee/Corewell/main/Data/Glucose.csv"))


## by text, this method is not recommended
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
# view the first 6 rows of the data
head(data)

# cut the continuous Glucose into categorical data
data$Glucose_grp <- cut(data$Glucose, 
                        # [arg:breaks] breaks are left exclusive right inclusive (x1, x2]
                        breaks = c(0,3.9,5.6,6), 
                        # [arg:labels] n break points corresponds to n-1 intervals
                        labels=c('Low', 'Normal', 'High'))

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
# pie plot for categorical variables
#===========================================

pie(prop.table(table(data$Glucose_grp)))


#===========================================
# bar plot for categorical variables
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


#===========================================
# summary stats for continuous variables
#===========================================
summary(data$Glucose) # summary statistics


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
  stat_summary(fun.y=mean, geom="point", shape=3, size=2, color="red", fill="red") 


#===================================================================
# histogram + density plot for continuous variables (optional)
#====================================================================
hist(data$Glucose,freq=FALSE)
lines(density(data$Glucose),col=4,lwd=3)

# alternative using ggplot
ggplot(data = data, aes(x = Glucose)) +
  geom_histogram(aes(y = after_stat(density)), bins = 8) +
  geom_density()

