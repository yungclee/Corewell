# install.packages("reshape2")

# import the package you need 
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)

#============================================================
#  calcium example (two-sample t-test)
#============================================================
#import data called calcium 
cal=read.csv("C:/Lili/BH/GEM courses/Data/calcium.csv")

head(cal)

# change group to a factor 
cal$group=as.factor(cal$group)

# summary stats by group
tapply(cal$decbp, cal$group, summary)

# boxplot by group
ggplot(data=cal, aes(x=group, y=decbp)) + 
  geom_boxplot(width=0.4)

attach(cal)

# two-sample t test
t.test(decbp[group==1],decbp[group==2],paired=FALSE, alternative = "two.sided",var.equal = TRUE)

detach(cal)
