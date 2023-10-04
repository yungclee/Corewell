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

# non-parametric test for two samples 
wilcox.test(decbp[group==1],decbp[group==2],correct = TRUE) 

detach(cal)
#============================================================
#   aspirin example  (paired two-samples)
#============================================================
data=read.csv("C:/Lili/BH/GEM courses/Data/aspirin.csv")
View(data)

# calcuate change

data$change=data$after - data$before
# summary stats
summary(data$change)

#change the data to long format
dd_long=data[,1:3]%>%
  gather(!c(Subject),key=time,value=temp)

# make a graph
# reorder the categories
dd_long$time <- factor(dd_long$time, levels=c('before','after'))

#spaghetti plot
ggplot(data = dd_long, aes(x = time, y = temp, group = Subject)) +
  geom_point(aes(color = time)) +
  geom_line(aes(group = Subject)) +
  theme(axis.text.y=element_text(size=15),axis.text.x=element_text(size=15,angle=0),
        axis.title=element_text(size=15,face="bold"),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        plot.title = element_text(color="black", size=20, face="bold.italic"))+xlab("")

# boxplot for the difference
boxplot(data$change,  outline = TRUE)

# paired t-test
t.test(data$after,data$before,paired=TRUE, alternative = "two.sided")

# which is equivalent to
t.test(data$change, alternative = "two.sided")

# non-parametric test for paired samples 
wilcox.test(data$after,data$before,paired=TRUE, alternative = "two.sided")

#========================================================================

#============================================================
#  Exercise: LOS
#============================================================
dd=read.csv("C:/Lili/BH/GEM courses/Data/LOS.csv")

# check size of the data
dim(dd)

head(dd)

# summary stats by group
tapply(dd$los, dd$group, summary)

# boxplot by group
ggplot(data=dd, aes(x=group, y=los)) + 
  geom_boxplot(width=0.4)

# histogram by group
ggplot(dd, aes(x = los)) + 
  geom_histogram(colour = "blue") +
  facet_grid(group ~ .)

# log transformation of LOS
dd$loglos=log(dd$los)

attach(dd)
# two-sample t test to compare LOS
t.test(los[group=="Control"],los[group=="Experimental"])

# two-sample t test to compare logLOS
t.test(loglos[group=="Control"],loglos[group=="Experimental"])

# non-parametric test for two samples 
wilcox.test(los[group=="Control"],los[group=="Experimental"]) 

detach(dd)

#============================================================
#  Exercise: mineral
#============================================================
dat=read.csv("C:/Lili/BH/GEM courses/Data/mineral.csv")

# boxplot for the difference
boxplot(dat$Diff,  outline = TRUE)

# paired t-test
t.test(dat$Tech_1,dat$Tech_2,paired=TRUE, alternative = "two.sided")

# non-parametric test for paired samples 
wilcox.test(dat$Tech_1,dat$Tech_2,paired=TRUE, alternative = "two.sided")

