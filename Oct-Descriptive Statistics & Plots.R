data=read.csv("C:/Lili/OJOC/2019/Biostat524/Data/Glucose.csv")
attach(data)
head(data)

##################################
# bar plot
#####################################
table(Gender)

barplot(table(Gender), width=c(0.15,0.15),xlim=c(0,1))


##################################
# box plot
#####################################
boxplot(Glucose,  outline = F)

?boxplot
summary(Glucose) # summary statistics

##################################
# histogram
#####################################
hist(Glucose,freq=TRUE)

hist(Glucose,freq=FALSE)

hist(Glucose,freq=FALSE,breaks=c(2,3,5,6))
hist(Glucose,freq=FALSE,breaks=10)
##################################
# histogram + density plot
#####################################
hist(Glucose,freq=FALSE,breaks=c(2,3,5,6))
lines(density(Glucose),col=4)


#####################################
# boxplot with raw data added
####################################
install.packages("beeswarm")
library(beeswarm)

boxplot(Glucose)
beeswarm(Glucose, col = 4,add = TRUE)


#--another beeswarm plot
boxplot(Glucose,  outline = TRUE)
beeswarm(Glucose, col = 4, pch = 16,add = TRUE,cex=1.7)


###############################################################################################################
# Violin plot: http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization
##############################################################################################################
library(ggplot2)
# Basic box plot
geom_boxplot(outlier.colour="black", outlier.shape=16,
             outlier.size=2, notch=FALSE)

p <- ggplot(data, aes(x=Gender, y=Glucose)) + 
  geom_boxplot()

# Basic violin plot
p=ggplot(data, aes(x=Gender, y=Glucose)) + 
  geom_violin()

p
# violin plot with median points
p + stat_summary(fun.y=median, geom="point", size=2, color="red")


#Add median and quartile
p + geom_boxplot(width=0.1)

# violin plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)


#####################################
# QQ with raw data added
####################################
par(mfrow = c(2,2))
qqnorm(Glucose)
qqline(Glucose,col=3)


x=c(8,10,11,12,14)

par(mfrow = c(2,2))
qqnorm(x)
qqline(x,col=3)

y <- rt(200, df = 500)
qqnorm(y); qqline(y, col = 2)
qqplot(y, rt(300, df = 5))


######################################################
# calcuate a proportion given x in a normal distribution
#######################################################

pnorm(q=0.81,mean=0,sd=1,lower.tail = TRUE)
pnorm(q=-1.25,mean=0,sd=1,lower.tail = TRUE)


######################################################
# calcuate x given a proportion in a normal distribution
#######################################################
qnorm(p=0.1,mean=70,sd=2.8,lower.tail = TRUE)
