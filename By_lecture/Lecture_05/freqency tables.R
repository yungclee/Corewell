# import required R packages
library(reshape2)
library(epitools) # for OR and RR
library(fmsb) # for risk difference
#=========================================================
# Cold example in class
# Chi-squared test & Fisher's exact test
#=========================================================
# create a table
M <- as.table(rbind(c(17, 122), c(31,109)))
#Or
M<-matrix(c(17,31,122,109),nrow = 2, ncol = 2)

# assign names to th ematrix
dimnames(M) <- list(vitc = c("Y", "N"),
                    cold = c("Y","N"))

# calculate relative risk and conduct chi-square or Fisher's test
riskratio.wald(M,rev = c("both"))

# calculate odds ratio and conduct chi-square or Fisher's test
oddsratio.wald(M,rev = c("both"))

# calculate difference in risks
riskdifference(17, 31, 139, 140, CRC=TRUE, conf.level=0.95) 


#=========================================================
# Alternative R code for chi-square test
#==========================================================
res=chisq.test(M,correct = FALSE) 

res

# compare observed vs expected
res$observed   # observed counts (same as M)
res$expected   # expected counts under the null


#=========================================================
# Alternative R code for Fisher's exact test
#==========================================================
fisher.test(M)


