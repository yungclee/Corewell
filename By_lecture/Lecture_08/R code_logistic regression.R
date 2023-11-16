library(dplyr)

#********************************************************
#  * Cold example - a binary predictor
#*******************************************************
cold=read.csv("C:/Lili/BH/GEM courses/Data/pauling.csv")
cold$trmt=as.factor(cold$trmt)
res=glm(outcome ~ trmt, family=binomial, data=cold)
summary(res)

# For odds ratio
exp(res$coefficients)

# for confidence intervals
exp(confint(res))

#*************************************************************************
# Framingham study with two binary predictors (no interaction)
#***************************************************************************

## data
fram = data.frame(age = c(0,0,1,1),
                  sbp = c(1,0,1,0),
                  chd = c(23,71,21,49),
                  total = c(42,210,33,119))
fram$non_chd=fram$total-fram$chd

fram$age=as.factor(fram$age)
fram$sbp=as.factor(fram$sbp)

res=glm(cbind(chd,non_chd) ~ age + sbp, family=binomial, data=fram)
# p-value
summary(res)
# For odds ratio
exp(res$coefficients)[2:3]
# for confidence intervals
exp(confint(res))[2:3,]


#===================== Useful link for contrasts when there is interaction =======================
#https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/ 
#===============================================================================

#*************************************************************************
#  Framingham study with two binary predictors (with interaction)
#*************************************************************************
res1=glm(cbind(chd,non_chd) ~ age+ sbp+ age:sbp, family=binomial, data=fram)

library(emmeans) # for slice effect
# obtain OR for sbp for each  age-group comparison
emm1=emmeans(res1, specs = pairwise ~ age:sbp, adjust = "none")

# obtain confidence interval
emm1$contrasts %>% confint()
 
# obtain OR for sbp for each age group
emmeans(res1, specs = pairwise ~ sbp|age, type = "response")
 




