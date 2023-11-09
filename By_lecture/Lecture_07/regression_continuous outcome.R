library(dplyr)
#===========================================
# Reg - one continuous predictor
#===========================================

data=read.csv("C:/Lili/BH/GEM courses/Data/FEV1.csv")
View(data)

# pearson correlation
cor.test(data$height, data$FEV1, method = "pearson")

# spearman correlation
cor.test(data$height, data$FEV1, method = "spearman")

#Simple Linear Regression
fit=lm(FEV1~height, data=data)

#get summary from the linear reg fitting
summary(fit)

# get the regression coefficients
fit$coefficients

# get the fitted FEV1 value for each subject
fit$fitted.values

#95% confidence intervals of coefficients
confint(fit)

# scatter plot (Ray) and add fitted line to scatter plot
plot(data$height,data$FEV1, xlab="height",ylab="FEV1", main="Scatter Plot")
abline(coef(fit),lty=1)

## using ggplot2
p_reg = 
  ggplot2::ggplot(data = data, aes(x = height, y = FEV1)) +
  geom_point() + 
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")
p_reg

# make model diagnosis using residual plots
par(mfrow = c(2, 2))
plot(fit)

#===========================================
# Reg - one categorical predictor
#===========================================
data=read.csv("C:/Lili/BH/GEM courses/Data/bone.csv")
View(data)

data$group=as.factor(data$group)

# make a boxplot (Ray)

p_boxplot = 
  ggplot2::ggplot(data = data, aes(x = group, y =y )) + 
  geom_boxplot()

p_boxplot

# make a violin (Ray)

p_violin = 
  ggplot2::ggplot(data = data, aes(x = group, y =y , fill= group)) + 
  geom_violin(trim = FALSE,width=0.4) +
  geom_boxplot(width = 0.1,col=1,fill="white")+
  stat_summary(fun.y=mean, geom="point", shape=3, size=2, color="red", fill="red")

p_violin


# run simple linear regression 
model = lm(y ~ group,  data = data)
# obtain parameter estimates
summary(model)  

# type III test
#anova(model)

# pairwise comparisons
library(emmeans)
paircom=emmeans(model, specs = pairwise ~ group, adjust = "none")
# get confidence intervals
confint(paircom)

#paircom$contrasts %>% confint()

# Multiple regression (birthwegith data)

df_birthweight = read.csv("C:/Lili/BH/GEM courses/Data/birthweight.csv")

str(df_birthweight)

# change smoke status, race to categorical 
df_birthweight$smoke = factor(df_birthweight$smoke, levels = c(0,1))
df_birthweight$race = factor(df_birthweight$race, levels = c("white", "black", "other"))

str(df_birthweight)

## optional part
#------------------------------------------------------------------------------#
library(ggplot2)
#install.packages("GGally")
library(GGally) 
ggpairs(df_birthweight, showStrips = T)


# end of optional part
#------------------------------------------------------------------------------#

## multiple regression model 
m_lm_multiple = lm(bwt ~ age+ lwt+smoke+race, data = df_birthweight)

# put results into a table
tbl_summary = 
  cbind(summary(m_lm_multiple)$coefficients[,c(1,4)], confint(m_lm_multiple))
tbl_summary

## prediction based on same lwt, same race(white) and different age and smoke status

new_X = 
  expand.grid(age = 14:45, 
              lwt = mean(df_birthweight$lwt),
              smoke = as.factor(c(0,1)),
              race = as.factor(c("white", "black", "other")))
new_X$predicted_bwt = predict(m_lm_multiple, new_X)
new_X$group = paste0(new_X$race, "-", new_X$smoke)


g_pred = 
  ggplot(data = new_X, aes(x = age, y = predicted_bwt, color = group)) + 
  geom_point(aes(shape = group)) + 
  geom_line(aes(linetype = group)) +
  labs(title = "baby weight prediction",
       subtitle = "based on same lwt = mean(lwt) = 129.8148") +
  scale_colour_brewer(palette = "Set1") +
  theme_bw()
g_pred
