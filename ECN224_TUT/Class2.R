# Libraries and Init ################################################################
library("tidyverse")
library("dplyr")
library("ggplot2")
library(ggthemes)
library(viridis)
caschool <- read_csv("ECN224_TUT/caschool.csv")

# Tasks ===========================================================================
# 2. (R) In question 1 of exercise set 1, only the instructions from the base R 
# and base packages were used. However, for many computational tasks convenient 
# add-on packages were developed. For econometric analysis, two particularly 
# useful packages are dplyr for data manipulation and ggplot2 for figure making.
# 
# Dataset caschool.csv contains data on test performance, school characteristics 
# and student demographic backgrounds. Using this dataset and R software, carry 
# out the following tasks.

# (a) Activate the dplyr package. Install it first if it had not been installed.
# Each package only needs to be installed once but it needs to be activated in each script.
# (b) Inspect the dataset using function glimpse().
# (c) Obtain summary statistics of variables in the dataset applying the 
# summary() function. What is the minimum and maximum test score ( testscr) in the
# sample, and what is the average test score? What about the county variable?
#   (d) Activate the ggplot2 package. Install it first if needed.
# (e) Make a scatterplot of test scores (testscr) against class size (str).
# Comment on your results.
# (f) Plot a histogram of test scores. 
# What does the histogram tell you about the test score data in the sample?
#   3. (R) Continue with the analysis of the caschool dataset carrying out the following tasks.
# (a) Estimate the OLS regression for 420 California school districts:
#   TestScore = β0 +β1STR+ui,
# i = 1,...,n.
# Report the regression estimates in the same form as in question 1 above, 
  # including standard errors, R2 and the standard error of regression.
# (b) Plot a scattergram of the data together with the fitted line.
# (c) Test the null hypothesis H0: β1 = 0 against the alternative hypothesis;
# H1: β1 ̸ = 0 at the 5% and 1 significance level.
# (d) Construct a 95% and 99% confidence interval for β1, the regression slope coefficient.
# (e) Calculate the p-value for the two-sided test of the null hypothesis;
# i.e.H0: β1 = 0. Do you reject the null hypothesis at the 5% level? At the 1% level?
# ==========================================================

## Question 2 ～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—

glimpse(caschool)
summary(caschool)
print(summary(caschool$testscr))
# Min.   1st Qu.  Median  Mean   3rd Qu.   Max. 
# 605.6   640.0   654.4   654.2   666.7   706.8 

"--------------------------------------"

scatterplot1 <- caschool %>%
  ggplot(mapping=aes(x=str,y=testscr)) +geom_point(size=0.9)+
  theme(plot.background = element_rect(fill='grey95'),
        panel.background = element_rect(linetype=3,linewidth=1,colour='grey15'))+
  labs(title ='Scatter Plot of California Test Scores & Classroom Density',subtitle='for ECN224 Class Work, 15/10/26',caption="* test score was averaged across students in each education facility's cohort")+
  xlab(label='Student-Teacher Ratio (STR)')+ 
  ylab(label='School SAT Test Score (SCR)')

scatterplot1

"--------------------------------------"

histogram1 <- caschool %>%
  ggplot() +
  geom_histogram(mapping=aes(x=testscr),alpha=0.8,fill='palevioletred',colour='palevioletred4',bins=35)+
  theme(plot.background = element_rect(fill='#F6F0D7'),
        panel.background = element_rect(fill='#F6ECC3',linetype=3,linewidth=1,colour='#501B02'))+
  labs(title ='Histogram of California Test Scores',subtitle='for ECN224 Class Work, 15/10/26',caption="* test score was averaged across students in each education facility's cohort")+
  xlab(label='School SAT Test Score (SCR)')+ 
  ylab(label='Number of Schools')

histogram1

## Question 3 ～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—～—
'Y = β0 + β1X1 + u'

"--------------------------------------"

attach(caschool)
β1 <- cov(testscr,str) / var(str)
β0 <- mean(testscr) - β1*mean(str)
scatterplot1 + geom_abline(slope=β1,intercept=β0,colour='red3') #manual OLS

"--------------------------------------"

scatterplot1 + geom_smooth() #uses LOESS, a different method
scatterplot1 + geom_smooth(method='lm',se=F)+ #must specify for OLS (linear model)
  annotate("label",label='SCR=698-2.28STR',x=24,y=652,colour='blue4',fontface='bold')

"--------------------------------------"


regr <- lm(testscr ~ str, data = caschool) #best way for statistics (HOMOSKEDASTICITY ASSUMED)


summary(regr) #from this, you can see all the necessary statistics to do a Hypothesis Test 
class(regr)
names(regr)
print(regr$call)


confint(regr, "str") #95% confidence interval
confint(regr, "str", level=0.99) #99% confidence interval"

"--------------------------------------"

pvalue <- summary(regr)$coefficients["str", 4] 

print(pvalue) #2.783306e
#p-value is extremely low (0.000278%) so accept for all Sig. lvl




