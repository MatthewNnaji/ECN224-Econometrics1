library(readr)
library(tidyverse)
library(ggplot2)

Bea <- read_csv("ECN224_TUT/caschool.csv")
names(Bea)
?names()
?head()
head(Bea)
dim(Bea)
str(Bea)
glimpse(Bea)

qplot(computer, testscr, data=Bea)

#========R Intro (from qm+)========================================================
# Step 1. Import the dataset
caschool <- read.csv("~/Dropbox (QMUL-SEF)/QMUL/ECN224-Econometrics-I/Datasets/caschool.csv", sep=";")

# Plot:
# str: student teacher ratio = enrl_tot/teachers
# testscr: avg test score = (read_scr+math_scr)/2

# Step 2. attach 
attach(Bea)
detach(Bea)

# Step 3. plot
plot(str, testscr)

# if we do not "attach()" the dataset we can use:
plot(caschool$str, caschool$testscr)

# Bonus: 

# ggplot from tidyverse
install.packages("ggplot2")
library(ggplot2)

ggplot(caschool, aes(x = str, y = testscr)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red")