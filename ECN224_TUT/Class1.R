library(readr)
library(tidyverse)
library(ggplot2)

Bea <- read_csv("C:/Users/oxonn/OneDrive/Desktop/R files/ECN224_TUT/caschool.csv")
names(Bea)
?names()
?head()
head(Bea)
dim(Bea)
str(Bea)

qplot(computer, testscr, data=Bea)
