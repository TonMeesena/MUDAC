install.packages("tydyverse")
install.packages("randomForest")
install.packages("corrplot")
install.packages("dplyr")
install.packages("Metrics")
install.packages("caret")
install.packages("plyr")
install.packages("ggplot2")
install.packages("psych")

library(psych)
library(ggplot2)
library(plyr)
library(tidyverse)
library(caret)
library(Metrics)
library(dplyr)
library(corrplot)
library(randomForest)

train_dockets<-read.csv("train_dockets.csv",header=TRUE)
train_termination_motions<-read.csv("train_terminating_motions.csv",header=TRUE)
train_other_motions<-read.csv("train_other_motions.csv",header=TRUE)
test_dockets<-read.csv("test_dockets.csv",header=TRUE)
test_termination_motions<-read.csv("test_terminating_motions.csv",header=TRUE)
test_other_motions<-read.csv("test_other_motions.csv",header=TRUE)







