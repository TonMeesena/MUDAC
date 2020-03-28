install.packages("ggplot2")
install.packages("dplyr")
install.packages("MASS")

# Library
library(ggplot2)
library(dplyr)
library(MASS)   

train_dockets <- read.csv("raw_data/train_dockets.csv",header=TRUE)
train_other <- read.csv("raw_data/train_other_motions.csv",header=TRUE)
train_terminating <- read.csv("raw_data/train_terminating_motions.csv",header=TRUE)

test_dockets <- read.csv("raw_data/test_dockets.csv",header=TRUE)
test_other <- read.csv("raw_data/test_other_motions.csv",header=TRUE)
test_terminating <- read.csv("raw_data/test_terminating_motions.csv",header=TRUE)

dockets_district <- train_dockets$district
dockets_district.frequency <- table(dockets_district)
barplot(dockets_districts.frequency)

ggplot(train_dockets, aes(district)) + geom_histogram()
