install.packages("ggplot2")
install.packages("dplyr")
install.packages("MASS")
install.packages("gridExtra")

# Library
library(ggplot2)
library(dplyr)
library(MASS)   
library(gridExtra)

#loading data
train_dockets <- read.csv("raw_data/train_dockets.csv",header=TRUE)
train_other <- read.csv("raw_data/train_other_motions.csv",header=TRUE)
train_terminating <- read.csv("raw_data/train_terminating_motions.csv",header=TRUE)
test_dockets <- read.csv("raw_data/test_dockets.csv",header=TRUE)
test_other <- read.csv("raw_data/test_other_motions.csv",header=TRUE)
test_terminating <- read.csv("raw_data/test_terminating_motions.csv",header=TRUE)
districts <- read.csv("raw_data/districts.csv",header=TRUE)
district_fips_code <- read.csv("raw_data/district_fips_code.csv",header=TRUE)

#preparing the district data
dockets_district <- train_dockets$district
dockets_district.frequency <- table(dockets_district)
df.dockets_district <- as.data.frame(table(dockets_district))

#manipulating data
df.dockets_district$population <- districts$census_2010_population
df.dockets_district$ratioTo100000 <- df.dockets_district$Freq/df.dockets_district$population * 100000

#labelling high and low
identifier <- median(df.dockets_district$ratioTo100000)
df.dockets_district$highOrLow <-
  ifelse(df.dockets_district$ratioTo100000 >= identifier, 1, 0)

#plotting
p1 <- ggplot(df.dockets_district, aes(x = dockets_district, y = Freq)) +
  geom_bar(stat = "identity")
p2 <- ggplot(df.dockets_district, aes(x = dockets_district, y = ratioTo100000)) +
  geom_bar(stat = "identity")
grid.arrange(p1, p2, nrow = 1)
