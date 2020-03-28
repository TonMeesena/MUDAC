install.packages("ggplot2")
install.packages("tidyverse")
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
  geom_bar(stat = "identity") + ggtitle("Districts' Frequencies") +
  xlab("Districts") + ylab("Frequency")
p2 <- ggplot(df.dockets_district, aes(x = dockets_district, y = ratioTo100000)) +
  geom_bar(stat = "identity") + 
  geom_col(fill = ifelse(df.dockets_district$highOrLow >= 1, "lightsalmon2", "gray39")) + 
  ggtitle("Districts' Ratio Over 100,000") + xlab("Districts") + ylab("Ratio Over 100,000")

grid.arrange(p1, p2, nrow = 1)


#dealing with demographoic
#preparing the FIPS data

train_dockets$highOrLow <- -1

for(i in 1:dim(df.dockets_district)[1]){
  for(j in 1:dim(train_dockets)[1]){
    if (train_dockets$district[j] == df.dockets_district$dockets_district[i]){
      train_dockets$highOrLow[j] <- df.dockets_district$highOrLow[i]
    }
  }
}

df.dockets_FIPS_HIGH %>% filter(train_dockets, highOrLow == 1)
df.dockets_FIPS_LOW <- filter(train_dockets, highOrLow == 0)
df.dockets_FIPS_HIGH <- select_(df.dockets_FIPS_HIGH, "district", "filers_county")
df.dockets_FIPS_LOW <- select_(df.dockets_FIPS_LOW, "district", "filers_county")

#dealing with high filing
high.frequency <- table(df.dockets_FIPS_HIGH$filers_county)
df.high_FIPS <- as.data.frame(high.frequency)

#dealing with low filing
low.frequency <- table(df.dockets_FIPS_LOW$filers_county)
df.low_FIPS <- as.data.frame(low.frequency)

#plotting
p3 <- ggplot(df.high_FIPS, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") + ggtitle("High Filing FIPS' Frequencies") +
  xlab("FIPS") + ylab("Frequency")
p4 <- ggplot(df.low_FIPS, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") + ggtitle("Low Filing FIPS' Frequencies") +
  xlab("FIPS") + ylab("Frequency")

grid.arrange(p3, p4, nrow = 1)

