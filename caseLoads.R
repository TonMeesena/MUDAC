install.packages("ggplot2")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("MASS")
install.packages("gridExtra")
install.packages("corrplot")

# Library
library(ggplot2)
library(dplyr)
library(MASS)   
library(gridExtra)
library(forcats)
library(corrplot)


#loading data
train_dockets <- read.csv("raw_data/train_dockets.csv",header=TRUE)
train_other <- read.csv("raw_data/train_other_motions.csv",header=TRUE)
train_terminating <- read.csv("raw_data/train_terminating_motions.csv",header=TRUE)
test_dockets <- read.csv("raw_data/test_dockets.csv",header=TRUE)
test_other <- read.csv("raw_data/test_other_motions.csv",header=TRUE)
test_terminating <- read.csv("raw_data/test_terminating_motions.csv",header=TRUE)
districts <- read.csv("raw_data/districts.csv",header=TRUE)
district_fips_code <- read.csv("raw_data/district_fips_code.csv",header=TRUE)
acs2015_tract <- read.csv("raw_data/us-census-demographic-data/acs2015_census_tract_data.csv",header=TRUE)
acs2015_county <- read.csv("raw_data/us-census-demographic-data/acs2015_county_data.csv",header=TRUE) 


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

df.dockets_FIPS_HIGH <- filter(train_dockets, highOrLow == 1)
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
p3 <- ggplot(df.high_FIPS, aes(x = fct_reorder(Var1, desc(Freq)), y = Freq)) +
  geom_bar(stat = "identity") + ggtitle("High Filing FIPS' Frequencies") +
  xlab("FIPS") + ylab("Frequency")
p4 <- ggplot(df.low_FIPS, aes(x = fct_reorder(Var1,desc(Freq)), y = Freq)) +
  geom_bar(stat = "identity") + ggtitle("Low Filing FIPS' Frequencies") +
  xlab("FIPS") + ylab("Frequency")

grid.arrange(p3, p4, nrow = 1)

#modify for demographic

df.full_FIPS <- select_(train_dockets, "filers_county", "highOrLow")
df.freq_tmp <- as.data.frame(table(df.full_FIPS))

#cleaning modified data
df.freq_full_FIPS <- data.frame(matrix(ncol=3, nrow=0))
name <- c("filers_county", "highOrLow", "Freq")
colnames(df.freq_full_FIPS) <- name

df.freq_tmp[2,1:3]
for (i in 1:dim(df.freq_tmp)[1]){
  if (df.freq_tmp$Freq[i] > 0) {
    df.freq_full_FIPS <- rbind(df.freq_full_FIPS, df.freq_tmp[i,1:3])
  }
}

#getting rid of repetitions
df.freq_full_FIPS <- df.freq_full_FIPS %>% distinct(filers_county, .keep_all = TRUE)

#joining data with demographic
acs2015_county <- acs2015_county %>% mutate(CensusId = as.factor(CensusId))
df.freq_full_FIPS <-left_join(df.freq_full_FIPS, acs2015_county, by=c("filers_county" = "CensusId"))

#Adding data
df.freq_full_FIPS$MaleFemaleRatio <- df.freq_full_FIPS$Men/df.freq_full_FIPS$Women 

#Visualizing data
pd1 <- ggplot(df.freq_full_FIPS, aes(x = MaleFemaleRatio, y = highOrLow)) +
  geom_point() + 
  ggtitle("Demo1") + xlab("Ratio of Male VS Female") + ylab("high or low")
pd1

pd2 <- ggplot(df.freq_full_FIPS, aes(x = Unemployment, y = highOrLow)) +
  geom_point() + 
  ggtitle("Demo1") + xlab("Unemployment Rate") + ylab("high or low")
pd2

pd3 <- ggplot(df.freq_full_FIPS, aes(x = Poverty, y = highOrLow)) +
  geom_bar(stat = "identity") + 
  ggtitle("Demo1") + xlab("Poverty") + ylab("high or low")
pd3


