# This R Script creates a grand dataset by Mergining Datasets from Govt NIC related to suicides

library(dplyr)

## Here I have assumed all the datasets are in csv format and have been imported to the global environment

## Merging the Suicides in India Causes Dataset from 2001 to 2015

## Reading the datasets
ca0112 <- read.csv("datasets/RawDatasets/Cause 2001 2012.csv")
ca13 <- read.csv("datasets/RawDatasets/causes 2013.csv")
ca14 <- read.csv("datasets/RawDatasets/causes 2014.csv")
ca15 <- read.csv("datasets/RawDatasets/cause 2015.csv")

## Renaming Coloumn headings from Cause 2001 to 2012 dataset
ca0112 <- rename(ca0112, State=X0, Year=X1, Cause=X2, 
                 "Male upto 14 years"="X3", "Male 15-29 years"="X4", "Male 30-44 years"="X5", 
                 "Male 45-59 years"="X6", "Male 60 years and above"="X7", 
                 "Total Male"="X8", "Female upto 14 years"="X9", "Female 15-29 years"="X10", 
                 "Female 30-44 years"="X11", "Female 45-59 years"="X12", 
                 "Female 60 years and above"="X13", "Total Female"="X14", "Grand Total"="X15")
## Copying the same names to Cause 2013 dataset
ca13 <- setName(ca13,names(ca0112))

## Merging the two datasets
merged <- merge(ca0112, ca13, all=TRUE)

## Reshaping and Modifying the Cause 2014 dataset
for(i in 1:30200) {
  if(ca14$fields__label[i] == "")
  {
    ca14$fields__label[i] = ca14$fields__label[i-25]
  }
}

State <- ca14 %>% filter(fields__label == "State") %>% select("data")
Year <- ca14 %>% filter(fields__label == "Year") %>% select("data")
Cause <- ca14 %>% filter(fields__label == "Cause") %>% select("data")

df14 <- data.frame(State,Year)

## Subsetting
names <- ca14$fields__label[1:25]