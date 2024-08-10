#Week 3 Assignment - Data Science as a Field


#imports
library(tidyverse)
library(lubridate)
library(ggplot2)
#library(scales)
library(dplyr)

#NYPD Shooting Incident Data
shooting.data.url <- "https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD"

#read to a dataframe
shooting.data <- read_csv(shooting.data.url)

#show summary
#note that some columns have data that is not useful for visualization. 
#for example, the case ID or "incident_key" number does not need to be analyzed
#precint numbers might be useful, but as categorical data
summary(shooting.data)

#taking a look at the first few columns
head(shooting.data)

#visualizations
#suspect by age group
#we notice several apparent mistakes in the data, since "1028" is not a valid age
age.data.clean <- shooting.data[!is.na(shooting.data$PERP_AGE_GROUP), ]
#removing any manually added "null" values 
age.data.clean <- age.data.clean[(age.data.clean$PERP_AGE_GROUP != "(null)"),]
#removing suspcious ages
age.data.clean <- age.data.clean[!(age.data.clean$PERP_AGE_GROUP %in% c(940, 1020, 1028, 224)), ]
suspect.age <- table(age.data.clean$PERP_AGE_GROUP)
pie(suspect.age)
#suspect.age



#victim by race
#some are unknown, but I choose to leave those in for comparison
age.data.clean <- shooting.data[!is.na(shooting.data$VIC_RACE), ]

#removing any manually added "null" values 
age.data.clean <- age.data.clean[(age.data.clean$VIC_RACE != "UNKNOWN"),]
victim.race <- table(age.data.clean$VIC_RACE)

#bar plot showing the differences in victims
#this does not work for the labels
#barplot(victim.race, horiz = FALSE)
#trying again with smaller font:
barplot(victim.race, horiz = FALSE, cex.names = .53)
#victim.race



#model portion
## of shootings over time
#for data tidying, we only need to remove null dates. The fact that there was a shooting is shown by the existence of a row
#shooting.data.sorted <- arrange(shooting.data, shooting.data$OCCUR_DATE)
shooting.data$OCCUR_DATE <- as.Date(shooting.data$OCCUR_DATE, format = "%m/%d/%Y")
shooting.data.sorted <- shooting.data[order(shooting.data$OCCUR_DATE), ]

#pivoting to summarize by month. adding a column for "month" by formatting the date object
shooting.data.sorted$month <- format(shooting.data.sorted$OCCUR_DATE, "%Y-%m")
monthly.data <- table(shooting.data.sorted$month)

#monthly.data.sorted <- order(monthly.data)
#a barplot as a sanity check. 
#note there appears to be seasonality to the dates of shootings...
barplot(monthly.data)

#monthly.data
#this is what I want to model. 
#a more complex model would use time series analysis due to the clear seasonality, but I will start with a basic linear model over time. 

#linear model using lm()
#monthly.data
#in order to regress, I need to turn the months into sequential numbers 1, 2, ...n
X <- 1:length(monthly.data)

linear.model <- lm(monthly.data ~ X, data = monthly.data)
#summarizing the model
summary(linear.model)

plot(monthly.data, col = "black", main = "Shootings by Month, with Fitted Least Squares Line")
abline(linear.model, col = "blue")
