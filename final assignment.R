#author: Stuart T
#date: 10 Aug 2024

library(tidyverse)
library(ggplot2)
library(dplyr)
library(wordcloud)

url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'

covid.data <- read_csv(url)

#renaming the country column so I can summarize
covid.data <- covid.data %>%
  rename(
    "country_region" = "Country/Region"
  )

summary(covid.data)
head(covid.data)

#pivot data - combine all regions of a single country
#pivot in R by:
covid.data.pivot <- covid.data %>%
    group_by(country_region) %>%
    summarize(
      across(where(is.numeric), sum, na.rm = TRUE) 
    )


#visualize - total cases by country
#cases.total <- rowSums(covid.data.pivot[ ,4:ncol(covid.data.pivot)])
#the above did not work - after some searching, this is a cumulative sum, meaning the total is simply the last column
cases.total <-  covid.data.pivot[ , 1146]
countries <- covid.data.pivot[ ,1 ]
cases_by_country <- data.frame(
 country = countries, 
 cases = cases.total
)
colnames(cases_by_country) <- c("country", "total")

#putting data in named vector form to pass to the word cloud
vectorized.data <- setNames(cases_by_country$total, cases_by_country$country)
wordcloud(words = names(vectorized.data), 
          freq = vectorized.data)


#head(covid.data)

#visualize - total cases over time - everyone
#from the word cloud we see what some of the larger countries are in terms of infections. Therefore, we select several of these and compare them over time
high.infection.countries <- covid.data.pivot[order(-covid.data.pivot[[ncol(covid.data.pivot)]]), ]

#clipping off the top 10
high.infection.countries <- high.infection.countries[1:10, ]
#plot(high.infection.countries)
#there is too much data to just plot(data) with, so I will select just a few hundred days of data, somewhere around the middle
matplot(t(high.infection.countries[, 400:1100]), type = "l", 
        main = "Cumulative Infections by Country", 
        xlab = "selected_days 2021 +",
        ylab = "country")
legend("topright", legend = high.infection.countries$country_region, col = 1:nrow(high.infection.countries), 
       lty = 1)
        

#model - silly one - length of country name affecting number of cases
name.length.effect <- data.frame(
  country = covid.data.pivot$country_region, 
  namelen = nchar(covid.data.pivot$country_region), 
  total = covid.data.pivot[, 1146]
)
colnames(name.length.effect) = c("country", "name_len", "total")


model <- lm(name.length.effect$total ~ name.length.effect$name_len, data = name.length.effect)
summary(model)

plot(name.length.effect$name_len, name.length.effect$total, 
     main = "Fitted Model",
     xlab = "length of the country name (in English)", 
     ylab = "total cases")
abline(model, col = "red")
#interestingly, there is a weak correlation when looking at the summary. The F statistic for the full model suggests that the length of 
#a country's name (in English) does actually affect the total case count. This is probably a spurious correlation, but is interesting nonetheless.
