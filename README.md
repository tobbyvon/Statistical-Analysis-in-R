# Statistical-Analysis-in-R
This repository contains R code for analyzing mortality rate data in four selected countries across different regions, including South Asian countries, Sub-Saharan African countries, and high-income European countries. The analysis involves data preprocessing, statistical analysis, data visualization Correlation analysis, hypothesis testing, linear regression, and time series analysis.

Mortality Rate Analysis

## Getting Started

### Prerequisites

Make sure you have R installed on your machine. You can download it from [CRAN](https://cran.r-project.org/).

### Installing Packages

Install the required R packages by running the following commands in your R environment:

```R
install.packages("datarium")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("rcompanion")
install.packages("psych")
install.packages("dplyr")
install.packages("tidyr")
install.packages("GPArotation")
install.packages("conflicted")
Load the installed packages:

R
Copy code
library(conflicted)
library(datarium)
library(tidyverse)
library(corrplot)
library(rcompanion)
library(psych)
library(dplyr)
library(GPArotation)
library(tidyr)

#loading my Dataset
Mortality_Rate <- read.csv("Mortality Rate_Data.csv", header= TRUE)


names(Mortality_Rate)

head(Mortality_Rate)

tail(Mortality_Rate)

str(Mortality_Rate)

#checking for missing value
sum(is.na(Mortality_Rate))

#changed the ".." in my dataset to NA as R could not read ".."
Mortality_Rate[Mortality_Rate ==".."]<-NA

sum(is.na(Mortality_Rate))


#renaming my indicators as they are very long
# New short names
short_names <- c("Country", "Year", "LE", "TM", "CVD", "PM", "FM", "MM", "ID", "SR", "CDR", "MAR_M", "GHE", "HE_GDP", "AP", "MDR")

# Assign short names to the columns
colnames(Mortality_Rate) <- short_names

# View the updated data frame
head(Mortality_Rate)






#Handling Numeric Columns
numeric_cols <- c("GHE", "HE_GDP", "AP", "MDR")
Mortality_Rate[numeric_cols] <- lapply(Mortality_Rate[numeric_cols], as.numeric)

# Define a function to impute missing values with the mean
impute_missing <- function(x) {
  replace_na(x, mean(x, na.rm = TRUE))
}

# Apply the impute_missing function to each numeric column
Mortality_Rate[numeric_cols] <- lapply(Mortality_Rate[numeric_cols], impute_missing)

sum(is.na(Mortality_Rate))




#statistical analysis across all the countries
summary(Mortality_Rate)

describe(Mortality_Rate)

# correlation between Two indicators
plot(Mortality_Rate$FM, Mortality_Rate$MM) #Female mortality & Male Mortality
plot(Mortality_Rate$FM,Mortality_Rate$HE_GDP) #Female mortality & health expenditure
plot(Mortality_Rate$MM,Mortality_Rate$HE_GDP) #Male mortality & health expenditure
plot(Mortality_Rate$MDR,Mortality_Rate$HE_GDP) #maternal death risk & health expenditure
plot(Mortality_Rate$GHE,Mortality_Rate$HE_GDP) #Gov_Health_Expenditure & health expenditure
plot(Mortality_Rate$MDR,Mortality_Rate$MAR_M) #maternal death risk & Maternal Mortality
plot(Mortality_Rate$MDR,Mortality_Rate$AP) #maternal death risk & aneamia prevelence
plot(Mortality_Rate$MM,Mortality_Rate$AP)  #Male mortality & aneamia prevelence
plot(Mortality_Rate$SR,Mortality_Rate$CDR) #suicide rate & crude death rate



# correlation between multiple variables in mortality_rate table

#Dropping the country and year variable
New_Mortality_Rate <- Mortality_Rate[ -c(1:2) ]
head(New_Mortality_Rate, 5)

# converting categorical variable to numerical
new.data <- char2numeric(New_Mortality_Rate)  #this makes all character numeric

#correlation
round(cor(new.data), digits = 2)

#spearman correlation
correlation_matrix = round(cor(new.data, method = "spearman"), digit=2)
correlation_matrix


#Plot
# Enlarge the margin
par(mar = c(0, 6, 4, 2), pin = c(5, 3))

# Create the correlation plot
corrplot(correlation_matrix, method = "number", type = "upper")




#Satistical Analysis based on region

# Extract south Asian countries data set  
southAsia_Countries  <- Mortality_Rate %>%
  dplyr::filter(Country %in% c('Afghanistan', 'Bangladesh', 'Nepal', 'India'))
southAsia_Countries


# Extract sub Saharan African countries data set  
SubSahara_Countries  <- Mortality_Rate %>%
  dplyr::filter(Country %in% c('Nigeria', 'Burundi', 'South Sudan', 'Cote d\'Ivoire'))
SubSahara_Countries


# Extract high income countries in Europe data set  
European_Countries  <- Mortality_Rate %>%
  dplyr::filter(Country %in% c('United States', 'United Kingdom', 'Canada', 'Germany'))
European_Countries



#basic statistics of each region
describe(southAsia_Countries)
describe(SubSahara_Countries)
describe(European_Countries)


#DATA VISUALIZATION


#Checking for Outliers
# Boxplot for selected variables in southAsia_Countries  
boxplot(southAsia_Countries[, c(3:16)], col = rainbow(14), main = "south Asian Countries")

# Boxplot for selected variables in SubSahara_Countries
boxplot(SubSahara_Countries[, c(3:16)], col = rainbow(14), main = "Sub-Saharan African Countries")

# Boxplot for selected variables in European_Countries
boxplot(European_Countries[, c(3:16)], col = rainbow(14), main = " High Income European Countries")


#EDA
# Life Expectancy in southAsia_Countries
library(ggplot2)

# Melt the data for ggplot
melted_data <- southAsia_Countries %>%
  dplyr::select(Country, LE, Year)  # Select the columns needed for plotting

# Plot the bar chart with a single color (blue)
ggplot(melted_data, aes(x = Country, y = LE /10 )) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Life Expectancy in South Asian Countries",
       x = "Country", y = "Life Expectancy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Life Expectancy in SubSahara_Countries
# Melt the data for ggplot
melted_data <- SubSahara_Countries %>%
  dplyr::select(Country, LE, Year)  # Select the columns needed for plotting

# Plot the bar chart with a single color (blue)
ggplot(melted_data, aes(x = Country, y = LE)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Life Expectancy in Sub-Sahara African Countries",
       x = "Country", y = "Life Expectancy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Life Expectancy in  High income European Countries
# Melt the data for ggplot
melted_data <- European_Countries %>%
  dplyr::select(Country, LE, Year)  # Select the columns needed for plotting

# Plot the bar chart with a single color (blue)
ggplot(melted_data, aes(x = Country, y = LE/ 10 )) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Life Expectancy in European Countries",
       x = "Country", y = "Life Expectancy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Scatterplot matrix for southAsia_Countries variables
pairs(southAsia_Countries[, c(3,7,8,11)], col = southAsia_Countries$Year)

pairs(southAsia_Countries[, c(7,12,15,16)], col = southAsia_Countries$Year)

pairs(southAsia_Countries[, c(4,5,6,10)], col = southAsia_Countries$Year)



# Scatterplot matrix for SubSahara_Countries variables
pairs(SubSahara_Countries[, c(3,7,8,11)], col = SubSahara_Countries$Year)

pairs(SubSahara_Countries[, c(7,12,15,16)], col = SubSahara_Countries$Year)

pairs(SubSahara_Countries[, c(4,5,6,10)], col = SubSahara_Countries$Year)



# Scatterplot matrix for high income European Countries variables
pairs(European_Countries[, c(3,7,8,11)], col = European_Countries$Year)

pairs(European_Countries[, c(7,12,15,16)], col = European_Countries$Year)

pairs(European_Countries[, c(4,5,6,10)], col = European_Countries$Year)



#bar chart Explration Maternity Mortality in South Asia
install.packages("reshape2")
library(reshape2)
# Melt the data for ggplot
melted_data <- reshape2::melt(southAsia_Countries, 
                              id.vars = c("Country", "Year", "MAR_M"))

# Plot the 
#bar chart
ggplot(melted_data, aes(x = Year, y = MAR_M /10)) +
  geom_bar(stat = "identity", fill = "red") +
  facet_wrap(~Country, scales = "free_y") +
  labs(title = "Maternity Mortality in South Asia",
       x = "Year", y = "MAR_M") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#bar chart Exploration Maternity Mortality in Sub-Sahara Africa
melted_data <- reshape2::melt(SubSahara_Countries, 
                              id.vars = c("Country", "Year", "MAR_M"))

# Plot the bar chart
ggplot(melted_data, aes(x = Year, y = MAR_M /10)) +
  geom_bar(stat = "identity", fill = "red") +
  facet_wrap(~Country, scales = "free_y") +
  labs(title = "Maternity Mortality in Sub-Sahara Africa",
       x = "Year", y = "MAR_M") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#bar chart Explration Maternity Mortality in European countries
melted_data <- reshape2::melt(European_Countries, 
                              id.vars = c("Country", "Year", "MAR_M"))

# Plot the bar chart
ggplot(melted_data, aes(x = Year, y = MAR_M /10)) +
  geom_bar(stat = "identity", fill = "red") +
  facet_wrap(~Country, scales = "free_y") +
  labs(title = "Maternity Mortality in High Income European Countries",
       x = "Year", y = "MAR_M") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#bar chart Exploration Female Mortality in European countries
melted_data <- reshape2::melt(European_Countries, 
                              id.vars = c("Country", "Year", "FM"))

# Plot the bar chart
ggplot(melted_data, aes(x = Year, y = FM /10)) +
  geom_bar(stat = "identity", fill = "red") +
  facet_wrap(~Country, scales = "free_y") +
  labs(title = "Female Mortality in High Income European Countries",
       x = "Year", y = "FM") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#bar chart Exploration Male Mortality in European countries
melted_data <- reshape2::melt(European_Countries, 
                              id.vars = c("Country", "Year", "MM"))

# Plot the bar chart
ggplot(melted_data, aes(x = Year, y = MM /10)) +
  geom_bar(stat = "identity", fill = "red") +
  facet_wrap(~Country, scales = "free_y") +
  labs(title = "Male Mortality in High Income European Countries",
       x = "Year", y = "MM") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#bar chart Exploration Female Mortality in sub-sahara countries
melted_data <- reshape2::melt(SubSahara_Countries, 
                              id.vars = c("Country", "Year", "FM"))

# Plot the bar chart
ggplot(melted_data, aes(x = Year, y = FM /10)) +
  geom_bar(stat = "identity", fill = "red") +
  facet_wrap(~Country, scales = "free_y") +
  labs(title = "Female Mortality in Sub-Sahara African Countries",
       x = "Year", y = "FM") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#bar chart Exploration Male Mortality in sub-sahara countries
melted_data <- reshape2::melt(SubSahara_Countries, 
                              id.vars = c("Country", "Year", "MM"))

# Plot the bar chart
ggplot(melted_data, aes(x = Year, y = MM /10)) +
  geom_bar(stat = "identity", fill = "red") +
  facet_wrap(~Country, scales = "free_y") +
  labs(title = "Male Mortality in Sub-Sahara African Countries",
       x = "Year", y = "MM") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#bar chart Exploration Male Mortality in south Asian countries
melted_data <- reshape2::melt(southAsia_Countries, 
                              id.vars = c("Country", "Year", "MM"))

# Plot the bar chart
ggplot(melted_data, aes(x = Year, y = MM /10)) +
  geom_bar(stat = "identity", fill = "red") +
  facet_wrap(~Country, scales = "free_y") +
  labs(title = "Male Mortality in South Asia Countries",
       x = "Year", y = "MM") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#bar chart Exploration Male Mortality in south Asian countries
melted_data <- reshape2::melt(southAsia_Countries, 
                              id.vars = c("Country", "Year", "FM"))

# Plot the bar chart
ggplot(melted_data, aes(x = Year, y = FM /10)) +
  geom_bar(stat = "identity", fill = "red") +
  facet_wrap(~Country, scales = "free_y") +
  labs(title = "Female Mortality in South Asia Countries",
       x = "Year", y = "FM") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






#correlation Analysis of multiple continous variable in each region

#South Asian Countries
#Dropping the country and year variable
NEWsouthAsia_Countries <- southAsia_Countries[ -c(1:2) ]
head(NEWsouthAsia_Countries, 5)

NEWsouthAsia_Countries <- char2numeric(New_Mortality_Rate)  #this makes all character numeric

#spearman correlation
correlation_matrix <- round(cor(NEWsouthAsia_Countries, method = "spearman"), digit=2)
correlation_matrix

# Enlarge the margin
par(mar = c(0, 1, 2, 0))

# Create the correlation plot
corrplot(correlation_matrix, method = "number", type = "upper")


#Sub Sahara African Countries
#Dropping the country and year variable
NEWSubSahara_Countries <- SubSahara_Countries[ -c(1:2) ]
head(NEWSubSahara_Countries, 5)

#correlation

#spearman correlation
correlation_matrix <- round(cor(NEWSubSahara_Countries, method = "spearman"), digit=2)
correlation_matrix

# Enlarge the margin
par(mar = c(0, 2, 2, 1))

# Create the correlation plot
corrplot(correlation_matrix, method = "number", type = "upper")


#correlation analysis of high income European Countries
#Dropping the country and year variable
NEWEuropean_Countries <- European_Countries[ -c(1:2) ]
head(NEWEuropean_Countries, 5)

#correlation

#spearman correlation
correlation_matrix <- round(cor(NEWEuropean_Countries, method = "spearman"), digit=2)
correlation_matrix

# Enlarge the margin
par(mar = c(0, 2, 2, 0))

# Create the correlation plot
corrplot(correlation_matrix, method = "number", type = "upper")


#Hypothesis testing

#more library installations



install.packages("ggplot2")


library(ggplot2)
library(qqplotr)


#Assessing the Normality of Data

#Q-Q plot (Visual Method)
plot.new()
# Visually confirm if the southAsia_Country plot appears to be on a normal curve
ggplot(mapping = aes(sample=mice2$before)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("southAsia_Countries") + ylab("Sample")

# Visually confirm if the SubSahara_Countries plot appears to be on a normal curve
ggplot(mapping = aes(sample=mice2$before)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("SubSahara_Countries") + ylab("Sample")

# Visually confirm if the European_Countries plot appears to be on a normal curve
ggplot(mapping = aes(sample=mice2$before)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("European_Countries") + ylab("Sample")




# Shapiro-Wilk Test for Normality Across indicators in south Asian Countries
shapiro_results <- sapply(southAsia_Countries[, c(3:16)], shapiro.test)
shapiro_results

# Shapiro-Wilk Test for Normality Across indicators in Sub-Sahara African Countries
shapiro_results <- sapply(SubSahara_Countries[, c(3:16)], shapiro.test)
shapiro_results

# Shapiro-Wilk Test for Normality Across indicators in high income European Countries
shapiro_results <- sapply(European_Countries[, c(3:16)], shapiro.test)
shapiro_results

#test if there is a significant difference in the Maternal_Mortality variable between the countries in the south Asian Countries

## Perform ANOVA
anova_result <- aov(MM ~ Country, data = southAsia_Countries)

# Summary of ANOVA
summary(anova_result)



#test if there is a significant difference in the Maternal_Mortality variable between the countries in the Sub-Sahara African Countries

## Perform ANOVA
anova_result <- aov(MM ~ Country, data = SubSahara_Countries)

# Summary of ANOVA
summary(anova_result)



#test if there is a significant difference in the Maternal_Mortality variable between the countries in the high income European Countries

## Perform ANOVA
anova_result <- aov(MM ~ Country, data = European_Countries)

# Summary of ANOVA
summary(anova_result)



# test to see if there is a significant difference in the Maternal_Mortality variable
# between the countries in the southAsia_Countries dataset and the countries in the 
# European_Countries dataset
# T-test
t_test_result <- t.test(southAsia_Countries$MM, European_Countries$MM)

# Summary of t-test
t_test_result



# test to see if there is a significant difference in the Maternal_Mortality variable
# between the countries in the Sub-Sahara African Countries data set and the countries in the 
# European_Countries dataset
# T-test
Africa_t_test_result <- t.test(SubSahara_Countries$MM, European_Countries$MM)

# Summary of t-test
t_test_result


# test to see if there is a significant difference in the Life Expectancy(LE) variable
# between the countries in the south-Asian Countries data set and the countries in the 
# European_Countries dataset
# T-test
LE_test_result <- t.test(southAsia_Countries$LE, European_Countries$LE)

# Summary of t-test
t_test_result


# test to see if there is a significant difference in the Life Expectancy(LE) variable
# between the countries in the Sub-Sahara African Countries data set and the countries in the 
# European_Countries dataset
# T-test
t_test_result <- t.test(SubSahara_Countries$LE, European_Countries$LE)

# Summary of t-test
t_test_result


# test to see if there is a significant difference in the Life Expectancy(Crude_Death_Rate) variable
# between the countries in the southAsia_Countries data set and the countries in the 
# European_Countries dataset
# T-test
t_test_result <- t.test(southAsia_Countries$CDR, European_Countries$CDR)

# Summary of t-test
t_test_result


# test to see if there is a significant difference in the Life Expectancy(LE) variable
# between the countries in the Sub-Sahara African Countries data set and the countries in the 
# southAsia_Countries dataset
# T-test
t_test_result <- t.test(SubSahara_Countries$LE, southAsia_Countries$LE)

# Summary of t-test
t_test_result



#LINEAR REGRESSION
# linear regression analysis on each region exploring the factors 
# influencing male an female mortality and understanding how different
# variables contribute to it. 


corrplot(cor(NEWsouthAsia_Countries))



#South Asian Countires
# multiple linear regression
#Female
model <- lm(FM ~ TM + PM  + MM  + AP + MAR_M +  MDR, data = southAsia_Countries)

# Summary of the regression model
summary(model)


# Male
model <- lm(MM ~ TM + PM  + MAR_M  +  AP + FM + MDR, data = southAsia_Countries)

# Summary of the regression model
summary(model)



#Sub-Sahara African Countires
# multiple linear regression
#Female
model <- lm(FM ~ TM + PM + MAR_M  + AP  + MM +  MDR, data = SubSahara_Countries)

# Summary of the regression model
summary(model)


# Male
model <- lm(MM ~ TM + PM  + MAR_M  +  AP + FM  + MDR, data = SubSahara_Countries)

# Summary of the regression model
summary(model)


#High income European Countries
# multiple linear regression
#Female
model <- lm(FM ~ TM + PM  + MAR_M  + AP  + MM +  MDR, data = European_Countries)

# Summary of the regression model
summary(model)


# Male
model <- lm(MM ~ TM + PM  + MAR_M  +  AP + FM  + MDR, data = European_Countries)

# Summary of the regression model
summary(model)



#LINEAR REGRESSION
# linear regression analysis on each region exploring the factors 
# influencing Maternal Mortality rate and understanding how different
# variables contribute to it. 

#Maternity mortality rate in Sub-Sahara African Countries
# Using multiple linear regression
model <- lm(MM ~ TM + PM  + FM +  MM + CDR + AP + MDR, data = SubSahara_Countries)

# Summary of the regression model
summary(model)




#Maternity mortality rate in South Asian Countries
# Using multiple linear regression
model <- lm(MAR_M ~ TM + PM  + FM +  MM + CDR + AP + MDR, data = southAsia_Countries)

# Summary of the regression model
summary(model)



#Maternity mortality rate in high income European Countries
# Using multiple linear regression
model <- lm(MAR_M ~ TM + PM  + FM +  MM + CDR + AP + MDR, data = European_Countries)

# Summary of the regression model
summary(model)



#LINEAR REGRESSION
# linear regression analysis on each region exploring
#Government Health Expenditure as a factor 
# influencing Life Expectancy.

#Life Expectancy in Sub-Sahara African Countries
# Simple linear regression
model <- lm(LE ~ GHE, data = SubSahara_Countries)

# Summary of the regression model
summary(model)


#Life Expectancy in South Asia Countries
# Simple linear regression
model <- lm(LE ~ GHE, data = southAsia_Countries)

# Summary of the regression model
summary(model)


#Life Expectancy in high income European Countries
# Simple linear regression
model <- lm(LE ~ GHE, data = European_Countries)

# Summary of the regression model
summary(model)





#Time Series Analysis
#Performing a time series analysis on the Maternity mortality 
# in the three regions over the years using the simple exponential smoothing model
# Install and load required packages
#install.packages("forecast")
#install.packages("zoo")
library(forecast)
library(zoo)
library(ggplot2)



#South Asia
# Assuming your data frame is named 'your_data'
ggplot(southAsia_Countries, aes(x = Year, y = MAR_M, group = Country, color = Country)) +
  geom_line() +
  labs(title = "Maternal Mortality Over Time by Country")

summary(southAsia_Countries$Maternal_Mortality)

southAsia_Countries$Year <- as.Date(paste0(southAsia_Countries$Year, "-01-01"))
ts_data <- ts(southAsia_Countries$MAR_M, 
              start = min(southAsia_Countries$Year), 
              frequency = 12) # Assuming monthly data because my dataset is small
plot(ts_data)
decomposed_ts <- decompose(ts_data)
decomposed_ts$seasonal
plot(decomposed_ts)

# Explore seasonality using seasonal subseries plot
monthplot(ts_data)

# Perform simple exponential smoothing (SES)
ses_model <- HoltWinters(ts_data, beta = FALSE, gamma = FALSE)

# Print the model summary
print(ses_model)
ses_model$fitted
plot(ses_model)
#measure of accuracy
ses_model$SSE

#Make a forecast of rainfall for the years (8 more years) using forecast()
ses_model2 <- forecast(ses_model, h=8)
ses_model2
plot(ses_model2)

#calculate a correlogram of the in-sample forecast errors 
acf(ses_model2$residuals, lag.max=20 , na.action = na.pass)

# “Box.test() to check for a significant evidence for non-zero correlations at lags 1-20
Box.test(ses_model2$residuals, lag=20, type="Ljung-Box")

#check to see if forecast errors are normally distributed with mean zero and constant variance
plot.ts(ses_model2$residuals)


plotForecastErrors <- function(forecasterrors)
  {
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

ses_model2$residuals <-
  ses_model2$residuals[!is.na(ses_model2$residuals)]
plotForecastErrors(ses_model2$residuals)




#Sub-Sahara Africa
ggplot(SubSahara_Countries, aes(x = Year, y = MAR_M, group = Country, color = Country)) +
  geom_line() +
  labs(title = "Maternal Mortality Over Time by Country")

summary(SubSahara_Countries$Maternal_Mortality)

SubSahara_Countries$Year <- as.Date(paste0(SubSahara_Countries$Year, "-01-01"))
ts_data <- ts(SubSahara_Countries$MAR_M, 
              start = min(SubSahara_Countries$Year), 
              frequency = 12) # Assuming monthly data
plot(ts_data)
decomposed_ts <- decompose(ts_data)
decomposed_ts$seasonal
plot(decomposed_ts)

# Explore seasonality using seasonal subseries plot
monthplot(ts_data)

# Perform simple exponential smoothing (SES)
ses_model <- HoltWinters(ts_data, beta = FALSE, gamma = FALSE)

# Print the model summary
print(ses_model)
ses_model$fitted
plot(ses_model)
#measure of accuracy
ses_model$SSE

#Make a forecast of rainfall for the years (8 more years) using forecast()
ses_model2 <- forecast(ses_model, h=8)
ses_model2
plot(ses_model2)

#calculate a correlogram of the in-sample forecast errors 
acf(ses_model2$residuals, lag.max=20 , na.action = na.pass)

# “Box.test() to check for a significant evidence for non-zero correlations at lags 1-20
Box.test(ses_model2$residuals, lag=20, type="Ljung-Box")

#check to see if forecast errors are normally distributed with mean zero and constant variance
plot.ts(ses_model2$residuals)


plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

ses_model2$residuals <-
  ses_model2$residuals[!is.na(ses_model2$residuals)]
plotForecastErrors(ses_model2$residuals)






#European Countries
ggplot(European_Countries, aes(x = Year, y = MAR_M, group = Country, color = Country)) +
  geom_line() +
  labs(title = "Maternal Mortality Over Time by Country")

summary(European_Countries$MAR_M)

European_Countries$Year <- as.Date(paste0(European_Countries$Year, "-01-01"))
ts_data <- ts(European_Countries$MAR_M, 
              start = min(European_Countries$Year), 
              frequency = 12) # Assuming monthly data
plot(ts_data)
decomposed_ts <- decompose(ts_data)
decomposed_ts$seasonal
plot(decomposed_ts)

# Explore seasonality using seasonal subseries plot
monthplot(ts_data)

# Perform simple exponential smoothing (SES)
ses_model <- HoltWinters(ts_data, beta = FALSE, gamma = FALSE)

# Print the model summary
print(ses_model)
ses_model$fitted
plot(ses_model)
#measure of accuracy
ses_model$SSE

#Make a forecast of rainfall for the years (8 more years) using forecast()
ses_model2 <- forecast(ses_model, h=8)
ses_model2
plot(ses_model2)

#calculate a correlogram of the in-sample forecast errors 
acf(ses_model2$residuals, lag.max=20 , na.action = na.pass)

# “Box.test() to check for a significant evidence for non-zero correlations at lags 1-20
Box.test(ses_model2$residuals, lag=20, type="Ljung-Box")

#check to see if forecast errors are normally distributed with mean zero and constant variance
plot.ts(ses_model2$residuals)


plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

ses_model2$residuals <-
  ses_model2$residuals[!is.na(ses_model2$residuals)]
plotForecastErrors(ses_model2$residuals)







#Time Series Analysis
#Performing a time series analysis on the Life Expectancy 
# in the three regions over the years using the ARIMA model

#southAsia_Countries
# visual plot of life Expectation in southAsia_Countries
ggplot(southAsia_Countries, aes(x = Year, y = LE, group = Country, color = Country)) +
  geom_line() +
  labs(title = "Maternal Mortality Over Time by Country")

#Convert southAsia_Countries Dataset to Time Series:
ts_data <- 
  ts(southAsia_Countries$LE, start = min(southAsia_Countries$Year),
     frequency = 12)

plot(ts_data)

#Decompose the time series into trend, seasonality, and remainder components:
decomposed_ts <- decompose(ts_data)
plot(decomposed_ts)

#Check if the time series is stationary:
adf_test <- ur.df(ts_data, type = "trend", lags = 10)
summary(adf_test)

#Examine the summary of the ARIMA model and check diagnostic
arima_model <- auto.arima(ts_data)
summary(arima_model)

# Generate forecasts using the ARIMA model
forecast_arima <- forecast(arima_model, h = 8)
plot(forecast_arima)

# Check the accuracy of the forecast and inspect the residuals
accuracy(forecast_arima)





#southAsia African Countries
# visual plot of life Expectation in southAsia_Countries
ggplot(SubSahara_Countries, aes(x = Year, y = LE, group = Country, color = Country)) +
  geom_line() +
  labs(title = "Maternal Mortality Over Time by Country")

#Convert southAsia_Countries Dataset to Time Series:
ts_data <- 
  ts(SubSahara_Countries$LE, start = min(SubSahara_Countries$Year),
     frequency = 12)

plot(ts_data)

#Decompose the time series into trend, seasonality, and remainder components:
decomposed_ts <- decompose(ts_data)
plot(decomposed_ts)

#Check if the time series is stationary:
adf_test <- ur.df(ts_data, type = "trend", lags = 10)
summary(adf_test)

#Examine the summary of the ARIMA model and check diagnostic
arima_model <- auto.arima(ts_data)
summary(arima_model)

# Generate forecasts using the ARIMA model
forecast_arima <- forecast(arima_model, h = 8)
plot(forecast_arima)

# Check the accuracy of the forecast and inspect the residuals
accuracy(forecast_arima)


#European Countries
# visual plot of life Expectation in SubSahara_Countries
ggplot(European_Countries, aes(x = Year, y = LE, group = Country, color = Country)) +
  geom_line() +
  labs(title = "Maternal Mortality Over Time by Country")

#Convert European_Countries Dataset to Time Series:
ts_data <- 
  ts(European_Countries$LE, start = min(European_Countries$Year),
     frequency = 12)

plot(ts_data)

#Decompose the time series into trend, seasonality, and remainder components:
decomposed_ts <- decompose(ts_data)
plot(decomposed_ts)

#Check if the time series is stationary:
adf_test <- ur.df(ts_data, type = "trend", lags = 10)
summary(adf_test)

#Examine the summary of the ARIMA model and check diagnostic
arima_model <- auto.arima(ts_data)
summary(arima_model)

# Generate forecasts using the ARIMA model
forecast_arima <- forecast(arima_model, h = 8)
plot(forecast_arima)

# Check the accuracy of the forecast and inspect the residuals
accuracy(forecast_arima)





#Sub-Sahara African Countries
# visual plot of life Expectation in European_Countries
ggplot(SubSahara_Countries, aes(x = Year, y = LE, group = Country, color = Country)) +
  geom_line() +
  labs(title = "Maternal Mortality Over Time by Country")

#Convert SubSahara_Countries Dataset to Time Series:
ts_data <- 
  ts(SubSahara_Countries$LE, start = min(SubSahara_Countries$Year),
     frequency = 12)

plot(ts_data)

#Decompose the time series into trend, seasonality, and remainder components:
decomposed_ts <- decompose(ts_data)
plot(decomposed_ts)

#Check if the time series is stationary:
adf_test <- ur.df(ts_data, type = "trend", lags = 10)
summary(adf_test)

#Examine the summary of the ARIMA model and check diagnostic
arima_model <- auto.arima(ts_data)
summary(arima_model)

# Generate forecasts using the ARIMA model
forecast_arima <- forecast(arima_model, h = 8)
plot(forecast_arima)

# Check the accuracy of the forecast and inspect the residuals
accuracy(forecast_arima)

Authors
[Oluwatobiloba Vaughan]
