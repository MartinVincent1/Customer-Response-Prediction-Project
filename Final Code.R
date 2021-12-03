#Final Project

rm(list = ls())
setwd("C:/Users/mvinc/OneDrive/Assignments Fall 2021/Predictive Analytics/Final Project")

library(tidyverse)
library(stats)
library(lubridate)
library(stringr)

#Import database
mkt_campaign <- read.csv("marketing_campaign.csv", sep ='\t')


#Creating 'age' variable
mkt_campaign$Campaign_Year <- str_sub(mkt_campaign$Dt_Customer, -4) #extracting year part from Dt_Customer
mkt_campaign$Campaign_Year = as.integer(mkt_campaign$Campaign_Year) #converting to integer

mkt_campaign$Age <- mkt_campaign$Campaign_Year - mkt_campaign$Year_Birth #calculating age


#Logistic Regression, selecting them for business purposes
logit_mktcmp <- glm(Response ~ Recency + 
                          MntWines + MntFruits + MntMeatProducts + MntFishProducts + MntSweetProducts + 
                          MntGoldProds + NumDealsPurchases + NumWebPurchases + NumStorePurchases + 
                          NumWebVisitsMonth + AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5 + 
                          AcceptedCmp1 + AcceptedCmp2, data = mkt_campaign, family = "binomial")
summary(logit_mktcmp)

#Prediction
mkt_campaign$accept_prob<- predict(logit_mktcmp, newdata = mkt_campaign, type = "response")
mkt_campaign$accept_prob


###profit analysis - Martin
#Find total spend divided by number of visits for all customers to get avg spent per visit
mkt_campaign$total_visits <- mkt_campaign$NumCatalogPurchases + mkt_campaign$NumDealsPurchases + 
  mkt_campaign$NumStorePurchases + mkt_campaign$NumWebPurchases

mkt_campaign$total_spent <- mkt_campaign$MntWines + mkt_campaign$MntFruits + mkt_campaign$MntMeatProducts +
  mkt_campaign$MntFishProducts + mkt_campaign$MntSweetProducts + mkt_campaign$MntGoldProds

mkt_campaign_filtered <- filter(mkt_campaign, total_visits!=0)

mkt_campaign_filtered$spent_per_visit<- mkt_campaign_filtered$total_spent/mkt_campaign_filtered$total_visits

mean(mkt_campaign_filtered$spent_per_visit)

#avg profit assuming a 4% profit margin
profit_per_person <- mean(mkt_campaign_filtered$spent_per_visit) * .04

#assuming cost per letter sent to a person is $0.46, then breakeven response rate is:
cutoff <- .46/profit_per_person

#https://www.uspsdelivers.com/direct-mail-cost-calculator/
#https://thegrocerystoreguy.com/what-is-the-profit-margin-for-grocery-stores/

#filter out customers who have accept probability greater than breakeven rate
campaign_customers <- filter(mkt_campaign, mkt_campaign$accept_prob > cutoff)
campaign_customers <- campaign_customers[,c("ID", "accept_prob")]

####visuals
library(ggplot2)
ggplot(data = mkt_campaign_filtered) +
  geom_histogram(aes(x=accept_prob), bins = 10, color = "black", fill = "lightblue") +
  ggtitle("Predicted Acceptance Probabilities") +
  ylab("Frequency") + xlab("Acceptance Probability")
