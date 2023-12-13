#Loading in packages
library(tidyverse)
library(tidyselect)
library(skimr)
library(ggthemes)

#Loading in data 
Stock_data <- read.csv("/Users/jackkatz/Downloads/data.csv")

#Cleaning data
Stock_data_clean <- Stock_data %>% 
  mutate(Close.Last.Numeric = gsub("\\$","", Stock_data$Close.Last),
         Open.Numeric = gsub("\\$","", Stock_data$Open),
         Open.Numeric = gsub("\\$","", Stock_data$Open),
         High.Numeric = gsub("\\$","", Stock_data$High),
         Low.Numeric = gsub("\\$","", Stock_data$Low)) %>% 
  select(Company, Date, Volume, Close.Last.Numeric:Low.Numeric)

#Creating new variables for analysis

Stock_data_clean %>% 
  mutate(Lagged_Close = lag(Close.Last.Numeric),
         Change = as.numeric(Close.Last.Numeric) - as.numeric(Lagged_Close)) 
  




  


  
  
