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

#Creating new variables for analysis, Separating by month day and year will make 
#it easier to group and filter observations by year. 

Volatility_analysis <- Stock_data_clean %>% 
  separate(Date, into = c("Month/Day","Year"), sep = 6 )

unique(Stock_data$Company)

#Historical volatility of each company for each year. 

Historical_Volatility <- Volatility_analysis %>% 
  group_by(Company, Year) %>% 
  summarise(Volatility = sd(Close.Last.Numeric))

#Now that the data has been cleaned and analyzed we can show our findings using 
#ggplot

ggplot(Historical_Volatility,
       mapping = aes(x = Year,
                     y = Volatility),
       grouping = 1)+
  geom_point()+
  facet_wrap( . ~Company)+
  theme_economist_white()

