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
         High.Numeric = gsub("\\$","", Stock_data$High),
         Low.Numeric = gsub("\\$","", Stock_data$Low)) %>% 
  mutate(Close_Last = as.numeric(Close.Last.Numeric),
         Open_Numeric = as.numeric(Open.Numeric),
         High_Numeric = as.numeric(High.Numeric),
         Low_Numeric  = as.numeric(Low.Numeric)) %>% 
  select(Company, Date, Volume, Close_Last:Low_Numeric)

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

Years <- c(2013, 2015, 2017, 2019, 2021, 2023)

Volatility <- ggplot(Historical_Volatility,
       mapping = aes(x = as.numeric(Year),
                     y = Volatility
                     )) +
  geom_point(color = "blue") +
  geom_line(color = "green") +
  facet_wrap( . ~Company) +
  ggtitle("Stock Price Volatility from 2013-2023", subtitle = "In dollars") +
  labs(x = "Year") +
  theme_economist_white()

Volatility + scale_x_continuous(breaks = scales::breaks_width(2))
levels(Stock_data$Company)


unique(Stock_data$Company)



skim(Stock_data_clean)

summarise(Stock_data)



