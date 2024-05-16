# SUNY Geneseo, Fall 2023
# DANL 200: Introduction to Data Analytics
# Homework Assignment 3

# Instructor: Byeong-Hak Choe
# Student Name: Jack Katz
# Student ID: G00761527

# List of students whom you work with:
# 
#
# ... 

# List of tutors and/or TA whom you work with:
# 
#
# ... 

# List of AI tools (e.g. ChatGPT, Bard, etc.) you use:
# 
#
# ... 


# Loading R packages ------------------------------------------------------

library(tidyverse)
library(skimr)


# Question 1. Working with NYC Restaurant Inspection Data ---------------

# Read the data file, custdata_rev.csv, 
# as the data.frame object with the name, 
# restaurant:

restaurant <- read_csv(
  'https://bcdanl.github.io/data/DOHMH_NYC_Restaurant_Inspection.csv'
)

# Variable Description for restaurant ---------------------------------------

# The following describes the variables 
# in the `restaurant` data.frame.

# `CAMIS`:
#   -   This is an unique identifier for the entity (restaurant);
#   -   10-digit integer
# `DBA`:
#   -   This field represents the name (doing business as) of the entity (restaurant);
#   -   Public business name, may change at discretion of restaurant owner
# `BORO`:
#   -   Borough in which the entity (restaurant) is located.;
#   -   • `1` = MANHATTAN
#   -   • `2` = BRONX
#   -   • `3` = BROOKLYN
#   -   • `4` = QUEENS
#   -   • `5` = STATEN ISLAND
#   -   • `0` = Missing;
# `CUISINE DESCRIPTION`:
#   -   This field describes the entity (restaurant) cuisine.
# `ACTION`:
#   -   This field represents the actions that is associated with each restaurant inspection. ;
#   -   • Violations were cited in the following area(s).
#   -   • No violations were recorded at the time of this inspection.
#   -   • Establishment re-opened by DOHMH
#   -   • Establishment re-closed by DOHMH
#   -   • Establishment Closed by DOHMH.
#   -   • Violations were cited in the following area(s) and those requiring immediate action were addressed.
# `VIOLATION CODE`:
#   -   Violation code associated with an establishment (restaurant) inspection
# `VIOLATION DESCRIPTION`:
#   -   Violation description associated with an establishment (restaurant) inspection
# `CRITICAL FLAG`:
#   -   Indicator of critical violation;
#   -   • `Critical`
#   -   • `Not Critical`
#   -   • `Not Applicable`;
#   -   Critical violations are those most likely to contribute to food-borne illness
# `SCORE`:
#   -   Total score for a particular inspection;
# `GRADE`:
#   -   Grade associated with the inspection;
#   -   • `N` = Not Yet Graded
#   -   • `A` = Grade A
#   -   • `B` = Grade B
#   -   • `C` = Grade C
#   -   • `Z` = Grade Pending
#   -   • `P` = Grade Pending issued on re-opening following an initial inspection that resulted in a closure



# Q1a --------------------------------------------------------------
# What are the mean, standard deviation, first quartile, median, third quartile, 
# and maximum of `SCORE` for each `GRADE` of restaurants?


# Answer for Q1a

Q1a <- restaurant %>% 
  group_by(GRADE) %>% 
  summarise(SD_value = sd(SCORE),
            first_quartile = quantile(SCORE, 0.25),
            median_value = median(SCORE),
            third_quartile = quantile(SCORE, .75),
            MAX_value = max(SCORE))




# Q1b --------------------------------------------------------------
# How many restaurants with a `GRADE` of `A` are there in NYC?
# How much percentage of restaurants in NYC are a `GRADE` of `C`?

# Answer for Q1b

Q1b <- restaurant %>% 
  group_by(GRADE) %>% 
  summarise(count = n()) 

C_Grades <- restaurant %>% 
  filter(GRADE == "C") %>% 
  summarise(count = n())

(C_Grades/nrow(restaurant))*100

view(Q1b)





# Q1c --------------------------------------------------------------
# Provide both (1) ggplot code and (2) a simple comment to describe 
# how the distribution of `SCORE` varies by `GRADE` and `CRITICAL FLAG`.


# Answer for Q1c

ggplot(data = restaurant)+
  geom_boxplot(mapping = aes(x = GRADE, y = SCORE))

ggplot(data = restaurant)+
  geom_boxplot(mapping = aes(x = `CRITICAL FLAG`, y = SCORE))

#Restaurants with higher worse grades have higher scores. Similarly, restaurants
#with critical flags also have higher scores.



# Q1d --------------------------------------------------------------
# Provide both (1) ggplot code and (2) a simple comment to describe 
# how the proportion of `CRITICAL FLAG` varies by `GRADE` and `BORO`.


# Answer for Q1d

Q1d <- restaurant %>% 
  group_by(GRADE, BORO ,`CRITICAL FLAG`) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))
  

ggplot(data = Q1d)+
  geom_col(mapping = aes(x = BORO, 
                         y = prop, 
                         )+
  facet_grid(GRADE ~ BORO))

ggplot(data = restaurant)+
  geom_bar(mapping = aes(x = GRADE, 
                         y = stat(prop), 
                         group = `CRITICAL FLAG`,
                         fill = `CRITICAL FLAG`))






# Q1e --------------------------------------------------------------
# For the 10 most common `CUISINE DESCRIPTION` values, 
# find the `CUISINE DESCRIPTION` value 
# that has the highest proportion of `GRADE` `A`.

# Answer for Q1e

Q1e <- restaurant %>% 
  group_by(`CUISINE DESCRIPTION`) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(n = 10)

Grades <- restaurant %>% 
  filter(`CUISINE DESCRIPTION` %in% c(Q1e$`CUISINE DESCRIPTION`))

ggplot(data = Grades)+
  geom_bar(mapping = aes(x = `CUISINE DESCRIPTION`, 
                         y = stat(prop),
                         group = GRADE,
                         fill = GRADE))
Q1e
#Donut establishments have the highest proportion of grade A


  





# Q1f --------------------------------------------------------------
# Find the 3 most common names of restaurants (`DBA`) in each `BORO`.
# If the third most common `DBA` values are multiple, 
#   please include all the `DBA` values.
# Overall, which `DBA` value is most common in NYC?

# Answer for Q1f

Q1f <- restaurant %>% 
  group_by(`DBA`) %>% 
  summarise(count = n()) %>%   
  arrange(desc(count))
  


view(Q1f)
#The most common DBA value is Dunkin with 402 locations.




# Q1g --------------------------------------------------------------
# For all the `DBA` values that appear in the result of Q1f, 
# find the `DBA` value that is most likely to commit critical violation.

# Answer for Q1g

Q1g <- restaurant %>% 
  group_by(`DBA`, `CRITICAL FLAG`) %>% 
  summarise(count = n())
    
critical <- Q1g %>% 
  group_by(`DBA`) %>% 
  summarise(Critical_prop = mean(count) )%>% 
  arrange(desc(Critical_prop))
  



view(Q1g)
view(critical)



# Blank ------------------------------------------------------------------
# This section is left intentionally blank.



