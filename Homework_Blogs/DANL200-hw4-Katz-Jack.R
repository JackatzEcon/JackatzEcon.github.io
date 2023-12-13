# SUNY Geneseo, Fall 2023
# DANL 200: Introduction to Data Analytics
# Homework Assignment 4

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

# For Question 1, run the following R command to read the CSV file, 
# `spotify_all.csv` as data.frame, `spotify_all`:



spotify_all <- read_csv('https://bcdanl.github.io/data/spotify_all.csv')

# The data.frame `spotify_all` includes information about Spotify users' playlists

# Variable Description for spotify_all ---------------------------------------

# -   `pid`: playlist ID; unique ID for playlist
# -   `playlist_name`: a name of playlist
# -   `pos`: a position of the track within a playlist (zero-based)
# -   `artist_name`: name of the track's primary artist
# -   `track_name`: name of the track
# -   `duration_ms`: duration of the track in milliseconds
# -   `album_name`: name of the track's album



# Q1a --------------------------------------------------------------
# - Find the ten most popular song.
#   - A value of a song is defined as a combination of a `artist_name` value and a `track_name` value. 
#   - Who are artists for those ten most popular song?


# Answer for Q1a
Q1a <- spotify_all %>% 
  arrange(-`duration_ms`) %>% 
  filter(playlist_name != "Audiobooks") %>% 
  slice_head(n = 10) %>% 
  select(`artist_name`)



# Q1b --------------------------------------------------------------
# - Find the five most popular artist in terms of the number of occurrences 
# in the data.frame, `spotify_all`.
# - What is the most popular song for each of the five most popular artist?

# Answer for Q1b

Q1b <- spotify_all %>% 
  group_by(`artist_name`) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  slice_head(n = 5)
  
Q1b2 <- spotify_all %>% 
  group_by(`track_name`, `artist_name`) %>%
  filter(`artist_name` %in% Q1b$artist_name) %>% 
  summarise(po_song = n()) %>% 
  arrange(-po_song) 
  
answer <- Q1b2 %>% 
  group_by(`artist_name`) %>% 
  filter(po_song == max(po_song))

# Q1c --------------------------------------------------------------
# Provide both (1) ggplot codes and (2) a couple of sentences 
# to describe the relationship between `pos` and the ten most popular artists.


# Answer for Q1c
Top10 <- spotify_all %>% 
  group_by(`artist_name`) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  slice_head(n = 10)


Artists   <- spotify_all %>% 
  filter(`artist_name` %in% Top10$artist_name) 

ggplot(Artists, 
       mapping = aes(x = `artist_name`, y = `pos`))+
  geom_boxplot()

#for the top 10 artist there median `pos` is below 50. 

# Q1d --------------------------------------------------------------
# Create the data.frame with `pid`-`artist` level of observations 
# with the following four variables:

# -   `pid`: playlist id
# -   `playlist_name`: name of playlist
# -   `artist`: name of the track's primary artist, which appears only once within a playlist
# -   `n_artist`: number of occurrences of artist within a playlist

# Answer for Q1d

Q1d <- spotify_all %>% 
  select()





# Question 2. Working with Beer Market Data ---------------

# For Question 2, run the following R command to read the CSV file, 
# `beer_markets.csv` as data.frame, `beer_mkts`:

beer_mkts <- read_csv('https://bcdanl.github.io/data/beer_markets.csv')

 ## Variable Description
# - `hh`: an identifier of the household;
# - `X_purchase_desc`: details on the purchased item;
# - `quantity`: the number of items purchased;
# - `brand`: Bud Light, Busch Light, Coors Light, Miller Lite, or Natural Light;
# - `dollar_spent`: total dollar value of purchase;
# - `beer_floz`: total volume of beer, in fluid ounces;
# - `price_per_floz`: price per fl.oz. (i.e., beer spent/beer floz);
# - `container`: the type of container;
# - `promo`: Whether the item was promoted (coupon or otherwise);
# - `market`: Scan-track market (or state if rural);
# - demographic data, including gender, marital status, household income, class of work, race, education, age, the size of household, and whether or not the household has a microwave or a dishwasher.



# Q2a --------------------------------------------------------------
# - Find the top 5 markets in terms of the total `beer_floz`.
# - Find the top 5 markets in terms of the total `beer_floz` of *BUD LIGHT*.
# - Find the top 5 markets in terms of the total `beer_floz` of *BUSCH LIGHT*.
# - Find the top 5 markets in terms of the total `beer_floz` of *COORS LIGHT*.
# - Find the top 5 markets in terms of the total `beer_floz` of *MILLER LITE*.
# - Find the top 5 markets in terms of the total `beer_floz` of *NATURAL LIGHT*.

# Answer for Q2a

#Top 5 markets
Q2a1 <- beer_mkts %>% 
  group_by(`market`) %>%
  mutate(sum = sum(`beer_floz`)) %>% 
  arrange(desc(sum)) %>% 
  slice_head(n = 1) %>% 
  arrange(-sum) %>% 
  head(n = 5) %>% 
  select(`market`, sum)

#Top 5 Bud light markets 
Q2a2 <- beer_mkts %>% 
  group_by(`market`, `brand`) %>%
  summarise(sum = sum(`beer_floz`)) %>% 
  filter(`brand` == "BUD LIGHT") %>% 
  arrange(-sum) %>% 
  head(n = 5)
  
#Top 5 Busch Light Markets 
Q2a3 <- beer_mkts %>% 
  group_by(`market`, `brand`) %>%
  summarise(sum = sum(`beer_floz`)) %>% 
  filter(`brand` == "BUSCH LIGHT") %>% 
  arrange(-sum) %>% 
  head(n = 5)

#Top 5 Coors Light markets 
Q2a4 <- beer_mkts %>% 
  group_by(`market`, `brand`) %>%
  summarise(sum = sum(`beer_floz`)) %>% 
  filter(`brand` == "COORS LIGHT") %>% 
  arrange(-sum) %>% 
  head(n = 5)

#Top 5 Miller Light markets
Q2a5 <- beer_mkts %>% 
  group_by(`market`, `brand`) %>%
  summarise(sum = sum(`beer_floz`)) %>% 
  filter(`brand` == "MILLER LITE") %>% 
  arrange(-sum) %>% 
  head(n = 5)

#Top 5 Natural light markets
Q2a6 <- beer_mkts %>% 
  group_by(`market`, `brand`) %>%
  summarise(sum = sum(`beer_floz`)) %>% 
  filter(`brand` == "NATURAL LIGHT") %>% 
  arrange(-sum) %>% 
  head(n = 5)



class(beer_mkts$market)


as.factor(beer_mkts$market)




# Q2b --------------------------------------------------------------
# - For households that purchased *BUD LIGHT* at least once, 
#     what fraction of households did purchase only *BUD LIGHT*?
# - For households that purchased *BUSCH LIGHT* at least once, 
#     what fraction of households did purchase only *BUSCH LIGHT*?
# - For households that purchased *COORS LIGHT* at least once, 
#     what fraction of households did purchase only *COORS LIGHT*?
# - For households that purchased *MILLER LITE* at least once, 
#     what fraction of households did purchase only *MILLER LITE*?
# - For households that purchased *NATURAL LIGHT* at least once, 
#     what fraction of households did purchase only *NATURAL LIGHT*?

# - Which beer brand does have the largest proportion of such loyal consumers?

# Answer for Q2b

# - For households that purchased *BUD LIGHT* at least once, 
#     what fraction of households did purchase only *BUD LIGHT*?

#All people that bought bud light

Q2b1 <- beer_mkts %>% 
  group_by(`hh`, `brand`) %>% 
  summarise(count = n()) %>% 
  filter(`brand` == "BUD LIGHT") 

beer_mkts %>% 
  group_by()



#Essentially we are looking for households that only bought budlight
#and households that bought budlight and other beer 
  


  
  
  







# Q2c --------------------------------------------------------------
# - For each household, calculate the number of beer transactions. 
# - For each household, calculate the proportion of each beer brand choice.

# Answer for Q2c

#number of beer transactions
Q2c1 <- beer_mkts %>% 
  group_by( `hh`) %>% 
  summarise(count = n()) 
  
#Proportion of each beer brand choice

Q2c2 <- beer_mkts %>% 
  group_by(`brand` , `hh`) %>% 
  summarise(count = n()) %>% 
  group_by(`hh`) %>% 
  summarise(count = n())
  
  
  mutate(Beer_purchases = )



  
 



# Blank ------------------------------------------------------------------
# This section is left intentionally blank.
