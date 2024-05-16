library(ggthemes)
# ggplot for 1980 sample 




ggplot_1980 <- Cities_1980 %>%
  group_by(SPEAKENG_new) %>% 
  summarise(mean = mean(AAA))

ggplot(ggplot_1980,
       mapping = aes(x = SPEAKENG_new,
                     y = mean))+
  geom_line()+
  geom_point(color = "blue",
             size = 2.5)+ 
  xlab("English Proficiency")+
  ylab("Average Age at Arrival")+
  ggtitle("1980 Sample")+
  theme_economist_white()

class(Cities_1980$SPEAKENG_new)

# ggplot for 1990 Sample 

ggplot_1990 <- Cities_1990 %>%
  group_by(SPEAKENG_new) %>% 
  summarise(mean = mean(AAA))

ggplot(ggplot_1990,
       mapping = aes(x = SPEAKENG_new,
                     y = mean))+
  geom_line()+
  geom_point(color = "blue",
             size = 2.5)+ 
  xlab("English Proficiency")+
  ylab("Average Age at Arrival")+
  ggtitle("1990 Sample")+
  theme_economist_white()
  



# ggplot for 2000 Sample

ggplot_2000 <- Cities_2000 %>%
  group_by(SPEAKENG_new) %>% 
  summarise(mean = mean(AAA))

ggplot_2000

ggplot(ggplot_2000,
       mapping = aes(x = SPEAKENG_new,
                     y = mean))+
  geom_line()+
  geom_point(color = "blue",
             size = 2.5)+ 
  xlab("English Proficiency")+
  ylab("Average Age at Arrival")+
  ggtitle("2000 Sample")+
  theme_economist_white()

# ggplot for 2010

ggplot_2010 <- Cities_2010 %>%
  group_by(SPEAKENG_new) %>% 
  summarise(mean = mean(AAA))

ggplot(ggplot_2010,
       mapping = aes(x = SPEAKENG_new,
                     y = mean))+
  geom_line()+
  
  geom_point(color = "blue",
             size = 2.5)+ 
  xlab("English Proficiency")+
  ylab("Average Age at Arrival")+
  ggtitle("2010 Sample")+
  theme_economist_white()

# ggplot for 2019

ggplot_2019 <- Cities_2019 %>%
  group_by(SPEAKENG_new) %>% 
  summarise(mean = mean(AAA))

ggplot(ggplot_2019,
       mapping = aes(x = SPEAKENG_new,
                     y = mean))+
  geom_line()+
  
  geom_point(color = "blue",
             size = 2.5)+ 
  xlab("English Proficiency")+
  ylab("Average Age at Arrival")+
  ggtitle("2019 Sample")+
  theme_economist_white()

