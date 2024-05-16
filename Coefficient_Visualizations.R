# 1980 Coefficients 

coefficients(ENG1980)
coef_1980 <- coefficients(iv1980)

# 1990 Coefficients

coefficients(ENG1990)
coef_1990 <- coefficients(iv1990)

# 2000 Coefficients 

coefficients(ENG2000)
coef_2000 <- coefficients(iv2000)

# 2010 Coefficients

coefficients(ENG2010)
coef_2010 <- coefficients(iv2010)

# 2019 Coefficients

coefficients(ENG2019)
coef_2019 <- coefficients(iv2019)

# Creating ggplot for change in coefficients

df <- data.frame(coef_1980, coef_1990, coef_2000, coef_2010, coef_2019)
view(df)

names(df)
transposed_df <- data.frame(t(df))
transposed_df

years <- c(1980, 1990, 2000, 2010, 2019)

ggplot(transposed_df,
       mapping = aes(y = SPEAKENG_new,
                     x = years))+
  geom_point()+
  geom_line()+
  ggtitle("Change in Returns to English Proficiency")+
  xlab("Sample Years")+
  ylab("Percentage of Income Attributable to English Proficiency")+
  theme_economist_white()

