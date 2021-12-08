install.packages('tidyverse')
install.packages('readr')
install.packages('dplyr')

require(devtools)
library(readr)
library(readxl)
library(ggplot2)
require(dplyr)


savant_data_12_7 <- read_excel("savant_data_12_7.xlsx")
mydata <- savant_data_12_7[, c(3, 1, 11)]
mydata

ggplot(savant_data_12_7, aes(woba, pitches)) + geom_point() + geom_smooth()

library(readxl)
sportsref_download <- read_excel("sportsref_download.xlsx", 
                                 col_types = c("numeric", "text", "date", 
                                               "text", "text", "text", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "text", "numeric", "numeric"))
View()
wobadata <- sportsref_download %>% select(Player, HBP, H, `2B`, `3B`, HR, AB, BB, IBB, SF,PA) %>%
  mutate(wOBA=((.69*(BB-IBB)+(.72*HBP)+(.89*H)+(1.27*`2B`)+(1.62*`3B`)+(2.1*HR))/(AB+BB-IBB+SF+HBP)))
wobadata


groupedwoba <- wobadata %>%
  group_by(Player) %>%
  summarise_at(vars(wOBA),
               list(avgwOBA = mean, PA = sum)) %>%
  arrange(desc(avgwOBA)) %>%
  filter(Player=="Freddie Freeman" | Player=="Pete Alonso" | Player=="Paul Goldschmidt" | Player=="Matt Olson" | Player=="Yuli Gurriel" | Player=="Carlos Santana" | Player=="José Abreu" | Player=="Vladimir Guerrero Jr." | Player=="Joey Votto" | Player=="Anthony Rizzo")
