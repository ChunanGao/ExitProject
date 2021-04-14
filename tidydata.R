library(tidyverse)

Des <- read_csv("~/R/ExitProject/Rowdata/Neighborhood_Destinations.csv")
head(Des)
Des %>% 
  mutate(des_count = count(Destination_Type)
  group_by(Neighborhood) %>% 
  summarise(Total_des = count(Destination_Type)) %>% 
  arrange(-Total_des)

Des %>% 
  pivot_wider(names_from = Destination_Type, values_from = StudentID) %>% 
  
    