---
title: "DSCI 210: Mapping 2020 Election Results"
author: "Aaryan Bhatta"
---
  
library(tidyverse)
library(sf)
library(readxl)
library(RColorBrewer)

# Reading the Map 
map2022 <- st_zm(st_read("C:\\Users\\Aaryan D B\\Documents\\GitHub\\DSCI210-f24\\data\\maps\\PRECINCT2020_052219.shp"))
print(map2022,n=3)

# Displaying the Map
map2022 %>%
  ggplot(aes())+
  geom_sf()

# Getting Election Data
results2022 <- read_excel("G22_Official_Canvass.xlsx", 
                          skip=1)

# Merge Election results into Shape File
mapANDresults2022 <-
  left_join(map2022, results2022, by = c("PRECINCT" = "PRECINCT"))

# Creating the Map
mapANDresults2022 %>% 
  mutate(Dem.prop = `R. Bernard Mundy`/( `R. Bernard Mundy`+ `Megan E. Shanahan`)) %>%
  ggplot(aes(fill=Dem.prop)) +
  geom_sf()

mapANDresults2022 %>% 
  mutate(Dem.prop = `R. Bernard Mundy`/( `R. Bernard Mundy`+ `Megan E. Shanahan`)) %>%
  ggplot(aes(fill=Dem.prop)) +
  geom_sf()+
  labs(title = "2022 General Election", 
       subtitle = "Bernard Mundy vs Megan Shanahan",
       fill = "Vote for \nBernard Mundy (%)", 
       caption = "")+
  scale_fill_gradientn(colours=brewer.pal(n=10,name="RdBu"),na.value = "transparent",
                       breaks=c(0,.25,0.5,.75,1),labels=c("0%","25%","50%","75%","100%") ,
                       limits=c(0,1))

---
title: "Base/Swing Results Map of Previous Common Pleas Races"
---
# Reading the Map 
map2022 <- st_zm(st_read("C:\\Users\\Aaryan D B\\Documents\\GitHub\\DSCI210-f24\\data\\maps\\precincts_2022.shp"))
print(map2022,n=3)

#Display the map
map2022 %>%
  ggplot(aes())+
  geom_sf()

# Getting Election Data
results2022 <- read_excel("G22_Official_Canvass.xlsx", 
                          skip=1)
# Merge Election results into Shape File
mapANDresults2022 <-
  left_join(map2022, results2022, by = c("NAME" = "PRECINCT"))

# Creating the Map
  
mapANDresults2022 %>% 
  mutate(Dem.prop = `R. Bernard Mundy`/( `R. Bernard Mundy`+ `Megan E. Shanahan`)) %>%
  mutate(Mundy.baseswing=cut(Dem.prop,breaks=c(-0.001,.4,.6,1),label=c('Residual','Swing','Base'))) %>% 
  ggplot(aes(fill=Mundy.baseswing)) +
  geom_sf()+
  labs(title = "2022 General Election", 
       subtitle = "Bernard Mundy vs Megan Shanahan",
       fill = "Vote for \nBernard Mundy (%)", 
       caption = "")+
  scale_fill_manual(
    values = c(
    "Residual" = "red",
     "Base" = "blue",
     "Swing" = "yellow"
  ))



