---
title: "Dot Density"
author: "Aaryan Bhatta"
---
# Looked at a year that has the same conditions as this year: 2012 general elections
  
  # Dot density
  
results_2012 = read_excel("G12OffCanvass.xls",sheet = "Sheet1",skip=1,col_names = TRUE)
map_2012 = st_zm(st_read("C:\\Users\\Aaryan D B\\Documents\\GitHub\\DSCI210-f24\\data\\maps\\map2012.shp"))

totalVotes=219927+193326
print(totalVotes)
print(219927/totalVotes)
print(193326/totalVotes)
map_2012 = mutate(map_2012,PRECINCT = as.numeric(PRECINCT))
resultsAndmap2012 = left_join(map_2012, results_2012, by = c("PRECINCT" = "...1")) 

romneyobama <-
  resultsAndmap2012 %>% 
  select(`REP...11`,`DEM...10`) %>% 
  pivot_longer(cols =c("REP...11","DEM...10"), names_to="candidate",values_to = "votes")

romneyobama_dots <- as_dot_density(
  st_make_valid(romneyobama),
  value = "votes",
  values_per_dot = 100,
  group = "candidate"
)
specific_county <- resultsAndmap2012 %>%
  filter(PRECINCT >= 1 & PRECINCT <= 2616 )
ggplot() +
  geom_sf(data = romneyobama,
          fill = "white",
          color = "grey") +
  geom_sf(data = romneyobama_dots,
          aes(color = candidate),
          size = 0.01) +
  scale_color_manual(values =  c("REP...11" = "red", "DEM...10" = "blue"),
                     labels = c("REP...11" = "Romney and Ryan", "DEM...10" = "Obama and Biden"))+
  labs(title="Population Density for 2012 Presidential Elections")+
  theme_void()

#Common Pleas Population Density

commonPleas <-
  resultsAndmap2012 %>% 
  select(`...74`,`...75`,`...76`,`...77`) %>% 
  pivot_longer(cols =c("...74","...75","...76","...77"), names_to="candidate",values_to = "votes")

commonpleas_dots <- as_dot_density(
  st_make_valid(commonPleas),
  value = "votes",
  values_per_dot = 100,
  group = "candidate"
)
ggplot() +
  geom_sf(data = commonPleas,
          fill = "white",
          color = "grey") +
  geom_sf(data = commonpleas_dots,
          aes(color = candidate),
          size = 0.01) +
  scale_color_manual(values =  c("...74" = "blue","...75" = "blue","...76" = "red","...77" = "red"),
                     labels = c("...74" = "Nadine Allen: 32.46%", "...75" = "Stephen Black: 23.11%","...76" = "Leslie Ghiz: 24.94", "...77" = "Heather Russel: 19.49"))+
  labs(title="Population Density for Common Pleas 2012")+
  theme_void()+
  theme(legend.position = c(0.2, 0.05))

# Cincinnati Outline
romneyobama <-
  resultsAndmap2012 %>% 
  select(`REP...11`,`DEM...10`) %>% 
  pivot_longer(cols =c("REP...11","DEM...10"), names_to="candidate",values_to = "votes")

romneyobama_dots <- as_dot_density(
  st_make_valid(romneyobama),
  value = "votes",
  values_per_dot = 100,
  group = "candidate"
)
specific_county <- resultsAndmap2012 %>%
  filter(PRECINCT >= 1 & PRECINCT <= 2616 )
ggplot() +
  geom_sf(data = romneyobama,
          fill = "white",
          color = "grey") +
  geom_sf(data = romneyobama_dots,
          aes(color = candidate),
          size = 0.01) +
  geom_sf(data = specific_county,  # Use the filtered data for the specific counties
          fill = NA,                # No fill color
          color = "black",          # Outline color
          size = 1)  
scale_color_manual(values =  c("REP...11" = "red", "DEM...10" = "blue"),
                   labels = c("REP...11" = "Romney and Bryan", "DEM...10" = "Obama and Biden", "Total Votes =413253",
                   ))+
  labs(title="Population Density Map for the Presidential Elections 2012")+
  theme_void()


  
  
  