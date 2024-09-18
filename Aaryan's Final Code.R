# Aaryan's Code

# Not all of my code was used in the final product


# Filtering data with creative perspectives
new_data <-my_data %>% 
select(`Student ID-Random`,Major,Degree,`Creative Perspectives`,`Semester Number at Xavier`) %>% 
rename(ID =`Student ID-Random`) %>% 
filter(`Creative Perspectives`==1)

str(my_data)

# Barplot Version
my_data2 <- my_data %>% 
  select(`Semester Number at Xavier`) %>% 
  mutate(
    `Semester Number at Xavier` = case_when(
      as.numeric(`Semester Number at Xavier`) >2000 ~ "Sum",
      as.numeric(`Semester Number at Xavier`) > 8 ~"9+",
      TRUE ~ `Semester Number at Xavier`
    )
  ) %>% 
  table() %>% 
  barplot()

# GGplot Version
my_data %>% 
  select(`Semester Number at Xavier`,) %>% 
  mutate(
    `Semester Number at Xavier` = case_when(
      as.numeric(`Semester Number at Xavier`) >2000 ~ "Sum",
      as.numeric(`Semester Number at Xavier`) >8 ~"9+",
      TRUE ~ `Semester Number at Xavier`
    )
  ) %>% 
  ggplot(aes(x=`Semester Number at Xavier`))+
  geom_bar(stat="count",fill="navy",color="black")+
  labs(title="Barplot showing Enrollment of Core Classes per Semseter",x="Category",y="Value")

# GGplot with percentage
my_data %>% 
  select(`Semester Number at Xavier`) %>% 
  mutate(
    `Semester Number at Xavier` = case_when(
      as.numeric(`Semester Number at Xavier`) >2000 ~ "Sum",
      as.numeric(`Semester Number at Xavier`) > 9 ~"9+",
      TRUE ~ `Semester Number at Xavier`
    )
  ) %>% 
  ggplot(aes(x = `Semester Number at Xavier`,))+
  geom_bar(aes(y=(after_stat(count))/sum(after_stat(count))*100),fill="red",color="black")+
  labs(title="Enrollment in Core Classes by Semester at Xavier",y='Percentage of Core Courses (%)')

# GGplot with English
plot1<- my_data %>% 
  select(`Semester Number at Xavier`,English) %>% 
  filter(`English` =="1") %>%
  mutate(
    `Semester Number at Xavier` = case_when(
      as.numeric(`Semester Number at Xavier`) >2000 ~ "Sum",
      as.numeric(`Semester Number at Xavier`) >8 ~"9+",
      TRUE ~ `Semester Number at Xavier`
    )
  ) %>% 
  ggplot(aes(x = `Semester Number at Xavier`,))+
  geom_bar(aes(y=(after_stat(count))/sum(after_stat(count))*100),fill="red",color="black")+
  labs(title="Enrollment in English Classes by Semesters at Xavier",y='Percentage of Core Courses (%)')

# GGplot with Foreign Language
plot2 <-my_data %>% 
  select(`Semester Number at Xavier`,language) %>% 
  filter(`language` =="1") %>%
  mutate(
    `Semester Number at Xavier` = case_when(
      as.numeric(`Semester Number at Xavier`) >2000 ~ "Sum",
      as.numeric(`Semester Number at Xavier`) >8 ~"9+",
      TRUE ~ `Semester Number at Xavier`
    )
  ) %>% 
  ggplot(aes(x = `Semester Number at Xavier`,))+
  geom_bar(aes(y=(after_stat(count))/sum(after_stat(count))*100),fill="red",color="black")+
  labs(title="Enrollment in Foreign Language Classes by Semesters at Xavier",y='Percentage of Core Courses (%)')

# GGplot with Lit and Moral Imagination
plot3 <-my_data %>% 
  select(`Semester Number at Xavier`,`Lit and Moral Imagination`) %>% 
  filter(`Lit and Moral Imagination` =="1") %>%
  mutate(
    `Semester Number at Xavier` = case_when(
      as.numeric(`Semester Number at Xavier`) >2000 ~ "Sum",
      as.numeric(`Semester Number at Xavier`) >8 ~"9+",
      TRUE ~ `Semester Number at Xavier`
    )
  ) %>% 
  ggplot(aes(x = `Semester Number at Xavier`,)) +
  geom_bar(aes(y=(after_stat(count))/sum(after_stat(count))*100),fill="red",color="black")+
  labs(title="Enrollment in Lit Moral Classes by Semesters at Xavier",y='Percentage of Core Courses (%)')

grid.arrange(plot1,plot2,plot3,ncol=3) # Combine all 3 plots

#Automating Barplots above

indiv.courses =colnames(my_data)[12:33]

for (X in indiv.courses)
{
  temp <- my_data %>%
    select(`Semester Number at Xavier`, sym(X)) %>%
    filter(!!sym(X) == "1") %>%
    mutate(`Semester Number at Xavier` = case_when(
      as.numeric(`Semester Number at Xavier`) >= 200000 ~ "Summer",
      as.numeric(`Semester Number at Xavier`) > 8 ~ "9+",
      TRUE ~ `Semester Number at Xavier` # Default case, if none of the above conditions are met
    )
    ) 
  
  print(ggplot(temp,aes(x = `Semester Number at Xavier`))+
          geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))*100), fill = "blue", color = "black") +
          labs(title = paste("Enrollment in",X,"Course by Semester at Xavier"), y = "Percentage of Courses (%)"))
}

# Automating Boxplots 

indiv.courses =colnames(my_data) [12:33]

# Reshaping data from wide to long for box plots

my_data %>% 
  filter(as.numeric(`Semester Number at Xavier`)<=8) %>% 
  pivot_longer(cols=12:33,names_to = "Requirement", values_to="Filled") %>% 
  filter(Filled =="1") %>% 
  #  mutate(SemesterGroup = ifelse(as.numeric(`Semester Number at Xavier`) > 8, "9+", as.character(`Semester Number at Xavier`))) %>%
  ggplot(aes(x= as.numeric(`Semester Number at Xavier`),y= reorder(Requirement, as.numeric(`Semester Number at Xavier`),mean)))+
  geom_boxplot(fill="light blue",color ="black")+
  theme_minimal() +
  labs(title="Timing of Core Courses at Xavier 2015-2023", x= "Semesters at Xavier", y="Core Curriculum")

# Boxplots but major specific

stem <- my_data %>%   # Stem Focused
  filter(my_data$Degree == "BS" | my_data$Degree == "HBS")

stem %>% 
  filter(as.numeric(`Semester Number at Xavier`)<=8) %>% 
  pivot_longer(cols=12:33,names_to = "Requirement", values_to="Filled") %>% 
  filter(Filled == "1") %>% 
  ggplot(aes(x= as.numeric(`Semester Number at Xavier`),y= reorder(Requirement, as.numeric(`Semester Number at Xavier`),mean)))+
  geom_boxplot(fill="light blue",color ="black")+
  theme_minimal() +
  labs(title="Timing of Core Courses at Xavier 2015-2023 for Stem Students", x= "Semesters at Xavier", y="Core Curriculum")

business <- my_data %>% # Business Focused
  filter(my_data$Degree =="BSBA"| my_data$Degree=="BSN")

business %>% 
  filter(as.numeric(`Semester Number at Xavier`)<=8) %>% 
  pivot_longer(cols=12:33,names_to = "Requirement", values_to="Filled") %>% 
  filter(Filled == "1") %>% 
  ggplot(aes(x= as.numeric(`Semester Number at Xavier`),y= reorder(Requirement, as.numeric(`Semester Number at Xavier`),mean)))+
  geom_boxplot(fill="light blue",color ="black")+
  theme_minimal() +
  labs(title="Timing of Core Courses at Xavier 2015-2023 for Business Students", x= "Semesters at Xavier", y="Core Curriculum")

humanities <- my_data %>% # Humanities Focused
  filter(my_data$Degree=="BA"| my_data$Degree=="BSW")

humanities %>% 
  filter(as.numeric(`Semester Number at Xavier`)<=8) %>% 
  pivot_longer(cols=12:33,names_to = "Requirement", values_to="Filled") %>% 
  filter(Filled == "1") %>% 
  ggplot(aes(x= as.numeric(`Semester Number at Xavier`),y= reorder(Requirement, as.numeric(`Semester Number at Xavier`),mean)))+
  geom_boxplot(fill="light blue",color ="black")+
  theme_minimal() +
  labs(title="Timing of Core Courses at Xavier 2015-2023 for Humanities Students", x= "Semesters at Xavier", y="Core Curriculum")

arts <-my_data %>% # Arts Focused
  filter(my_data$Degree=="BLA"| my_data$Degree=="BM"| my_data$Degree=="BFA")

arts %>% 
  filter(as.numeric(`Semester Number at Xavier`)<=8) %>% 
  pivot_longer(cols=12:33,names_to = "Requirement", values_to="Filled") %>% 
  filter(Filled == "1") %>% 
  ggplot(aes(x= as.numeric(`Semester Number at Xavier`),y= reorder(Requirement, as.numeric(`Semester Number at Xavier`),mean)))+
  geom_boxplot(fill="light blue",color ="black") +
  theme_minimal() +
  labs(title="Timing of Core Courses at Xavier 2015-2023 for Arts Students", x= "Semesters at Xavier", y="Core Curriculum")

nursing <- my_data %>% 
  filter(my_data$Degree == "BSN")

nursing %>% 
  filter(as.numeric(`Semester Number at Xavier`)<=8) %>% 
  pivot_longer(cols=12:33,names_to = "Requirement", values_to="Filled") %>% 
  filter(Filled == "1") %>% 
  ggplot(aes(x= as.numeric(`Semester Number at Xavier`),y= reorder(Requirement, as.numeric(`Semester Number at Xavier`),mean)))+
  geom_boxplot(fill="light blue",color ="black") +
  theme_minimal() +
  labs(title="Timing of Core Courses at Xavier 2015-2023 for Nursing Students", x= "Semesters at Xavier", y="Core Curriculum")

# Combine data manipulation for all groups
combined_data <- filtered %>%
  filter(as.numeric(`Semester Number at Xavier`) <= 8) %>%
  pivot_longer(cols = 12:33, names_to = "Requirement", values_to = "Filled") %>%
  filter(Filled == "1") %>%
  mutate(Group = case_when(
    Major %in% c("MACS"," APPH", "BIOC", "BSFB", "BIOL", "BIMS", "BIOP", "CHEM", "CSCI", "DSCI","ENVS", "ECON", "MATH", "PHYS", 
                 "ENPH", "MATH", "PHYS", "CSCA") ~ "Stem",
    Major %in% c("ADVT", "ARTS", "CLHU", "CMST", "DIF", "DIME", "ECSS", "ENGL", "FREN", "GDST", "GRAP", "HIST", 
                 "INST", "MUED", "MUSC", "MUTR", "ORGL", "PHIL", "PPPU", "POLI", "PUBL", "SOCI", "SPAN","THTR",
                 "THED","THEO") ~ "Humanities/Social Science",
    Major %in% c("ACCT", "BAIS", "BUUN", "ECON", "ENTR", "FINC", "INBU", "MGMT", "MKTG") ~ "Business",
    Major %in% c("ECED", "MCED", "MONT", "SPEC","ELEM", "TLSC", "SEDU") ~ "Education",
    Major %in% c("NURS", "NURE") ~ "Nursing",
    Major %in% c("CJUS","EXES","HSEA", "PSYC", "SOCW","SPMG","SPMK") ~ "Other/CPS",
    TRUE~NA_character_ # excludes default case
  )) %>% 
  filter(!is.na(Group))
# Plotting with facets
ggplot(combined_data, aes(x = as.numeric(`Semester Number at Xavier`), y = reorder(Requirement, as.numeric(`Semester Number at Xavier`), mean))) +
  geom_boxplot(fill = "light blue", color = "black") +
  theme_minimal() +
  labs(title = "Timing of Core Courses at Xavier 2015-2023", x = "Semesters at Xavier", y = "Core Curriculum") +
  facet_wrap(~ Group, scales = "free_y")

#Filtering out Masters Degrees

filtered <- my_data %>% 
  filter(Degree != "AA" & Degree != "NDPG" & Degree != "NDPU"& Degree != "MOT"&Degree != "MS"&Degree != "MBA"&Degree != "MA"&Degree != "MED"&Degree != "AS")

# Combine data manipulation for all groups
combined_data <- filtered %>%
  filter(as.numeric(`Semester Number at Xavier`) <= 8) %>%
  pivot_longer(cols = 12:33, names_to = "Requirement", values_to = "Filled") %>%
  filter(Filled == "1") %>%
  mutate(Group = case_when(
    Major %in% c("MACS"," APPH", "BIOC", "BSFB", "BIOL", "BIMS", "BIOP", "CHEM", "CSCI", "DSCI","ENVS", "ECON", "MATH", "PHYS", 
                 "ENPH", "MATH", "PHYS", "CSCA") ~ "Stem",
    Major %in% c("ADVT", "ARTS", "CLHU", "CMST", "DIF", "DIME", "ECSS", "ENGL", "FREN", "GDST", "GRAP", "HIST", 
                 "INST", "MUED", "MUSC", "MUTR", "ORGL", "PHIL", "PPPU", "POLI", "PUBL", "SOCI", "SPAN","THTR",
                 "THED","THEO") ~ "Humanities/Social Science",
    Major %in% c("ACCT", "BAIS", "BUUN", "ECON", "ENTR", "FINC", "INBU", "MGMT", "MKTG") ~ "Business",
    Major %in% c("ECED", "MCED", "MONT", "SPEC","ELEM", "TLSC", "SEDU") ~ "Education",
    Major %in% c("NURS", "NURE") ~ "Nursing",
    Major %in% c("CJUS","EXES","HSEA", "PSYC", "SOCW","SPMG","SPMK") ~ "Other/CPS",
    TRUE~NA_character_ # excludes default case
  )) %>% 
  filter(!is.na(Group))

# Plotting with facets
ggplot(combined_data, aes(x = as.numeric(`Semester Number at Xavier`), y = reorder(Requirement, as.numeric(`Semester Number at Xavier`), mean))) +
  geom_boxplot(fill = "light blue", color = "black") +
  theme_minimal() +
  labs(title = "Timing of Core Courses at Xavier 2015-2023", x = "Semesters at Xavier", y = "Core Curriculum") +
  facet_wrap(~ Group, scales = "free_y")

# Boxplots with NF Cohort Only 
filtered <- my_data %>% 
  filter(Degree != "AA" & Degree != "NDPG" & Degree != "NDPU"& Degree != "MOT"&Degree != "MS"&Degree != "MBA"&Degree != "MA"&Degree != "MED"&Degree != "AS" & `Cohort Code`=="NF")


nf_boxplots <- filtered %>%
  filter(as.numeric(`Semester Number at Xavier`) <= 8) %>%
  pivot_longer(cols = 12:33, names_to = "Requirement", values_to = "Filled") %>%
  filter(Filled == "1") %>%
  mutate(Group = case_when(
    Major %in% c("MACS"," APPH", "BIOC", "BSFB", "BIOL", "BIMS", "BIOP", "CHEM", "CSCI", "DSCI","ENVS","MATH", "PHYS", 
                 "ENPH", "MATH", "PHYS", "CSCA") ~ "Stem",
    Major %in% c("ADVT", "ARTS", "CLHU", "CMST", "DIF", "DIME", "ECSS", "ENGL", "FREN", "GDST", "GRAP", "HIST", 
                 "INST", "MUED", "MUSC", "MUTR", "ORGL", "PHIL", "PPPU", "POLI", "PUBL", "SOCI", "SPAN","THTR",
                 "THED","THEO") ~ "Humanities/Social Science",
    Major %in% c("ACCT", "BAIS", "BUUN", "ECON", "ENTR", "FINC", "INBU", "MGMT", "MKTG") ~ "Business",
    Major %in% c("ECED", "MCED", "MONT", "SPEC","ELEM", "TLSC", "SEDU") ~ "Education",
    Major %in% c("NURS", "NURE") ~ "Nursing",
    Major %in% c("CJUS","EXES","HSEA", "PSYC", "SOCW","SPMG","SPMK") ~ "Other/CPS",
    TRUE~NA_character_ # excludes default case
  )) %>% 
  filter(!is.na(Group))
# Plotting with facets
ggplot(nf_boxplots, aes(x = as.numeric(`Semester Number at Xavier`), y = reorder(Requirement, as.numeric(`Semester Number at Xavier`), mean))) +
  geom_boxplot(fill = "light blue", color = "black") +
  theme_minimal() +
  labs(title = "Timing of Core Courses at Xavier for New Freshman (NF) Students 2015-2023", x = "Semesters at Xavier", y = "Core Curriculum") +
  facet_wrap(~ Group, scales = "free_y")

# Boxplots with NT Cohort Only
filtered <- my_data %>% 
  filter(Degree != "AA" & Degree != "NDPG" & Degree != "NDPU"& Degree != "MOT"&Degree != "MS"&Degree != "MBA"&Degree != "MA"&Degree != "MED"& Degree != "AS" & `Cohort Code`=="NT")

nt_boxplots <- filtered %>%
  filter(as.numeric(`Semester Number at Xavier`) <= 8) %>%
  pivot_longer(cols = 12:33, names_to = "Requirement", values_to = "Filled") %>%
  filter(Filled == "1") %>%
  mutate(Group = case_when(
    Major %in% c("MACS"," APPH", "BIOC", "BSFB", "BIOL", "BIMS", "BIOP", "CHEM", "CSCI", "DSCI","ENVS", "ECON", "MATH", "PHYS", 
                 "ENPH", "MATH", "PHYS", "CSCA") ~ "Stem",
    Major %in% c("ADVT", "ARTS", "CLHU", "CMST", "DIF", "DIME", "ECSS", "ENGL", "FREN", "GDST", "GRAP", "HIST", 
                 "INST", "MUED", "MUSC", "MUTR", "ORGL", "PHIL", "PPPU", "POLI", "PUBL", "SOCI", "SPAN","THTR",
                 "THED","THEO") ~ "Humanities/Social Science",
    Major %in% c("ACCT", "BAIS", "BUUN", "ECON", "ENTR", "FINC", "INBU", "MGMT", "MKTG") ~ "Business",
    Major %in% c("ECED", "MCED", "MONT", "SPEC","ELEM", "TLSC", "SEDU") ~ "Education",
    Major %in% c("NURS", "NURE") ~ "Nursing",
    Major %in% c("CJUS","EXES","HSEA", "PSYC", "SOCW","SPMG","SPMK") ~ "Other/CPS",
    TRUE~NA_character_ # excludes default case
  )) %>% 
  filter(!is.na(Group))
# Plotting with facets
ggplot(nt_boxplots, aes(x = as.numeric(`Semester Number at Xavier`), y = reorder(Requirement, as.numeric(`Semester Number at Xavier`), mean))) +
  geom_boxplot(fill = "light blue", color = "black") +
  theme_minimal() +
  labs(title = "Timing of Core Courses at Xavier for New Transfer (NT) Students 2015-2023", x = "Semesters at Xavier", y = "Core Curriculum") +
  facet_wrap(~ Group, scales = "free_y")

# Amount of Core Classes that are Requirements Within All Majors
Core_Majors_Final %>% 
  pivot_longer(cols=4:25,names_to = "Requirement", values_to="Filled") %>%
  slice(-64:-68) %>% 
  filter(Filled== "1") %>% 
  group_by(Requirement) %>% 
  summarize(Count=n()) %>% 
  arrange(desc(Count)) %>% 
  mutate(Requirement = factor(Requirement, levels = Requirement)) %>%
  ggplot(aes(x=reorder(Requirement,Count),y=Count)) +
  geom_bar(stat = "identity", fill = "navyblue",width = 0.75) +
  coord_flip()+
  labs(title= "Bar Plot Showing the Amount of Core Classes that are Within All Majors Requirements",x="Core Classes",y="Count")+
  theme_minimal()

# Amount of Core Classes that are Requirements Within All Majors (Percentages %)

Core_Majors_Final %>% 
  pivot_longer(cols=4:25,names_to = "Requirement", values_to="Filled") %>%
  filter(Filled== "1") %>% 
  group_by(Requirement) %>% 
  summarize(Count=n()) %>% 
  mutate(Percentage = round(Count/63*100,2)) %>% 
  arrange(desc(Percentage)) %>% 
  mutate(Requirement = factor(Requirement, levels = unique(Requirement))) %>%
  ggplot(aes(x=reorder(Requirement,Percentage),y=Percentage)) +
  geom_bar(stat = "identity", fill = "navyblue",width = 0.75) +
  geom_text(aes(label=paste0(Percentage,"%")),vjust = 0.5,hjust = -0.5,color="black",size = 4) +
  coord_flip()+
  labs(title= "The Percentage of Majors that Fufill each Core Requirement",x="Core Classes",y="Percentage")+
  theme_minimal()

# Amount of Core Classes that are not Requirements Within All Majors (Percentages %)

Core_Majors_Final %>% 
  pivot_longer(cols=4:25,names_to = "Requirement", values_to="Filled") %>%
  filter(Filled== "0") %>% 
  group_by(Requirement) %>% 
  summarize(Count=n()) %>% 
  mutate(Percentage = round(Count/63*100,2)) %>% 
  arrange(desc(Percentage)) %>% 
  mutate(Requirement = factor(Requirement, levels = unique(Requirement))) %>%
  ggplot(aes(x=reorder(Requirement,Percentage),y=Percentage)) +
  geom_bar(stat = "identity", fill = "navyblue",width = 0.75) +
  geom_text(aes(label=paste0(Percentage,"%")),vjust = 0.5,hjust = -0.5,color="black",size = 4) +
  coord_flip()+
  labs(title= "Bar Plot Showing the Percentage of Core Classes that are not Within All Majors Requirements", x ="Core Classes",y ="Percentage")+
  theme_minimal()

# Amount of Core Classes Required with Categories

required_barplots <- Core_Majors_Final %>%
  pivot_longer(cols = 4:25, names_to = "Requirement", values_to = "Filled") %>%
  filter(Filled == "1") %>%
  mutate(Group = case_when(
    Major %in% c("MACS", "BIOC", "BSFB", "BIOL", "BIMS", "BIOP", "CHEM", "CSCI", "DSCI","ENPH","ENVS","MATH", "PHYS") ~ "Stem",
    Major %in% c("ADVT", "ARTS", "CLHU", "CMST", "DIFT", "DIME", "ECSS", "ENGL", "FREN", "GDST", "GRAP", "HIST", 
                 "INST", "MUED", "MUSC", "MUTR", "ORGL", "PHIL", "PPPU", "POLI", "PUBL", "SOCI", "SPAN","THTR",
                 "THED","THEO") ~ "Humanities/Social Sci",
    Major %in% c("ACCT", "BAIS", "BUUN", "ECON", "ENTR", "FINC", "INBU", "MGMT", "MKTG") ~ "Business",
    Major %in% c("CJUS","EXES","HSEA", "PSYC", "SOCW","SPMG","SPMK") ~ "Other/CPS",
    Major %in% c("MCED", "MONT","ECED", "SPEC","ELEM", "TLSC", "SEDU") ~ "Education",
    Major %in% c("NURE") ~ "Nursing",
    TRUE~NA_character_ # excludes default case
  )) %>% 
  filter(!is.na(Group)) %>% 
  group_by(Group,Requirement) %>% 
  summarise(Total = n(),.groups='drop') %>% 
  ungroup() %>% 
  complete(Group,Requirement,fill=list(Total=0))
# Plotting with facets
ggplot(required_barplots, aes(x = Requirement, y = Total,fill=Group)) +
  geom_bar(stat="identity",position="dodge",color = "black") +
  geom_text(aes(label=Total),position = position_dodge(width=0.9),vjust=-0.5,size=3.5,color="black")+
  labs(title = "Bar plot Showing the Number of Majors within Catgories and the Core Classes that are Requirements for their Majors", x = "Categories", y = "Total") +
  #facet_wrap(~ Group, scales = "free")+
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1),
        strip.text.x = element_text(size=10,face="bold",color="black"),
        strip.background = element_blank(),
        strip.placement="outside",
        plot.margin = margin(t=5,r=-3,b=5,l=-0.5,unit="pt"))

# Amount of Core Classes Required with Categories

required_barplots <- Core_Majors_Final %>%
  pivot_longer(cols = 4:25, names_to = "Requirement", values_to = "Filled") %>%
  filter(Filled == "1") %>%
  mutate(Group = case_when(
    Major %in% c("MACS", "BIOC", "BSFB", "BIOL", "BIMS", "BIOP", "CHEM", "CSCI", "DSCI","ENPH","ENVS","MATH", "PHYS") ~ "Stem",
    Major %in% c("ADVT", "ARTS", "CLHU", "CMST", "DIFT", "DIME", "ECSS", "ENGL", "FREN", "GDST", "GRAP", "HIST", 
                 "INST", "MUED", "MUSC", "MUTR", "ORGL", "PHIL", "PPPU", "POLI", "PUBL", "SOCI", "SPAN","THTR",
                 "THED","THEO") ~ "Humanities/Social Science",
    Major %in% c("ACCT", "BAIS", "BUUN", "ECON", "ENTR", "FINC", "INBU", "MGMT", "MKTG") ~ "Business",
    Major %in% c("CJUS","EXES","HSEA", "PSYC", "SOCW","SPMG","SPMK") ~ "Other/CPS",
    Major %in% c("MCED", "MONT","ECED", "SPEC","ELEM", "TLSC", "SEDU") ~ "Education",
    Major %in% c("NURE") ~ "Nursing",
    TRUE~NA_character_ # excludes default case
  )) %>% 
  filter(!is.na(Group)) %>% 
  group_by(Group,Requirement) %>% 
  summarise(Total = n(),.groups='drop') %>% 
  ungroup() %>% 
  complete(Group,Requirement,fill=list(Total=0))

# Plotting with facets
ggplot(required_barplots, aes(x = Requirement, y = Total,fill=Group)) +
  geom_bar(stat="identity",position="dodge",color = "black") +
  geom_text(aes(label=Total),position = position_dodge(width=0.9),vjust=-0.5,size=3,color="black")+
  labs(title = "Bar plot Showing the Number of Majors within Catgories and the Core Classes that are Requirements for their Majors", x = "Categories", y = "Total") +
  facet_wrap(~ Group, scales = "free")+
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1),
        strip.text.x = element_text(size=9,face="bold",color="black"),
        strip.background = element_blank(),
        strip.placement="outside",
        plot.margin = margin(t=20,r=10,b=10,l=10,unit="pt"))
