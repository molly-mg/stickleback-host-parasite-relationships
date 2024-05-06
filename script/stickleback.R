# PACKAGES ----
library(usethis)
library(tidyverse)
library(janitor)
library(dplyr)
#_________________----

# LOADING DATA ----
stickleback <- read_csv("data/parasite_exp.csv") # loading csv formatted data into script

#head(stickleback) # checking the data has loaded correctly
#_________________----

# CLEANING DATA ----
stickleback <- janitor::clean_names(stickleback) # converting all column names to snake case

stickleback <- rename(stickleback,
                      "id"="fish_id",
                      "sex" = "fish_sex",
                      "length_mm" = "initial_total_length_mm") # concise column names

#stickleback %>%
  duplicated() # checking each observation for duplication
sum() # summing these duplications, there was 0
    
stickleback %>% 
  summarise(min=min(length_mm, na.rm=TRUE), # checking for implausibly small length
            max=max(length_mm, na.rm=TRUE)) # checking for implausibly large length
    
#stickleback %>%
  distinct(sex) # distinct values in a variable - should only be M or F

#stickleback %>%
  distinct(treatment)

#stickleback %>%
  is.na() %>% # checking each observation for an NA
  sum() # summing all of the NA values

#summary(stickleback)

#stickleback %>%
  summarise(
    mean_length_mm = mean(length_mm, na.rm=TRUE),
    mean_diplo_intensity_log = mean(diplo_intensity_log)) # mean length = 41.6 and mean intensity log = 1.91

#_______________----
  
# DATA VISUALISATION ----
  
diplo_stickleback <- select(.data = stickleback,
                            treatment, sex, length_mm, diplo_intensity_log)
  
ggplot(data = diplo_stickleback, aes(x = treatment, y = length_mm))+
  geom_jitter(aes(colour = treatment),
             width = 0.1,
              alpha = 0.7,
             show.legend = FALSE) # visualising the effect of treatment on length

ggplot(data = diplo_stickleback, aes(x = treatment, y = diplo_intensity_log))+
  geom_jitter(aes(colour = treatment),
              width = 0.1,
              alpha = 0.7,
              show.legend = FALSE)
#_________________----


