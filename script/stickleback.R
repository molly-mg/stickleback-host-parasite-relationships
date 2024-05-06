# PACKAGES ----
library(usethis)
library(tidyverse)
library(janitor)
library(dplyr)
#_________________----

# LOADING DATA ----
stickleback <- read_csv("data/parasite_exp.csv") # loading csv formatted data into script

head(stickleback) # checking the data has loaded correctly
#_________________----

# CLEANING DATA ----
stickleback <- janitor::clean_names(stickleback) # converting all column names to snake case

stickleback <- rename(stickleback,
                      "id"="fish_id",
                      "sex" = "fish_sex",
                      "length_mm" = "initial_total_length_mm") # concise column names

stickleback %>%
  duplicated() # checking each observation for duplication
sum() # summing these duplications, there was 0
    
stickleback %>% 
  summarise(min=min(length_mm, na.rm=TRUE), # checking for implausibly small length
            max=max(length_mm, na.rm=TRUE)) # checking for implausibly large length
    
stickleback %>%
  distinct(sex) # distinct values in a variable - should only be M or F

stickleback %>%
  is.na() %>%
  sum()

summary(stickleback)

    
#_________________----


