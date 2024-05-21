library(dplyr)
library(patchwork)
library(tidyverse)
library(gghighlight)


#___________________----

# LOADING DATA ----
# loading csv format data into script
stickleback <- read_csv("data/parasite_exp.csv")

#___________________----

# CLEANING DATA ----
# converting all column names to snake case
stickleback <- janitor::clean_names(stickleback) 

stickleback <- rename(stickleback,
                      "sex" = "fish_sex",
                      "length_mm" = "initial_total_length_mm") # concise column names for id, sex and length

diplo_stickleback <- select(.data = stickleback,
                            treatment, sex, length_mm, diplo_intensity_log) # tibble of columns of interest 
#___________________----

# DATA EXPLORATION ----
ggplot(data = diplo_stickleback, aes(x = treatment, y = length_mm))+
  geom_jitter(aes(colour = treatment),
              width = 0.1,
              alpha = 0.7,
              show.legend = FALSE) # visualising the effect of treatment on length - doesnt really show anything but no significant outliers

ggplot(data = diplo_stickleback, aes(x = treatment, y = length_mm))+
  geom_jitter(aes(colour = sex),
              width = 0.1,
              alpha = 0.7,
              show.legend = FALSE) # effect of sex on length

ggplot(data = diplo_stickleback, aes(x = treatment, y = diplo_intensity_log))+
  geom_jitter(aes(colour = treatment),
              width = 0.1,
              alpha = 0.7,
              show.legend = FALSE) # effect of treatment on diplo intensity log

ggplot(data = diplo_stickleback, aes(x = treatment, y = diplo_intensity_log))+
  geom_jitter(aes(colour = sex),
              width = 0.1,
              alpha = 0.7,
              show.legend = FALSE) # effect of sex of treatment specific diplo intensity


diplo_stickleback %>%
  filter(treatment=='Infected LG') %>%
  ggplot(aes(x = sex, y = diplo_intensity_log))+
  geom_boxplot(aes(fill = sex),
               alpha = 0.2,
               width = 0.5,
               outlier.shape=NA)+
  geom_jitter(aes(colour = sex),
              width=0.2)+
  theme(legend.position = "none") # comparing sex and intensity specifically for diplo intensity



stickleback %>%
  #filter(treatment=='Infected LG') %>%
  ggplot(aes(x = sex, y = diplo_right_eye))+
  geom_boxplot(aes(fill = sex),
               alpha = 0.2,
               width = 0.5,
               outlier.shape=NA)+
  geom_jitter(aes(colour = sex),
              width=0.2)+
  theme(legend.position = "none")

stickleback %>%
  ggplot(aes(x=age_at_dissection,
             y = length_mm))+
  geom_jitter(aes(colour = age_at_dissection),
              width = 0.1,
              alpha = 0.7,
              show.legend = FALSE)

#_________________----

# DATA INSIGHTS ----
diplo_stickleback %>%
  group_by(treatment, sex) %>%
  summarise(n = n()) # number of observations per treatment per sex

diplo_stickleback %>%
  filter(treatment == 'Infected LG') %>%
  ggplot()+
  geom_histogram(aes(x=length_mm),
                 bins=15) # distribution of length per treatment




diplo_stickleback %>% drop_na %>% ggplot(aes(x = diplo_intensity_log, y = treatment)) +
  geom_density_ridges(aes(fill = treatment),
                      alpha = 0.8,
                      bandwidth = 0.2) # visualising treatment by diplo intensity


stickleback %>% ggplot(aes(x = diplo_right_eye, y = sex)) +
  geom_density_ridges(aes(fill = sex),
                      alpha = 0.8,
                      bandwidth = 0.8) # visualising diplo per eye per sex



diplo_stickleback_summary <- stickleback %>%
  summarise(mean_diplo_intensity=mean(diplo_intensity_log, na.rm = T),
            sd = sd(diplo_intensity_log, na.rm = T),
            median_diplo_intensity=median(diplo_intensity_log, na.rm = T),
            iqr = IQR(diplo_intensity_log, na.rm = T))

diplo_stickleback_summary # data summary


stickleback %>% 
  ggplot()+
  geom_histogram(aes(x=diplo_intensity_log),
                 alpha=0.8,
                 bins = 10,
                 fill="steelblue",
                 colour="darkgrey")+
  geom_vline(data=diplo_stickleback_summary,
             aes(xintercept=mean_diplo_intensity),
             colour="red",
             linetype="dashed")+
  geom_vline(data=diplo_stickleback_summary,
             aes(xintercept=median_diplo_intensity),
             colour="black",
             linetype="dashed")+
  labs(x = "Diplo Intensity",
       y = "Frequency")+
  theme_classic() # data distribution

ggplot(stickleback, aes(sample = diplo_intensity_log))+
  stat_qq()+
  stat_qq_line()


stickleback %>%
  pull(diplo_intensity_log) %>%
  car::qqPlot() # checking normality of residue observations

