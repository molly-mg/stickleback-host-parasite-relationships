# PACKAGES ----

library(usethis)
library(tidyverse)
library(janitor)
library(dplyr)
library(ggridges)
library(lubridate)
#library(car)
library(patchwork)
library(GGally)
library(performance)
library(gghighlight)
#library(ggdist)
library(colorBlindness)
library(rstatix)
library(lmtest)

#_________________----

# LOADING DATA ----

stickleback <- read_csv("data/parasite_exp.csv") # loading csv format data into script

#_________________----

# CLEANING DATA ----

stickleback <- janitor::clean_names(stickleback) # converting all column names to snake case

stickleback <- rename(stickleback,
                      "sex" = "fish_sex",
                      "length_mm" = "initial_total_length_mm") # concise column names for id, sex and length

#_______________----
  
# DATA VISUALISATION ----

diplo_stickleback <- select(.data = stickleback,
                            treatment, sex, length_mm, diplo_intensity_log) # tibble of columns of interest
  















# PLOT 1 ------

ggplot(diplo_stickleback,
       aes(x = diplo_intensity_log,
           fill = treatment))+
  geom_density(
    alpha = 0.6,
    weight = 0.5)+
  gghighlight()+
  labs(y = "Density")+
  scale_fill_manual(
    values = c("white", 
               "red", 
               "goldenrod1",
               "green"))+
  theme_classic()+
  theme(legend.position = "none")+
  facet_wrap(~treatment)+
  geom_vline(data=diplo_intensity_mean,
             aes(xintercept=mean_diplo_intensity),
             colour="black",
             linetype="solid")


control_mean <- diplo_stickleback %>%
    filter(treatment == "Control") %>%
    pull(diplo_intensity_log) %>%
    mean(na.rm=T)

uninfected_mean <- diplo_stickleback %>%
  filter(treatment == "Uninfected") %>%
  pull(diplo_intensity_log) %>%
  mean(na.rm=T)

infected_lg_mean <- diplo_stickleback %>%
  filter(treatment == "Infected LG") %>%
  pull(diplo_intensity_log) %>%
  mean(na.rm=T)

infected_hg_mean <- diplo_stickleback %>%
  filter(treatment == "Infected HG") %>%
  pull(diplo_intensity_log) %>%
  mean(na.rm=T)



d0 <- ggplot(
  filter(.data = diplo_stickleback, treatment %in% c("Control")),
  aes(x = diplo_intensity_log,
      fill = treatment))+
  geom_density(
    alpha = 0.6,
    weight = 0.5)+
  gghighlight()+
  labs(y = "Density")+
  scale_fill_manual(
    values = c("white"))+
  theme_classic()+
  theme(legend.position = "none")+
  geom_vline(data=NULL,
             aes(xintercept=control_mean),
             colour="black",
             linetype="dashed")




d1 <- ggplot(
            filter(.data = diplo_stickleback, treatment %in% c("Control", "Infected HG")),
              aes(x = diplo_intensity_log,
                  fill = treatment))+
              geom_density(
                alpha = 0.6,
                weight = 0.5)+
              gghighlight()+
              labs(y = "Density")+
              scale_fill_manual(
                values = c("white", 
                           "#FF0500"))+
              theme_classic()+
              theme(legend.position = "none")+
              geom_vline(data=NULL,
                aes(xintercept=infected_hg_mean),
                         colour="blue",
                         linetype="dashed")+
              geom_vline(data=NULL,
                aes(xintercept=control_mean),
                colour="black",
                linetype="dashed")


d2 <- ggplot(
              filter(.data = diplo_stickleback, treatment %in% c("Control", "Infected LG")),
              aes(x = diplo_intensity_log,
                  fill = treatment))+
              geom_density(
                alpha = 0.6,
                weight = 0.5)+
              gghighlight()+
              labs(y = "Density")+
              scale_fill_manual(
                values = c("white", 
                           "#FFBE00"))+
              theme_classic()+
              theme(legend.position = "none")+
  geom_vline(data=NULL,
             aes(xintercept=infected_lg_mean),
             colour="blue",
             linetype="dashed")+
  geom_vline(data=NULL,
             aes(xintercept=control_mean),
             colour="black",
             linetype="dashed")

d3 <- ggplot(
              filter(.data = diplo_stickleback, treatment %in% c("Control", "Uninfected")),
              aes(x = diplo_intensity_log,
                  fill = treatment))+
              geom_density(
                alpha = 0.6,
                weight = 0.5)+
              gghighlight()+
              labs(y = "Density")+
              scale_fill_manual(
                values = c("white", 
                           "#FCFF19"))+
              theme_classic()+
              theme(legend.position = "none")+
  geom_vline(data=NULL,
             aes(xintercept=uninfected_mean),
             colour="blue",
             linetype="dashed")+
  geom_vline(data=NULL,
             aes(xintercept=control_mean),
             colour="black",
             linetype="dashed")


plot_1 <- d0+d1+d2+d3 # chosen plot 1

plot_1


#PLOT 2 ----




# PLOT 3 -----

axis_limit <- coord_cartesian(xlim=c(40,20))


p1 <- diplo_stickleback %>% drop_na %>%
  filter(treatment == "Control") %>%
  ggplot(aes(x = sex, y = length_mm))+
  geom_violin(aes(colour=sex, fill=sex),
              alpha = 1,
              width = 1,
              show.legend = FALSE)+
  geom_boxplot(colour="black",
               fill="white",
               alpha = 1,
               width = 0.4,
               outlier.shape=NA,
               show.legend = FALSE)


p2 <- diplo_stickleback %>% drop_na %>%
  filter(treatment == "Infected HG") %>%
  ggplot(aes(x = sex, y = length_mm))+
  geom_violin(aes(colour=sex, fill=sex),
              alpha = 1,
              width = 1,
              show.legend = FALSE)+
  geom_boxplot(colour="black",
               fill="white",
               alpha = 1,
               width = 0.4,
               outlier.shape=NA,
               show.legend = FALSE)


p3 <- diplo_stickleback %>% drop_na %>%
  filter(treatment == "Infected LG") %>%
  ggplot(aes(x = sex, y = length_mm))+
  geom_violin(aes(colour=sex, fill=sex),
              alpha = 1,
              width = 1,
              show.legend = FALSE)+
  geom_boxplot(colour="black",
               fill="white",
               alpha = 1,
               width = 0.4,
               outlier.shape=NA,
               show.legend = FALSE)


p4 <- diplo_stickleback %>% drop_na %>%
  filter(treatment == "Uninfected") %>%
  ggplot(aes(x = sex, y = length_mm))+
  geom_violin(aes(colour=sex, fill=sex),
              alpha = 1,
              width = 1,
              show.legend = FALSE)+
  geom_boxplot(colour="black",
               fill="white",
               alpha = 1,
               width = 0.4,
               outlier.shape=NA,
               show.legend = FALSE)


treatment_colours <- c("white", "white", "#FF0500", "#FFBE00", "#FCFF19")

p5 <- diplo_stickleback %>% drop_na %>%
  ggplot(aes(x = treatment, y = length_mm, fill=treatment))+
  geom_violin(
    alpha = 1,
    width = 1,
    show.legend = FALSE,
  ) +
  geom_boxplot(
    aes(fill=""),
    alpha = 1,
    width = 0.3,
    outlier.shape=NA)+
  
  theme(legend.position = "none")+
  scale_fill_manual(values = treatment_colours)

((
  p1 + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
  |
    p2 + theme(panel.background = element_rect(fill = '#FFB1B1', colour = 'black'))
  |
    p3 + theme(panel.background = element_rect(fill = '#FFE6B1', colour = 'black'))
  |
    p4 + theme(panel.background = element_rect(fill = '#FFFEB1', colour = 'black'))
)/p5 + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
) &
  ylim(30, 55)








# LINEAR MODELS ----

treatment_lmodel <- lm(diplo_intensity_log ~ treatment, data = diplo_stickleback)
summary(treatment_lmodel) # ANOVA

check_model(treatment_lmodel)
#violates normality of residues

#square root transformation

sqrt_treatment_lmodel <- lm(sqrt(diplo_intensity_log) ~ treatment, data = diplo_stickleback)
summary(sqrt_treatment_lmodel)
check_model(sqrt_treatment_lmodel)
# improved normality of residues

confint(sqrt_treatment_lmodel)


length_treatment_lmodel <- lm(length_mm ~ treatment, data = diplo_stickleback)
summary(length_treatment_lmodel) #ANOVA

check_model(length_treatment_lmodel)

confint(length_treatment_lmodel)

#square root transformation

sqrt_length_treatment_lmodel <- lm(sqrt(length_mm) ~ treatment, data = diplo_stickleback)
summary(sqrt_length_treatment_lmodel)
check_model(sqrt_length_treatment_lmodel)

confint(sqrt_length_treatment_lmodel)

#log transformation

log_length_treatment_lmodel <- lm(log(length_mm) ~ treatment, data = diplo_stickleback)
summary(sqrt_length_treatment_lmodel)
check_model(log_length_treatment_lmodel)

# slightly violates normality of residues and posterior positive check
# neither data transformation improved the assumptions

sex_length_lmodel <- lm(length_mm ~ sex, data = diplo_stickleback)
summary(sex_lmodel) # linear model

confint(sex_length_lmodel)

