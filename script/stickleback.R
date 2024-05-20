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
library(gt)
library(gtExtras)

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
  

# Define Colours

col_control <- 'white'
col_infected_hg <- "#FF0500"
col_infected_lg <- "#FFBE00"
col_uninfected <- "#FCFF19"


# PLOT 1 ------


treatment_vs_intensity <- function(field, colour) {
  fields <- c(field)
  if (field != "Control"){
    fields <- append(fields, "Control")
  }

  p <-ggplot(
    filter(.data = diplo_stickleback, treatment %in% fields),
    aes(x = diplo_intensity_log,
        fill = treatment))+
    geom_density(
      alpha = 0.6,
      weight = 0.5)+
    gghighlight()+
    labs(y = "Density", x= "Diplo Intensity")+
    scale_fill_manual(
      values = c("white",colour))+
    theme_classic()+
    theme(legend.position = "none")+
    geom_vline(data=NULL,
               aes(xintercept=
                     diplo_stickleback %>%
                     filter(treatment == "Control") %>%
                     pull(diplo_intensity_log) %>%
                     mean(na.rm=T)),
               colour="black",
               linetype="dashed") &
    xlim(0,4)
  
  if (field != "Control") {
    p <- p +  geom_vline(data=NULL,
                        aes(xintercept=
                              diplo_stickleback %>%
                              filter(treatment == field) %>%
                              pull(diplo_intensity_log) %>%
                              mean(na.rm=T)
                            ),
                        colour="blue",
                        linetype="dashed")
  }
  return (p)
}

plot_1 <- treatment_vs_intensity("Control", col_control) +
  treatment_vs_intensity("Uninfected", col_uninfected) +
  treatment_vs_intensity("Infected LG", col_infected_lg) +
  treatment_vs_intensity("Infected HG", col_infected_hg)

plot_1


#PLOT 2 ----


plot_density_treatment <- function(treatment_arg) {
  
  cols <- list()
  cols['Control'] <- 'grey'
  cols['Infected HG'] <- col_infected_hg
  cols['Infected LG'] <- col_infected_lg
  cols['Uninfected'] <- col_uninfected
  
  full_range <- diplo_stickleback |>
    pull(diplo_intensity_log) |>
    range()
  
  diplo_stickleback |>
    filter(treatment == treatment_arg) |>
    ggplot(aes(x = diplo_intensity_log, y = treatment)) +
    geom_violin(fill = cols[treatment_arg]) +
    theme_minimal() +
    scale_y_discrete(breaks = NULL) +
    scale_x_continuous(breaks = NULL) +
    labs(x = element_blank(), y = element_blank()) +
    coord_cartesian(xlim = full_range)
}



mean_table <- diplo_stickleback |>
  group_by(treatment) |>
  summarise(
    Min = min(diplo_intensity_log) |> round(digits = 2),
    Mean = mean(diplo_intensity_log) |> round(digits = 2),
    Max = max(diplo_intensity_log) |> round(digits = 2)
  ) |>
  mutate(treatment = as.character(treatment)) |>
  rename(Treatment = treatment) |>
  mutate(Distribution = Treatment) |> 
  gt() |>
  tab_spanner(
    label = 'Parasite B Intensity',
    columns = -Treatment
  ) |>
  text_transform(
    locations = cells_body(columns = 'Distribution'),
    fn = function(column) {
      map(column, plot_density_treatment) |>
        ggplot_image(height = px(50), aspect_ratio = 3)
    }
  ) 




# PLOT 3 -----


sex_graph <- function(treat, fill_col){diplo_stickleback %>% drop_na %>%
    filter(treatment == treat) %>%
    ggplot(aes(x = sex, y = length_mm))+
    labs(x='Sex', y='Length (mm)')+
    geom_violin(aes(colour=sex, fill=sex),
                alpha = 1,
                width = 1,
                show.legend = FALSE)+
    geom_boxplot(colour="black",
                 fill="white",
                 alpha = 1,
                 width = 0.4,
                 outlier.shape=NA,
                 )+
    theme(
      panel.background = element_rect(fill = fill_col, colour = 'black'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
      )
  
}



treatment_colours <- c("white", "white", "#FF0500", "#FFBE00", "#FCFF19")

p5 <- diplo_stickleback %>% drop_na %>%
  ggplot(aes(x = treatment, y = length_mm, fill=treatment))+
  labs(x='Treatment', y='Length (mm)')+
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
  scale_fill_manual(values = treatment_colours)+
  theme(legend.position = "none",
        panel.background = element_rect(fill = 'white', colour = 'black'))


plot_3 <- ((
    sex_graph("Control", "white")
  |
    sex_graph("Infected HG", '#FFB1B1')
  |
    sex_graph("Infected LG", '#FFE6B1') 
  |
    sex_graph("Uninfected", '#FFFEB1')
) / p5) &
  ylim(30, 55)

plot_3






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
summary(sex_length_lmodel) # linear model

confint(sex_length_lmodel)




