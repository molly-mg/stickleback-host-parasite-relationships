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
  

diplo_stickleback %>%
  
  ggplot(aes(x = treatment, y = diplo_intensity_log))+
  geom_boxplot(aes(colour = treatment),
               alpha = 0.2,
               width = 0.5,
               outlier.shape=NA)+
  #geom_jitter(aes(colour = treatment),
              #width=0.2)+
  geom_violin(aes(colour=treatment, fill=treatment),
              alpha = 0.2,
              width = 1,
              show.legend = FALSE)
theme(legend.position = "none")

diplo_intensity_mean <- diplo_stickleback %>%
  summarise(mean_diplo_intensity=mean(diplo_intensity_log, na.rm=T))


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


diplo_stickleback %>%
  drop_na(sex) %>%
  ggplot(aes( x = treatment,
              y = length_mm))+
  ggdist::stat_interval()+
  ggdist::stat_halfeye(aes(fill = sex),
                       .width = 0,
                       shape = 21,
                       colour = "white",
                       slab_alpha = .4,
                       size = .5,
                       position = position_nudge(x = 0.1))
  


#v <- mean(filter(diplo_stickleback, treatment == "Infected HG")$diplo_intensity_log, na.rm=T)



# PLOT 4 -----

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
    width = 0.5,
    outlier.shape=NA)+
  
  theme(legend.position = "none")+
  scale_fill_manual(values = treatment_colours)

((
  p1 + theme(panel.background = element_rect(fill = 'green', colour = 'red'))
  |
    p2 + theme(panel.background = element_rect(fill = 'blue', colour = 'orange'))
  |
    p3 + theme(panel.background = element_rect(fill = 'blue', colour = 'orange'))
  |
    p4 + theme(panel.background = element_rect(fill = 'blue', colour = 'orange'))
  )/p5 + theme(panel.background = element_rect(fill = 'blue', colour = 'orange'))
  ) &
  ylim(30, 55)





# PLOT 1 ------


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


plot_1 <- d1+d2+d3 # chosen plot 1

plot_1




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




length_treatment_lmodel <- lm(length_mm ~ treatment, data = diplo_stickleback)
summary(length_treatment_lmodel) #ANOVA

check_model(length_treatment_lmodel)

#square root transformation

sqrt_length_treatment_lmodel <- lm(sqrt(length_mm) ~ treatment, data = diplo_stickleback)
summary(sqrt_length_treatment_lmodel)
check_model(sqrt_length_treatment_lmodel)

#log transformation

log_length_treatment_lmodel <- lm(log(length_mm) ~ treatment, data = diplo_stickleback)
summary(sqrt_length_treatment_lmodel)
check_model(log_length_treatment_lmodel)

# slightly violates normality of residues and posterior positive check
# neither data transformation improved the assumptions

sex_lmodel <- lm(length_mm ~ sex, data = diplo_stickleback)
summary(sex_lmodel) # linear model

























# 14.4.1 to get the other mean/standard error/CIs

# standard error (and therefore CIs) are the same for all at the variance is pooled, they vary numerically thanks to the sample sizes

performance::check_model(treatment_model)
# slightly violates normality of reisdues

performance::check_model(treatment_model, check=c("normality","qq"))

plot(treatment_model, which=c(2,2))

performance::check_model(treatment_model, check="homogeneity")

plot(treatment_model, which=c(1,3))

performance::check_model(treatment_model, check="outliers")

plot(treatment_model, which=c(4,4))

# reasonably good model on the whole (normality of residues is iffy)
# a model which violates assumptions may be difficult to apply to irl situations

#t-test (paired)
 
diplo_stickleback %>%
  mutate(pair = as_factor(pair)) %>%
  lm(diplo_intensity_log ~ treatment + pair, data = .) %>%
  broom::tidy()


# Pearson's R
diplo_stickleback %>%
  cor_test(diplo_intensity_log, length_mm)



# ANOVA

anova(treatment_model)













diplo_stickleback %>% drop_na %>%
  ggplot(aes(x = treatment, y = length_mm))+
  geom_boxplot(aes(fill = treatment),
               alpha = 0.2,
               width = 0.5,
               outlier.shape=NA)+
  geom_jitter(aes(colour = sex),
              width=0.2)+
  scale_fill_manual(
    values = c("white", 
               "green",
               "orange",
               "red"))+
  theme_classic()+
  theme(legend.position = "none")










diplo_stickleback %>%
  ggplot(aes(x = treatment, y = diplo_intensity_log))+
  geom_boxplot(aes(fill = treatment),
               alpha = 0.2,
               width = 0.5,
               outlier.shape=NA)+
  geom_jitter(aes(colour = treatment),
              width=0.2)+
  scale_fill_manual(
    values = c("white", 
               "green",
               "orange",
               "red"))+
  theme_classic()+
  theme(legend.position = "none") # plot 2?








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
  theme(legend.position = "none")


  
  
stickleback %>%
  group_by(treatment, sex) %>%
  summarise(n_distinct(id))

summarise(diplo_stickleback)  


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
  summarise(n = n())
              
diplo_stickleback %>%
  filter(treatment == 'Infected LG') %>%
  ggplot()+
  geom_histogram(aes(x=length_mm),
                 bins=15)


diplo_stickleback %>%
  filter(treatment == 'Infected LG') %>%
  qqplot(diplo_stickleback, aes(sample = treatment))+
  stat_qq() +
  stat_qq_line()


diplo_stickleback %>% drop_na %>% ggplot(aes(x = diplo_intensity_log, y = treatment)) +
  geom_density_ridges(aes(fill = treatment),
                      alpha = 0.8,
                      bandwidth = 0.2)



diplo_stickleback %>%
  #filter(treatment=='Infected LG') %>%
  ggplot(aes(x = treatment, y = length_mm))+
  geom_boxplot(aes(fill = treatment),
               alpha = 0.2,
               width = 0.5,
               outlier.shape=NA)+
  geom_jitter(aes(colour = treatment),
              width=0.2)+
  geom_violin(aes(colour=treatment, fill=treatment),
              alpha = 0.2,
              width = 1,
              show.legend = FALSE)
  theme(legend.position = "none")
  

  stickleback %>% ggplot(aes(x = length_mm, y = diplo_intensity_log)) +
    geom_density_ridges(aes(fill = treatment),
                        alpha = 0.8,
                        bandwidth = 0.2)  
  
diplo_stickleback %>% 
  filter(treatment == "Control") %>%
  ggplot(aes(x=diplo_intensity_log,
             y = length_mm,
             colour=treatment))+
  geom_point()+
  geom_smooth(method="lm",
              se=FALSE)+
  theme_void()
  
eye_stickleback <- select(.data = stickleback,
                          treatment, diplo_right_eye, diplo_left_eye, sex, length_mm, diplo_intensity_log)
  
#eye_stickleback <- mutate(.data = eye_stickleback,
                          #compare_eye = paste(
                            #as.character(diplo_left_eye),
                            #as.character(diplo_right_eye),
                            #sep=","))



stickleback %>% drop_na %>% ggplot(aes(x = diplo_right_eye, y = treatment)) +
  geom_density_ridges(aes(fill = treatment),
                      alpha = 0.8,
                      bandwidth = 0.6)  



diplo_stickleback  %>% ggplot(aes(x = diplo_intensity_log, y = treatment)) +
  geom_density_ridges(aes(fill = treatment),
                      alpha = 0.8,
                      bandwidth = 0.2)


stickleback %>% ggplot(aes(x = diplo_right_eye, y = sex)) +
  geom_density_ridges(aes(fill = sex),
                      alpha = 0.8,
                      bandwidth = 0.8)



diplo_stickleback_summary <- stickleback %>%
  summarise(mean_diplo_intensity=mean(diplo_intensity_log, na.rm = T),
            sd = sd(diplo_intensity_log, na.rm = T),
            median_diplo_intensity=median(diplo_intensity_log, na.rm = T),
            iqr = IQR(diplo_intensity_log, na.rm = T))

diplo_stickleback_summary


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
  theme_classic()

ggplot(stickleback, aes(sample = diplo_intensity_log))+
  stat_qq()+
  stat_qq_line()


stickleback %>%
  pull(diplo_intensity_log) %>%
  car::qqPlot()




colour_fill <- "darkorange"
colour_line <- "steelblue"
lims <- c(0,5)

diplo_intensity_plot <- function(){
  
  stickleback %>% 
    ggplot(aes(x="",
               y= diplo_intensity_log))+
    labs(x= " ",
         y = "Diplo Intensity")+
    scale_y_continuous(limits = lims)+
    theme_minimal()
}

plot_1 <- diplo_intensity_plot()+
  geom_jitter(fill = colour_fill,
              colour = colour_line,
              width = 0.2,
              shape = 21)

plot_2 <- diplo_intensity_plot()+
  geom_boxplot(fill = colour_fill,
               colour = colour_line,
               width = 0.4)

plot_3 <- diplo_stickleback_summary %>% 
  ggplot(aes(x = " ",
             y = mean_diplo_intensity))+
  geom_bar(stat = "identity",
           fill = colour_fill,
           colour = colour_line,
           width = 0.2)+
  geom_errorbar(data = diplo_stickleback_summary,
                aes(ymin = mean_diplo_intensity - sd,
                    ymax = mean_diplo_intensity + sd),
                colour = colour_line,
                width = 0.1)+
  labs(x = " ",
       y = "Diplo Intensity")+
  scale_y_continuous(limits = lims)+
  theme_minimal()


plot_1 + plot_2 + plot_3 # NOT working


stickleback %>% 
  ggplot(aes(x=diplo_intensity_log,
             fill=treatment))+
  geom_histogram(alpha=0.6,
                 bins=30,
                 position="identity")+
  facet_wrap(~treatment,
             ncol=1)

stickleback_summary <- stickleback %>% 
  group_by(treatment) %>% 
  summarise(mean=mean(diplo_intensity_log),
            sd=sd(diplo_intensity_log))

stickleback_summary %>%
  ggplot(aes(x=treatment,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()


lsmodel0 <- lm(formula = diplo_intensity_log ~ 1, data = stickleback)

summary(lsmodel0)

stickleback %>%
  ggplot(aes(x=treatment,
             y=diplo_intensity_log,
             colour=treatment))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw() # include


lsmodel1 <- lm(sex ~ treatment, data=stickleback)

broom::tidy(lsmodel1)

GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95) # include?


performance::check_model(lsmodel1)


sum(stickleback[["diplo_right_eye"]])


stickleback %>% 
  filter(diplo_left_eye) %>%
  is.na() %>% 
  sum()
