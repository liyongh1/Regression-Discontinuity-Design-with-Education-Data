## UofT iSchool INF2178 PS5
## Author: Zhaotian Li, Yonghao Li, Hongtianxu Hua
## Date: 8 Apr 2020

library(skimr)
library(ggplot2)
library(rddensity)
library(rdrobust)
library(broom)
library(tidyverse)
library(ggpubr)

#########################
# Import and cleaning data
#########################

# import data from github
dt_raw <- read_csv("https://raw.githubusercontent.com/georgehua/INF2178-PS/master/PS5/CIT_2019_Cambridge_education.csv")

# only select distance to GPA cutoff from -1.6 to 1.6
dt <- dt_raw %>%
  filter(X >= -1.6 & X <= 1.6)

#########################
# Summary stats
#########################
# sanity check of original dataset
skim(dt_raw)

# how many total data points after filtering
nrow(dt)

# how many unique values after filtering
length(unique(dt$X))

# frequency distribution of the running variable X
ggplot(dt, aes(x=X)) +
  geom_histogram(breaks=seq(-1.6, 0, by = 0.1), col="black", fill="pink", alpha = 1)+
  geom_histogram(breaks=seq(0, 1.6, by = 0.1), col="black", fill="orange", alpha = 1)+
  geom_vline(aes(xintercept=0),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  ggtitle("Frequency distribution of the running variable (distance from GPA cutoff)") +
  theme_bw()

#########################
# Assumptions
#########################

## Running variable continuous at the cutoff ##

# robust test
density_check <- rddensity(dt$X)
summary(density_check)

# Density plot test
rdplotdensity(density_check, dt$X, xlabel = "Distance to the GPA cut-off",
              ylabel = "Density", title = "Density plot of X (Distance to the GPA cut-off)")


## Continuous for other revelent variables ##

# These summaries provide raw outputs
# We created table 5.2.2 manually by inserting the results
summary(rdrobust(dt$hsgrade_pct, dt$X))
summary(rdrobust(dt$totcredits_year1, dt$X))
summary(rdrobust(dt$age_at_entry, dt$X))
summary(rdrobust(dt$male, dt$X))
summary(rdrobust(dt$bpl_north_america, dt$X))
summary(rdrobust(dt$english, dt$X))
summary(rdrobust(dt$loc_campus1, dt$X))
summary(rdrobust(dt$loc_campus2, dt$X))
summary(rdrobust(dt$loc_campus3, dt$X))

#########################
# Model and Results
#########################
# Probation on Immediate decision, by gender

## FEMALE ##
female_rdd <- dt %>% filter(male == 0)
rdplot(female_rdd$left_school, female_rdd$X,  x.label = "Distance to GPA cut-off",
       y.label = "Left University Voluntariliy", title="Effect of Academic Probation on Attrition, Female", col.dots="red")
summary(rdrobust(female_rdd$left_school, female_rdd$X))

## MALE ##
male_rdd <- dt %>% filter(male == 1)
rdplot(male_rdd$left_school, male_rdd$X,  x.label = "Distance to GPA cut-off", 
       y.label = "Left University Voluntariliy", title="Effect of Academic Probation on Attrition, Male", col.dots="red")
summary(rdrobust(male_rdd$left_school, male_rdd$X))

# Probation on Immediate decision, by native-language

## ENGLISH ##
eng_rdd <- dt %>% filter(english == 1)
rdplot(eng_rdd$left_school, eng_rdd$X,  x.label = "Distance to GPA cut-off", 
       y.label = "Left University Voluntariliy", title="Effect of Native Language on Attrition, English", col.dots="red")
summary(rdrobust(eng_rdd$left_school, eng_rdd$X))

## NON-ENGLISH ##
non_eng_rdd <- dt %>% filter(english == 0)
rdplot(non_eng_rdd$left_school, non_eng_rdd$X,  x.label = "Distance to GPA cut-off", 
       y.label = "Left University Voluntariliy", title="Effect of Native Language on Attrition, Non-English", col.dots="red")
summary(rdrobust(non_eng_rdd$left_school, non_eng_rdd$X))

