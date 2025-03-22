## 5. ESTIMATING CAUSAL EFFECTS WITH OBSERVATIONAL DATA

## 5.3 THE EFFECT OF RUSSIAN TV ON UKRAINIANS' VOTING BEHAVIOR
pacman::p_load(here, tidyverse)

## Load the dataset
uas <- read_csv(here("DSS/DSS/UA_survey.csv")) # reads and stores data

## Understand the data
## (Read about description of variables and unit of observation)
head(uas) # shows first observations

## Identify the types of variables included
## (character vs. numeric; binary vs. non-binary)

## Identify the number of observations
dim(uas) # provides dimensions of dataframe: rows, columns

### 5.3.1 USING THE SIMPLE LINEAR MODEL TO COMPUTE 
### THE DIFFERENCE-IN-MEANS ESTIMATOR

## Option A: Compute the difference-in-means estimator directly
mean(uas$pro_russian_vote[uas$russian_tv==1])-
  mean(uas$pro_russian_vote[uas$russian_tv==0])

## Option B: Fit linear model

modelo_lin_1 <- lm(pro_russian_vote ~ russian_tv, data=uas) 

uas %>% ggplot(aes(x = russian_tv,
           y = pro_russian_vote)) +
  geom_jitter(width = .1, height = .1, alpha = .4, color = "steelblue") +
  stat_summary(color = "darkorange") +
  geom_abline(intercept = modelo_lin_1$coefficients[1],
              slope = modelo_lin_1$coefficients[2],
              color = "darkorange", alpha = .5) +
  labs(x = "TV rusa", y = "Voto pro-Rusia") +
  theme_minimal()
  

modelo_log_1 <- glm(pro_russian_vote ~ russian_tv, data=uas, family = "binomial") 

modelo_log_1
parameters::model_parameters(modelo_log_1)
parameters::model_parameters(modelo_log_1, exponentiate =TRUE)
### 5.3.2 CONTROLLING FOR CONFOUNDERS 
### USING A MULTIPLE LINEAR REGRESSION MODEL

## Compute correlation
cor(uas$within_25km, uas$russian_tv)


uas %>% ggplot(aes(x = russian_tv,
                   y = within_25km)) +
  geom_jitter(width = .1, height = .1, alpha = .4, color = "steelblue") +
  stat_summary(color = "darkorange") +
  labs(x = "TV rusa", y = "Distancia < 25km") +
  theme_minimal()

## Create two-way table of frequencies
table(uas$within_25km, uas$russian_tv)

## Fit linear model
modelo_lin_2 <- lm(pro_russian_vote ~ russian_tv + within_25km, data=uas) 
parameters::model_parameters(modelo_lin_2)
## 5.4 THE EFFECT OF RUSSIAN TV ON UKRAINIAN ELECTORAL OUTCOMES

## Load the dataset
uap <- read_csv(here("DSS/DSS/UA_precincts.csv")) # reads and stores data

## Understand the data
## (Read about description of variables and unit of observation)
head(uap) # shows first observations

uap %>% ggplot(aes(x = as.factor(russian_tv),
                   y = pro_russian - prior_pro_russian)) +
  geom_jitter(width = .1, height = 0, alpha = .1, color = "steelblue") +
  stat_summary(color = "darkorange") +
  labs(x = "TV rusa", y = "Cambio de % de voto pro-Rusia") +
  theme_minimal()

uap %>% ggplot(aes(x = pro_russian - prior_pro_russian,
                   y = after_stat(density))) +
  geom_histogram(color = "white", fill = "steelblue", alpha = .4) +
  geom_density(color = "darkorange", linewidth = 1) +
  labs(x = "Cambio de % de voto pro-Rusia", 
       y = "Densidad") +
  theme_minimal()

## Identify the types of variables included
## (character vs. numeric; binary vs. non-binary)

## Identify the number of observations
dim(uap) # provides dimensions of dataframe: rows, columns

### 5.4.1 USING THE SIMPLE LINEAR MODEL TO COMPUTE 
### THE DIFFERENCE-IN-MEANS ESTIMATOR

## Create pro-Russian change variable
uap$pro_russian_change <- uap$pro_russian - uap$prior_pro_russian

## Create histogram
hist(uap$pro_russian_change)

## Fit linear model
change_model_1 <- lm(pro_russian_change ~ russian_tv, data=uap)
summary(change_model_1)
parameters::model_parameters(change_model_1)
uap %>% ggplot(aes(x = russian_tv,
                   y = pro_russian_change)) +
  geom_jitter(width = .1, height = 0, alpha = .1, color = "steelblue") +
  stat_summary(color = "darkorange") +
  geom_abline(intercept = change_model_1$coefficients[1],
              slope = change_model_1$coefficients[2],
              color = "darkorange", alpha = .5) +
  labs(x = "TV rusa", y = "Cambio de % de voto pro-Rusia") +
  theme_minimal()

### 5.4.2 CONTROLLING FOR CONFOUNDERS 
### USING A MULTIPLE LINEAR REGRESSION MODEL

## Compute correlation
cor(uap$within_25km, uap$russian_tv)

## Fit linear model
change_model_2 <- lm(pro_russian_change ~ russian_tv + within_25km, data=uap) 
parameters::model_parameters(change_model_2)


uap %>% ggplot(aes(x = within_25km,
                   y = pro_russian_change)) +
  geom_jitter(width = .1, height = 0, alpha = .1, color = "steelblue") +
  labs(y = "Cambio de % de voto pro-Rusia", 
       x = "Cercanía a la frontera") +
  stat_summary(color = "darkorange") +
  theme_minimal()

# Black candidates
black_tbl <- read_csv(here("DSS/additional_exercises_by_chapter/5_OBSERVATIONAL/black-candidates-black-turnout/black-candidates-black-turnout-files/districts.csv"))
data("congress")

congress_2015_tbl <- congress |> 
  dplyr::filter(year == 2006) |>
  select(-c("state", "year")) |>
  dplyr::rename(state = "st", district = "district_number")
congress_2015_tbl

data_full <- black_tbl |> left_join(congress_2015_tbl)
data_full |> write_csv(here("data/black_turnout.csv"))
codebook |> write_csv(here("data/black_turnout_codebook.csv"))
