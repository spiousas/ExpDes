library(tidyverse)
library(here)
library(ggthemes)
library(parameters)
library(performance)

data_black <- read_csv(here("data/black_turnout.csv"))

# Solo la relación causal asumiendo as-if random
data_black |>
  ggplot(aes(x = as.factor(black_candidate),
             y = black_turnout)) +
  geom_jitter(width = .1, alpha = .4, color = "steelblue") +
  labs(x = "Candidato negro", y = "Porcentaje de votantes") +
  theme_clean()

model_base <- lm(data = data_black, black_turnout ~ black_candidate)
model_parameters(model_base)

# Agreguemos a proportion_black como un confusor
data_black |>
  ggplot(aes(x = as.factor(black_candidate),
             y = proportion_black)) +
  geom_jitter(width = .1, alpha = .4, color = "steelblue") +
  labs(x = "Candidato negro", y = "Proporción de negros") +
  theme_clean()

model_proportion <- lm(data = data_black, black_turnout ~ black_candidate + proportion_black)
model_parameters(model_proportion)

# Agreguemos a total_population como un confusor
data_black |>
  ggplot(aes(x = proportion_black,
             y = black_turnout)) +
  geom_jitter(width = .1, alpha = .4, color = "steelblue") +
  labs(x = "Candidato negro", y = "Proporción de negros") +
  theme_clean()

model_totalpop <- lm(data = data_black, black_turnout ~ black_candidate + proportion_black + median_houseincome)
model_parameters(model_totalpop)

cor(data_black$totalpop, data_black$black_turnout)
