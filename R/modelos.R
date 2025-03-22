library(ISLR2)
library(here)
library(MASS)
library(tidyverse)
library(parameters)

# Leemos los datos ####
Advertising <- read_csv(here("data/Advertising.csv")) |>
  select(-"...1")
# ventas en miles de unidades y gasto en miles de USD

# El modelo simple ####
model <- lm(data = Advertising, sales ~ TV)
summary(model)
plot(model)

# El modelo full ####
model_full <- lm(data = Advertising, sales ~ .)
summary(model_full)
plot(model_full)
confint(model_full)
parameters::model_parameters(model_full)

## Predicción con el modelo simple ####
pred <- model_full$coefficients[1] + 120 * model_full$coefficients[2] + 
  30 * model_full$coefficients[3] + 30 * model_full$coefficients[4]
predict(model_full, newdata = tibble(TV = 120, radio = 30, newspaper =30))

# Logística ####
Default |> ggplot(aes(x = balance,
                      y = default)) +
  geom_point(alpha = .2) +
  theme_bw()

Default |> mutate(default_num = if_else(default == "Yes", 1, 0)) |>
  ggplot(aes(x = balance,
             y = default_num)) +
  geom_point(alpha = .2) +
  geom_smooth() +
  theme_bw()
  
linear_model <- lm(data = Default |> mutate(default_num = if_else(default == "Yes", 1, 0)), 
                   default_num ~ balance)
linear_model

Default |> mutate(default_num = if_else(default == "Yes", 1, 0)) |>
  ggplot(aes(x = balance,y = default_num)) +
  # geom_abline(intercept = linear_model$coefficients[1], slope = linear_model$coefficients[2],
  #             color = "steelblue", linewidth = 2) +
  geom_point(alpha = .2) +
  theme_bw()

log_reg <- glm(data = Default |> mutate(default_num = if_else(default == "Yes", 1, 0)), 
               default_num ~ balance, family = "binomial")
log_reg


fit <- tibble(
  balance   = seq(0, 3000, 1),
  default_num = exp(log_reg$coefficients[1]+log_reg$coefficients[2]*balance)/
    (1+exp(log_reg$coefficients[1]+log_reg$coefficients[2]*balance))
)

Default |> mutate(default_num = if_else(default == "Yes", 1, 0)) |>
  ggplot(aes(x = balance,y = default_num)) +
  # geom_abline(intercept = linear_model$coefficients[1], slope = linear_model$coefficients[2],
  #             color = "steelblue", linewidth = 2) +
  geom_point(alpha = .2) +
  geom_line(data = fit, aes(x = balance, y = default_num),
            color = "darkorange", linewidth = 2) +
  theme_bw()

balance_new = 1600

exp(log_reg$coefficients[1]+log_reg$coefficients[2]*balance_new)/ (1+exp(log_reg$coefficients[1]+log_reg$coefficients[2]*balance_new))
predict(log_reg, newdata = tibble(balance = 1600))

exp(-1.853064)/(1+exp(-1.853064 ))
predict(log_reg, newdata = tibble(balance = 1600), type = "response")
