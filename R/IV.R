library(tidyverse)
library(patchwork)
library(broom)
library(modelsummary) 
library(gtExtras)
library(estimatr)  

ed_fake <- read_csv("data/father_education.csv")

model_prohibido <- lm(wage ~ educ + ability, data = ed_fake)
tidy(model_prohibido)

model_naive <- lm(wage ~ educ, data = ed_fake)
tidy(model_naive)

modelsummary(list("Naive" = model_naive, 
                  "Prohibido" = model_prohibido),
             output = "gt") %>%
  gt_highlight_rows(rows = 3, 
                    fill = "lightyellow",
                    font_weight = "bold")

# Datos falsos ####
## Relevancia ####
ed_fake |> ggplot(aes(x = fathereduc,
           y = educ)) +
  geom_point(alpha = .4, pch = 21, fill = "black") +
  geom_smooth(method = lm, color = "darkorange") +
  labs(x = "Educación del padre (años)", y = "Educación (años)") +
  theme_bw()

primera_etapa <- lm(educ ~ fathereduc, data = ed_fake)
tidy(primera_etapa)
glance(primera_etapa)  

## Exclusión ####
ed_fake |> ggplot(aes(x = fathereduc,
                      y = wage)) +
  geom_point(alpha = .4, pch = 21, fill = "black") +
  geom_smooth(method = lm, color = "darkorange") +
  labs(x = "Educación del padre (años)", y = "Salario (semana)") +
  theme_bw()

## Exogeneidad ####
# Ojo que esto en el mundo real no lo podemos ver
ed_fake |> ggplot(aes(x = fathereduc,
                      y = ability)) +
  geom_point(alpha = .4, pch = 21, fill = "black") +
  geom_smooth(method = lm, color = "darkorange") +
  labs(x = "Educación del padre (años)", y = "Habilidad") +
  theme_bw()

## 2SLS ####
primera_etapa <- lm(educ ~ fathereduc, data = ed_fake)
tidy(primera_etapa)
glance(primera_etapa)

ed_fake_predictions <- augment_columns(primera_etapa, data = ed_fake) %>% 
  rename(educ_hat = .fitted)
head(ed_fake_predictions)

p1 <- 
  ed_fake_predictions |> ggplot(aes(x = educ,
                                    y = wage)) +
  geom_point(alpha = .4, pch = 21, fill = "black") +
  geom_smooth(method = lm, color = "darkorange") +
  labs(x = "Educación (años)", y = "Salario", title = "Educación y salario") +
  theme_bw()

p2 <- 
  ed_fake_predictions |> ggplot(aes(x = educ_hat,
                                    y = wage)) +
  geom_point(alpha = .4, pch = 21, fill = "black") +
  geom_smooth(method = lm, color = "darkorange") +
  labs(x = "Educación hat (años)", y = "Salario", title = "Educación ajustada y salario") +
  theme_bw()

p1 | p2

# La diferencia enter education and educ_hat
ed_fake_predictions |> ggplot(aes(x = educ,
                                  y = educ-educ_hat)) +
  geom_point(alpha = .4, pch = 21, fill = "black") +
  geom_smooth(method = lm, color = "darkorange") +
  labs(x = "Educación (años)", y = "Educación - educación hat (años)") +
  theme_bw()

# Segunda etapa
segunda_etapa <- lm(wage ~ educ_hat,
                    data = ed_fake_predictions)
tidy(segunda_etapa)

modelsummary(list("Naive" = model_naive, 
                  "Prohibido" = model_prohibido,
                  "2SLS" = segunda_etapa),
             statistic = NULL,
             estimate = "{estimate} {stars}",
             gof_omit = ".*",
             output = "gt") %>%
  gt_highlight_rows(rows = c(2,4), 
                    fill = "lightyellow",
                    font_weight = "bold")

## Usando estimatr ####
model_iv_robust <- iv_robust(wage ~ educ | fathereduc,
                             data = ed_fake)
tidy(model_iv_robust)


modelsummary(list("Naive" = model_naive, 
                  "Prohibido" = model_prohibido,
                  "2SLS" = segunda_etapa,
                  "IV robust" = model_iv_robust),
             statistic = NULL,
             estimate = "{estimate} ({std.error})",
             gof_omit = ".*",
             output = "gt") %>%
  gt_highlight_rows(rows = c(2,4), 
                    fill = "lightyellow",
                    font_weight = "bold")
