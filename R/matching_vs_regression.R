library(tidyverse)

# Genero la data ####
df <- tibble(
  # x es un confusor
  x = runif(1000, -1, 4),
  # Afecta la probabilidad de recibir el tratamiento de forma NO LINEAL (función escalón)
  prob_d = ifelse(x > 0.5 & x < 2.5, 0.1, 0.9),
  d = rbinom(1000, 1, prob_d),
  noise = rnorm(1000, sd = 0.1),
  # Pra simplificar, el ATE es homogeneo y vale 1
  treat_effect = 1,
  # x afecta al outcome de manera no lineal (una función seno)
  outcome = sin(x) + d*treat_effect + noise
) %>% 
  mutate(d_factor = factor(d,
                           levels=c(0,1), labels=c("No tratado",
                                                   "Tratado")))

ggplot(df,
       aes(x, outcome, color = d_factor)) +
  geom_point() + 
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Estado del tratamiento") +
  theme_bw() +
  theme(legend.position = "top")

# Ajustemos un modelo lineal ####
linear_model1 <- lm(outcome ~ d + x, data = df)
#install.packages("parameters")
library(parameters)
model_parameters(linear_model1)

# Con matching ####
library(MatchIt)
nearest_control <- matchit(d ~ x, 
                           data = df,
                           method = "nearest", 
                           distance = "mahalanobis",
                           replace = T,
                           ratio = 1)

match_df <- match.data(nearest_control)

mathing_model1 <- lm(outcome ~ d + x, 
                     data = match_df, 
                     weights = weights)

model_parameters(mathing_model1)

# Veamos los ajustes graficamente ####
#install.packages("FNN")
library(FNN)
knn1 <- knn.reg(
  train = dplyr::select(df, x, d),
  y = df$outcome,
  test = dplyr::select(df, x, d),
  k = 10,
  algorithm = "brute"
)

df_knn <- df %>% 
  mutate(pred_knn = knn1$pred)

# Regression
df_linear <- df %>%
  modelr::add_predictions(linear_model1, var = "pred_linear")

# Plots
plot_matching <- 
  ggplot(df_knn) +
  aes(x = x, color = d_factor) +
  geom_point(aes(y = outcome), alpha = 0.3) +
  geom_line(aes(y = pred_knn), linewidth = 1.5) +
  labs(color = "Treatment status") +  
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Estado del tratamiento") +
  theme_bw() +
  theme(legend.position = "top")
plot_matching

plot_linear <-
  ggplot(df_linear) +
  aes(x = x, color = d_factor) +
  geom_point(aes(y = outcome), alpha = 0.3) +
  geom_line(aes(y = pred_linear), size = 1.5) +
  labs(color = "Treatment status") +
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Estado del tratamiento") +
  theme_bw() +
  theme(legend.position = "top")
plot_linear

# Asumamos que la relación es senoidal ####
linear_model_sin <- lm(outcome ~ d + sin(x), data = df)
#install.packages("parameters")
model_parameters(linear_model_sin)

# Regression
df_linear <- df %>%
  modelr::add_predictions(linear_model_sin, var = "pred_linear")

plot_linear_sin <-
  ggplot(df_linear) +
  aes(x = x, color = d_factor) +
  geom_point(aes(y = outcome), alpha = 0.3) +
  geom_line(aes(y = pred_linear), size = 1.5) +
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Estado del tratamiento") +
  theme_bw() +
  theme(legend.position = "top")
plot_linear_sin

# Datos sin soporte común ####
df_wo_common_support <- tibble(
  # x es un confusor
  x = runif(1000, -1, 4),
  # No hay más prob_d, es determinístico
  d = ifelse(x > 0.5 & x < 2.5, 0, 1),
  noise = rnorm(1000, sd = 0.1),
  # Pra simplificar, el ATE es homogeneo y vale 1
  treat_effect = 1,
  # x afecta al outcome de manera no lineal (una función seno)
  outcome = sin(x) + d*treat_effect + noise
) %>% 
  mutate(d_factor = factor(d,
                           levels=c(0,1), labels=c("No tratado",
                                                   "Tratado")))

ggplot(df_wo_common_support,
       aes(x, outcome, color = d_factor)) +
  geom_point() + 
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Estado del tratamiento") +
  theme_bw() +
  theme(legend.position = "top")

# Modelo lineal sin soporte comun ####
reg_wo_common_support <- lm(outcome ~ d + sin(x), data = df_wo_common_support)
model_parameters(reg_wo_common_support)

# Esto es sólo una cosa para plotar los datos
df_wo_common_support <- df_wo_common_support %>%
  mutate(group = case_when(
    x < 0.5 ~ "segment1",
    x > 2.5 ~ "segment3",
    TRUE ~ "segment2"
  ))

# Los labels a una función
creating_factor_d <- function(x) factor(x,
                                        levels = c(0, 1),
                                        labels = c("No tratado",
                                                   "Tratado"))

df_wo_cs_treated <- df_wo_common_support %>% 
  mutate(extrapolation = ifelse(d == 1, "No", "Yes"),
         d = 1,
         d_factor = creating_factor_d(d)) %>% 
  modelr::add_predictions(reg_wo_common_support, var = "pred_treated")

df_wo_cs_untreated <- df_wo_common_support %>% 
  mutate(extrapolation = ifelse(d == 0, "No", "Yes"),
         d = 0,
         d_factor = creating_factor_d(d)) %>% 
  modelr::add_predictions(reg_wo_common_support, var = "pred_untreated")

plot_wo_cs_reg <- 
  ggplot() +
  aes(x, outcome, color = d_factor) +
  geom_point(data= df_wo_common_support, alpha = 0.3) +
  geom_line(data = df_wo_cs_untreated,
            aes(y = pred_untreated,
                alpha = extrapolation,
                linetype = extrapolation,
                group = group), size = 1.5) +
  geom_line(data = df_wo_cs_treated,
            aes(y = pred_treated,
                alpha = extrapolation,
                linetype = extrapolation,
                group = group), size = 1.5) +
  scale_alpha_manual(values = c("Yes" = 0.5, "No" = 1)) +
  scale_linetype_manual(values = c("Yes" = "dashed", "No" = "solid")) +
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Estado del tratamiento",
       linetype = "Extrapolación",
       alpha = "Extrapolación") +
  theme_bw() +
  theme(legend.position = "left",
        legend.key.width=unit(2,"cm"))
plot_wo_cs_reg

# Matching sin soporte común ####
nearest_control <- matchit(d ~ x, 
                           data = df_wo_common_support,
                           method = "nearest", 
                           distance = "mahalanobis",
                           replace = T,
                           ratio = 1)

match_df_wo_common_support <- match.data(nearest_control)

mathing_wo_common_support <- lm(outcome ~ d + x, 
                     data = match_df_wo_common_support, 
                     weights = weights)

model_parameters(mathing_wo_common_support)

# Ahora ploteemos qué está pasando
x_values <- df_wo_common_support %>% dplyr::select(x)

knn_wo_cs_treated <- knn.reg(
  train = df_wo_common_support %>% 
    filter(d == 1) %>% 
    dplyr::select(x),
  y = df_wo_common_support %>% 
    filter(d == 1) %>% 
    dplyr::pull(outcome),
  test = x_values,
  k = 15,
  algorithm = "brute"
)

knn_wo_cs_untreated <- knn.reg(
  train = df_wo_common_support %>% 
    filter(d == 0) %>% 
    dplyr::select(x),
  y = df_wo_common_support %>% 
    filter(d == 0) %>% 
    dplyr::pull(outcome),
  test = x_values,
  k = 15,
  algorithm = "brute"
)

df_untr_matching_wo_cs <-
  tibble(
    y_pred = knn_wo_cs_untreated$pred,
    x = df_wo_common_support$x,
    d = 0
  ) %>%
  mutate(d_factor = creating_factor_d(d))

df_tr_matching_wo_cs <-
  tibble(
    y_pred = knn_wo_cs_treated$pred,
    x = df_wo_common_support$x,
    d = 1
  ) %>%
  mutate(d_factor = creating_factor_d(d),
         group = case_when(
           x < 0.5 ~ "segment1",
           x > 2.5 ~ "segment3",
           TRUE ~ "segment2"
         ))

plot_matching_wo_cs <- 
  ggplot() +
  aes(x, outcome, color = d_factor) +
  geom_point(data= df_wo_common_support, alpha = 0.3) +
  geom_line(data = df_untr_matching_wo_cs,
            aes(y = y_pred), size = 1.5) +
  geom_line(data = df_tr_matching_wo_cs,
            aes(y = y_pred, group = group), size = 1.5) +
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Estado del tratamiento") +
  theme_bw() +
  theme(legend.position = "top")

plot_matching_wo_cs
