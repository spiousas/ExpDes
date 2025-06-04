library(tidyverse)

# Simulación de los datos ####
set.seed(1989)

n_east <- 130
n_west <- 220
n_north <- 500
n_total <- n_east+n_west+n_north

customers <-
  tibble(
    customer_id = seq(n_total),
    region = c(rep("East", n_east),
               rep("West", n_west),
               rep("North", n_north)),
    # El efecto del tratamiento es 300USD + ruido [N(300,200)]
    treatment_effect = rnorm(n_total, mean = 300, sd = 200),
    # Ventas por cliente cuando no se ofrece ning'un descuento (no tratado)
    y0 = c(
      rnorm(n_east, mean = 3000, sd = 200),
      rnorm(n_west, mean = 2000, sd = 200),
      rnorm(n_north, mean = 3500, sd = 200)
    ),
    # Ventas cuando se ofrece un descuento
    y1 = y0 + treatment_effect,
    # El vector de asignación de tratamiento D
    d = c(
      rbinom(n_east, 1, 0.5),
      rbinom(n_west, 1, 0.8),
      rbinom(n_north, 1, 0.2)
    ),
    # Switching equation
    y = y0 + treatment_effect*d
    
  )
head(customers)

# SDO ####
SDO <-  mean(customers$y[customers$d==1]) - mean(customers$y[customers$d==0])

# subclas_estimator ####
SDO_east <- mean(customers$y[customers$region=="East" & customers$d==1]) - 
  mean(customers$y[customers$region=="East" & customers$d==0])
SDO_east

SDO_north <- mean(customers$y[customers$region=="North" & customers$d==1]) - 
  mean(customers$y[customers$region=="North" & customers$d==0])
SDO_north

SDO_west <- mean(customers$y[customers$region=="West" & customers$d==1]) - 
  mean(customers$y[customers$region=="West" & customers$d==0])
SDO_west

subclas_estimator <- SDO_east * (n_east/n_total) +
  SDO_north * (n_north/n_total) +
  SDO_west * (n_west/n_total)
subclas_estimator

# Una función para el estimador ####
subclas_estimator <- function(df, outcome = Y, treatment = D, covariates) {
  df %>% 
    # Creo estratos de las variables presentes en los datos
    group_nest({{ covariates }}) %>% 
    # Calculo la frecuencia de aparicion y el SDO para cada estrato
    mutate(freq = map_dbl(data, nrow),
           strata_sdo = map_dbl(data, sdo, {{ outcome }}, {{ treatment }})) %>% 
    # Y calculo el promedio pesado por las frecuencias relativas
    summarise(estimate_ATE = weighted.mean(strata_sdo, freq)) %>% 
    pull(estimate_ATE)
  
}

# Función auxiliar que calcula el SDO
sdo <- function(df, outcome = Y, treatment = D) {
  
  df <- df %>% 
    group_by({{treatment}}) %>% 
    summarise(mean = mean({{outcome}}))
  
  mean_t <- 
    df %>% 
    filter({{treatment}} == 1) %>% 
    pull(mean)
  
  mean_u <- 
    df %>% 
    filter({{treatment}} == 0) %>% 
    pull(mean)
  
  mean_t - mean_u
  
}

# Y si llamamos así al estimador obtenemos la estimación!
subclas_estimator(customers,
                  outcome = y,
                  treatment = d,
                  covariates = region)

# El ATE y los sesgos ya que tenemos los potential outcomes ####
ATE <- mean(customers$y1 - customers$y0)
ATT <- mean(customers$y1[customers$d==1] - customers$y0[customers$d==1])
ATU <- mean(customers$y1[customers$d==0] - customers$y0[customers$d==0])

# Sesgo de efecto heterogéneo
pi <- sum(customers$d)/nrow(customers)
HTEBias <- (1-pi) * (ATT - ATU)

# Sesgo de selección
SelBias <- mean(customers$y0[customers$d==1]) - mean(customers$y0[customers$d==0])

# Ambas coas dan lo mismo
HTEBias + SelBias + ATE
SDO

# Plots de independencia ####
# Armo un tibble para plotear
customers_4_plot <- customers %>% 
  select(region, d, y1, y0) %>% 
  mutate(d = factor(d, levels = c(0,1),
                    labels = c("Untreated", "Treated"))) %>% 
  pivot_longer(cols = c(y1, y0),
               names_to = "potential_outcome",
               values_to = "value")

# Independencia
ggplot(customers_4_plot) +
  aes(potential_outcome, value, colour = factor(d)) +
  geom_boxplot() +
  labs(x = "Potential Outcome",
       y = "Ventas por mes por cliente",
       colour = "Treatment status",) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(color = NULL) +
  theme(legend.position = "top")

# Independencia condicional a la región
ggplot(customers_4_plot) +
  aes(potential_outcome, value, colour = factor(d)) +
  geom_boxplot() +
  facet_wrap(~region) +
  labs(x = "Potential Outcome",
       y = "Ventas por mes por cliente",
       colour = "Treatment status",) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(color = NULL) +
  theme(legend.position = "top")
  
