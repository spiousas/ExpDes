# install.packages("ggside")
library(ggside)
library(tidyverse)

# Cargamos los datos ####
trainees_df <- read_csv("https://github.com/matheusfacure/python-causality-handbook/raw/master/causal-inference-for-the-brave-and-true/data/trainees.csv",
                        show_col_types=FALSE) %>% 
  rename(trainee = trainees)

trainees_df

# Vemos las distribuciones marginales
trainees_df %>% 
  mutate(treatment_status = factor(trainee,
                                   levels = c(0, 1),
                                   labels = c("Trainee", "Not Trainee"))) %>% 
  ggplot() +
  aes(age, earnings, color = treatment_status) +
  geom_jitter() +
  geom_xsidedensity(
    aes(
      y = after_stat(density),
      fill = treatment_status
    ),
    alpha = 0.5,
    size = 1,
    position = "identity"
  ) +
  geom_ysidedensity(
    aes(
      x = after_stat(density),
      fill = treatment_status
    ),
    alpha = 0.5,
    size = 1,
    position = "identity"
  ) + 
  theme(
    ggside.panel.scale.x = 0.3,
    ggside.panel.scale.y = 0.3,
    ggside.axis.text.y = element_blank(),
    ggside.axis.ticks.y = element_blank(),
    ggside.axis.text.x = element_blank(),
    ggside.axis.ticks.x = element_blank()
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(color = NULL, fill = NULL) +
  theme(legend.position = "top")

# El estimador exact matching del ATT ####
# Renombro la columna como outcome
trainees_df <- trainees_df %>%
  rename(outcome := earnings) %>% 
  mutate(id = 1:n())

# Creo un dataset sólo con las unidades tratadas
treated_df <- trainees_df %>% 
  filter(trainee == 1)

# Creo un dataset sólo con las unidades NO tratadas
control_df <- trainees_df %>% 
  filter(trainee == 0)

# Matcheo las tratadas y no tratadas para iguales valores de age
treated_matched <- treated_df %>% 
  left_join(control_df, by = "age",
            suffix = c("_i", "_j"))

# El estimador
estimate_att <- 
  treated_matched %>% 
  # Promedio si hay más de un match
  group_by(id_i) %>%
  summarise(outcome_j = mean(outcome_j),
            outcome_i = first(outcome_i)) %>%
  # Calculo las diferencias de cada unidad con su match
  mutate(treat_effect = outcome_i - outcome_j) %>% 
  # Promedio las diferencias
  summarise(estimate_att = mean(treat_effect)) %>% 
  # Et voilá
  pull(estimate_att)
estimate_att


# Función para estimar el ATT con matching ####
# Extact Matching Estimator of the ATT
exact_matching_estimator_att <- function(df, #dataframe
                                         outcome, # nombre de la variable outcome
                                         treatment, # nombre de la variable donde está el tratamiento
                                         covariates # string o vector de strings
) {
  
  # Renombro la columna como outcome
  df <- df %>%
    rename(outcome := {{ outcome }}) %>% 
    mutate(id = 1:n())
  
  # Creo un dataset sólo con las unidades tratadas
  treated_df <- df %>% 
    filter({{ treatment }} == 1)
  
  # Creo un dataset sólo con las unidades NO tratadas
  control_df <- df %>% 
    filter({{ treatment }} == 0)
  
  # Matcheo las tratadas y no tratadas para iguales valores de las covariates
  treated_matched <- treated_df %>% 
    left_join(control_df, by = covariates,
              suffix = c("_i", "_j"))
  
  # El estimador
  estimate_att <- 
    treated_matched %>% 
    # Promedio si hay más de un match
    group_by(id_i) %>%
    summarise(outcome_j = mean(outcome_j),
              outcome_i = first(outcome_i)) %>%
    # Calculo las diferencias de cada unidad con su match
    mutate(treat_effect = outcome_i - outcome_j) %>% 
    # Promedio las diferencias
    summarise(estimate_att = mean(treat_effect)) %>% 
    # Me quedo con la variable
    pull(estimate_att)
  
  estimate_att
}

# Cargo los datos de nuevo
trainees_df <- read_csv("https://github.com/matheusfacure/python-causality-handbook/raw/master/causal-inference-for-the-brave-and-true/data/trainees.csv",
                        show_col_types=FALSE) %>% 
  rename(trainee = trainees)

# Llamo a la función
exact_matching_estimator_att(
  trainees_df,
  outcome = earnings,
  treatment = trainee,
  covariates = "age"
)

# El estimador exact matching del ATE ####
# Cargo los datos de nuevo
trainees_df <- read_csv("https://github.com/matheusfacure/python-causality-handbook/raw/master/causal-inference-for-the-brave-and-true/data/trainees.csv",
                        show_col_types=FALSE) %>% 
  rename(trainee = trainees)

#Renombro la columna como outcome
trainees_df <- trainees_df %>%
  rename(outcome := earnings,
         treatment := trainee) %>% 
  mutate(id = 1:n())

# Creo un dataset sólo con las unidades tratadas
treated_df <- trainees_df %>% 
  filter(treatment == 1)

# Creo un dataset sólo con las unidades NO tratadas
control_df <- trainees_df %>% 
  filter(treatment == 0)

# Matcheo las tratadas y no tratadas para iguales valores de age
treated_matched <- treated_df %>% 
  left_join(control_df, by = "age",
            suffix = c("_i", "_j"))

# Agregamos los controles
control_matched <- control_df %>% 
  left_join(treated_df, by = "age",
            suffix = c("_i", "_j"))

estimate_ate <- 
  # Juntamos las dos muestras matcheadas
  bind_rows(
    treated_matched,
    control_matched
  ) %>% 
  group_by(id_i) %>%
  summarise(outcome_j = mean(outcome_j),
            outcome_i = first(outcome_i),
            treatment_i = first(treatment_i)) %>%
  mutate(treat_effect = outcome_i - outcome_j,
         # Multiplicamos por -1 la diferencia entre controles
         # y sus matches. Porque es como el "anti tratamiento"
         treat_effect = ifelse(
           treatment_i == 1,
           yes = treat_effect,
           no = -1*treat_effect
         )) %>%
  summarise(estimate_ate = mean(treat_effect)) %>% 
  pull(estimate_ate)
estimate_ate

# Función para estimar el ATE con matching ####
# Extact Matching Estimator of the ATE
exact_matching_estimator_ate <- function(df, #dataframe
                                         outcome, # nombre de la variable outcome
                                         treatment, # nombre de la variable donde está el tratamiento
                                         covariates # string o vector de strings
) {
  df <- df %>%
    # Renombro outcome y tratemiento
    rename(outcome := {{ outcome }},
           treatment := {{ treatment }}) %>% 
    mutate(id = 1:n())
  
  treated_df <- df %>% 
    filter(treatment == 1)
  
  control_df <- df %>% 
    filter(treatment == 0)
  
  treated_matched <- treated_df %>% 
    left_join(control_df, by = covariates,
              suffix = c("_i", "_j"))
  
  # Ahora matcheo también los controles
  control_matched <- control_df %>% 
    left_join(treated_df, by = covariates,
              suffix = c("_i", "_j"))
  
  estimate_ate <- 
    # Juntamos las dos muestras matcheadas
    bind_rows(
      treated_matched,
      control_matched
    ) %>% 
    group_by(id_i) %>%
    summarise(outcome_j = mean(outcome_j),
              outcome_i = first(outcome_i),
              treatment_i = first(treatment_i)) %>%
    mutate(treat_effect = outcome_i - outcome_j,
           # Multiplicamos por -1 la diferencia entre controles
           # y sus matches. Porque es como el "anti tratamiento"
           treat_effect = ifelse(
             treatment_i == 1,
             yes = treat_effect,
             no = -1*treat_effect
           )) %>%
    summarise(estimate_ate = mean(treat_effect)) %>% 
    pull(estimate_ate)
  
  estimate_ate
}

# Cargo los datos de nuevo
trainees_df <- read_csv("https://github.com/matheusfacure/python-causality-handbook/raw/master/causal-inference-for-the-brave-and-true/data/trainees.csv",
                        show_col_types=FALSE) %>% 
  rename(trainee = trainees)

exact_matching_estimator_ate(
  trainees_df,
  outcome = earnings,
  treatment = trainee,
  covariates = "age"
)

# Para ver qué está pasando ####
treated_df <- trainees_df %>%
  filter(trainee == 1)

control_df <- trainees_df %>%
  filter(trainee == 0)

treated_matched <- treated_df %>%
  left_join(control_df, by = "age",
            suffix = c("_i", "_j"))

control_matched <- control_df %>%
  left_join(treated_df, by = "age",
            suffix = c("_i", "_j"))

bind_rows(
  control_matched,
  treated_matched
)
# Se ve que hay combinaciones i-j que están vacías