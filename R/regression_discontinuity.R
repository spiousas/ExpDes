library(tidyverse)
library(broom)  # Convierte modelos a dataframes
library(modelsummary)  # Para armar tablas de regresión combinadas
library(knitr)
library(here)

tutoring <- read_csv(here("data/tutoring_program.csv"))

# Determinemos si el proceso de asignación al tratamiento depende de una regla ####
ggplot(tutoring, aes(x = entrance_exam, 
                     y = tutoring, 
                     color = tutoring)) +
  # Hacemos los puntos semitransparentes y los movemos un poco
  geom_point(size = 1.5, alpha = 0.5, 
             position = position_jitter(width = 0, height = 0.25, seed = 1234)) + 
  # Ponemos una línea vertical en el umbral
  geom_vline(xintercept = 70, color = "darkblue", linetype = "dashed") + 
  # Labels
  labs(x = "Puntaje en el examen de entrada", y = "Participación en el programa de tutorías") + 
  # Sacó la leyenda de color
  guides(color = FALSE) +
  # Colores más chetos
  scale_color_brewer(palette = "Dark2") +
  # Theme sin fondo gris
  theme_minimal()

rect_claro <- tibble(xmin = 60, xmax = 80, ymin = 0, ymax = 3)
rect_oscuro <- tibble(xmin = 65, xmax = 75, ymin = 0, ymax = 3)

ggplot(tutoring) +
  # Los rectángulos
  geom_rect(data = rect_claro, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "gray20", color = "white", alpha = .2) +
  geom_rect(data = rect_oscuro, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "gray20", color = "gray80", alpha = .4) +
  # Hacemos los puntos semitransparentes y los movemos un poco
  geom_point( aes(x = entrance_exam, 
                  y = tutoring, 
                  color = tutoring),
              size = 1.5, alpha = 0.5, 
             position = position_jitter(width = 0, height = 0.25, seed = 1234)) + 
  # Ponemos una línea vertical en el umbral
  geom_vline(xintercept = 70, color = "yellow", linetype = "dashed") + 
  # Labels
  labs(x = "Puntaje en el examen de entrada", y = "Participación en el programa de tutorías") + 
  # Sacó la leyenda de color
  guides(color = FALSE) +
  # Colores más chetos
  scale_color_brewer(palette = "Dark2") +
  # Theme sin fondo gris
  theme_minimal()

ggplot(tutoring) +
  # Los rectángulos
  geom_rect(data = rect_claro, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "gray20", color = "white", alpha = .2) +
  geom_rect(data = rect_oscuro, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "gray20", color = "gray80", alpha = .4) +
  # Hacemos los puntos semitransparentes y los movemos un poco
  geom_point( aes(x = entrance_exam, 
                  y = tutoring, 
                  color = tutoring),
              size = 1.5, alpha = 0.5, 
              position = position_jitter(width = 0, height = 0.25, seed = 1234)) + 
  # Ponemos una línea vertical en el umbral
  geom_vline(xintercept = 70, color = "yellow", linetype = "dashed") + 
  # Labels
  labs(x = "Puntaje en el examen de entrada", y = "Participación en el programa de tutorías") + 
  # Sacó la leyenda de color
  guides(color = FALSE) +
  # Colores más chetos
  scale_color_brewer(palette = "Dark2") +
  # Cierro el eje
  scale_x_continuous(limits = c(55,85)) +
  # Theme sin fondo gris
  theme_minimal()

datasummary_skim(data = tutoring, by = "tutoring", 
                 output = "gt") |>
  gt_highlight_rows(rows = c(3,4), 
                    fill = "lightyellow",
                    font_weight = "bold")

# Ahora miremos como se comporta la variable outcomes en función de la running variable ####
ggplot(tutoring, aes(x = entrance_exam, 
                     y = exit_exam, 
                     color = tutoring,
                     fill = tutoring)) +
  geom_point(size = 1.5, alpha = .3) + 
  # Agregamos una linea basada en un modelo lineal para la running variable menor a 70
  geom_smooth(data = filter(tutoring, entrance_exam <= 70), method = "lm") +
  # Agregamos una linea basada en un modelo lineal para la running variable mayor a 70
  geom_smooth(data = filter(tutoring, entrance_exam > 70), method = "lm") +
  # Ponemos una línea vertical en el umbral
  geom_vline(xintercept = 70, color = "darkblue", linetype = "dashed") + 
  # Las lables
  labs(x = "Puntaje en el examen de entrada", 
       y = "Puntaje en el examen de salida",
       color = "Participó en la tutoría",
       fill = "Participó en la tutoría") + 
  # Colores más chetos
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  # Theme sin fondo gris
  theme_minimal() +
  theme(legend.position = "top")



# Midamos el tamaño del efecto ####
## Estimación paramétrica ####

# Primero centramos a cada grupo en la running variable
tutoring_centered <- tutoring |> 
  mutate(entrance_centered = entrance_exam - 70)

head(tutoring_centered)

ggplot(tutoring_centered, aes(x = entrance_centered, 
                     y = exit_exam, 
                     color = tutoring,
                     fill = tutoring)) +
  geom_point(size = 1.5, alpha = .3) + 
  # Agregamos una linea basada en un modelo lineal para la running variable menor a 70
  geom_smooth(data = filter(tutoring_centered, tutoring), method = "lm") +
  # Agregamos una linea basada en un modelo lineal para la running variable mayor a 70
  geom_smooth(data = filter(tutoring_centered, !tutoring), method = "lm") +
  # Ponemos una línea vertical en el umbral
  geom_vline(xintercept = 0, color = "darkblue", linetype = "dashed") + 
  # Las lables
  labs(x = "Puntaje en el examen de entrada (centrado)", 
       y = "Puntaje en el examen de salida",
       color = "Participó en la tutoría",
       fill = "Participó en la tutoría") + 
  # Colores más chetos
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  # Theme sin fondo gris
  theme_minimal() +
  theme(legend.position = "top")

model_simple <- lm(exit_exam ~ entrance_centered + tutoring,
                   data = tutoring_centered)

#install.packages("gtExtras")
library(gtExtras)
modelsummary(model_simple, output = "gt") |>
  gt_highlight_rows(rows = 5, 
                    fill = "lightyellow",
                    font_weight = "bold")

# Ahora probemos bajando el bandwidth
# BW = 10
model_bw_10 <- lm(exit_exam ~ entrance_centered + tutoring,
                  data = filter(tutoring_centered,
                                entrance_centered >= -10 & 
                                  entrance_centered <= 10))
tidy(model_bw_10)

# BW = 5
model_bw_5 <- lm(exit_exam ~ entrance_centered + tutoring,
                 data = filter(tutoring_centered,
                               entrance_centered >= -5 & 
                                 entrance_centered <= 5))
tidy(model_bw_5)

# Comparemos todos los modelos
modelsummary(list("Full data" = model_simple, 
                  "Bandwidth = 10" = model_bw_10, 
                  "Bandwidth = 5" = model_bw_5), 
             coef_rename = c('entrance_centered' = 'Pendiente', 
                             'tutoringTRUE' = 'Tutoring'),
             statistic = "p = {p.value}", 
             output = "gt") |>
  gt_highlight_rows(rows = 5, 
                    font_weight = "bold")

# Y el sample size
modelsummary(list("Full data" = model_simple, 
                  "Bandwidth = 10" = model_bw_10, 
                  "Bandwidth = 5" = model_bw_5), 
             coef_rename = c('entrance_centered' = 'Pendiente', 
                             'tutoringTRUE' = 'Tutoring'),
             statistic = "p = {p.value}", 
             output = "gt") |>
  gt_highlight_rows(rows = 5, 
                    fill = "lightyellow",
                    font_weight = "bold") |>
  gt_highlight_rows(rows = 7, 
                    fill = "lightyellow",
                    font_weight = "bold")

# Veamos el ajuste de cada uno de estos modelos
# Full BW
p_full <- ggplot(tutoring, aes(x = entrance_exam, 
                               y = exit_exam, 
                               color = tutoring,
                               fill = tutoring)) +
  geom_point(size = 1.5, alpha = 0.5) + 
  # Add a line based on a linear model for the people scoring 70 or less
  geom_segment(x = min(tutoring$entrance_exam), xend = 70,
               y = model_simple$coefficients[1]+min(tutoring_centered$entrance_centered)*model_simple$coefficients[2] +
                 model_simple$coefficients[3],
               yend = model_simple$coefficients[1] + model_simple$coefficients[3],
               color = "gray30", linewidth = 1.5) +
  # Add a line based on a linear model for the people scoring more than 70
  geom_segment(x = 70, xend = max(tutoring$entrance_exam),
               y = model_simple$coefficients[1],
               yend = model_simple$coefficients[1] + model_simple$coefficients[2]*max(tutoring_centered$entrance_centered),
               color = "gray30", linewidth = 1.5) +
  # Ponemos una línea vertical en el umbral
  geom_vline(xintercept = 70, color = "darkblue", linetype = "dashed") + 
  # Las labels
  labs(x = "Puntaje en el examen de entrada", 
       y = "Puntaje en el examen de salida",
       color = "Participó en la tutoría",
       fill = "Participó en la tutoría") + 
  # Colores más chetos
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  # Theme sin fondo gris
  theme_minimal() +
  theme(legend.position = "none")


# BW = 10
p_bw10 <- tutoring_centered |>
  dplyr::filter(between(entrance_centered, -10, 10)) |>
  ggplot(aes(x = entrance_exam, 
             y = exit_exam, 
             color = tutoring,
             fill = tutoring)) +
  geom_point(size = 1.5, alpha = 0.5) + 
  geom_point(data = tutoring_centered |>
               dplyr::filter(!between(entrance_centered, -10, 10)), 
             size = 1.5, alpha = 0.3, color = "gray") + 
  # Add a line based on a linear model for the people scoring 70 or less
  geom_segment(x = 60, xend = 70,
               y = model_simple$coefficients[1]-10*model_simple$coefficients[2] +
                 model_simple$coefficients[3],
               yend = model_simple$coefficients[1] + model_simple$coefficients[3],
               color = "gray30", linewidth = 1.5) +
  # Add a line based on a linear model for the people scoring more than 70
  geom_segment(x = 70, xend = 80,
               y = model_simple$coefficients[1],
               yend = model_simple$coefficients[1] + model_simple$coefficients[2]*10,
               color = "gray30", linewidth = 1.5) +
  # Ponemos una línea vertical en el umbral
  geom_vline(xintercept = 70, color = "darkblue", linetype = "dashed") + 
  # Las labels
  labs(x = "Puntaje en el examen de entrada", 
       y = "Puntaje en el examen de salida",
       color = "Participó en la tutoría",
       fill = "Participó en la tutoría") + 
  # Colores más chetos
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  # Theme sin fondo gris
  theme_minimal() +
  theme(legend.position = "none")

# BW = 5
p_bw5 <- tutoring_centered |>
  dplyr::filter(between(entrance_centered, -5, 5)) |>
  ggplot(aes(x = entrance_exam, 
             y = exit_exam, 
             color = tutoring,
             fill = tutoring)) +
  geom_point(size = 1.5, alpha = 0.5) + 
  geom_point(data = tutoring_centered |>
               dplyr::filter(!between(entrance_centered, -5, 5)), 
             size = 1.5, alpha = 0.3, color = "gray") + 
  # Add a line based on a linear model for the people scoring 70 or less
  geom_segment(x = 65, xend = 70,
               y = model_simple$coefficients[1]-5*model_simple$coefficients[2] +
                 model_simple$coefficients[3],
               yend = model_simple$coefficients[1] + model_simple$coefficients[3],
               color = "gray30", linewidth = 1.5) +
  # Add a line based on a linear model for the people scoring more than 70
  geom_segment(x = 70, xend = 75,
               y = model_simple$coefficients[1],
               yend = model_simple$coefficients[1] + model_simple$coefficients[2]*5,
               color = "gray30", linewidth = 1.5) +
  # Ponemos una línea vertical en el umbral
  geom_vline(xintercept = 70, color = "darkblue", linetype = "dashed") + 
  # Las labels
  labs(x = "Puntaje en el examen de entrada", 
       y = "Puntaje en el examen de salida",
       color = "Participó en la tutoría",
       fill = "Participó en la tutoría") + 
  # Colores más chetos
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  # Theme sin fondo gris
  theme_minimal() +
  theme(legend.position = "none")

library(patchwork)
p_full / p_bw10 / p_bw5 + plot_layout(guides = "collect")

## Estimación no paramétrica ####
ggplot(tutoring_centered, aes(x = entrance_centered, 
                              y = exit_exam, 
                              color = tutoring,
                              fill = tutoring)) +
  geom_point(size = 1.5, alpha = .3) + 
  # Agregamos una linea basada en un modelo lineal para la running variable menor a 70
  geom_smooth(data = filter(tutoring_centered, tutoring)) +
  # Agregamos una linea basada en un modelo lineal para la running variable mayor a 70
  geom_smooth(data = filter(tutoring_centered, !tutoring)) +
  # Ponemos una línea vertical en el umbral
  geom_vline(xintercept = 0, color = "darkblue", linetype = "dashed") + 
  # Las lables
  labs(x = "Puntaje en el examen de entrada (centrado)", 
       y = "Puntaje en el examen de salida",
       color = "Participó en la tutoría",
       fill = "Participó en la tutoría") + 
  # Colores más chetos
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  # Theme sin fondo gris
  theme_minimal() +
  theme(legend.position = "top")

# install.packages("rdrobust")
library(rdrobust)
rdrobust(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70) |> 
  summary()

rdplot(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70)

# Cómo elige el BW
rdbwselect(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70) |> 
  summary()

# Cuales son los posibles
rdbwselect(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70, all = TRUE) |> 
  summary()

# Un approach posible es usar el recomendado, el doble y la mitad
full <- rdrobust(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70, h = 9.969)
doble <- rdrobust(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70, h = 9.969 * 2)
mitad <- rdrobust(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70, h = 9.969 * 0.5) 

tibble(BW = c("Full = 9.97", "Doble = 19.94", "Mitad = 4.99"),
       Estimate = -round(c(full$Estimate[1], doble$Estimate[1], mitad$Estimate[1]), 3)) |> 
  gt()

# También podemos cambiar el kernel
triangular <- rdrobust(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70, kernel = "triangular")
epanechnikov <- rdrobust(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70, kernel = "epanechnikov")
uniforme <- rdrobust(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70, kernel = "uniform")

tibble(Kernel = c("Triangular", "Epanechnikov", "Uniforme"),
       Estimate = -round(c(triangular$Estimate[1], epanechnikov$Estimate[1], uniforme$Estimate[1]), 3)) |> 
  gt()

rdplot(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70, h = 9.969, kernel = "uniform")
rdplot(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70, h = 9.969, kernel = "triangular")
rdplot(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70, h = 9.969, kernel = "epanechnikov")

## Miremos todas las estimaciones

tabla_results <- tibble(Método = c(rep("Paramétrico", 3), rep("No paramétrico", 5)),
                        Kernel = c(rep("No pesado",3), rep("Triangular",3), "Epanechnikov", "Uniforme"),
                        BW = c("Full", 10, 5, round(c(full$bws[1,1], doble$bws[1,1], mitad$bws[1,1], epanechnikov$bws[1,1], uniforme$bws[1,1]), 3)),
                        Estimate = round(c(model_simple$coefficients[3], model_bw_10$coefficients[3], model_bw_5$coefficients[3], 
                                           -full$Estimate[1], -doble$Estimate[1], -mitad$Estimate[1], -epanechnikov$Estimate[1], -uniforme$Estimate[1]), 3))

tabla_results |> 
  gt()

tabla_results |> 
  gt() |>
  gt_highlight_rows(rows = 4, 
                    fill = "lightyellow",
                    font_weight = "bold")

# Chequeamos si hay cambios de un lado y otro del umbral ####
install.packages("rddensity")
library(rddensity)
test_density <- rddensity(tutoring$entrance_exam, c = 70)
plot_density_test <- rdplotdensity(rdd = test_density, 
                                   X = tutoring$entrance_exam, 
                                   type = "both") 

