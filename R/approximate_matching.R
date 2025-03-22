library(tidyverse)

# Las distancias en 2D ####
x_i <- c(34, 10.27E6)

x_1 <- c(27, 10.28E6)
x_2 <- c(32, 9.12E6)
x_3 <- c(36, 9.43E6)
x_4 <- c(38, 11.5E6)

# La distancia euclidea
distancia <- function(x_i, x_j) {
  dist <- sqrt((x_i[1]-x_j[1])^2 + (x_i[2]-x_j[2])^2)
  dist
}

# Usando la función
distancia(x_i, x_1)
distancia(x_i, x_2)
distancia(x_i, x_3)
distancia(x_i, x_4)

# La distancia como producto escalar
sqrt(t(x_i-x_2) %*% (x_i-x_2))

# Normalicemos las distancias ####
X <- rbind(x_i, x_1, x_2, x_3, x_4)
X

X_sc <- X
X_sc[,1] <- (X_sc[,1]-mean(X_sc[,1]))/sd(X_sc[,1]) 
X_sc[,2] <- (X_sc[,2]-mean(X_sc[,2]))/sd(X_sc[,2]) 
X_sc

scale(X)

# Ahora las distancias del vector escaleado
distancia(X_sc[1,], X_sc[2,]) # X1
distancia(X_sc[1,], X_sc[3,]) # X2
distancia(X_sc[1,], X_sc[4,]) # X3
distancia(X_sc[1,], X_sc[5,]) # X4


# Las distancias definidas para tibbles ####
euclidian_distance <- function(x_i, x_j) {
  
  map2_dbl(x_i, x_j, ~(.x - .y)^2) %>%
    reduce(sum) %>% 
    sqrt()
  
}

# `x_i` y `x_j` osn dos vectores numéricos que representan dos observaciones.
# Ambos vectores tienen la misma longitud M, donde M es el número de ovariables.

# cov es la matriz de varinza y covarianza (Sigma) de las variables X [MxM].
mahalanobis_distance <- function(x_i,
                                 x_j,
                                 cov){
  # Calculamos la inverse de cov
  inv_covar <- solve(cov)
  
  matrix_multiplication_output <-
    # Vector de 1xM * Matirz de MxM * Vector de Mx1 = Un escalar
    (t(as.matrix(x_i - x_j)) %*% inv_covar %*% as.matrix(x_i - x_j))
  
  sqrt(matrix_multiplication_output)
}

# El ejemplo ####
medicine_impact_recovery <- 
  read_csv("https://raw.githubusercontent.com/matheusfacure/python-causality-handbook/master/causal-inference-for-the-brave-and-true/data/medicine_impact_recovery.csv",
           show_col_types = FALSE)

medicine_impact_recovery

X <- as.matrix(medicine_impact_recovery[,1:3])
sigma <- cov(X)

X_i <- as.matrix(medicine_impact_recovery[1,1:3])
X_j <- as.matrix(medicine_impact_recovery[2,1:3])

mah <- sqrt((X_i-X_j) %*% solve(sigma) %*% t(X_i-X_j))
mah
# Calculemos el SDO
mean_treated <- mean(medicine_impact_recovery$recovery[medicine_impact_recovery$medication == 1])
mean_untreated <- mean(medicine_impact_recovery$recovery[medicine_impact_recovery$medication == 0])

mean_treated - mean_untreated

# El estimador de matching 
# install.packages("MatchIt")
# install.packages("optmatch")
library(MatchIt)

nearest_control <- matchit(medication ~ sex + age + severity, 
                           data = medicine_impact_recovery,
                           method = "nearest", 
                           distance = "mahalanobis",
                           replace = T,
                           ratio = 1)
nearest_control
summary(nearest_control, un = F)
plot(summary(nearest_control))

match_medicine <- match.data(nearest_control)

# install.packages("marginaleffects")
library("marginaleffects")

# Ajustando estamos reduciendo el sesgo debido al desbalance
fit <- lm(recovery ~ medication * (sex + age + severity), 
          data = match_medicine, 
          weights = weights)

avg_comparisons(fit,
                variables = "medication",
                vcov = ~subclass,
                newdata = subset(match_medicine, medication == 1),
                wts = "weights")

# Ejemplo de sesgo ####
library(tidyverse)
# install.packages("haven")
library(haven)

read_data <- function(df)
{
  full_path <- paste("https://github.com/scunning1975/mixtape/raw/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

training_bias_reduction <- read_data("training_bias_reduction.dta") %>% 
  mutate(
    Y1 = case_when(Unit %in% c(1,2,3,4) ~ Y),
    Y0 = c(4,0,5,1,4,0,5,1))

train_reg <- lm(Y ~ X, training_bias_reduction)
training_bias_reduction <- training_bias_reduction %>% 
  mutate(u_hat0 = predict(train_reg))


# Exact matching con MatchIt ####
trainees_df <- read_csv("https://github.com/matheusfacure/python-causality-handbook/raw/master/causal-inference-for-the-brave-and-true/data/trainees.csv",
                        show_col_types=FALSE) %>% 
  rename(trainee = trainees)

trainees_df

# ATT exact matching
exact_control <- matchit(trainee ~ age, 
                           data = trainees_df,
                           method = "exact",)
exact_control

match_trainees <- match.data(exact_control)

# Ajustando estamos reducneindo el sesgo debido al desbalance
fit <- lm(earnings ~ trainee * (age), 
          data = match_trainees, 
          weights = weights)

avg_comparisons(fit,
                variables = "trainee",
                vcov = ~subclass,
                newdata = subset(match_trainees, trainee == 1),
                wts = "weights")

# ATE exact matching
exact_control_ATE <- matchit(trainee ~ age, 
                             data = trainees_df,
                             method = "exact",
                             estimand = "ATE")
exact_control_ATE

match_trainees <- match.data(exact_control_ATE)

# Ajustando estamos reducneindo el sesgo debido al desbalance
fit <- lm(earnings ~ trainee * (age), 
          data = match_trainees, 
          weights = weights)

avg_comparisons(fit,
                variables = "trainee",
                vcov = ~subclass,
                newdata = subset(match_trainees, trainee == 1),
                wts = "weights")

# Subclassification con MatchIt ####
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

subclas_control_ATE <- matchit(d ~ region, 
                             data = customers,
                             method = "subclass",
                             estimand = "ATE")
subclas_control_ATE

match_customers <- match.data(subclas_control_ATE)

# Ajustando estamos reducneindo el sesgo debido al desbalance
fit <- lm(y ~ d * (region), 
          data = match_customers, 
          weights = weights)

avg_comparisons(fit,
                variables = "d",
                vcov = ~subclass,
                newdata = subset(match_customers, d == 1),
                wts = "weights")
