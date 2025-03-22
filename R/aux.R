library(here)
library(tidyverse)

set.seed(123)
x <- rnorm(20, mean = 1, sd = 3)

hist(x)

mean(x)
sd(x)
t.test(x)

2*pnorm(-abs(mean(x)/sqrt(3^2/20)))

means <- c()
set.seed(123)
for (i in 1:10000) {
  x <- rnorm(20, mean = 1, sd = 3)
  means <- c(means, mean(x))
}

x_means <- tibble(means)
x_means %>% ggplot(aes(x = means, y = ..density..)) +
  geom_histogram(fill = "steelblue") +
  theme_bw()
  
Zs <- c()
set.seed(123)
for (i in 1:10000) {
  x <- rnorm(20, mean = 0, sd = 3)
  Zs <- c(Zs, (mean(x))/sqrt(3^2/20))
}

Z_means <- tibble(Zs)

Z_means %>% ggplot() +
  geom_histogram(aes(x = Zs, y = ..density..), fill = "steelblue") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  theme_bw()

mean(x)/sqrt(9/20)

set.seed(123)
x <- rnorm(20, mean = 1, sd = 3)

Z_means %>% ggplot() +
  geom_histogram(aes(x = Zs, y = ..density..), fill = "steelblue") +
  geom_vline(xintercept = mean(x)/sqrt(9/20), linetype = "dashed", color = "red") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  theme_bw()

Z_means %>% ggplot() +
  geom_histogram(aes(x = Zs, y = ..density..), fill = "steelblue") +
  geom_vline(xintercept = c(1, -1) * mean(x)/sqrt(9/20), linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(1, -1) * 1.96, linetype = "dashed", color = "blue") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  theme_bw()

ps <- c()
set.seed(123)
for (i in 1:1E4) {
  x <- rnorm(20, mean = 1, sd = 3)
  p <- 2*pnorm(-abs(mean(x)/sqrt(3^2/20)))
  ps <- c(ps, p)
}

p_values <- tibble(ps)
p_values %>% ggplot(aes(x = ps)) +
  geom_histogram(fill = "steelblue", color = "white", 
                 binwidth = .05, boundary = .05) +
  theme_bw()

set.seed(12)
x <- rnorm(20, mean = 1, sd = 3)
mean(x)
2*pnorm(-abs(mean(x)/sqrt(3^2/20)))

Z_means %>% ggplot() +
  geom_histogram(aes(x = Zs, y = ..density..), fill = "steelblue", alpha = .4) +
  geom_vline(xintercept = 0, color = "black") +
  geom_vline(xintercept = c(1, -1) * mean(x)/sqrt(9/20), linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(1, -1) * qnorm(0.05/2), linetype = "dashed", color = "blue") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  stat_function(fun = dnorm, args = list(mean = 1/sqrt(9/20), sd = 1), 
                color = "red", linewidth = 1) +
  theme_bw()

1 + pnorm(qnorm(.05/2)-1/(sqrt(3^2/20))) -
  pnorm(qnorm(1-.05/2)-1/(sqrt(3^2/20)))

potencia <- function(sigma, mu, n, alpha) {
  pnorm(qnorm(alpha/2)-mu/(sqrt(sigma^2/n))) + 1 -
    pnorm(qnorm(1-alpha/2)-mu/(sqrt(sigma^2/n)))
}
pot <- potencia(3,seq(-3,3,.1),20,0.05)

pot_tbl <- tibble(mu = seq(-3,3,.1), potencia = pot)

pot_tbl %>% ggplot(aes(x = mu,
           y = potencia)) +
  geom_line(linewidth = 1) +
  theme_bw()
  
pot <- potencia(3,1,20,seq(0, 0.05, 0.001))

pot_tbl <- tibble(alpha = seq(0, 0.05, 0.001), potencia = pot)

pot_tbl %>% ggplot(aes(x = alpha,
                       y = potencia)) +
  geom_line(linewidth = 1) +
  theme_bw()

pot <- potencia(3,1,seq(5, 200),0.05)

pot_tbl <- tibble(n = seq(5, 200), potencia = pot)

pot_tbl %>% ggplot(aes(x = n,
                       y = potencia)) +
  geom_line(linewidth = 1) +
  theme_bw()

## t dist
norm_mean <- c()
n <- 6
mu <- 1
sigma <- 3
set.seed(123)
for (i in 1:10000) {
  x <- rnorm(n, mean = mu, sd = sigma)
  norm_mean <- c(norm_mean, (mean(x)-mu)/sqrt(sd(x)^2/n))
}


norm_means <- tibble(norm_mean)
norm_means %>% ggplot() +
  geom_histogram(aes(x = norm_mean, y = ..density..), fill = "steelblue", alpha = .4) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  stat_function(fun = dt, args = list(df = n-1), 
                color = "red", linewidth = 1) +
  scale_x_continuous(limits = c(-5,5)) +
  theme_bw()

norm_means %>% ggplot() +
  #geom_histogram(aes(x = norm_mean, y = ..density..), fill = "steelblue", alpha = .4) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  stat_function(fun = dt, args = list(df = 5), 
                color = "red", linewidth = 1) +
  stat_function(fun = dt, args = list(df = 14), 
                color = "darkgreen", linewidth = 1) +
  scale_x_continuous(limits = c(-5,5)) +
  theme_bw()

norm_means %>% ggplot() +
  #geom_histogram(aes(x = norm_mean, y = ..density..), fill = "steelblue", alpha = .4) +
  # stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
  #               color = "black", linewidth = 1) +
  stat_function(fun = dt, args = list(df = 19), 
                color = "darkgreen", linewidth = 1) +
  scale_x_continuous(limits = c(-5,5)) +
  labs(title = "t19 (n=20)", y = NULL) 

## Effect size
set.seed(123)
u <- rnorm(20000, mean = .1, sd = 3)

t.test(u)

means <- c()
set.seed(123)
for (i in 1:10000) {
  x <- rnorm(20000, mean = .1, sd = 3)
  means <- c(means, mean(x))
}

x_means <- tibble(means)
x_means %>% ggplot(aes(x = means, y = ..density..)) +
  geom_histogram(fill = "steelblue") +
  labs(title = "n = 20000")

means <- c()
set.seed(123)
for (i in 1:10000) {
  x <- rnorm(100, mean = .1, sd = 3)
  means <- c(means, mean(x))
}

x_means <- tibble(means)
x_means %>% ggplot(aes(x = means, y = ..density..)) +
  geom_histogram(fill = "steelblue") +
  labs(title = "n = 100")


## normalidad
means <- c()
set.seed(123)
for (i in 1:10000) {
  x <- rexp(100,rate = 1)
  means <- c(means, mean(x))
}

Xs <- tibble(x)

Xs %>% ggplot() +
  geom_histogram(aes(x = x, y = ..density..), fill = "steelblue") +
  stat_function(fun = dexp, args = list(rate =1), 
                color = "black", linewidth = 1) +
  labs(x = "V")
  
mean(x)
sd(x)
x_means <- tibble(means)
x_means %>% ggplot(aes(x = means, y = ..density..)) +
  geom_histogram(fill = "steelblue") +
  theme_bw()

n = 1000
Zs <- c()
set.seed(123)
for (i in 1:10000) {
  x <- rexp(n, rate = 1)
  Zs <- c(Zs, (mean(x) - 1)/sqrt(sd(x)^2/n))
}

Z_means <- tibble(Zs)
Z_means %>% ggplot() +
  geom_histogram(aes(x = Zs, y = ..density..), fill = "steelblue") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  labs(title = paste("n = ", n), x = "Medias de v") +
  scale_x_continuous(limits = c(-4, 4))

# Posttest only ####
set.seed(123)
n <- 50
t <- rnorm(2*n, 50, 10)
x <- t[1:n]
y <- t[(n+1):(2*n)] + 5

data <- tibble(tiempo = c(x,y),
               condicion = c(rep("Website A", n), rep("Website B", n)))

data %>% ggplot(aes(x = condicion, y = tiempo, color = condicion)) +
  geom_jitter(width = .2) + 
  labs(y = "Tiempo (s)", color = NULL, x = NULL) +
  theme(legend.position = "top")
  
m1 <- lm(data = data, tiempo ~ condicion, model = T)
model.matrix(m1)
summary(m1)

## mas n ####
set.seed(123)
n <- 200
t <- rnorm(2*n, 50, 10)
x <- t[1:n]
y <- t[(n+1):(2*n)] + 5

data_larga <- tibble(tiempo = c(x,y),
                     condicion = c(rep("Website A", n), rep("Website B", n)))

data_larga %>% ggplot(aes(x = condicion, y = tiempo, color = condicion)) +
  geom_jitter(width = .2) + 
  labs(y = "Tiempo (s)", color = NULL, x = NULL) +
  theme(legend.position = "top")

modelo_largo <- lm(data = data_larga, tiempo ~ condicion, model = T)
model.matrix(m1)
summary(modelo_largo)

## Con sesgo ####
set.seed(123)
n <- 50
t <- rnorm(2*n, 50, 10)
x <- sort(t)[1:n]
y <- sort(t)[(n+1):(2*n)] + 5

data_sesgada <- tibble(tiempo = c(x,y),
               condicion = c(rep("Website A", n), rep("Website B", n)))

data_sesgada %>% ggplot(aes(x = condicion, y = tiempo, color = condicion)) +
  geom_jitter(width = .2) + 
  labs(y = "Tiempo (s)", color = NULL, x = NULL) +
  theme(legend.position = "top")

modelo_sesgado <- lm(data = data_sesgada, tiempo ~ condicion)
summary(modelo_sesgado)


# Pretest-posttest ####
set.seed(1234)
n <- 50
t <- rnorm(2*n, 50, 5)

x_pre  <- t[1:n] 
y_pre  <- t[(n+1):(2*n)]
x_post <- t[1:n] + rnorm(n,  0, 1)
y_post <- t[(n+1):(2*n)] + rnorm(n,  5, 1)

data_pre <- tibble(tiempo_pre = c(x_pre, y_pre),
                   tiempo_post = c(x_post, y_post),
                   condicion = c(rep("Website A", n), rep("Website B", n)))

## Matching ####
data_pre <- data_pre %>%
  mutate(Categoria = as.factor(cut(tiempo_pre, 5, label = FALSE)))

data_pre %>% ggplot(aes(x = condicion, y = tiempo_post, color = condicion)) +
  geom_jitter(width = .2) + 
  stat_summary(color = "black") + 
  labs(y = "Tiempo (s)", color = NULL, x = NULL) +
  facet_grid(.~Categoria) +
  theme(legend.position = "top")

modelo_pre_basico <- lm(data = data_pre, tiempo_post ~ condicion)
summary(modelo_pre_basico)

model_matching <- lm(data = data_pre, tiempo_post ~ 0 + Categoria + condicion)
summary(model_matching)

data_pre %>% ggplot(aes(x = tiempo_pre, y = tiempo_post, color = condicion)) +
  geom_jitter(width = .2) + 
  labs(y = "Tiempo (s)", color = NULL, x = NULL) +
  theme(legend.position = "top")

data_pre %>% ggplot(aes(x = tiempo_pre, y = tiempo_post, color = condicion)) +
  geom_jitter(width = .2) + 
  labs(y = "Tiempo (s)", color = NULL, x = NULL) +
  geom_smooth(method = "lm", se = F) +
  theme(legend.position = "top")

data_pre %>% ggplot(aes(x = condicion, y = tiempo_post, color = condicion)) +
  geom_jitter(width = .2) + 
  labs(y = "Tiempo (s)", color = NULL, x = NULL) +
  theme(legend.position = "top")

modelo_pre_basico <- lm(data = data_pre, tiempo_post ~ condicion)
summary(modelo_pre_basico)

modelo_pre <- lm(data = data_pre, tiempo_post ~ condicion + tiempo_pre)
summary(modelo_pre)

errores <- tibble(rho = seq(0, 1, 0.01),
                  sigma_Ancova = (1-rho^2))

errores %>% ggplot(aes(x = rho,
           y = sigma_Ancova)) +
  geom_line() + 
  geom_hline(yintercept = c(0,1), linetype = "dashed")
  theme_bw()
  

# Interacción ####
set.seed(1234)
n <- 50
t <- rnorm(2*n, 50, 5)

x_pre  <- t[1:n]
y_pre  <- t[(n+1):(2*n)]
x_post <- t[1:n] + rnorm(n,  0, 2)
y_post <- t[(n+1):(2*n)] + rnorm(n, 5, 2) + 2 * (t[(n+1):(2*n)]-mean(t)) 

data_pre <- tibble(tiempo_pre = c(x_pre, y_pre),
                   tiempo_post = c(x_post, y_post),
                   condicion = c(rep("Website A", n), rep("Website B", n)))

data_pre %>% ggplot(aes(x = tiempo_pre, y = tiempo_post, color = condicion)) +
  geom_jitter(width = .2) + 
  labs(y = "Tiempo (s)", color = NULL, x = NULL) +
  theme(legend.position = "top")

modelo_pre_basico <- lm(data = data_pre, tiempo_post ~ condicion + tiempo_pre)
summary(modelo_pre_basico)

modelo_interaccion <- lm(data = data_pre, tiempo_post ~ condicion * tiempo_pre)
summary(modelo_interaccion)



## Centrado ####
x_pre  <- t[1:n] - mean(t)
y_pre  <- t[(n+1):(2*n)] - mean(t)

data_pre <- tibble(tiempo_pre = c(x_pre, y_pre),
                   tiempo_post = c(x_post, y_post),
                   condicion = c(rep("Website A", n), rep("Website B", n)))

data_pre %>% ggplot(aes(x = tiempo_pre, y = tiempo_post, color = condicion)) +
  geom_jitter(width = .2) + 
  labs(y = "Tiempo (s)", color = NULL, x = NULL) +
  theme(legend.position = "top")

modelo_pre_basico <- lm(data = data_pre, tiempo_post ~ condicion + tiempo_pre)
summary(modelo_pre_basico)

modelo_interaccion <- lm(data = data_pre, tiempo_post ~ condicion * tiempo_pre)
summary(modelo_interaccion)


## Cuadrático ####
set.seed(1234)
n <- 50
t <- rnorm(2*n, 50, 5)

x_pre  <- t[1:n]
y_pre  <- t[(n+1):(2*n)]
x_post <- t[1:n] + rnorm(n,  0, 2)
y_post <- t[(n+1):(2*n)] + rnorm(n, 5, 2) + .01*t[(n+1):(2*n)]*t[(n+1):(2*n)]

data_pre <- tibble(tiempo_pre = c(x_pre, y_pre),
                   tiempo_post = c(x_post, y_post),
                   condicion = c(rep("Website A", n), rep("Website B", n)))

data_pre %>% ggplot(aes(x = tiempo_pre, y = tiempo_post, color = condicion)) +
  geom_jitter(width = .2) + 
  labs(y = "Tiempo (s)", color = NULL, x = NULL) +
  theme(legend.position = "top")

modelo_pre_basico <- lm(data = data_pre, tiempo_post ~ condicion + tiempo_pre)
summary(modelo_pre_basico)

modelo_interaccion <- lm(data = data_pre, tiempo_post ~ condicion * tiempo_pre)
summary(modelo_interaccion)

### DAGS

beta_hat_simple <- c()
beta_hat_control <- c()
set.seed(1234)
for (i in 1:10000) {
  C <- rnorm(200)
  X <- .5 * C + rnorm(200,.1)
  Y <- .5 * C + .5 * X + rnorm(200, .1)

  m.simple <- lm(Y~X)
  m.control <- lm(Y~X+C)
  
  beta_hat_simple <- c(beta_hat_simple, m.simple$coefficients[2])
  beta_hat_control <- c(beta_hat_control, m.control$coefficients[2])
  
}

mean(beta_hat_simple)
mean(beta_hat_control)


beta_hat_simple <- c()
beta_hat_control <- c()
set.seed(1234)
for (i in 1:10000) {
  X <- rnorm(200)
  Y <- .5 * X + rnorm(200,.1)
  C <- .5 * X + .5 * Y + rnorm(200, .1)
  
  m.simple <- lm(Y~X)
  m.control <- lm(Y~X+C)
  
  beta_hat_simple <- c(beta_hat_simple, m.simple$coefficients[2])
  beta_hat_control <- c(beta_hat_control, m.control$coefficients[2])
  
}


mean(beta_hat_simple)
mean(beta_hat_control)

library(tidyverse)
library(stargazer)

tb <- tibble(
  female = ifelse(runif(10000)>=0.5,1,0),
  ability = rnorm(10000),
  discrimination = female,
  occupation = 1 + 2*ability + 0*female - 2*discrimination + rnorm(10000),
  wage = 1 - 1*discrimination + 1*occupation + 2*ability + rnorm(10000) 
)

tb %>% ggplot(aes(x = as.factor(female),
           y = wage)) +
  geom_jitter(alpha = .1) + 
  labs(x = "Female", y = "Wage") +
  stat_summary(color = "red", size = 2) + 
  theme_bw()
  
tb %>% ggplot(aes(x = as.factor(female),
                  y = occupation)) +
  geom_jitter(alpha = .1) + 
  labs(x = "Female", y = "Occupation") +
  stat_summary(color = "red", size = 2) + 
  theme_bw()

tb %>% ggplot(aes(x = occupation,
                  y = wage)) +
  geom_point(alpha = .1) + 
  labs(x = "Occupation", y = "Wage") +
  geom_smooth(color = "red", method = "lm", se = F) + 
  theme_bw()

lm_1 <- lm(wage ~ female, tb)
lm_2 <- lm(wage ~ female + occupation, tb)
lm_3 <- lm(wage ~ female + occupation + ability, tb)

stargazer(lm_1,lm_2,lm_3, type = "text", 
          column.labels = c("Biased Unconditional", 
                            "Biased",
                            "Unbiased Conditional"))

library(tidyverse)
library(patchwork)

set.seed(3444)

star_is_born <- tibble(
  beauty = rnorm(2500),
  talent = rnorm(2500),
  score = beauty + talent,
  c85 = quantile(score, .85),
  star = ifelse(score>=c85,1,0)
)

p1 <- star_is_born %>% 
  ggplot(aes(x = talent, y = beauty)) +
  geom_point(size = 1, alpha = 0.5) + xlim(-4, 4) + ylim(-4, 4) + 
  geom_smooth(method = 'lm', se = FALSE) + 
  labs(title = "Todos") + 
  theme_bw()
p1

p2 <- star_is_born %>% 
  ggplot(aes(x = talent, y = beauty, color = factor(star))) +
  geom_point(size = 1, alpha = 0.25) + xlim(-4, 4) + ylim(-4, 4) + 
  geom_smooth(method = 'lm', se = FALSE) + 
  labs(title = "Todos, pero diferente") +
  scale_color_brewer(palette = "Dark2", name = "Star") +
  theme_bw() + 
  theme(legend.position = "bottom")
p2

library(patchwork)
p1 + p2

lm_1 <- lm(talent ~ beauty, star_is_born)
lm_2 <- lm(talent ~ beauty + star, star_is_born)

stargazer(lm_1,lm_2, type = "text", 
          column.labels = c("Sin controlar", 
                            "Controlando por star")) 


## Ejemplos de DAGS
dag <- dagitty::dagitty('dag {
C [pos="-0.300,-0.082"]
X [exposure,pos="-2.200,1.5"]
Y [outcome,pos="1.400,1.5"]
C -> X
C -> Y
X -> Y
}')

tidy_dag <- tidy_dagitty(dag)
ggdag(tidy_dag) +
  theme_dag()


dag <- dagitty::dagitty('dag {
Temp. [pos="-0.300,-0.082"]
Short [exposure,pos="-2.200,1.5"]
Helado [outcome,pos="1.400,1.5"]
Temp. -> Short
Temp. -> Helado
Short -> Helado
  }')

tidy_dag <- tidy_dagitty(dag)
ggdag(tidy_dag, text_size = 3) + 
  theme_dag()

tb <- tibble(
  C = rnorm(1000),
  X = 1 + 1*C + rnorm(1000),
  Y = 1 + 2*C + rnorm(1000)
)

library(stargazer)

tb  %>% ggplot(aes(x = X,
           y = Y)) +
  geom_point(alpha = .2) +
  geom_smooth(color = "red", method = "lm", se = F) +
  theme_bw()

  
lm_1 <- lm(Y ~ X, tb)
lm_2 <- lm(Y ~ X + C, tb)

stargazer(lm_1,lm_2, type = "text", 
          column.labels = c("Sin controlar", 
                            "Controlando por C")) 

tb <- tibble(
  C = rnorm(20),
  X = 1 + 1*C + rnorm(20),
  Y = 1 + 2*C + 2*X + rnorm(20)
)

library(stargazer)

tb  %>% ggplot(aes(x = X,
                   y = Y)) +
  geom_point(alpha = .2) +
  geom_smooth(color = "red", method = "lm", se = F) +
  theme_bw()


lm_1 <- lm(Y ~ X, tb)
lm_2 <- lm(Y ~ X + C, tb)

stargazer(lm_1,lm_2, type = "text", 
          column.labels = c("Sin controlar", 
                            "Controlando por C"))

## Colliders ###
## Ejemplos de DAGS
dag <- dagitty::dagitty('dag {
C [pos="-0.300,-0.082"]
X [exposure,pos="-2.200,1.5"]
Y [outcome,pos="1.400,1.5"]
C <- X
C <- Y
X -> Y
}')

tidy_dag <- tidy_dagitty(dag)
ggdag(tidy_dag) +
  theme_dag()

tb <- tibble(
  X = rnorm(10000),
  Y = 1 + 2*X + rnorm(10000),
  C = 1 + 1*X + 1*Y + rnorm(10000)
)

tb %>% ggplot(aes(x = X,
                  y = Y)) +
  geom_point(alpha = .2) +
  geom_smooth(color = "red", 
              method = "lm", 
              se = F) +
  theme_bw()

lm_1 <- lm(Y ~ X, tb)
lm_2 <- lm(Y ~ X + C, tb)

stargazer(lm_1,lm_2, type = "text", 
          column.labels = c("Sin controlar", 
                            "Controlando por C"))

## Ejemplo Google ####

base_dag <- dagitty::dagitty('dag {
bb="0,0,1,1"
D [exposure,pos="0.300,0.300"]
Y [outcome,pos="0.450,0.300"]
D -> Y
}
')
base_dag %>% ggdag() + theme_dag()

google_dag <- dagitty::dagitty('dag {
bb="0,0,1,1"
D [exposure,pos="0.300,0.300"]
F [pos="0.200,0.300"]
O [pos="0.300,0.604"]
Y [outcome,pos="0.450,0.400"]
D <- O
D -> Y
F -> D
O -> Y
}
')
google_dag %>% ggdag() + theme_dag()

discrimination_dag <- dagitty::dagitty('dag {
bb="0,0,1,1"
A [latent,pos="0.450,0.600"]
D [exposure,pos="0.300,0.300"]
F [pos="0.200,0.300"]
O [pos="0.300,0.604"]
Y [outcome,pos="0.450,0.400"]
A -> O
A -> Y
D -> O
D -> Y
F -> D
O -> Y
}
')

discrimination_dag %>% ggdag() + theme_dag()

dag_Ob %>% ggdag_paths() + 
  theme_dag() +
  theme(legend.position = "bottom")

dag_Ob %>% ggdag_paths(from = "Ob", to = "Mort",
                       adjust_for = "Diab", shadow = TRUE) + 
  theme_dag() +
  labs(hue = NULL) +
  theme(legend.position = "bottom")

dag_Ob %>% ggdag_paths(from = "Ob", to = "Mort",
                       adjust_for = c("Diab", "Smok"), shadow = TRUE) + 
  theme_dag() +
  theme(legend.position = "bottom")

discrimination_dag %>% ggdag_adjustment_set() + theme_dag()

discrimination_dag %>% ggdag_collider() + 
  theme_dag() +
  scale_color_brewer(palette = "Dark2")

discrimination_dag %>% ggdag_paths_fan(from = "D", to = "Y") + 
  theme_dag() +
  scale_color_brewer(palette = "Dark2")

discrimination_dag %>% ggdag_paths(from = "D", to = "Y") + 
  theme_dag() 

discrimination_dag %>% ggdag_paths(from = "D", to = "Y",
                          adjust_for = "O", shadow = TRUE) + 
  theme_dag() 

discrimination_dag %>% ggdag_paths(from = "D", to = "Y",
                                   adjust_for = c("O","A"), shadow = TRUE) + 
  theme_dag() 

## Imputation ####
library(dplyr) #for selecting and using the pipe
hsbnomiss <- read_csv('https://stats.idre.ucla.edu/wp-content/uploads/2016/02/hsbdemo.dat')
#indicating the variable names 
hsbnomiss2 <- select(hsbnomiss, 2, 6:8) 
#only select columns we want
names(hsbnomiss2) <- c('female', 'read', 'write', 'math') 
#name the columns
head(hsbnomiss2)

nomiss1 <- lm(write ~ female + read + math, data = hsbnomiss2)
summary(nomiss1)$coef %>% round(3)

library(lavaan)
nomiss2 <- sem('write ~ female + read + math', data = hsbnomiss2)
summary(nomiss2) #lot more information

hsbwmiss <- read.csv('https://stats.idre.ucla.edu/wp-content/uploads/2017/04/hsbmis2.dat', 
                     header = F)
#missing data are coded as -9999, recode to NA
hsbwmiss[hsbwmiss == -9999] <- NA
#I know you can do this in dplyr using some command
#but this is quick and basic
hsbwmiss2 <- dplyr::select(hsbwmiss, 2, 8:10)
names(hsbwmiss2) <- c('female', 'read', 'write', 'math')
hsbwmiss2

library(mice)
md.pattern(hsbwmiss2, rotate.names = T)

nomiss1 <- lm(write ~ female + read + math, data = hsbnomiss2)
wmiss1 <- lm(write ~ female + read + math, data = hsbwmiss2)
library(stargazer) 
stargazer(nomiss1, wmiss1, 
          star.cutoffs = c(.05, .01, .001), 
          no.space = T, type = 'text',
          column.labels = c("Datos completos", 
                            "Datos faltantes"))

imp <- mice(hsbwmiss2, m = 50, seed = 1234)
mi1 <- with(imp, lm(write ~ female + read + math))
summary(pool(mi1))

# LMEM

betalm <- c()
betalmer <- c()

for (i in 1:100) {
  set.seed(i)
  tb <- tibble(
    student = rep(1:10000),
    classroom = sample(1:60, 10000, replace = T)) |>
    group_by(classroom) |>
    mutate(tratamiento = rbinom(1,1,.5),
           mu_classroom = rnorm(1,200, 10) + tratamiento * 2) |>
    ungroup() |>
    mutate(Yij = mu_classroom + rnorm(10000,0,100))
  
  
  # tb |> ggplot(aes(x = as.factor(tratamiento):as.factor(classroom),
  #                  y = Yij,
  #                  color = as.factor(classroom),
  #                  group = as.factor(classroom))) +
  #   geom_jitter() +
  #   stat_summary(color = "black") +
  #   theme(legend.position = "none")
  
  mlm <- lm(Yij ~ tratamiento, data = tb)
  mlmer <- lmer(Yij ~ tratamiento + (1|classroom), data = tb)
  
  betalm <- c(betalm, coef(mlm)[2])
  betalmer <- c(betalmer, fixef(mlmer)[2])
}
sd(betalm)
sd(betalmer)

