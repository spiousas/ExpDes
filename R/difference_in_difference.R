library(dplyr)
library(readr)
library(ggplot2)
library(here)
#install.packages("forcats")
library(forcats)

data <- read_csv(here("data/billboard_impact.csv"))
head(data)

data_summ <- data |> 
  mutate(poa = fct_recode(as.factor(poa), "Porto Alegre" = "1", "Florianopolis" = "0"),
         jul = fct_recode(as.factor(jul), "Julio" = "1", "Junio" = "0")) |>
  group_by(jul, poa) |>
  summarise(m_deposits = mean(deposits)) 

data |> 
  mutate(poa = fct_recode(as.factor(poa), "Porto Alegre" = "1", "Florianopolis" = "0"),
         jul = fct_recode(as.factor(jul), "Julio" = "1", "Junio" = "0")) |>  
  ggplot(aes(x = jul,
             y = deposits,
             color = as.factor(poa),
             group = as.factor(poa))) +
  geom_point(size = 1, alpha = .3, position = position_dodge(width = .2)) +
  geom_point(data = data_summ, aes(y = m_deposits), size = 3, position = position_dodge(width = .2)) +
  geom_line(data = data_summ, aes(y = m_deposits), size = 1, position = position_dodge(width = .2)) +
  labs(color = NULL, x = NULL, y = "Depósitos") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "top")

data_summ |>
  ggplot(aes(x = jul,
             y = m_deposits,
             color = poa,
             group = poa)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1) +
  labs(color = NULL, x = NULL, y = "Promedio de depósitos") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "top")

# Calculo \hat{ATET} = E[Y(1)|D=1] - E[Y(0)|D=1]
poa_before <- data$deposits[data$poa==1 & data$jul==0]
poa_after <- data$deposits[data$poa==1 & data$jul==1]

mean(poa_after) - mean(poa_before)

# Calculo \hat{ATET} = E[Y(1)|D=1] - E[Y(1)|D=0]
fl_after <- data$deposits[data$poa==0 & data$jul==1]

mean(poa_after) - mean(fl_after)

# Finalmente calculo el estimador DiD
fl_before <- data$deposits[data$poa==0 & data$jul==0]

diff_in_diff <- (mean(poa_after)-mean(poa_before))-(mean(fl_after)-mean(fl_before))
diff_in_diff

# Armemos y grafiquemos el contrafáctico
data_summ |> bind_rows(tibble(jul = as.factor(c("Junio", "Julio")),
                              poa = as.factor(rep("Contrafáctico", 2)),
                              m_deposits = mean(poa_before) + c(0, mean(fl_after)-mean(fl_before)))) |>
  ggplot(aes(x = jul,
             y = m_deposits,
             color = poa,
             group = poa)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1) +
  labs(color = NULL, x = NULL, y = "Promedio de depósitos") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "top")

#install.packages("ggmagnify")
library(ggmagnify)
data_summ |> bind_rows(tibble(jul = as.factor(c("Junio", "Julio")),
                              poa = as.factor(rep("Contrafáctico", 2)),
                              m_deposits = mean(poa_before) + c(0, mean(fl_after)-mean(fl_before)))) |>
  ggplot(aes(x = jul,
             y = m_deposits,
             color = poa,
             group = poa)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1) +
  labs(color = NULL, x = NULL, y = "Promedio de depósitos") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  geom_magnify(from = c(1.9, 2.1, 
                        75, 92), 
               to = c(1, 2, 100, 160)) +
  theme(legend.position = "top")


# Como regresión
library(parameters)
diff_in_diff_model <- lm(data = data,
                         deposits ~ poa * jul)
model_parameters(diff_in_diff_model)

# Organ donation
library(tidyverse)
#install.packages("modelsummary")
library(modelsummary)
#install.packages("fixest")
library(fixest)
#install.packages("causaldata")
library(causaldata)
od <- causaldata::organ_donations
od

# Variable tratamiento
od <- od |>
  mutate(Treated = State == 'California' & 
           Quarter %in% c('Q32011','Q42011','Q12012'))

datasummary_skim(od)
datasummary_balance(data = od, ~Treated)
datasummary_crosstab(data = od, State ~ Treated)

od |>
  filter(!(State %in% "California")) |>
  ggplot(aes(x = Quarter_Num,
             y = Rate)) +
  geom_jitter(width = .2, height = 0, alpha = .2, size = 2) +
  geom_point(data = od |> filter(State %in% "California"), 
             aes(x = Quarter_Num,
                  y = Rate),
             color = "darkorange", size = 3) +
  geom_vline(xintercept = 3.5, linetype = "dashed", linewidth = 1) +
  theme_minimal()
  
od |>
  filter(State %in% c("California", "Arizona", "Alaska")) |>
  ggplot(aes(x = Quarter_Num,
             y = Rate,
             color = State,
             group= State)) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = 3.5, linetype = "dashed", linewidth = 1) +
  theme_minimal()

clfe <- feols(Rate ~ Treated | State + Quarter,
              data = od)
msummary(clfe, stars = c('*' = .1, '**' = .05, '***' = .01))

# Placebos
od <- causaldata::organ_donations %>%
  # Use only pre-treatment data
  filter(Quarter_Num <= 3)

# Create our fake treatment variables
od <- od %>%
  mutate(FakeTreat1 = State == 'California' & 
           Quarter %in% c('Q12011','Q22011'),
         FakeTreat2 = State == 'California' &
           Quarter == 'Q22011')

od |>
  filter(!(State %in% "California")) |>
  ggplot(aes(x = Quarter_Num,
             y = Rate)) +
  geom_jitter(width = .2, height = 0, alpha = .2, size = 2) +
  geom_point(data = od |> filter(State %in% "California"), 
             aes(x = Quarter_Num,
                 y = Rate),
             color = "darkorange", size = 3) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), linetype = "dashed", linewidth = 1) +
  theme_minimal()

# Tratamientos falsos
clfe1 <- feols(Rate ~ FakeTreat1 | State + Quarter,
               data = od)
clfe2 <- feols(Rate ~ FakeTreat2 | State + Quarter,
               data = od)

msummary(list(clfe1,clfe2), stars = c('*' = .1, '**' = .05, '***' = .01))

# En el tiempo
od <- causaldata::organ_donations

# Treatment variable
od <- od %>% mutate(California = State == 'California')

# Interact quarter with being in the treated group using
# the fixest i() function, which also lets us specify
# a reference period (using the numeric version of Quarter)
clfe <- feols(Rate ~ i(Quarter_Num, California, ref = 3) | 
                State + Quarter_Num, data = od)

# And use coefplot() for a graph of effects
coefplot(clfe)
tidy(clfe) |>
  select(c(term, estimate, std.error)) |>
  mutate(Quarter = c(1,2,4,5,6)) |>
  select(-term) |>
  bind_rows(tibble(Quarter = 3, estimate = 0, std.error = 0)) |>
  ggplot(aes(x = Quarter,
             y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  geom_pointrange(aes(ymin = estimate-std.error, ymax = estimate+std.error), color = "darkorange") +
  geom_vline(xintercept = 3.5, linetype = "dashed", linewidth = 1) +
  labs(x = "Quarter", y = "Parámetro estimado") +
  theme_bw()
    
