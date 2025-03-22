# install.packages("remotes")
# remotes::install_github("wjakethompson/taylor")
library(tidyverse)

# Supongamos que tenemos datos de salarios y edades de 200 empleados de una compañía
# La mitad son asignados al grupo tratamiento y la mitad al grupo control
set.seed(12)
empleados_tbl <- tibble(Grupo = c(rep("tratamiento", 100), rep("control", 100)),
                        Edad = rnorm(n = 200, mean = 35, sd = 5),
                        Salario = Edad*1500 + rnorm(n = 200, mean = 0, sd = 5000))

text_tbl <- tibble(x = min(empleados_tbl$Edad, na.rm = T),
                   y = max(empleados_tbl$Salario, na.rm = T),
                   label = paste("r pearson =", round(cor(empleados_tbl$Edad, empleados_tbl$Salario, use="complete.obs"), 2)))

empleados_tbl |> ggplot(aes(x = Edad,
                            y = Salario)) +
  geom_point(aes(color = Grupo)) +
  geom_smooth(method = "lm", color = "black", se = F) +
  scale_color_brewer(palette = "Dark2") +
  labs(color = NULL) +
  geom_text(data = text_tbl, aes(x = x, y = y, label = label), hjust = 0) +
  theme_bw() +
  theme(legend.position = "top")

I_emp <- 2 # Al que le queremos encontrar la distancia más cercana

empleados_tbl |> ggplot(aes(x = Edad,
                            y = Salario)) +
  geom_point(aes(color = Grupo), alpha = 0.2) +
  geom_point(data = empleados_tbl[I_emp,], aes(color = Grupo), size = 3) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  labs(color = NULL) +
  theme(legend.position = "top")

matriz_varianzas <- matrix(c(var(empleados_tbl$Edad), 0, 0, var(empleados_tbl$Salario)),
                           ncol = 2)
matriz_varianzas_inv <- solve(matriz_varianzas)

matriz_covarianzas <- var(as.matrix(empleados_tbl[,2:3]))
matriz_covarianzas_inv <- solve(matriz_covarianzas)

euclidean <- rep(NA, nrow(empleados_tbl))
mahalanobis <- rep(NA, nrow(empleados_tbl))

for (i in 1:nrow(empleados_tbl)) {
  resta <- as.matrix(empleados_tbl[I_emp,2:3])-as.matrix(empleados_tbl[i,2:3])
  euclidean[i] <- resta %*% matriz_varianzas_inv %*% t(resta)
  mahalanobis[i] <- resta %*% matriz_covarianzas_inv %*% t(resta)
}
min_euclidean <- which.min(euclidean[101:200]) + 100
min_mahalab <- which.min(mahalanobis[101:200]) + 100

library(ggmagnify)
empleados_tbl |> ggplot(aes(x = Edad,
                            y = Salario)) +
  geom_point(aes(color = Grupo), alpha = 0.2) +
  geom_point(data = empleados_tbl[I_emp,], color = "darkorange", size = 3) +
  geom_point(data = empleados_tbl[min_euclidean,], size = 3, color = "black") +
  geom_point(data = empleados_tbl[min_mahalab,], size = 3, color = "steelblue") +
  theme_bw() +
  geom_magnify(from = c(empleados_tbl$Edad[I_emp]*.98, empleados_tbl$Edad[I_emp]*1.01, 
                        empleados_tbl$Salario[I_emp]*.98, empleados_tbl$Salario[I_emp]*1.01), 
               to = c(35, 45, 3E4, 5E4)) +
  scale_color_brewer(palette = "Dark2") +
  labs(color = NULL) +
  theme(legend.position = "top")

# Ahora miremos lo mismo con datos de canciones de Taylor Swift
library(taylor)
text_tbl <- tibble(x = min(taylor_album_songs$energy, na.rm = T),
                   y = max(taylor_album_songs$valence, na.rm = T),
                   label = paste("r pearson =", round(cor(taylor_album_songs$energy, taylor_album_songs$valence, use="complete.obs"), 2)))

taylor_album_songs |> ggplot(aes(x = energy,
           y = valence)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkorange") +
  geom_text(data = text_tbl, aes(x = x, y = y, label = label), hjust = 0) +
  theme_bw()

I_tay <- 102 # Al que le queremos encontrar la distancia más cercana
taylor_album_songs$track_name[I_tay]

taylor_album_songs |> ggplot(aes(x = energy,
                                 y = valence)) +
  geom_point(alpha = .2) +
  geom_point(data = taylor_album_songs[I_tay,], color = "darkorange", size = 3) +
  theme_bw()

matriz_varianzas <- matrix(c(var(taylor_album_songs$energy, na.rm = T), 0, 0, var(taylor_album_songs$valence, na.rm = T)),
                           ncol = 2)
matriz_varianzas_inv <- solve(matriz_varianzas)

matriz_covarianzas <- var(as.matrix(cbind(taylor_album_songs$energy, taylor_album_songs$valence)), na.rm = T)
matriz_covarianzas_inv <- solve(matriz_covarianzas)

euclidean <- rep(NA, nrow(taylor_album_songs))
mahalanobis <- rep(NA, nrow(taylor_album_songs))

for (i in 1:nrow(taylor_album_songs)) {
  resta <- as.matrix(cbind(taylor_album_songs$energy[I_tay], taylor_album_songs$valence[I_tay])) -
    as.matrix(cbind(taylor_album_songs$energy[i], taylor_album_songs$valence[i]))
  euclidean[i] <- resta %*% matriz_varianzas_inv %*% t(resta)
  mahalanobis[i] <- resta %*% matriz_covarianzas_inv %*% t(resta)
}
euclidean[I_tay] <- NA
mahalanobis[I_tay] <- NA
min_euclidean <- which.min(euclidean)
min_mahalab <- which.min(mahalanobis)


taylor_album_songs |> ggplot(aes(x = energy,
                                 y = valence)) +
  geom_point(alpha = .2) +
  geom_point(data = taylor_album_songs[I_tay,], color = "darkorange", size = 3) +
  geom_point(data = taylor_album_songs[min_euclidean,], color = "black", size = 3) +
  geom_point(data = taylor_album_songs[min_mahalab,], color = "steelblue", size = 3) +
  theme_bw()

taylor_album_songs$track_name[min_euclidean]
taylor_album_songs$track_name[min_mahalab]

taylor_album_songs |> ggplot(aes(x = energy,
                                 y = valence)) +
  geom_point(alpha = .2) +
  geom_point(data = taylor_album_songs[I_tay,], color = "darkorange", size = 3) +
  geom_point(data = taylor_album_songs[min_euclidean,], color = "black", size = 3) +
  geom_point(data = taylor_album_songs[min_mahalab,], color = "steelblue", size = 3) +
  theme_bw() +
  geom_magnify(from = c(taylor_album_songs$energy[I_tay]*.9, taylor_album_songs$energy[I_tay]*1.1, 
                        taylor_album_songs$valence[I_tay]*.5, taylor_album_songs$valence[I_tay]*1.5), 
               to = c(.5, .9, .25,.65))
  