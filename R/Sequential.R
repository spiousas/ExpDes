pacman::p_load(tidyverse)

n_rep <- 10000
n <- 3
d <- 0

p <- rep(NA, n_rep)
for (i in 1:n_rep) {
  data <- rnorm(n, mean = d)
  p_i <- t.test(data)$p.value
  if (between(p_i,0.05,0.15)) {
    p[i] <- t.test(c(data, rnorm(n, mean = d)))$p.value
  } else {
    p[i] <- p_i
  }
}

sum((p<0.05)/n_rep)

tibble(p) |>
  filter(p>.05) |>
  ggplot(aes(x = p)) +
  geom_histogram(color = "darkorange", fill = "darkorange", alpha = .3,
                 binwidth = .05, boundary = 0.5) +
  geom_histogram(data = tibble(p) |> filter(p<.05),
                 color = "steelblue", fill = "steelblue", alpha = .3,
                 binwidth = .05, boundary = 0.5) +
  theme_bw()
    
