set.seed(1014)

pacman::p_load(dplyr, readr, tidyr, ggplot2, tibble, forcats, patchwork, ggdag, 
               dagitty, stargazer, kableExtra, kableExtra, lme4, modelsummary,
               MatchIt, gtExtras, FNN, here, rdrobust, modelr, rdrobust, rddensity,
               ggside)

pacman::p_load_gh("hadley/emo", "ggmagnify")

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  fig.retina = 2,
  fig.width = 6,
  fig.asp = 2/3,
  fig.show = "hold"
)

options(
  dplyr.print_min = 6,
  dplyr.print_max = 6,
  pillar.max_footer_lines = 2,
  pillar.min_chars = 15,
  stringr.view_n = 6,
  # Temporarily deactivate cli output for quarto
  cli.num_colors = 0,
  cli.hyperlink = FALSE,
  pillar.bold = TRUE,
  width = 77 # 80 - 3 for #> comment
)

ggplot2::theme_set(ggplot2::theme_minimal(12))