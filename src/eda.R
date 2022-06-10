library(tidyverse)
library(corrplot)
library(heatmaply)
#library(fpp3)
library(tsibble)

fb <- read_csv("data/Historico_FB_Ads_Elektra_v2.csv",
               locale = locale(encoding = "UTF-8"))

gads <- read_csv("data/2022_Data_GoogleAds_InfoAcumulado.csv", skip = 2,
                 locale = locale(decimal_mark = ",", grouping_mark = ".")) %>%
  rename_with(~str_replace_all(.x, c(" " = "_", "ó"="o", "\\."="", "í"="i")))




gads_grouped <- gads %>%
  mutate_at(vars(Clics:Conversiones),
            ~as.numeric(str_replace(.x, pattern = ",", replacement = ""))) %>%
  group_by(dia = Dia, objetivo_ga = Tipo_de_campaña) %>%
  summarise(
    ga_n = n_distinct(Campaña),
    ga_clicks = sum(Clics, na.rm = T),
    ga_impresiones = sum(Impr, na.rm = T),
    ga_costo = sum(Coste, na.rm = T),
    ga_conversiones = sum(Conversiones, na.rm = T),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = objetivo_ga,
    values_from = c("ga_n", "ga_clicks", "ga_impresiones", "ga_costo", "ga_conversiones"),
    values_fill = 0,
    names_vary = "slowest"
  )


fb_grouped <- fb %>%
  group_by(dia = Dia, objetivo_fb = Objetivo) %>%
  summarise(
    fb_n = n_distinct(Nombre_campaña),
    fb_alcance = sum(Alcance, na.rm = T),
    fb_impresiones = sum(Impresiones, na.rm = T),
    fb_resultados = sum(Resultados, na.rm = T),
    fb_costo = sum(Importe_gastado_USD, na.rm = T),
    fb_clicks = sum(Clics_enlace, na.rm = T),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = objetivo_fb,
    values_from = c("fb_n", "fb_alcance", "fb_impresiones", "fb_resultados", "fb_costo", "fb_clicks"),
    values_fill = 0,
    names_vary = "slowest"
  )

full_data <- full_join(
  gads_grouped,
  fb_grouped,
  by = "dia"
  ) %>%
  rowwise() %>%
  mutate(conversiones =  sum(across(matches("conversiones")), na.rm = T)) %>%
  relocate(conversiones, .after = dia) %>%
  select(-matches("conversiones_")) %>%
  rename_with(~str_replace_all(.x, c(" " = "_"))) %>%
  rename_with(~str_to_lower(.x)) %>%
  #filter(dia >= '2021-11-01') %>%
  select(-c(matches("messages"), matches("lead_generation"),
            matches("post_engagement"), matches("shopping"),
            matches("rendimiento_máximo")))

M = full_data %>%
  select(-dia) %>%
  cor(use = "pairwise.complete.obs")

M %>% .[,1] %>% abs() %>% sort(decreasing = TRUE)


corrplot(M, method = 'number') # colorful number
corrplot(M, method = 'color', order = 'alphabet')
corrplot(M) # by default, method = 'circle'
corrplot(M, order = 'AOE') # after 'AOE' reorder


heatmaply_cor(
  M,
  xlab = "Features",
  ylab = "Features",
  scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
    low = scales::muted("red"),
    high = scales::muted("blue"),
    midpoint = 0,
    limits = c(-1, 1)
  ),
  k_col = 2,
  k_row = 2
)

ts_full_data <- tsibble::as_tsibble(
  full_data,
  index = dia,
  .drop = F
)

ts_full_data %>% select(dia, conversiones) %>%
  filter(conversiones != 0)

ts_full_data %>%
  ggplot(aes(x = dia, y = conversiones)) +
  geom_line()

ts_full_data %>%
  filter(dia >= '2021-11-17', dia < '2022-06-01') %>%
  gg_season(conversiones, labels = "both", period = "year") +
  labs(
    y = "$ USD", x = "Trimsestres",
    title = "Gráfico Estacional: Conversiones Elektra"
    )

ts_full_data %>%
  filter(dia >= '2021-11-17', dia < '2022-06-01') %>%
  gg_season(conversiones, labels = "both", period = "year", polar = T) +
  labs(
    y = "$ USD", x = "Trimsestres",
    title = "Gráfico Estacional: Conversiones Elektra"
    )

ts_full_data %>%
  filter(dia >= '2021-11-17', dia < '2022-06-01') %>%
  gg_subseries(conversiones, period = "year") +
  labs(
    y = "$ (millions)", x = "Día de semana",
    title = "Gráfico Estacional: Conversiones Elektra"
  )

################################################################################

ts_full_data %>%
  filter(dia >= '2021-11-17', dia < '2022-06-01') %>%
  gg_season(conversiones, labels = "both", period = "week", ) +
  labs(
    y = "$ USD", x = "Día de semana",
    title = "Gráfico Estacional: Conversiones Eelektra"
    )

ts_full_data %>%
  filter(dia >= '2021-11-17', dia < '2022-06-01') %>%
  gg_season(conversiones, labels = "both", period = "week", polar = T) +
  labs(
    y = "$ USD", x = "Día de semana",
    title = "Gráfico Estacional: Conversiones Eelektra"
    )

ts_full_data %>%
  filter(dia >= '2021-11-17', dia < '2022-06-01') %>%
  gg_subseries(conversiones, period = "week") +
  labs(
    y = "$ (millions)", x = "Día de semana",
    title = "Gráfico Estacional: Conversiones Elektra"
  )




























