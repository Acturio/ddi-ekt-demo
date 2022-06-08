library(tidyverse)

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

gads_grouped %>% count(objetivo_ga)
fb_grouped %>% count(objetivo_fb)


full_join(
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
  glimpse()





