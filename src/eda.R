library(tidyverse)
library(corrplot)
library(heatmaply)
library(fpp3)
library(tsibble)
library(DataExplorer)
library(forecast)
library(skimr)
library(flipTime)

fb <- read_csv("data/Historico_FB_Ads_Elektra_v2.csv",
               locale = locale(encoding = "UTF-8"), na=c("","NA","null"))

gads <- read_csv("data/2022_Data_GoogleAds_InfoAcumulado.csv", skip = 2,
                 locale = locale(decimal_mark = ",", grouping_mark = "."), na=c("","NA","null")) %>%
  rename_with(~str_replace_all(.x, c(" " = "_", "ó"="o", "\\."="", "í"="i")))

gticks_tr <- read_csv2("data/Elektra_Analytics_Transacciones Historico.csv", na=c("","NA","null")) %>%
  rename(Fuente_medio = `Fuente/Medio`) %>%
  mutate(Fecha = dmy(Fecha))


gticks_usd <- read_csv("data/Extracción de datos Producto elektra.csv", na=c("","NA","null")) %>%
  rename(Ingresos = `Ingresos del producto`) %>%
  mutate(Fecha = dmy(Fecha)) %>%
  filter(!is.na(Fecha))

##########################################

skimr::skim(fb)
skimr::skim(gads)
skimr::skim(gticks_tr)
skimr::skim(gticks_usd)

##########################################

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
DataExplorer::plot_missing(gads_grouped)

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
DataExplorer::plot_missing(fb_grouped)

gticks_tr_grouped <- gticks_tr %>%
  group_by(Fecha) %>%
  summarise(
    N_fuente_medio = n_distinct(Fuente_medio),
    Sesiones = sum(Sesiones, na.rm = T),
    Transacciones = sum(Transacciones, na.rm = T),
    Usuarios = sum(Usuarios, na.rm = T)
    )
DataExplorer::plot_missing(gticks_tr_grouped)

gticks_usd_grouped <- gticks_usd %>%
  group_by(Fecha) %>%
  summarise(
    n_distinct_products = n_distinct(Producto),
    Cantidad = sum(Cantidad, na.rm = T),
    Ingresos = sum(Ingresos, na.rm = T)
  )
DataExplorer::plot_missing(gticks_usd_grouped)

full_data <- full_join(
  gads_grouped,
  fb_grouped,
  by = "dia"
  ) %>%
  rowwise() %>%
  mutate(conversiones =  sum(across(matches("conversiones")), na.rm = T)) %>%
  ungroup() %>%
  relocate(conversiones, .after = dia) %>%
  select(-matches("conversiones_")) %>%
  rename_with(~str_replace_all(.x, c(" " = "_"))) %>%
  rename_with(~str_to_lower(.x)) %>%
  filter(dia >= '2021-11-17', dia <= "2022-06-01") %>%
  select(-c(matches("messages"), matches("lead_generation"),
            matches("post_engagement"), matches("shopping"),
            matches("rendimiento_máximo"))) %>%
  map_if(is.numeric, replace_na, replace = 0) %>%
  as_tibble()

DataExplorer::plot_missing(full_data)


#############################################################

gticks_tr_grouped %>%
  #filter(Fecha >= '2021-12-01') %>%
  ggplot(aes(x = Fecha, y = Transacciones)) +
  geom_col(fill = "gray", color = "black") +
  scale_x_date(date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

gticks_usd_grouped %>%
  #filter(Fecha >= '2021-12-01') %>%
  ggplot(aes(x = Fecha, y = Cantidad)) +
  geom_col(fill = "gray", color = "black") +
  scale_x_date(date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#############################################################

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

ts_full_data %>%
  GGally::ggpairs(columns = c(2, 4, 5, 6, 9, 14, 8))

#### Rango tiempo
ts_full_data %>% select(dia, conversiones) %>%
  filter(conversiones != 0) %>%
  filter(row_number() == 1 | row_number() == n())

#### Time Serie "Conversiones"
ts_full_data %>%
  ggplot(aes(x = dia, y = conversiones)) +
  geom_line()

#### Time Series By Year
ts_full_data %>%
  filter(dia >= '2021-11-17', dia < '2022-06-01') %>%
  gg_season(conversiones, labels = "both", period = "year") +
  labs(
    y = "$ USD", x = "Trimsestres",
    title = "Gráfico Estacional: Conversiones Elektra"
    )

#### Time Series By Year (Polar)
ts_full_data %>%
  filter(dia >= '2021-11-17', dia < '2022-06-01') %>%
  gg_season(conversiones, labels = "both", period = "year", polar = T) +
  labs(
    y = "$ USD", x = "Trimsestres",
    title = "Gráfico Estacional: Conversiones Elektra"
    )

# ts_full_data %>%
#   filter(dia >= '2021-11-17', dia < '2022-06-01') %>%
#   gg_subseries(conversiones, period = "day") +
#   labs(
#     y = "$ (millions)", x = "Día de semana",
#     title = "Gráfico Estacional: Conversiones Elektra"
#   )

################################################################################

#### Time Series By Week
ts_full_data %>%
  filter(dia >= '2021-11-17', dia < '2022-06-01') %>%
  gg_season(conversiones, labels = "both", period = "week", ) +
  labs(
    y = "$ USD", x = "Día de semana",
    title = "Gráfico Estacional: Conversiones Eelektra"
    )

#### Time Series By Week (Polar)
ts_full_data %>%
  filter(dia >= '2021-11-17', dia < '2022-06-01') %>%
  gg_season(conversiones, labels = "both", period = "week", polar = T) +
  labs(
    y = "$ USD", x = "Día de semana",
    title = "Gráfico Estacional: Conversiones Eelektra"
    )

#### Time Series By Weekday
ts_full_data %>%
  filter(dia >= '2021-11-17', dia < '2022-06-01') %>%
  gg_subseries(conversiones, period = "week") +
  labs(
    y = "$ (millions)", x = "Día de semana",
    title = "Gráfico Estacional: Conversiones Elektra"
  )

#### Histogram, ACF & PACF
ts_full_data %>%
  gg_tsdisplay(
    y = conversiones,
    lag_max = 15,
    plot_type = "partial"
    )

ts_full_data %>%
  gg_lag(
    y = conversiones,
    geom="point",
    #period = "week",
    lags = 1:7
    )








##############################################


us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

autoplot(us_retail_employment, Employed) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

dcmp <- us_retail_employment %>%
  model(stl = STL(Employed))
components(dcmp)

components(dcmp) %>%
  as_tsibble() %>%
  autoplot(Employed, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

components(dcmp) %>% autoplot()




dcmp2 <- ts_full_data %>%
  model(stl = STL(conversiones))
components(dcmp2)

components(dcmp2) %>%
  as_tsibble() %>%
  autoplot(conversiones, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(
    y = "$ USD",
    title = "Total $USD of sales"
  )

components(dcmp2) %>% autoplot()

components(dcmp2) %>%
  as_tsibble() %>%
  autoplot(conversiones, colour="gray") +
  geom_line(aes(y=season_adjust), colour = "#D55E00") +
  labs(
    y = "$ USD",
    title = "Total $USD of sales"
  )

ts_full_data %>%
  model(
    classical_decomposition(log(conversiones), type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical additive decomposition of total $USD sales")


ts_full_data %>%
  model(
    STL(log(conversiones) ~ trend(window = 9) + season(window = "periodic"),
    robust = TRUE)) %>%
  components() %>%
  autoplot()






