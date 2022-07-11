library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(patchwork)
library(DT)
library(magrittr)
library(lubridate)
library(tidyr)
library(purrr)

data <- readRDS("data.rds") %>%
  mutate(
    Festivo = case_when(
between(fecha, as_date("2021-04-27"), as_date('2021-05-16')) ~ "Sí",
      between(fecha, as_date("2022-04-28"), as_date('2022-05-14')) ~ "Sí",
      between(fecha, as_date("2020-11-09"), as_date('2020-11-20')) ~ "Sí",
      between(fecha, as_date("2021-11-10"), as_date('2021-11-16')) ~ "Sí",
      between(fecha, as_date("2020-11-25"), as_date('2020-11-30')) ~ "Sí",
      between(fecha, as_date("2021-11-25"), as_date('2021-11-30')) ~ "Sí",
      between(fecha, as_date("2020-07-12"), as_date('2020-07-25')) ~ "Sí",
      between(fecha, as_date("2021-07-12"), as_date('2021-07-25')) ~ "Sí",
      between(fecha, as_date("2021-03-24"), as_date('2021-03-28')) ~ "Sí",
      between(fecha, as_date("2022-03-24"), as_date('2022-03-28')) ~ "Sí",
      between(fecha, as_date("2020-07-12"), as_date('2020-07-17')) ~ "Sí",
      between(fecha, as_date("2021-07-12"), as_date('2021-07-17')) ~ "Sí",
      between(fecha, as_date("2022-07-12"), as_date('2022-07-17')) ~ "Sí",
      between(fecha, as_date("2021-09-24"), as_date('2021-09-30')) ~ "Sí",
      between(fecha, as_date("2022-09-24"), as_date('2022-09-30')) ~ "Sí",
      TRUE ~ "No")) %>%
  filter(fecha >= "2020-06-01")

options(shiny.sanitize.errors = F)

ui <- fluidPage(

  #titlePanel("Multivariate Exploratory Data Analysis"),

  sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "response",
          choices = data %>% select_if(is.numeric) %>%  names(),
          label = "Variable de respuesta",
          selected = "gtics_transacciones"
        ),
        checkboxInput(inputId = "log1", "Escala logarítmica"),
        selectInput(
          inputId = "covariable",
          choices = data %>% select_if(is.numeric) %>%  names() %>%
            setdiff("gads_n_shopping"),
          label = "Variable explicativa",
          selected = "gads_costo_buscar"
        ),
        checkboxInput(
          inputId = "log2",
          label = "Escala logarítmica"),
        shiny::tags$hr(),
        numericInput(
          inputId = "n_interval",
          label = "Número de intervalos",
          value = 3, min = 2, max = 10, step = 1),
        width = 4
      ),

      mainPanel(
        checkboxInput(
          inputId = "split_date",
          label = "Diferenciar días comerciales especiales"),
        plotOutput("bivariate_plot"),
        checkboxInput(
          inputId = "split_year",
          label = "Diferenciar por año"),
        plotOutput("trivariate_plot"),
        width = 7
      )
  )
)

server <- function(input, output) {

  output$bivariate_plot <- renderPlot({

    if(str_sub(input$covariable, 1, 5) == "gtics"){
        fuente <- "Google Analytics"
      }
    else if(str_sub(input$covariable, 1, 4) == "gads"){
        fuente <- "Google Ads"
      }
    else({
        fuente <- "Facebook Ads"
    })

    data_log <- data
    if(input$log1){
      data_log[,input$response] <- data_log[,input$response] %>%
        pull() %>% +1 %>% log()
    }
    if(input$log2){
      data_log[,input$covariable] <- data_log[,input$covariable] %>%
        pull() %>% +1 %>% log()
    }

    data_summary <- data_log %>%
      mutate(
        #y = cut_interval(!!sym(input$response), n = m1, dig.lab = 1, labels = F),
        x =  cut(!!sym(input$covariable),
                 breaks = c(quantile(pull(data_log, !!sym(input$covariable)),
                 probs = seq(0, 1, by = 1/as.numeric(input$n_interval)),
                 na.rm = TRUE)),
                 dig.lab = 4)
        )

    if(input$split_date){
      data_summary %<>% group_by(Festivo, x)
    }
    else({
      data_summary %<>% group_by(x)
    })

    p <- data_summary %>%
      summarise(
        n = n(),
        y_mean = mean(!!sym(input$response), na.rm = T),
        y_se = sd(!!sym(input$response), na.rm = T)/sqrt(n()),
        y_sd = sd(!!sym(input$response), na.rm = T),
        .groups = "drop"
      ) %>%
      filter(!is.na(x)) %>%
      mutate(x = as.character(x)) %>%
      mutate(
        ymin = y_mean - 1.64 * y_se,
        ymax = y_mean + 1.64 * y_se,
        x = str_replace_all(x, c(","="-", "\\("="", "\\)"="", "\\]"="", "\\["="") )
      ) %>%
      separate(x, into = c("x1", "x2"), sep = "-") %>%
      map_at(vars("x1", "x2"), ~format(round(as.numeric(.x), 1), big.mark = ",")) %>%
      as_tibble() %>%
      unite(x, x1, x2, sep = " - ") %>%
      ggplot(aes(x = x, y_mean, colour = x)) +
      geom_point() +
      geom_errorbar(aes(ymin = ymin, ymax = ymax ), width = 0.1) +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))

    if(input$split_date){
      p <- p + facet_wrap(~ Festivo, ncol = 2)
    }

    p +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      labs(
        #title = "Relación entre transacciones e inversión",
        subtitle = if_else(input$split_date, "¿Es una fecha comercial especial?", ""),
        x = input$covariable,
        y = input$response,
      ) +
      theme(legend.position = "none")

  })

  output$trivariate_plot <- renderPlot({

    if(str_sub(input$covariable, 1, 5) == "gtics"){
        fuente <- "Google Analytics"
      }
    else if(str_sub(input$covariable, 1, 4) == "gads"){
        fuente <- "Google Ads"
      }
    else({
        fuente <- "Facebook Ads"
    })

    data_log <- data %>%
      filter(
        !is.na(!!sym(input$response)),
        !is.na(!!sym(input$covariable))
      )
    if(input$log1){
      data_log[,input$response] <- data_log[,input$response] %>%
        pull() %>% +1 %>% log()
      data_log %<>% filter(!!sym(input$response) > 0)
    }
    if(input$log2){
      data_log[,input$covariable] <- data_log[,input$covariable] %>%
        pull() %>% +1 %>% log()
      data_log %<>% filter(!!sym(input$covariable) > 0)
    }

    p <- data_log %>%
      as_tibble() %>%
      ggplot(aes_string(
        x = input$covariable,
        y = input$response,
        label = "fecha",
        colour = "Festivo")
      ) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm")

    if(input$split_year){
      p <- p + facet_wrap(~ year(fecha), scales = "free_x")
    }

    p +
      labs(
        #title = "Comparación de transacciones e inversión",
        #subtitle = paste("Fuente:", fuente),
        x = input$covariable,
        y = input$response,
        ) +
      theme(legend.position="bottom")

  })

}

shinyApp(ui = ui, server = server)
