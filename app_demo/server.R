library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(patchwork)
library(dplyr)
library(stringr)
library(tidyr)
library(magrittr)
library(purrr)
library(lubridate)
library(DT)
library(googleVis)
library(plotly)
library(modeltime)

library(tidymodels)
library(workflowsets)
library(timetk)
library(sknifedatar)
library(gt)

shinyServer(function(input, output) {

addCssClass(
  class = "action-button bttn bttn-float bttn-xs bttn-default bttn-block bttn-no-outline shiny-bound-input",
  selector = ".btn-file"
  )
# addCssClass(
#   class = "action-button bttn bttn-float bttn-xs bttn-default bttn-block bttn-no-outline shiny-bound-input",
#   selector = ".form-control"
#   )


data_log <- reactive({
  data_log <- data
  if (input$logscale){
    data_log[,input$variable] <- data_log[,input$variable] %>%
      pull() %>% +1 %>% log()
  }
  data_log
})

output$ts_plot <- renderPlotly({

  if (str_sub(input$variable, 1, 5) == "gtics"){
    fuente <- "Google Analytics"
  }
  else if (str_sub(input$variable, 1, 4) == "gads"){
    fuente <- "Google Ads"
  }
  else ({
    fuente <- "Facebook Ads"
  })

  ts_plot <- data_log() %>%
    ggplot(aes_string(x = "fecha", y = input$variable)) +
    geom_line(colour = "black") +
    scale_x_date(date_breaks = "1 month") +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(
      title = "Serie histórica",
      subtitle = str_c("Fuente: ", fuente),
      x = "Fecha",
      y = input$variable %>%
        str_remove_all(pattern = "(gads_)|(fb_)|(gtics_)") %>%
        str_remove_all(pattern = "_.*") %>%
        str_replace(pattern = "^n$", "Número de campañas activas") %>%
        str_to_sentence()
    )

  ts_plotly <- ggplotly(ts_plot)

  })

output$univariate_plot <- renderPlotly({

  if (str_sub(input$variable, 1, 5) == "gtics"){
    fuente <- "Google Analytics"
  }
  else if (str_sub(input$variable, 1, 4) == "gads"){
    fuente <- "Google Ads"
  }
  else ({
    fuente <- "Facebook Ads"
  })

  if (input$plot_type == "Histograma"){
    dist_plot <- data_log() %>%
      ggplot(aes_string(x = input$variable)) +
      geom_histogram(fill = "#f39c12", color = "black") +
      scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
      geom_vline(aes(xintercept = median(data_log()[,input$variable] %>% pull()),
                     color = "Mediana")) +
      geom_vline(aes(xintercept = mean(data_log()[,input$variable] %>% pull()), color = "Media")) +
      scale_color_manual(name = "Estadísticas", values = c("Media" = "black", "Mediana" = "red")) +
      labs(
        title = "Histograma",
        x = input$variable %>%
          str_remove_all(pattern = "(gads_)|(fb_)|(gtics_)") %>%
          str_remove_all(pattern = "_.*") %>%
          str_replace(pattern = "^n$", "Número de campañas activas"),
        y = "Conteo",
        subtitle =  str_c("Fuente: ", fuente))

    dist_plotly <- ggplotly(dist_plot)
  }
  else if (input$plot_type == "Boxplot"){

    dist_plot <- data_log() %>%
      ggplot(aes_string(x = 1, y = input$variable)) +
      geom_boxplot(fill = "#f39c12", width=0.1) +
      stat_summary(fun.data=data_summary, color = "red") +
      scale_y_continuous(n.breaks = 10, labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
      labs(
        title = "Boxplot",
        subtitle =  str_c("Fuente: ", fuente),
        y = input$variable %>%
          str_remove_all(pattern = "(gads_)|(fb_)|(gtics_)") %>%
          str_remove_all(pattern = "_.*") %>%
          str_replace(pattern = "^n$", "Número de campañas activas") %>%
          str_to_sentence(),
        x = "")
    dist_plotly <- ggplotly(dist_plot) %>% plotly::layout(boxgap=0.95)
  }
  else if (input$plot_type == "Violín"){

    dist_plot <- data_log() %>%
      ggplot(aes_string(x = 1, y = input$variable)) +
      geom_violin(fill = "#f39c12",  alpha = 0.6) +
      stat_summary(fun.data=data_summary, color = "red") +
      scale_y_continuous(n.breaks = 10, labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
      geom_boxplot(width=0.1) +
      #theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
      labs(
        title = "Distribución",
        subtitle =  str_c("Fuente: ", fuente),
        y = input$variable %>%
          str_remove_all(pattern = "(gads_)|(fb_)|(gtics_)") %>%
          str_remove_all(pattern = "_.*") %>%
          str_replace(pattern = "^n$", "Número de campañas activas") %>%
          str_to_sentence(),
        x = "")
    dist_plotly <- ggplotly(dist_plot) %>% plotly::layout(boxgap=0.95)
  }

  dist_plotly
  })

output$statistics <- renderDataTable({

  stats_transacciones <- data %>%
    summarise(
      total = sum(!!sym(input$variable), na.rm = T),
      `desviación estandar` = sd(!!sym(input$variable), na.rm = T),
      media = mean(!!sym(input$variable), na.rm = T),
      `desviación media` = sd(!!sym(input$variable), na.rm = T)/sqrt(n()),
      `valor mínimo` = min(!!sym(input$variable), na.rm = T),
      `percentil 10%` = quantile(!!sym(input$variable), probs = 0.10, na.rm = T),
      `percentil 25%` = quantile(!!sym(input$variable), probs = 0.25, na.rm = T),
      `percentil 50% (mediana)` = median(!!sym(input$variable), na.rm = T),
      `percentil 75%` = quantile(!!sym(input$variable), probs = 0.75, na.rm = T),
      `percentil 90%` = quantile(!!sym(input$variable), probs = 0.90, na.rm = T),
      `valor máximo` = max(!!sym(input$variable), na.rm = T),
      `rango intercuartil` = `percentil 75%` - `percentil 25%`#,
      #`coeficiente variación` = `desviación estandar`/media
    ) %>%
    round(1) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Estadístico",
      values_to = "Valor"
      ) %>%
    mutate(Estadístico = str_to_sentence(Estadístico))

  stats_transacciones %>%
    DT::datatable(
      rownames = F,
      options=list(dom="t", scrollX = TRUE, pageLength = 15)
      ) %>%
    DT::formatRound(columns = 2, interval = 3, mark = ",", digits = 0) %>%
    DT::formatStyle(columns = 0, target = "row", lineHeight='80%')
  })

output$calendar <- renderGvis({

  data_log() %>%
    dplyr::group_by(Date = fecha) %>%
    dplyr::summarise(Freq = sum(!!sym(input$variable), na.rm = T)) %>%
    dplyr::mutate(cumsum = cumsum(Freq)) %>%
    dplyr::filter(cumsum >= 1) %>%
    gvisCalendar(
      datevar="Date",
      numvar="Freq",
      options=list(
        colorAxis="{minvalue:0,
                    colors:['orange','red'] }",
        title = paste0("Calendario de ",
                       str_remove_all(input$variable, "(gads_)|(fb_)|(gtics)") %>%
                         str_replace_all("_", " ")),
        width = 2400,
        calendar = "{yearLabel: { fontName: 'Times-Roman',
             fontSize: 32, color: '#1A8763', bold: true},
             cellSize: 12,
             cellColor: { stroke: 'red', strokeOpacity: 0.2 },
             focusedCellColor: {stroke:'white'}}"
      )
  )

})

output$trivariate_holiday_plot <- renderPlot({


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
      x =  cut(
        !!sym(input$covariable),
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
      subtitle = if_else(input$split_date, "¿Es una fecha comercial especial?", ""),
      x = input$covariable,
      y = input$response %>%
        str_remove_all(pattern = "(gads_)|(fb_)|(gtics_)") %>%
        str_remove_all(pattern = "_.*") %>%
        str_replace(pattern = "^n$", "Número de campañas activas") %>%
        str_to_sentence(),
    ) +
    theme(legend.position = "none")
})

output$trivariate_yearly_plot <- renderPlot({

  if (str_sub(input$covariable, 1, 5) == "gtics"){
      fuente <- "Google Analytics"
    }
  else if (str_sub(input$covariable, 1, 4) == "gads"){
      fuente <- "Google Ads"
    }
  else ({
      fuente <- "Facebook Ads"
  })

  data_log <- data %>%
    filter(
      !is.na(!!sym(input$response)),
      !is.na(!!sym(input$covariable))
    )
  if (input$log1){
    data_log[,input$response] <- data_log[,input$response] %>%
      pull() %>% +1 %>% log()
    data_log %<>% filter(!!sym(input$response) > 0)
  }
  if (input$log2){
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

  if (input$split_year){
    p <- p + facet_wrap(~ year(fecha), scales = "free_x")
  }

  p +
    labs(
      x = input$covariable,
      y = input$response %>%
      str_remove_all(pattern = "(gads_)|(fb_)|(gtics_)") %>%
      str_remove_all(pattern = "_.*") %>%
      str_replace(pattern = "^n$", "Número de campañas activas") %>%
      str_to_sentence(),
      ) +
    theme(legend.position="bottom")
  })

output$download <- {downloadHandler(
    filename = function() {
      paste('sample-data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      testing_data %>%
        select(-c(transacciones, yoy, yoy_transacciones)) %>%
        write.csv(con, row.names = F)
    }
  )}

predictions <- reactive({

    if (is.null(new_data())){
      predict_new_data <- testing_data
    }
    else ({
      predict_new_data <- new_data()
    })

  table_forecast <- map(list_models, function( .wf = list_models){
    .model_table <- .wf$.fit_model[[1]] %>%
      modeltime_calibrate(new_data = testing_data, quiet = FALSE) %>%
      mutate(.model_desc = "")

  })

  pred <- bind_rows(table_forecast, .id = ".model_id") %>%
    modeltime_forecast(
      actual_data = training_data,
      new_data = predict_new_data,
      conf_interval =  input$alpha/100
      ) %>%
    mutate(
    .value = round(exp(.value), 0),
    .conf_lo = round(exp(.conf_lo), 0),
    .conf_hi = round(exp(.conf_hi), 0),
  )
})

output$ts_prediction <- renderPlotly({

  predictions() %>%
    plot_modeltime_forecast(
    .conf_interval_show = TRUE,
    .conf_interval_alpha = 1 - input$alpha/100,
    .plotly_slider = T
  )
})

output$predictions <- renderDataTable({

  predictions() %>%
    filter(.key == "prediction") %>%
    select(
      date = .index,
      value = .value,
      conf_low = .conf_lo,
      conf_high = .conf_hi,
      model_id = .model_id,
    ) %>%
    arrange(desc(model_id)) %>%
    datatable(options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'pdf', 'print'),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#f39c12', 'color': '#fff'});",
        "}")
    )) %>%
    DT::formatStyle(columns = 0, target = "row", lineHeight='95%')

})

new_data <- reactive({

  if (is.null(input$file)) return(NULL)

  file <- input$file
    ext <- tools::file_ext(file$datapath)

  req(file)
  validate(need(ext == "csv", "Cargue un archivo .csv"))

  read_csv(file$datapath) %>%
    mutate(
      fecha = as_date(fecha),
      yoy = fecha - as.period(dweeks(52))) %>%
    left_join(
      data %>% select(fecha, yoy_transacciones = gtics_transacciones),
      by = c("yoy" = "fecha")
    )

})


})
