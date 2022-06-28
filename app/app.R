library(dplyr)
library(shiny)
library(ggplot2)
library(stringr)
library(patchwork)
library(DT)

data <- readRDS("data.rds") %>%
  filter(fecha >= "2020-05-01")

data_summary <- function(x) {
   m <- mean(x)
   ymin <- quantile(x, probs = 0.1) %>% unname()
   ymax <- quantile(x, probs = 0.9) %>% unname()
   return(c("y"=m,"ymin"=ymin,"ymax"=ymax))
}

options(shiny.sanitize.errors = F)

# data %>%
#   names() %>%
#   str_remove_all(pattern = "(gads_)|(fb_)|(gtics_)") %>%
#   str_remove_all(pattern = "_.*") %>%
#   str_replace(pattern = "^n$", "Número de campañas activas")

shinyApp(

  ui <- fluidPage(
    shiny::selectInput(
      inputId = "variable",
      label = "Variable a graficar",
      choices = data %>% select_if(is.numeric) %>%  names(),
      selected = "gtics_transacciones"
      ),
    shiny::radioButtons(
      inputId = "plottype",
      label = "Seleccione el tipo de gráfico",
      choices = c("Histograma", "Violin"),
      selected = "Histograma",
      inline = T
      ),

    checkboxInput(inputId = "log", "Escala logarítmica"),
    plotOutput("univariate_plot"),
    dataTableOutput("statistics")
  ),

  server <- function(input, output){
    output$univariate_plot <- renderPlot({

      if(str_sub(input$variable, 1, 5) == "gtics"){
        fuente <- "Google Analytics"
      }
      else if(str_sub(input$variable, 1, 4) == "gads"){
        fuente <- "Google Ads"
      }
      else({
        fuente <- "Facebook Ads"
      })

      data_log <- data
      if(input$log){
        data_log[,input$variable] <- data_log[,input$variable] %>%
          pull() %>% +1 %>% log()
      }

      if(input$plottype == "Histograma"){
        dist_plot <- data_log %>%
          ggplot(aes_string(x = input$variable)) +
          geom_histogram(fill = "lightblue", color = "blue") +
          geom_vline(aes(xintercept = median(data_log[,input$variable] %>% pull()),
                         color = "Mediana")) +
          geom_vline(aes(xintercept = mean(data_log[,input$variable] %>% pull()), color = "Media")) +
          scale_color_manual(name = "Estadísticas", values = c("Media" = "black", "Mediana" = "red")) +
          labs(
            title = "Histograma",
            x = input$variable %>%
              str_remove_all(pattern = "(gads_)|(fb_)|(gtics_)") %>%
              str_remove_all(pattern = "_.*") %>%
              str_replace(pattern = "^n$", "Número de campañas activas"),
            y = "Conteo",
            subtitle =  str_c("Fuente: ", fuente))
      }
      else if(input$plottype == "Violin"){

        dist_plot <- data_log %>%
          ggplot(aes_string(x = 1, y = input$variable)) +
          geom_violin(fill = "purple") +
          geom_boxplot(width=0.1) +
          stat_summary(fun.data=data_summary, color = "red") +
          #coord_flip() +
          scale_y_continuous(n.breaks = 10) +
          theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
          labs(
            title = "Distribución",
            subtitle =  str_c("Fuente: ", fuente),
            y = input$variable %>%
              str_remove_all(pattern = "(gads_)|(fb_)|(gtics_)") %>%
              str_remove_all(pattern = "_.*") %>%
              str_replace(pattern = "^n$", "Número de campañas activas"),
            x = "")
      }

      ts_plot <- data_log %>%
        ggplot(aes_string(x = "fecha", y = input$variable)) +
          geom_line(colour = "black") +
          scale_x_date(date_breaks = "1 month") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
          labs(
            title = "Serie histórica",
            subtitle = str_c("Fuente: ", fuente),
            x = "Fecha",
            y = input$variable %>%
              str_remove_all(pattern = "(gads_)|(fb_)|(gtics_)") %>%
              str_remove_all(pattern = "_.*") %>%
              str_replace(pattern = "^n$", "Número de campañas activas")
            )

      ts_plot + dist_plot
    })

    output$statistics <- renderDataTable({
      stats_transacciones <- data %>%
        summarise(
          total = sum(!!sym(input$variable)),
          `desviación estandar` = sd(!!sym(input$variable)),
          media = mean(!!sym(input$variable), na.rm = T),
          `desviación media` = sd(!!sym(input$variable), na.rm = T)/sqrt(n()),
          min = min(!!sym(input$variable), na.rm = T),
          q10 = quantile(!!sym(input$variable), probs = 0.10, na.rm = T),
          q25 = quantile(!!sym(input$variable), probs = 0.25, na.rm = T),
          mediana = median(!!sym(input$variable), na.rm = T),
          q75 = quantile(!!sym(input$variable), probs = 0.75, na.rm = T),
          q90 = quantile(!!sym(input$variable), probs = 0.90, na.rm = T),
          max = max(!!sym(input$variable), na.rm = T),
          `rango intercuartil` = q75 - q25,
          `coeficiente variación` = `desviación estandar`/media
        ) %>%
        round(1)

      stats_transacciones %>%
        DT::datatable(
          rownames = F,
          options=list(dom="t", scrollX = TRUE)
          ) %>%
        DT::formatRound(columns = 1:12, interval = 3, mark = ",", digits = 0)
    })
  },

  options = list(height = 600)
)




