library(dplyr)
library(shiny)
library(ggplot2)
library(stringr)
library(plotly)
library(patchwork)

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
      choices = c("Histograma", "Violin", "qq-plot (normal)", "qq-plot (Chi-square)"),
      selected = "Histograma",
      inline = T
      ),

    plotOutput("univariate_plot"),
    checkboxInput(inputId = "log", "Escala logarítmica")

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

      df <- data.frame(y = data_log[,input$variable] %>% pull()) %>%
          filter(!is.na(y))
        params <- as.list(MASS::fitdistr(
          df$y, "chi-squared", start = list(df = 10),
          method="Brent",lower=0.1,upper=100)$estimate
        )

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
      else if(input$plottype == "qq-plot (normal)"){

        dist_plot <- data_log %>%
          ggplot(aes_string(sample = input$variable)) +
          geom_qq() +
          geom_qq_line(colour = "red") +
          ggtitle("QQ-Plot Normal") +
          labs(
            title = "QQ-Plot Normal",
            subtitle =  str_c("Fuente: ", fuente),
            y = input$variable %>%
              str_remove_all(pattern = "(gads_)|(fb_)|(gtics_)") %>%
              str_remove_all(pattern = "_.*") %>%
              str_replace(pattern = "^n$", "número de campañas activas") %>%
              str_c("Cuantiles de distribución de ", .),
            x = "Cuantiles de distribución normal")
      }
       else if(input$plottype == "qq-plot (Chi-square)"){

        dist_plot <- data_log %>%
          ggplot(aes_string(sample = input$variable)) +
          geom_qq(distribution = qchisq, dparams = params[[1]]) +
          geom_qq_line(distribution = qchisq, dparams = params[[1]], colour = "red") +
          ggtitle("QQ-Plot Chi-square") +
          labs(
            title = "QQ-Plot Chi-square",
            subtitle =  str_c("Fuente: ", fuente),
            y = input$variable %>%
              str_remove_all(pattern = "(gads_)|(fb_)|(gtics_)") %>%
              str_remove_all(pattern = "_.*") %>%
              str_replace(pattern = "^n$", "número de campañas activas") %>%
              str_c("Cuantiles de distribución de ", .),
            x = "Cuantiles de distribución Chi-square")
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
  },

  options = list(height = 600)
)




