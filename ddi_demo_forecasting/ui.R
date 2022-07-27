library(shiny)
#library(shinyjs)
library(shinydashboard)
#library(shinydashboardPlus)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(DT)
library(shinycssloaders)
library(readr)

#source("global.R")

options(shiny.sanitize.errors = F)

dashboardPage(

  title = "Elektra e-commerce",
  skin = "yellow",

  dashboardHeader(
    title = "Future Sales Demo",
    # tagList(
    #     span(class = "logo-lg", "Future Sales Demo"),
    #     img(src = "elektra-logo.png")
    #     ),

    tags$li(
      a(href = 'https://www.elektra.com.gt/',
        img(src = 'elektra-logo.png',
            title = "Elektra", height = "30px"),
        style = "padding-top:10px; padding-bottom:10px;"),
      class = "dropdown"
      )
    ),

  dashboardSidebar(

    tags$head(
      tags$style(HTML('#checkbox :after, #checkbox :before{background-color:#bff442;}')),
      tags$style(".pretty.p-default input:checked~.state label:after {background-color: #f39c12 !important;}"),
      tags$style("@import url(https://use.fontawesome.com/releases/v6.0.0/css/all.css);"),
      tags$style("@import url(https://use.fontawesome.com/releases/v6.1.0/css/all.css);"),
      tags$style(HTML('#file{height: 30px}'))
    ),

    loadingLogo(
      href = 'https://ddilatam.com/',
      src = 'logo.png',
      loadingsrc = 'logo.png',
      height = "70%",
      width = "100%"
    ),
    tags$br(),

    sidebarMenu(
      id = 'sidebar',
      style = "position: relative; overflow: visible;",

      menuItem(
        "Instrucciones",
        tabName = "info",
        icon = icon(name = "info"),
        startExpanded = F
      ),
      menuItem(
        "Análisis Univariado",
        tabName = "uni_eda",
        icon = icon(name = "chart-area"),
        startExpanded = F
      ),

      conditionalPanel(
        "input.sidebar == 'uni_eda'",
        fluidRow(
          column(1),
          column(11,
          shinyWidgets::pickerInput(
            inputId = "variable",
            label = "Variable a analizar",
            choices = data %>% select_if(is.numeric) %>%  names(),
            selected = "gtics_transacciones",
            multiple = F,
            options=pickerOptions(liveSearch=T)
          ),
          shinyWidgets::prettyCheckbox(
            inputId = "logscale",
            label = "Escala logarítmica",
            fill = T,
            icon = icon("check"),
            animation = "jelly",
            status = "warning"
          ),
          shinyWidgets::prettyRadioButtons(
            inputId = "plot_type",
            label = "Seleccione tipo de gráfico",
            choices = c("Histograma", "Boxplot", "Violín"),
            selected = "Histograma",
            icon = icon("check"),
            bigger = T,
            animation = "pulse",
            status = "warning"
            )
          )
        )
      ),
      menuItem(
        "Data Storytelling",
        tabName = "multi_eda",
        icon = icon(name = "dashboard"),
        startExpanded = F
      ),

      conditionalPanel(
        "input.sidebar == 'multi_eda'",
        fluidRow(
          column(1),
          column(11,
            shinyWidgets::pickerInput(
              inputId = "response",
              label = "Variable de respuesta",
              choices = data %>% select_if(is.numeric) %>%  names(),
              selected = "gtics_transacciones",
              multiple = F,
              options=pickerOptions(liveSearch=T)
            ),
            shinyWidgets::prettyCheckbox(
              inputId = "log1",
              label = "Escala logarítmica",
              fill = T,
              icon = icon("check"),
              animation = "jelly",
              status = "warning"
            ),
            shinyWidgets::pickerInput(
              inputId = "covariable",
              label = "Variable explicativa",
              choices = data %>% select_if(is.numeric) %>%  names() %>%
              setdiff("fb_costo_link_clicks"),
              selected = "gads_costo_buscar",
              multiple = F,
              options=pickerOptions(liveSearch=T)
            ),
            shinyWidgets::prettyCheckbox(
              inputId = "log2",
              label = "Escala logarítmica",
              fill = T,
              icon = icon("check"),
              animation = "jelly",
              status = "warning"
            ),
            shinyWidgets::numericInputIcon(
              inputId = "n_interval",
              label = "Número de intervalos",
              value = 5, min = 2, max = 10, step = 1,
              icon = icon("arrows-left-right-to-line", verify_va = F)
            ),
            shinyWidgets::prettyCheckbox(
              inputId = "split_date",
              label = "Diferenciar fechas especiales",
              value = F,
              fill = T,
              icon = icon("check"),
              animation = "jelly",
              status = "warning"
            ),
            shinyWidgets::prettyCheckbox(
              inputId = "split_year",
              label = "Diferenciar por año",
              fill = T,
              icon = icon("check"),
              animation = "jelly",
              status = "warning"
            )
          )
        )
      ),

      menuItem(
        "Pronósticos e-commerce",
        tabName = "forecast",
        icon = icon(name = "chart-line"),
        startExpanded = F
      ),

      conditionalPanel(
        "input.sidebar == 'forecast'",
        fluidRow(
          column(1),
          column(11,
            tags$br(),
            downloadBttn(
              outputId = "download",
              label = "Descargar muestra",
              style = "float",
              color = "default",
              size = "xs",
              block = T,
              no_outline = TRUE,
              icon = shiny::icon("download")
            ),
            shinyjs::useShinyjs(),
            fileInput(
              inputId = "file",
              label = "Cargar archivo",
              multiple = FALSE,
              accept = ".csv"
              ),
            shinyWidgets::numericInputIcon(
              inputId = "alpha",
              label = "Nivel de confianza (%)",
              value = 80, min = 70, max = 95, step = 5,
              size = "sm",
              icon = icon("arrows-left-right-to-line", verify_va = F)
            ),
            tags$br(),
            tags$br()
          )
        )
      ),
      hr()
    )
  ),

  dashboardBody(

    tags$head(
      tags$style(HTML(".tab-content { padding-left: 20px; padding-right: 30px; }")) ,
      tags$style( HTML("hr {border-top: 1px solid #000000;}") )

      ## to not show error message in shiny
      #tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
      #tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
      ),

    tabItems(
      tabItem(
        tabName = 'info',
        div( id = 'objective', objective() ),
        div( id = 'help_data_source', data_source() ),
        div( id = 'nomenclature', nomenclature() ),
        div( id = 'contact', contact() ),
      ),
      tabItem(
        tabName = "uni_eda",
        fluidRow(
          column(6, plotlyOutput("ts_plot")),
          column(6, plotlyOutput("univariate_plot"))
          ),
        tags$br(),
        fluidRow(
          column(5, htmlOutput("calendar", width = '110%')),
          column(2),
          column(4, dataTableOutput("statistics"))
        )
      ),
      tabItem(
        tabName = "multi_eda",
        fluidRow(
          plotlyOutput("trivariate_holiday_plot"),
          plotlyOutput("trivariate_yearly_plot")
        )
      ),
      tabItem(
        tabName = "forecast",
        fluidRow(
          plotlyOutput("ts_prediction")%>%
          shinyWidgets::addSpinner(spin = "folding-cube", color = "#f39c12"),
          tags$br(),
          dataTableOutput("predictions")%>%
          shinyWidgets::addSpinner(spin = "folding-cube", color = "#f39c12")#,
          #dataTableOutput("new_data")
        ),
      )
    )
  )
)