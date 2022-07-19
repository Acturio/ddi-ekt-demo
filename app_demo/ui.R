library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)

source("global.R")

dashboardPage(
  title = "Elektra e-commerce",
  skin = "black",

  dashboardHeader(title = "Future Sales Demo"),

  dashboardSidebar(

    loadingLogo(
      href = 'https://ddilatam.com/',
      src = 'logo.png',
      loadingsrc = 'logo.png',
      height = "70%",
      width = "100%"
    ),

    tags$br(),
    #useShinyjs(),

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
        shinyWidgets::prettyCheckboxGroup(
          inputId = "sources",
          label = "Fuentes de datos",
          choices = c("Google Analytics", "Google Ads", "Facebook Ads"),
          selected = c("Google Analytics", "Google Ads", "Facebook Ads"),
          fill = T,
          icon = icon("check"),
          animation = "tada",
          status = "primary"
        ),
        shinyWidgets::pickerInput(
          inputId = "variable",
          label = "Variable a analizar",
          choices = data %>% select_if(is.numeric) %>%  names(),
          selected = "gtics_transacciones",
          multiple = F,
          options = list(
            `actions-box` = TRUE,
            `deselect-all-text` = "None"
            )
        ),
        shinyWidgets::prettyCheckbox(
          inputId = "logscale",
          label = "Escala logarítmica",
          fill = T,
          icon = icon("check"),
          animation = "tada",
          status = "primary"
        ),
        shinyWidgets::prettyRadioButtons(
          inputId = "plot_type",
          label = "Seleccione tipo de gráfico",
          choices = c("Histograma", "Boxplot", "Violín"),
          selected = "Histograma",
          bigger = T,
          animation = "tada",
          status = "primary"
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
      menuItem(
        "Pronósticos e-commerce",
        tabName = "forecast",
        icon = icon(name = "chart-line"),
        startExpanded = F
      ),
      hr()
    )
  ),

  dashboardBody(

    tags$head(
      tags$style(HTML(".tab-content { padding-left: 20px; padding-right: 30px; }")) ,
      tags$style( HTML("hr {border-top: 1px solid #000000;}") ),

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
          plotOutput("univariate_plot"),
          dataTableOutput("statistics")
        )
      ),
      tabItem(
        tabName = "multi_eda"
      ),
      tabItem(
        tabName = "forecast"
      )
    )
  )
)