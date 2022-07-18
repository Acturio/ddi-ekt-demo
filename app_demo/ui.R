library(shiny)
library(shinydashboard)
library(shinyWidgets)

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

    sidebarMenu(
      id = 'sidebar',
      style = "position: relative; overflow: visible;",

      menuItem(" Instrucciones", tabName = "info", icon = icon(name = "info"), startExpanded = T),
      menuItem(
        " Análisis Univariado", icon = icon(name = "chart-area"), startExpanded = F,
          shinyWidgets::prettyRadioButtons(
            inputId = "button_coord",
            label = "Seleccione método",
            choices = c("Dirección" = "dir", "GPS" = "gps", "Coordenadas" = "coord"),
            selected = "coord",
            bigger = T,
            animation = "smoth"
          )
      ),
      menuItem("Data Storytelling", icon = icon(name = "dashboard"), startExpanded = F),
      menuItem("Pronósticos e-commerce", icon = icon(name = "chart-line"), startExpanded = F)
    )
  ),

  dashboardBody(

    tags$head(
      tags$style(HTML(".tab-content { padding-left: 20px; padding-right: 30px; }")) ,
      tags$style( HTML("hr {border-top: 1px solid #000000;}") ),

      ## to not show error message in shiny
      tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
      tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
      ),

    tabItems(
      tabItem(
        tabName = 'info',
        div( id = 'instrucciones', instrucciones() ),
        div( id = 'help_data_source', data_source() ),
      )
    )
  )
)