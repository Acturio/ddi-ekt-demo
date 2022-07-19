library(dplyr)

data <- readRDS("data/data.rds") %>%
  filter(fecha >= "2020-05-01")

loadingLogo <- function(href, src, loadingsrc, height = NULL, width = NULL, alt = NULL) {
  tagList(
    tags$head(
      tags$script(
        "setInterval(function(){
      if ($('html').attr('class')=='shiny-busy') {
      $('div.busy').show();
      $('div.notbusy').hide();
      } else {
      $('div.busy').hide();
      $('div.notbusy').show();
      }
    },100)")
    ),
    tags$a(href=href,
           div(class = "busy",
               img(src=loadingsrc,height = height, width = width, alt = alt)),
           div(class = 'notbusy',
               img(src = src, height = height, width = width, alt = alt))
    )
  )
}


objective <- function(){
 fluidRow(
  h2( " Objetivo" ),
  tags$p(" Esta aplicación DEMO permite conocer la historia de datos usados y recopilados en
          la actividad de e-commerce del portal de Elektra, Guatemala. A partir de la
          información proporcionada, se desarrollan modelos que permiten pronosticar con
         cierto nivel de certeza un rango de las transacciones futuras."
  )
 )
}

contact <- function(){
 fluidRow(
  h2( " Contacto" ),
  tags$p(" Sus sugerencias, feedback, comentarios o solicitudes son áltamente
          valoradas y nos proveerán una guía para mejorar el dashboard continuamente.

          Por favor escriba a ",
         tags$a( href="mailto:act.arturo.b@ciencias.unam.mx",
                 "act.arturo.b@ciencias.unam.mx",
                 target = '_blank'),
         "."
  )
 )
}

data_source <- function(){
 fluidRow(
  h2(paste0('Fuentes de datos')),
  tags$li(
   HTML(
    paste0("<b>", "Google Analytics: ", "</b>",
           "Esta es la fuente principal de información. Contiene los resultados
            de la actividad comercial tanto en monto de dinero como en número de transacciones,
            productos vendidos, etc. El número de transacciones ha sido seleccionada
            como la variable a pronosticar.")) ),
  tags$br(),
  tags$li(
   HTML(
    paste0("<b>", "Google Ads: ", "</b>",
           "A través de esta fuente de información, se agrega la información asociada a las
           campañas de marketing implementadas en Google Ads. Tanto la inversión
           realizada como los resultados de obtenidos en cada campaña son registrados aquí.
           Estos datos son usados como variables auxiliares para realizar los pronósticos
           de transacciones futuras")) ),
  tags$br(),
  tags$li(
   HTML(
    paste0("<b>", "Facebook Ads: ", "</b>",
           "A través de esta fuente de información, se agrega la información asociada a las
           campañas de marketing implementadas en Facebook Ads. Tanto la inversión
           realizada como los resultados de obtenidos en cada campaña son registrados aquí.
           Estos datos son usados como variables auxiliares para realizar los pronósticos
           de transacciones futuras"))
   )
 )
}

nomenclature <- function(){
 fluidRow(
  h2( " Nomenclatura" ),
  tags$p(" Para hacer un correcto uso de los datos, se proporcionan las instrucciones
         y nomenclatura implementada al nombrar las características y mediciones
         recolectadas de cada una de las fuentes de datos."),
  #tags$br(),
  tags$p("Para el caso de Facebook y Google Ads, las variables que se incluyen en
         este conjunto de datos presenta una nomenclatura particular que depende
         de la información que se analiza en cada columna para cada campaña.
         La nomenclatura es la siguiente:"),
  tags$div(tags$p("origen_métrica_propósito", style="text-align: center;")),
  tags$p("Donde:"),
  #tags$br(),
  tags$li(
    HTML(
      paste0("<b>", "Origen: ", "</b>",
            "Es la fuente de datos (Google Ads, Google Analytics, Facebook)."))
  ),
  tags$br(),
  tags$li(
    HTML(
      paste0("<b>", "Métrica: ", "</b>",
            "Hace referencia a la medición (likes, costo, alcance, conversiones, etc)."))
  ),
  tags$br(),
  tags$li(
    HTML(
      paste0("<b>", "Propósito: ", "</b>",
            "Refiere al objetivo que tiene cada campaña."))
  ),
  tags$br(),
  tags$p("En el caso de Google Analytics, la nomenclatura es ligeramente distinta,
         pues no contienen información de campañas sino de resultados de las
         sesiones en la plataforma de Elektra. Esta nomenclatura sigue la siguiente
         sintaxis:"),
  tags$div(tags$p("origen_métrica", style="text-align: center;"))
 )
}



data_summary <- function(x) {
   m <- mean(x)
   ymin <- quantile(x, probs = 0.1) %>% unname()
   ymax <- quantile(x, probs = 0.9) %>% unname()
   return(c("y"=m,"ymin"=ymin,"ymax"=ymax))
}





