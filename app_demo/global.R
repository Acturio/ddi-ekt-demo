
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


contact <- function(){
 fluidRow(
  h2( " Contacto" ),
  tags$p(" Esta aplicación DEMO permite conocer la historia de datos usados y recopilados en 
          la actividad de e-commerce del portal de Elektra. Esta información es usada para 
          pronosticar un rango de las transacciones futuras."),
  tags$br(),
  tags$p("Sus sugerencias, feedback, comentarios o solicitudes son áltamente 
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
           de transacciones futuras")) )
 )
}
