
library(shiny)

# setwd("~/Downloads/spain-unemployment")
source('app_functions.R')

min_date <- '2011-01-01'
max_date <- '2019-01-01'
unemployment_path <- './unemployment_data/'
unemployment_filename <- paste0(unemployment_path,'/unemployment_data.csv')
unemployment_data <- data.table::fread(unemployment_filename, stringsAsFactors = F)

# today <- Sys.time() %>% as.Date()
title <- "Observatorio del Desempleo en España"
subtitle <- "Visualización de datos de desempleo por municipios en España"
ui <- shiny::fluidPage(
  theme = shinythemes::shinytheme("sandstone"),
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  
  # shiny::tags$hr(),
  shiny::fluidRow(
    shiny::column(5, id = "leftColumn",
                  shiny::tags$h3(title),
                  shiny::tags$p(subtitle),
                  shiny::tags$hr(),
                  shiny::fluidRow(
                    shiny::column(3, dateInput2("date", label="Fecha", value = max_date, min = min_date, max = max_date, width = "150px",
                                                format = "MM-yyyy", startview = "month", endview = "year", weekstart = 1, language = "es")),
                    shiny::column(3, shiny::selectInput("minPob", "Habitantes", choices = c("> 0","> 100","> 1000","> 10000","> 40000"), selected="> 1000", width = "150px")),
                    shiny::column(3, shiny::downloadButton("download", label = "Descargar Datos"))
                  ),
                  shiny::HTML('<iframe width="100%" height="630" frameborder="0" src="https://imartinezl.carto.com/builder/e60b7971-76ba-472f-a444-9410380f315e/embed" allowfullscreen webkitallowfullscreen mozallowfullscreen oallowfullscreen msallowfullscreen></iframe>')
    ),
    
    shiny::column(7,
                  shinycssloaders::withSpinner(plotly::plotlyOutput("plot_paro_provincia", 
                                                                    width = "100%", height = "950px"))
    )
  ),
  shiny::tags$hr(),
  shiny::fluidRow( 
    shiny::column(6, 
                  shiny::tags$h4("Top 5 Municipios con Mayor Desempleo"),
                  shiny::tableOutput('mayor_desempleo')),
    shiny::column(6, 
                  shiny::tags$h4("Top 5 Municipios con Menor Desempleo"),
                  shiny::tableOutput('menor_desempleo'))
  ),
  shiny::tags$hr(),
  shiny::tags$h4("Evolucion Temporal del Desempleo"),
  shiny::fluidRow( 
    shiny::column(2,
                  shiny::selectInput("ccaa", "Comunidad Autonoma", width = "200px",
                                     unemployment_data$Comunidad_Autonoma %>% unique() %>% sort()),
                  shiny::uiOutput("provUI"),
                  shiny::uiOutput("muniUI")
    ),
    shiny::column(5,
                  shinycssloaders::withSpinner(plotly::plotlyOutput("plot_timeline",
                                                                    width = "100%", height = "500px"))
    ),
    shiny::column(5,
                  plotly::plotlyOutput("plot_sankey",
                                       width = "100%", height = "500px")
    )
  )
)
last_date <- NULL

server <- function(input, output) {
  
  values <- shiny::reactiveValues()
  shiny::observeEvent(input$date, {
    values$year_ <- lubridate::ymd(input$date) %>% lubridate::year()
    values$month_ <- lubridate::ymd(input$date) %>% lubridate::month()
    values$unemployment_filename <- paste0(unemployment_path,'/', paste(c("unemployment", values$year_ ,values$month_), collapse = "_" ), ".csv")
    values$unemployment_data <- read.csv(values$unemployment_filename, stringsAsFactors = F) %>% 
      dplyr::filter(Tasa_Desempleo < 50)
  })
  shiny::observeEvent({
    values$unemployment_data
    input$minPob
  },{
    new_data <- values$unemployment_data  %>%
      dplyr::filter(Habitantes > as.numeric(stringr::str_sub(input$minPob, 3))) %>% 
      dplyr::group_by(Provincia) %>% 
      dplyr::mutate(Tasa_Desempleo_Media = mean(Tasa_Desempleo), 
                    Tasa_Desempleo_Max = max(Tasa_Desempleo)) %>% 
      dplyr::ungroup()
    output$plot_paro_provincia <- plotly::renderPlotly({
      plot.unemployment(new_data, values$year_, values$month_)
    })
    output$mayor_desempleo <- shiny::renderTable(
      {new_data %>% 
          dplyr::select(Municipio, Provincia, Comunidad_Autonoma, Habitantes, Poblacion_Activa, Desempleados, Tasa_Desempleo) %>% 
          dplyr::rename("Comunidad Autonoma" = Comunidad_Autonoma, "Poblacion Activa" = Poblacion_Activa) %>%
          dplyr::top_n(5, wt = Tasa_Desempleo) %>% 
          dplyr::slice(1:5) %>%
          dplyr::arrange(-Tasa_Desempleo)
      }, width = '100%', rownames=F, hover = F, spacing = 'xs', digits = 2, align = 'c'
    )
    output$menor_desempleo <- shiny::renderTable(
      {new_data %>% 
          dplyr::select(Municipio, Provincia, Comunidad_Autonoma, Habitantes, Poblacion_Activa, Desempleados, Tasa_Desempleo) %>% 
          dplyr::rename("Comunidad Autonoma" = Comunidad_Autonoma, "Poblacion Activa" = Poblacion_Activa) %>% 
          dplyr::top_n(5, wt = -Tasa_Desempleo) %>% 
          dplyr::slice(1:5) %>% 
          dplyr::arrange(Tasa_Desempleo)
      }, width = '100%', rownames=F, hover = F, spacing = 'xs', digits = 2, align = 'c'
    )
  })
  output$download <- downloadHandler(
    filename = function() {
      values$unemployment_filename
    },
    content = function(file) {
      write.csv(values$unemployment_data, file, row.names = FALSE)
    }
  )
  output$provUI <- shiny::renderUI({
    shiny::req(input$ccaa)
    if(!is.null(input$ccaa)){
      shiny::selectInput("prov", "Provincia", width = "200px",
                         unemployment_data %>% 
                           dplyr::filter(Comunidad_Autonoma == input$ccaa) %>% 
                           dplyr::pull(Provincia) %>% unique() %>% sort())
    }
  })
  output$muniUI <- shiny::renderUI({
    shiny::req(input$prov)
    if(!is.null(input$prov)){
      shiny::selectInput("muni", "Municipio", width = "200px",
                         unemployment_data %>%
                           dplyr::filter(Provincia == input$prov) %>%
                           dplyr::pull(Municipio) %>% unique() %>% sort())
    }
  })
  output$plot_timeline <- plotly::renderPlotly({
    if(!is.null(input$muni)){
      plot.timeline(unemployment_data, input$ccaa, input$prov, input$muni) %>% 
        plotly::event_register('plotly_click')
    }
  })
  last_date <- NULL
  output$plot_sankey <- plotly::renderPlotly({
    shiny::req(input$muni)
    # print(last_date)
    s <- plotly::event_data("plotly_click", source = "timeline")
    # shiny::validate(shiny::need(!is.null(s), "Hover over the time series chart to populate this heatmap"))
    if (is.null(s)) {
      date <- last_date
    }else{
      date <- as.list(s)$x
      last_date <<- date
    }
    # print(date)
    plot.sankey(unemployment_data,
                shiny::isolate(input$ccaa),
                shiny::isolate(input$prov),
                shiny::isolate(input$muni), date)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

