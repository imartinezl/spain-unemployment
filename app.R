
library(shiny)


source('~/Downloads/spain-unemployment/app_functions.R')
setwd("~/Downloads/spain-unemployment")

min_date <- '2011-01-01'
max_date <- '2019-01-01'
# today <- Sys.time() %>% as.Date()

ui <- shiny::fluidPage(
  theme = shinythemes::shinytheme("sandstone"),
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  # shiny::tags$hr(),
  shiny::fluidRow(
    shiny::column(4,
                  shiny::tags$h3("Observatorio del Desempleo en España"),
                  shiny::tags$p('Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.'),
                  dateInput2("date", label="Fecha", value = max_date, min = min_date, max = max_date, width = "150px",
                             format = "MM-yyyy", startview = "month", endview = "year", weekstart = 1, language = "es"),
                  shiny::selectInput("minPob", "Población Minima", choices = c(0,100,1000,10000,40000), selected=1000, width = "150px")
                  # shiny::selectInput("maxPob", "Población Maxima", choices = c(0,100,1000,10000,40000,500000,2000000), selected=2000000),
                  # shiny::sliderInput("slider", label = h3("Slider Range"), min = 0, max = 2000000, value = c(0, 2000000))
    ),
    
    shiny::column(8,
                  plotly::plotlyOutput("plot_paro_provincia", width = "100%", height = "950px")
    )
  ),
  shiny::fluidRow(
    shiny::tableOutput('tbl')
  )
)

server <- function(input, output) {
  
  values <- shiny::reactiveValues()
  shiny::observeEvent(input$date, {
    values$year_ <- lubridate::ymd(input$date) %>% lubridate::year()
    values$month_ <- lubridate::ymd(input$date) %>% lubridate::month()
    values$unemployment_rate_data <- unemployment.data(values$year_, values$month_, unemployment_dists) %>% 
      unemployment.rate.data(values$year_) %>% 
      dplyr::filter(tasa_paro < 50)
  })
  shiny::observeEvent({
    values$unemployment_rate_data
    input$minPob
  },{
    new_data <- values$unemployment_rate_data  %>%
      dplyr::filter(poblacion > as.numeric(input$minPob)) %>% 
      dplyr::group_by(Provincia) %>% 
      dplyr::mutate(tasa_paro_media = mean(tasa_paro), 
                    tasa_paro_max = max(tasa_paro)) %>% 
      dplyr::ungroup()
    output$tbl <- shiny::renderTable(
      {values$unemployment_rate_data %>% 
        dplyr::select(Provincia, Municipio, Comunidad.Autónoma, poblacion, poblacion_activa, total.Paro.Registrado, tasa_paro) %>% 
        dplyr::rename(Paro.Registrado = total.Paro.Registrado,
                      Poblacion_Total = poblacion,
                      Poblacion_Activa = poblacion_activa,
                      Tasa_Desempleo = tasa_paro)
      }, width = '500px', rownames=F
    )
    output$plot_paro_provincia <- plotly::renderPlotly({
      plot.unemployment(new_data, values$year_, values$month_)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

