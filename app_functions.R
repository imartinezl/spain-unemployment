
library(dplyr)
setwd("~/Downloads/spain-unemployment")

fetch_url <- function(url){
  h <- curl::new_handle()
  curl::handle_setopt(h, ssl_verifyhost = 0, ssl_verifypeer=0)
  req <- curl::curl_fetch_memory(url, handle = h)
  req_data <- NULL
  if(req$status_code == 200){
    req_data <- jsonlite::fromJSON(rawToChar(req$content))
  }
  return(req_data)
}
fetch_dataset <- function(id){
  url_template <- "http://datos.gob.es/apidata/catalog/dataset/"
  url <- paste0(url_template, id)
  req_data <- fetch_url(url)
  return(req_data)
}
extract_distributions <- function(req_data){
  distributions <- req_data$result$items$distribution[[1]]
  distributions_year <- dplyr::select(distributions,-format) %>% 
    dplyr::bind_cols(distributions$format) %>% 
    dplyr::filter(grepl("csv",value)) %>% 
    dplyr::mutate(year = as.numeric(title))
  return(distributions_year)
}
unemployment.dists <- function(){
  # https://datos.gob.es/es/catalogo/e00142804-paro-registrado-por-municipios
  id <- "e00142804-paro-registrado-por-municipios"
  unemployment_dists <- fetch_dataset(id) %>% extract_distributions()
}
unemployment.data <- function(year_, month_, unemployment_dists){
  month_name <- lubridate::dmy(paste('1',month_,year_)) %>% lubridate::month(label = T, abbr = F)
  unemployment_data <- unemployment_dists %>% 
    dplyr::filter(year == year_) %>% 
    dplyr::pull(accessURL) %>% 
    read.csv(skip=1, header=T, sep=';',check.names = T, fileEncoding = "ISO-8859-1", stringsAsFactors = F) %>%
    dplyr::mutate(month = Código.mes %% 100,
                  year = floor(Código.mes/100)) %>% 
    dplyr::filter(month == month_)
}

unemployment_dists <- unemployment.dists()
unemployment_years <- unemployment_dists %>% dplyr::pull(year)
# month_ <- 1
# year_ <- 2019
# unemployment_data <- unemployment.data(year_, month_, unemployment_dists)

unemployment.rate.data <- function(unemployment_data, year_){
  poblacion_activa_edades <- c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")
  padron_files <- list.files('./padron/', full.names = T)
  padron_years <- stringr::str_match(padron_files, "_(\\d+).px")[,2] %>% as.numeric()
  selected <- which.min(abs(padron_years - year_))
  padron_data <- pxR::read.px(padron_files[selected])$DATA$value %>% 
    dplyr::rename(poblacion = value)
  
  poblacion_total <- padron_data %>% 
    dplyr::filter(sexo == "Ambos sexos",
                  edad..grupos.quinquenales. == "Total",
                  municipios != "Total")  %>% 
    dplyr::mutate(cod_mun = stringr::str_sub(municipios,1,5) %>% as.numeric,
                  municipios = stringr::str_sub(municipios,7))
  poblacion_activa <- padron_data %>% 
    dplyr::filter(sexo == "Ambos sexos",
                  edad..grupos.quinquenales. %in% poblacion_activa_edades,
                  municipios != "Total") %>% 
    dplyr::group_by(municipios) %>% 
    dplyr::summarise(poblacion_activa = sum(poblacion)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(cod_mun = stringr::str_sub(municipios,1,5) %>% as.numeric,
                  municipios = stringr::str_sub(municipios,7))
  unemployment_rate_data <- merge(merge(poblacion_total, poblacion_activa), unemployment_data, by.x = "cod_mun", by.y = "Codigo.Municipio") %>% 
    dplyr::mutate(tasa_paro = total.Paro.Registrado/poblacion_activa*100)
}
plot.unemployment <- function(unemployment_rate_data, year_, month_){
  tasa_paro_maxima <- unemployment_rate_data$tasa_paro %>% max()
  month_name <- lubridate::dmy(paste('1',month_,year_)) %>% lubridate::month(label = T, abbr = F)
  
  provincias <- unemployment_rate_data %>% 
    dplyr::select(Provincia, tasa_paro_max, tasa_paro_media) %>% 
    unique()
  
  provincia_top <- unemployment_rate_data %>%
    dplyr::slice(which.min(unemployment_rate_data$tasa_paro_media)) %>% 
    dplyr::pull(Provincia)
  municipio_min <- unemployment_rate_data %>%
    dplyr::filter(Provincia == provincia_top) %>% 
    dplyr::mutate(tasa_paro_min = min(tasa_paro)) %>%
    dplyr::filter(tasa_paro == tasa_paro_min) %>% 
    dplyr::mutate(texto = paste0("Municipio con la <i>menor</i><br>tasa de paro en ", provincia_top)) %>% 
    dplyr::slice(1)
  municipio_max <- unemployment_rate_data %>%
    dplyr::filter(Provincia == provincia_top) %>% 
    dplyr::mutate(tasa_paro_max = max(tasa_paro)) %>%
    dplyr::filter(tasa_paro == tasa_paro_max) %>% 
    dplyr::mutate(texto = paste0("Municipio con la <i>mayor</i><br>tasa de paro en ", provincia_top)) %>% 
    dplyr::slice(1)
  
  bg_color <- "#090f28"
  # bg_color <- 'rgba(0,0,0,0)'
  text_color <- "#e8eeff"
  grid_color <- "#113251"
  line_color <- "#113251"
  tick_color <- "#113251"
  gradient_colors <- list(c(0,'#ec2F4B'),c(1,'#009FFF'))
  font_family <- "Roboto"
  m <- list(l = 50, r = 50, b = 100, t = 100, pad = 0 )
  xaxis <- list(
    title='Tasa de Desempleo (%)', titlefont=list(color=text_color,familiy=font_family,size=17),
    zeroline = F, position=0, range=c(-4,tasa_paro_maxima+5),
    tickfont = list(color = text_color,family=font_family,size=14),
    ticks='outside', tick0=0, dtick=5,
    ticklen=4, tickwidth=1, tickcolor=tick_color,
    gridcolor=grid_color, gridwidth=0.25,
    linecolor=line_color, linewidth=2
  )
  xaxis2 <- list(
    title='Tasa de Desempleo (%)', titlefont=list(color=text_color,familiy=font_family,size=17),
    zeroline = F, position=1, range=c(-4,tasa_paro_maxima+5),
    overlaying = "x", side = "top",
    tickfont = list(color = text_color,family=font_family,size=14),
    ticks='outside', tick0=0, dtick=5,
    ticklen=4, tickwidth=1, tickcolor=tick_color,
    gridcolor=grid_color, gridwidth=0.25,
    linecolor=line_color, linewidth=2
  )
  yaxis <- list(
    title='', zeroline = F, showticklabels=F,
    showgrid=T, dtick=1, 
    gridcolor=grid_color, gridwidth=0.25
  )
  
  
  unemployment_rate_data %>% 
    plotly::plot_ly(type = "scatter", mode="markers", name = 'Tasa de Desempleo', #width = 1200, height = 1080, 
                    x = ~tasa_paro, y = ~reorder(Provincia, -tasa_paro_media),
                    marker=list(color=~tasa_paro, colorbar=F, colorscale= gradient_colors, reversescale =T),
                    text = ~paste(Municipio, ' ', round(tasa_paro,2), '%'), hoverinfo = 'text' ) %>%
    plotly::add_trace(data = provincias, x = ~tasa_paro_media, name = 'Tasa de Desempleo Media', mode = 'markers',
                      marker=list(color='#e8eeff', line = list(color = '#e8eeff',width = 0),size=10),
                      text = ~paste(Provincia, ' ', round(tasa_paro_media,2), '%'), hoverinfo = 'text', xaxis = "x2") %>%
    plotly::add_annotations(x = pmax(provincias$tasa_paro_media+8, provincias$tasa_paro_max+1),
                            y=provincias$Provincia, text=provincias$Provincia,
                            xref='x', yref='y',showarrow=F,font=list(color=text_color,size=10),
                            xanchor="left",yanchor = "middle") %>%
    plotly::add_annotations(x = municipio_min$tasa_paro, y=provincia_top, text=municipio_min$texto,
                            xref='x', yref='y',arrowhead=7,arrowsize=0.5,ax=-30,ay=-20,arrowwidth=1,arrowcolor="white",
                            xanchor="right",yanchor="bottom",font=list(color=text_color,size=10)) %>%
    plotly::add_annotations(x = municipio_max$tasa_paro, y=provincia_top, text=municipio_max$texto,
                            xref='x', yref='y',arrowhead=7,arrowsize=0.5,ax=30,ay=-20,arrowwidth=1,arrowcolor="white",
                            xanchor="left",yanchor = "bottom",font=list(color=text_color,size=10)) %>%
    plotly::add_annotations(x = municipio_max$tasa_paro_media, y=provincia_top, 
                            text=paste0("Tasa <i>media</i> de desempleo <br>en ", provincia_top),
                            xref='x', yref='y',arrowhead=7,arrowsize=0.5,ax=0,ay=-40,
                            arrowwidth=1, arrowcolor="white", font=list(color=text_color,size=10)) %>%
    # plotly::add_annotations(xref="paper", yref="paper", y=1.2, x=0.5,
    #                         paste0('<b>Tasa de Desempleo en España - ',month_name, ' de ', year_,'</b>'),
    #                         showarrow=F, font=list(color=text_color,size=17) ) %>%
    plotly::layout(
      xaxis = xaxis, xaxis2 = xaxis2, yaxis = yaxis,
      showlegend = F, paper_bgcolor = bg_color, plot_bgcolor = bg_color,
      margin = m, autosize = T
    )
}
# unemployment.rate.data(unemployment_data, year_) %>%
#   dplyr::filter(poblacion > 10000, poblacion<40000) %>%
#   plot.unemployment(year_, month_)

read.unemployment.time <- function(year_, month_){
  unemployment_path <- './unemployment_data/'
  unemployment_filename <- paste0(unemployment_path,'/', paste(c("unemployment", year_ ,month_), collapse = "_" ), ".csv")
  unemployment_data <- data.table::fread(unemployment_filename, stringsAsFactors = F)
}
read.unemployment.province <- function(province){
  unemployment_path <- './unemployment_data/'
  unemployment_filename <- paste0(unemployment_path,'/', paste(c("unemployment", year_ ,month_), collapse = "_" ), ".csv")
  unemployment_data <- data.table::fread(unemployment_filename, stringsAsFactors = F)
}

map <- rgdal::readOGR('~/Downloads/recintos_autonomicas_inspire_peninbal_etrs89/recintos_autonomicas_inspire_peninbal_etrs89.shp')
leaflet::leaflet(map) %>%
  leaflet::addTiles() %>%
  leaflet::addPolygons(stroke = T, weight = 1, smoothFactor = 0.3, fillOpacity = 0.1, label=~NAMEUNIT)
map <- rgdal::readOGR('~/Downloads/recintos_municipales_inspire_peninbal_etrs89/recintos_municipales_inspire_peninbal_etrs89.shp')
leaflet::leaflet(map) %>%
  # leaflet::addTiles() %>%
  leaflet::addPolygons(stroke = T, weight = 1, fillOpacity = 0.1, label=~NAMEUNIT)

library(mapview)
mapview(breweries)

dateInput2 <- function(inputId, label, endview = "years", ...) {
  d <- shiny::dateInput(inputId, label, ...)
  d$children[[2L]]$attribs[["data-date-min-view-mode"]] <- endview
  d
}
dateRangeInput2 <- function(inputId, label, endview = "years", ...) {
  d <- shiny::dateRangeInput(inputId, label, ...)
  d$children[[2L]]$children[[1]]$attribs[["data-date-min-view-mode"]] <- endview
  d$children[[2L]]$children[[3]]$attribs[["data-date-min-view-mode"]] <- endview
  d
}