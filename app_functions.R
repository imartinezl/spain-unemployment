
library(dplyr)
# setwd("~/Downloads/spain-unemployment")

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

# unemployment_dists <- unemployment.dists()
# unemployment_years <- unemployment_dists %>% dplyr::pull(year)
# month_ <- 1
# year_ <- 2019
# unemployment_data <- unemployment.data(year_, month_, unemployment_dists)

unemployment.rate.data <- function(unemployment_data, year_){
  poblacion_activa_edades <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")
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
plot.unemployment <- function(unemployment_data, year_, month_){
  # unemployment_data$Tasa_Desempleo <- unemployment_data$Tasa_Desempleo/100
  tasa_paro_maxima <- unemployment_data$Tasa_Desempleo %>% max()
  month_name <- lubridate::dmy(paste('1',month_,year_)) %>% lubridate::month(label = T, abbr = F)
  
  provincias <- unemployment_data %>% 
    dplyr::select(Provincia, Tasa_Desempleo_Max, Tasa_Desempleo_Media) %>% 
    unique()
  
  provincia_top <- unemployment_data %>%
    dplyr::slice(which.min(unemployment_data$Tasa_Desempleo_Media)) %>% 
    dplyr::pull(Provincia)
  municipio_min <- unemployment_data %>%
    dplyr::filter(Provincia == provincia_top) %>% 
    dplyr::mutate(Tasa_Desempleo_Minima = min(Tasa_Desempleo)) %>%
    dplyr::filter(Tasa_Desempleo == Tasa_Desempleo_Minima) %>% 
    dplyr::mutate(texto = paste0("Municipio con la <i>menor</i><br>tasa de paro en ", provincia_top)) %>% 
    dplyr::slice(1)
  municipio_max <- unemployment_data %>%
    dplyr::filter(Provincia == provincia_top) %>% 
    dplyr::mutate(Tasa_Desempleo_Maxima = max(Tasa_Desempleo)) %>%
    dplyr::filter(Tasa_Desempleo == Tasa_Desempleo_Maxima) %>% 
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
    ticks='outside', tick0=0, dtick=5, ticksuffix='%',
    ticklen=4, tickwidth=1, tickcolor=tick_color,
    gridcolor=grid_color, gridwidth=0.25,
    linecolor=line_color, linewidth=2
  )
  xaxis2 <- list(
    title='Tasa de Desempleo (%)', titlefont=list(color=text_color,familiy=font_family,size=17),
    zeroline = F, position=1, range=c(-4,tasa_paro_maxima+5),
    overlaying = "x", side = "top", 
    tickfont = list(color = text_color,family=font_family,size=14),
    ticks='outside', tick0=0, dtick=5, ticksuffix='%',
    ticklen=4, tickwidth=1, tickcolor=tick_color,
    gridcolor=grid_color, gridwidth=0.25,
    linecolor=line_color, linewidth=2
  )
  yaxis <- list(
    title='', zeroline = F, showticklabels=F,
    showgrid=T, dtick=1, 
    gridcolor=grid_color, gridwidth=0.25
  )
  
  unemployment_data %>% 
    plotly::plot_ly(type = "scatter", mode="markers", name = 'Tasa de Desempleo', #width = 1200, height = 1080, 
                    x = ~Tasa_Desempleo, y = ~reorder(Provincia, -Tasa_Desempleo_Media),
                    marker=list(color=~Tasa_Desempleo, colorbar=F, colorscale=gradient_colors, reversescale =T),
                    text = ~paste(Municipio, ' ', round(Tasa_Desempleo,2), '%'), hoverinfo = 'text' ) %>%
    plotly::add_trace(data = provincias, x = ~Tasa_Desempleo_Media, name = 'Tasa de Desempleo Media', mode = 'markers',
                      marker=list(color='#e8eeff', line = list(color = '#e8eeff',width = 0),size=10),
                      text = ~paste(Provincia, ' ', round(Tasa_Desempleo_Media,2), '%'), hoverinfo = 'text', xaxis = "x2") %>%
    plotly::add_annotations(data = provincias, x = ~pmax(Tasa_Desempleo_Media+8, Tasa_Desempleo_Max+1),
                            y=~Provincia, text=~Provincia,
                            xref='x', yref='y',showarrow=F,font=list(color=text_color,size=10),
                            xanchor="left",yanchor = "middle") %>%
    plotly::add_annotations(data = municipio_min, x = ~Tasa_Desempleo, y=provincia_top, text=~texto,
                            xref='x', yref='y',arrowhead=7,arrowsize=0.5,ax=-30,ay=-20,arrowwidth=1,arrowcolor="white",
                            xanchor="right",yanchor="bottom",font=list(color=text_color,size=10)) %>%
    plotly::add_annotations(data=municipio_max, x = ~Tasa_Desempleo, y=provincia_top, text=~texto,
                            xref='x', yref='y',arrowhead=7,arrowsize=0.5,ax=30,ay=-20,arrowwidth=1,arrowcolor="white",
                            xanchor="left",yanchor = "bottom",font=list(color=text_color,size=10)) %>%
    plotly::add_annotations(x = municipio_max$Tasa_Desempleo_Media, y=provincia_top,
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

plot.sankey <- function(unemployment_data, ccaa, prov, muni, date){
  a <- unemployment_data %>%
    dplyr::filter(Comunidad_Autonoma == ccaa,
                  Provincia == prov,
                  Municipio == muni) %>%
    dplyr::mutate(Fecha = lubridate::dmy(paste("1",month,year)))
  if(is.null(date)){
    a <- a %>% dplyr::slice(1)
  }else{
    a <- a %>% dplyr::filter(Fecha == date)
  }
  links <- rbind(
    c(0,1,a$Poblacion_Activa),
    c(0,2,a$Habitantes-a$Poblacion_Activa),
    c(1,3,a$Desempleados),
    c(1,4,a$Poblacion_Activa-a$Desempleados)
  ) %>% as.data.frame()
  colnames(links) <- c("source","target","value")
  nodes <- data.frame(name=c("Habitantes","Población Activa","Población No Activa",
                             "Desempleados","Ocupados"))
  bg_color <- "#090f28"
  text_color <- "#e8eeff"
  font_family <- "Roboto"
  plotly::plot_ly(type="sankey", orientation="h",
                  node = list(label = nodes$name, pad = 85, thickness = 15,
                              line = list(color = "black",width = 0.5),
                              hoverinfo="skip", color=c('#9e0031','#c64191','#087e8b','#e09f3e','#e03616')),
                  link = list(source = links$source, target=links$target, value=links$value,
                              color = 'rgba(255,255,255,0.7)'),
                  textfont = list(color="#100000", size=16, family=font_family)
  ) %>%
    plotly::layout(
      # title="a",
      font = list(size = 14, color=text_color, family=font_family),
      paper_bgcolor = bg_color, plot_bgcolor = bg_color
    )
}

plot.timeline <- function(unemployment_data, ccaa, prov, muni){
  bg_color <- "#090f28"
  text_color <- "#e8eeff"
  font_family <- "Roboto"
  
  unemployment_data %>% 
    dplyr::filter(Comunidad_Autonoma == ccaa,
                  Provincia == prov,
                  Municipio == muni) %>%
    dplyr::mutate(date = lubridate::dmy(paste("1",month,year)),
                  Poblacion_No_Activa = Habitantes - Poblacion_Activa,
                  Ocupados = Poblacion_Activa - Desempleados) %>% 
    plotly::plot_ly(x = ~date, y = ~Poblacion_No_Activa, name="Poblacion No Activa", source = "timeline",
                    type = 'scatter', mode = 'none', stackgroup = 'one', fillcolor = '#087e8b') %>% 
    plotly::add_trace(y =~ Ocupados, name="Ocupados", fillcolor = '#e03616') %>% 
    plotly::add_trace(y =~ Desempleados, name="Desempleados", fillcolor = '#e09f3e') %>% 
    plotly::layout(
      title = '',
      xaxis = list(title = "",showgrid = FALSE,
                   tickfont = list(color = text_color,family=font_family,size=14)),
      yaxis = list(title = "",showgrid = FALSE,
                   tickfont = list(color = text_color,family=font_family,size=14)),
      legend = list(x=0, y=0, font=list(color=text_color, family=font_family, size=14)),
      paper_bgcolor = bg_color, plot_bgcolor = bg_color
    )
}
