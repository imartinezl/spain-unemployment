setwd("~/Downloads/spain-unemployment")

library(dplyr)
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


# https://datos.gob.es/es/catalogo/e00142804-paro-registrado-por-municipios
id <- "e00142804-paro-registrado-por-municipios"
unemployment_dists <- fetch_dataset(id) %>% 
  extract_distributions()

unemployment_years <- unemployment_dists %>% dplyr::pull(year)
month_ <- 1
year_ <- 2018
month_name <- lubridate::dmy(paste('1',month_,year_)) %>% lubridate::month(label = T, abbr = F)


unemployment_path <- './unemployment_data/'
unemployment_filename <- paste0(unemployment_path,'/', paste(c("unemployment", year_ ,month_), collapse = "_" ), ".csv")
if(!dir.exists(unemployment_path)){
  dir.create(unemployment_path)
}
unemployment_files <- list.files(unemployment_path, full.names = T)
if(unemployment_filename %in% unemployment_files){
  unemployment_data <- read.csv(unemployment_filename, stringsAsFactors = F)
}else{
  unemployment_data <- unemployment_dists %>% 
    dplyr::filter(year == year_) %>% 
    dplyr::pull(accessURL) %>% 
    read.csv(skip=1, header=T, sep=';',check.names = T, fileEncoding = "ISO-8859-1", stringsAsFactors = F) %>%
    dplyr::mutate(month = Código.mes %% 100,
                  year = floor(Código.mes/100)) %>% 
    dplyr::filter(month == month_)
  write.csv(unemployment_data, file = unemployment_filename, row.names = F)
}



pobmun_files <- list.files('./pobmun/', full.names = T)
n <- as.numeric(gsub("\\D", "", pobmun_files))
pobmun_years <- ifelse(n>80, 1900, 2000) + n
selected <- which.min(abs(pobmun_years - year_))

# http://www.ine.es/dynt3/inebase/index.htm?padre=525
pobmun_data <- readxl::read_excel(pobmun_files[selected], skip = 1) %>% 
  dplyr::mutate(CPRO = as.numeric(CPRO),
                CMUN = as.numeric(CMUN),
                CMUN_CPL = CPRO*1000 + CMUN)


data_merged <- merge(pobmun_data, unemployment_data, by.x = "CMUN_CPL", by.y = "Codigo.Municipio") %>% 
  dplyr::mutate(tasa_paro = total.Paro.Registrado/POB*100) %>% 
  dplyr::filter(POB > 100) %>%
  dplyr::group_by(Provincia) %>% 
  dplyr::mutate(tasa_paro_media = mean(tasa_paro), 
                tasa_paro_max = max(tasa_paro)) %>% 
  dplyr::ungroup()

plot.unemployment <- function(data_merged){
  tasa_paro_maxima <- data_merged$tasa_paro_max %>% max()
  provincias <- data_merged %>% 
    dplyr::select(Provincia, tasa_paro_max, tasa_paro_media) %>% 
    unique()
  
  provincia_top <- data_merged %>%
    dplyr::slice(which.min(data_merged$tasa_paro_media)) %>% 
    dplyr::pull(Provincia)
  municipio_min <- data_merged %>%
    dplyr::filter(Provincia == provincia_top) %>% 
    dplyr::mutate(tasa_paro_min = min(tasa_paro)) %>%
    dplyr::filter(tasa_paro == tasa_paro_min) %>% 
    dplyr::mutate(texto = paste0("Municipio con la <i>menor</i><br>tasa de paro en ", provincia_top)) %>% 
    dplyr::slice(1)
  municipio_max <- data_merged %>%
    dplyr::filter(Provincia == provincia_top) %>% 
    dplyr::mutate(tasa_paro_max = max(tasa_paro)) %>%
    dplyr::filter(tasa_paro == tasa_paro_max) %>% 
    dplyr::mutate(texto = paste0("Municipio con la <i>mayor</i><br>tasa de paro en ", provincia_top)) %>% 
    dplyr::slice(1)
  
  bg_color <- "#0f1110"
  text_color <- "#e8eeff"
  grid_color <- "#113251"
  line_color <- "#113251"
  tick_color <- "#113251"
  gradient_colors <- list(c(0,'#ec2F4B'),c(1,'#009FFF'))
  font_family <- "Roboto"
  m <- list(l = 50, r = 50, b = 100, t = 125, pad = 4 )
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
  
  
  data_merged %>% 
    plotly::plot_ly(type = "scatter", mode="markers", name = 'Tasa de Desempleo',width = 1080, height = 2000, 
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
    plotly::add_annotations(xref="paper", yref="paper", y=1.2, x=0.5,
                            paste0('<b>Tasa de Desempleo en España - ',month_name, ' de ', year_,'</b>'),
                            showarrow=F, font=list(color=text_color,size=17) ) %>%
    plotly::layout(
      xaxis = xaxis, xaxis2 = xaxis2, yaxis = yaxis,
      showlegend = F, paper_bgcolor = bg_color, plot_bgcolor = bg_color,
      autosize = F, margin = m
    )
}
plot.unemployment(data_merged)
