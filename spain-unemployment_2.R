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
  dplyr::filter(POB > 1000) %>% 
  dplyr::group_by(Provincia) %>% 
  dplyr::mutate(tasa_paro_media = mean(tasa_paro), 
                tasa_paro_max = max(tasa_paro)) %>% 
  dplyr::ungroup()

provincias <- data_merged %>% 
  dplyr::select(Provincia, tasa_paro_max, tasa_paro_media) %>% 
  unique()

min <- data_merged %>%
  dplyr::slice(which.min(data_merged$tasa_paro_media))


# library(plotly)
data_merged %>% 
  plotly::plot_ly(type = "scatter", mode="markers", name = 'Tasa de Desempleo',
                  x = ~tasa_paro, y = ~reorder(Provincia, -tasa_paro_media), 
                  marker=list(color=~tasa_paro, colorbar=F, colorscale='Viridis', reversescale =T),
                  text = ~paste(Municipio, ' ', round(tasa_paro,2), '%'), hoverinfo = 'text' ) %>%
  plotly::add_trace(data = provincias, x = ~tasa_paro_media, name = 'Tasa de Desempleo Media', mode = 'markers',
                    marker=list(color='rgba(0,0,0,1)', line = list(color = 'rgb(0,0,0)',width = 0),size=10),
                    text = ~paste(Provincia, ' ', round(tasa_paro_media,2), '%'), hoverinfo = 'text') %>% 
  plotly::layout(
    title = paste0('Tasa de Desempleo en España - ',month_name, ' de ', year_),
    yaxis = list(title='',zeroline = F, showticklabels=F),
    xaxis = list(title='Tasa de Desempleo', zeroline = F, position=0),
    showlegend = F,
    annotations=c(list(x = provincias$tasa_paro_max+2, y=provincias$Provincia, text=provincias$Provincia,
                       xref='x', yref='y',showarrow=F),
                  list(x = provincias$tasa_paro_max+2, y=provincias$Provincia, text=provincias$Provincia,
                       xref='x', yref='y',showarrow=F,arrowhead=4,ax=0,ay=0))
  )


