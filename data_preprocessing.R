

source('~/Downloads/spain-unemployment/app_functions.R')


unemployment_dists <- unemployment.dists()
unemployment_years <- unemployment_dists %>% dplyr::pull(year)


min_date <- "2008-01-01"
max_date <- "2019-01-01"
dates <- data.frame(dates = seq(lubridate::ymd(min_date),lubridate::ymd(max_date), by = '1 month')) %>% 
  dplyr::mutate(month_ = lubridate::month(dates),
                year_ = lubridate::year(dates)) %>% 
  dplyr::select(-dates)

unemployment_data <- lapply(1:nrow(dates), function(i){
  year_ <- dates$year_[i]
  month_ <- dates$month_[i]
  print(paste(year_, month_))
  unemployment_path <- './unemployment_data/'
  unemployment_filename <- paste0(unemployment_path,'/', paste(c("unemployment", year_ ,month_), collapse = "_" ), ".csv")
  if(!dir.exists(unemployment_path)){
    dir.create(unemployment_path)
  }
  unemployment_files <- list.files(unemployment_path, full.names = T)
  if(unemployment_filename %in% unemployment_files){
    unemployment_data <- read.csv(unemployment_filename, stringsAsFactors = F)
  }else{
    unemployment_data <- unemployment.data(year_, month_, unemployment_dists) %>% 
      unemployment.rate.data(year_) %>% 
      dplyr::select(cod_mun, poblacion, poblacion_activa, Comunidad.Autónoma, Provincia, 
                    Municipio, total.Paro.Registrado, month, year, tasa_paro) %>% 
      dplyr::rename(Codigo_Municipio = cod_mun, Habitantes = poblacion, Poblacion_Activa = poblacion_activa,
                    Comunidad_Autonoma = Comunidad.Autónoma, Desempleados = total.Paro.Registrado,
                    Tasa_Desempleo = tasa_paro)
    write.csv(unemployment_data, file = unemployment_filename, row.names = F)
  }
  unemployment_data
  
}) %>% dplyr::bind_rows()

unemployment_path <- './unemployment_data/'
unemployment_filename <- paste0(unemployment_path,'/unemployment_data.csv')
unemployment_files <- list.files(unemployment_path, full.names = T)
if(unemployment_filename %in% unemployment_files){
  print("Reading")
  unemployment_data <- data.table::fread(unemployment_filename, stringsAsFactors = F)
}else{
  write.csv(unemployment_data, file = unemployment_filename, row.names = F)
}



# MAP DATASET -----------------------------------------------------------------------
library(sf)
map <- sf::st_read('lineas_limite/recintos_municipales_inspire_peninbal_etrs89/recintos_municipales_inspire_peninbal_etrs89.shp')
plot(sf::st_geometry(map))
map <- sf::st_read('~/Downloads/recintos_municipales_inspire_peninbal_etrs89/recintos_municipales_inspire_peninbal_etrs89.shp') %>% 
  dplyr::mutate(Codigo_Municipio = NATCODE %>% as.character() %>% as.numeric() %% 1e5)
plot(sf::st_geometry(map))
# sf::write_sf(map)

unemployment_data %>% colnames()
columns <- c("Habitantes","Poblacion_Activa","Desempleados","Tasa_Desempleo")
unemployment_data %>% 
  dplyr::filter(year == 2019, month==1) %>% 
  merge(map) %>% 
  sf::st_write('~/Downloads/recintos_municipales_inspire_peninbal_etrs89/recintos_municipales_inspire_peninbal_etrs89_2.shp',
               layer_options = "ENCODING=UTF-8")

sf::st_read('~/Downloads/recintos_municipales_inspire_peninbal_etrs89/recintos_municipales_inspire_peninbal_etrs89_2.shp') %>% 
  head()
