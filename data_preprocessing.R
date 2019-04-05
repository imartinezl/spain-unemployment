

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
