
library(dplyr)


# DESEMPLEO -------------------------------------------------------------------------

unemploy_data <- read.csv('Paro_por_municipios_2019.csv', skip=1, header=T, sep=';', check.names = T)
unemploy_data %>% 
  dplyr::filter(mes == "Enero de 2019") %>% 
  dplyr::arrange(Provincia, Comunidad.Autónoma) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(x=total.Paro.Registrado, y=Provincia, color=Comunidad.Autónoma))


# POB MUNICIPIOS ---------------------------------------------------------------------------

pobmun <- list.files('./pobmun/',full.names = T)
pobmun_data <- lapply(pobmun, function(i){
  print(i)
  n <- as.numeric(gsub("\\D", "", i))
  year <- ifelse(n>80, 1900, 2000) + n
  d <- readxl::read_excel(i, skip = 1) %>% 
    mutate(ANO = year,
           CPRO = as.numeric(CPRO),
           CMUN = as.numeric(CMUN))
  colnames(d)[ncol(d)-3]<- "POB"
  # lapply(d, class)
  d
}) %>% dplyr::bind_rows()
total_pob <- pobmun_data %>% 
  dplyr::select(CPRO,CMUN,POB,ANO) %>% 
  tidyr::spread(ANO,POB)

# POB PROVINCIA ---------------------------------------------------------------------

pobpro_files <- list.files('./pobpro/',full.names = T)

pobpro_data <- lapply(pobpro_files, function(pobpro){
  print(pobpro)
  cols <- readxl::read_excel(pobpro, skip = 6, n_max = 2, col_names = F, na = "NA")
  cols[1, ] <- zoo::na.locf(as.character(cols[1, ]), na.rm = FALSE)
  cols <- vapply(cols, FUN = function(x) if (is.na(x[1])) x[2] else paste0(rev(x), collapse = ""),FUN.VALUE = character(1))
  cols[is.na(cols)] <- "MUN"
  d <- readxl::read_excel(pobpro, skip = 8, col_names = cols, na = "NA")
  
  can.be.numeric <- function(x) {
    stopifnot(is.atomic(x) || is.list(x)) # check if x is a vector
    numNAs <- sum(is.na(x))
    numNAs_new <- suppressWarnings(sum(is.na(as.numeric(x))))
    return(numNAs_new == numNAs)
  }
  to.numeric <- function(x){
    if(can.be.numeric(x)){
      return(as.numeric(x))
    }else{
      return(x)
    }
  }
  
  types <- c("Total","Hombres","Mujeres")
  dd <- lapply(types, function(t){
    d %>% 
      dplyr::select("MUN",dplyr::contains(t)) %>% 
      tidyr::gather(Year,!!t,-MUN) %>% 
      dplyr::mutate(Year = gsub(t,"",Year)) %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(CMUN=unlist(strsplit(MUN," ",fixed = T))[1],
                    NOMBRE=unlist(strsplit(MUN," ",fixed = T))[2]) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate_all(to.numeric)
  }) %>% Reduce(cbind,.)
  dd <- dd[, !duplicated(colnames(dd))]
})
pobpro_data <- pobpro_data %>% Reduce(rbind,.)

a <- merge(unemploy_data, pobpro_data, by.x = Codigo.Municipio, by.y=)



# PARO POR PROVINCIA ----------------------------------------------------------------

poblacion_activa <- c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")
b <- pxR::read.px('./00000001.px')$DATA$value %>% 
  dplyr::filter(sexo == "Ambos sexos",
                edad..grupos.quinquenales. %in% poblacion_activa,
                municipios != "Total") %>% 
  dplyr::group_by(municipios) %>% 
  dplyr::summarise(pob = sum(value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cod_mun = stringr::str_sub(municipios,1,5) %>% as.numeric,
                municipios = stringr::str_sub(municipios,7) )



