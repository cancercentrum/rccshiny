map_swe_adm0 <- readRDS(paste0(getwd(), "/data-raw/map_swe_adm0.rds"))
map_swe_adm1 <- readRDS(paste0(getwd(), "/data-raw/map_swe_adm1.rds"))
map_swe_adm2 <- readRDS(paste0(getwd(), "/data-raw/map_swe_adm2.rds"))

devtools::use_data(map_swe_adm0, map_swe_adm1, map_swe_adm2, internal = TRUE)
