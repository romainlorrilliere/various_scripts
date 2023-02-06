library(climateExtract)


source("c:/git/climateExtract/R/climateExtract.r")

precipitation_data_2010_2015 <- extract_nc_value(2010, 2015, local_file = TRUE,file_path = "C:/git/data_weather/rr_ens_mean_0.25deg_reg_v22.0e.nc", clim_variable = "precipitation", grid_size = 0.25)

save(precipitation_data_2010_2015,file="precipitation_data_2010_2015.Rdata")

precipitation_data_2016_2020 <- extract_nc_value(2016, 2020, local_file = TRUE,file_path = "C:/git/data_weather/rr_ens_mean_0.25deg_reg_v22.0e.nc", clim_variable = "precipitation", grid_size = 0.25)
save(precipitation_data_2016_2020,file="precipitation_data_2016_2020.Rdata")

## climate_data2 <- extract_nc_value(2010, 2020, local_file = TRUE,file_path = "C:/git/data_weather/tg_ens_mean_0.25deg_reg_v22.0e.nc", clim_variable = "precipitation", grid_size = 0.25)



temperature_data_2010_2015 <- extract_nc_value(2010, 2015, local_file = TRUE,file_path = "C:/git/data_weather/tg_ens_mean_0.25deg_reg_v22.0e.nc", clim_variable = "temperature", grid_size = 0.25)

save(temperature_data_2010_2015,file="temperature_data_2010_2015.Rdata")

temperature_data_2016_2020 <- extract_nc_value(2016, 2020, local_file = TRUE,file_path = "C:/git/data_weather/tg_ens_mean_0.25deg_reg_v22.0e.nc", clim_variable = "temperature", grid_size = 0.25)
save(temperature_data_2016_2020,file="temperature_data_2016_2020.Rdata")



## 2022-07-27  colonne de table pour sophie
env  <-  c("temperature_C","relative_humidity","Lateral","Frontal")
alt <- c(6,5,2)
day <- c("J0","J1","pente5J")
dt_env <- expand.grid(env=env,day=day,alt=alt)
library(data.table)
setDT(dt_env)
dt_env[,alt := paste0("alt",alt)]
dt_env[,var := paste(env,day,alt,sep="_")]

autre <- c("bird_id","year","migration","julian","difftime_day","parti")


vec <- c(autre,dt_env[,var])
cat(vec)
