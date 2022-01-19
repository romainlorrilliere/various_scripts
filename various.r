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
