### Description map




### Summary map
ggplot() +
  geom_sf(data = cropyield_summary_map_2008_2015$Shape, aes(fill = cropyield_summary_map_2008_2015$mean_wheat)) + 
  theme(
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "white", linetype = "solid", colour = "black"),
    plot.title = element_text(face = "bold")
  ) + 
  scale_fill_gradient(breaks = c())
  
  

ggplot() +
  geom_sf(data = cropyield_summary_map_2017$Shape, aes(fill = cropyield_summary_map_2017$mean_wheat)) + 
  theme(
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "white", linetype = "solid", colour = "black"),
    plot.title = element_text(face = "bold")
  )
  
# Minimum/maximum STD mean wheat
cropyield_summary_map_2017 |>
  slice_min(mean_wheat)
cropyield_summary_map_2017 |>
  slice_max(mean_wheat)

cropyield_summary_map_2017 |>
  slice_min(sd_wheat)
cropyield_summary_map_2017 |>
  slice_max(sd_wheat)

# Minimum/maximum STD mean canola 
cropyield_summary_map_2017 |>
  slice_min(mean_canola)
cropyield_summary_map_2017 |>
  slice_max(mean_canola)

cropyield_summary_map_2017 |>
  slice_min(sd_canola)
cropyield_summary_map_2017 |>
  slice_max(sd_canola)

# Minimum, Maximum
summary(cropyield_summary_map_2017$Shape_Area)

## Number of NAs

annual_soilmoisture_obs <- sapply(moisture_objects[[1]], FUN = function(df) {
  return(c(dim(df)[2]-2, dim(df)[1]))
})

total_annual_soilmoisture_obs <- annual_soilmoisture_obs[1,] * annual_soilmoisture_obs[2,]

kTotalObservationsSoilMoisture <- sum(total_annual_soilmoisture_obs)

total_annual_missing_soilmoisture_obs <- sapply(moisture_objects[[1]], FUN = function(df) {
  return(sum(is.na(df)))
})

# Annual percentage of total missing soil moisture observations
perc_missing_soilmoisture_obs <- total_annual_missing_soilmoisture / total_annual_soilmoisture_obs

# Missing annual values after mean calculation 
sapply(SAD_moisture, FUN = function(df) sum(is.na(df)))
sapply(SAD_moisture_filled, FUN = function(df) sum(is.na(df)))




## Results from Iterative Analysis