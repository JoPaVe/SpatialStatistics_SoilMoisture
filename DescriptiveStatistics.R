library(ggplot2)

### Summary map

cropyield_summary_map_2008_2015 <- merge(Saskatchewan_cropyield_summary_to_merge_2008_2015, 
                                         SADRegions2008_2015_crs_transformed,
                                         by = c("SADRegionUID"))

cropyield_summary_map_2017 <- merge(Saskatchewan_cropyield_summary_to_merge_2017, 
                                         SADRegions2017_crs_transformed,
                                         by = c("SADRegionUID"))


ggplot() +
  geom_sf(data = cropyield_summary_map_2008_2015$Shape, aes(fill = cropyield_summary_map_2008_2015$mean_wheat))

ggplot() +
  geom_sf(data = cropyield_summary_map_2017$Shape, aes(fill = cropyield_summary_map_2017$mean_wheat))


## Number of NAs