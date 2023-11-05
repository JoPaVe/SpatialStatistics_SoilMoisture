library(dplyr)
library(plyr)
library(stringr)
############## Preprocessing crop yield data ###############

Saskatchewan <- lapply(cropyield_data, FUN = function(df) {
  df <- df |>
    filter(str_detect(df$GEO,"Saskatchewan"))
})
