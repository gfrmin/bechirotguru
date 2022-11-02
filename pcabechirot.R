library(sf)
library(rmapshaper)
library(tidyverse)
library(tidymodels)

israel_shapes_raw <- st_read("ezorim_statistiim_2022.gpkg") %>% st_make_valid %>% st_transform(crs = 4326)
israel_shapes_union <- israel_shapes_raw %>% group_by(SEMEL_YISHUV) %>% summarise(geometry = st_union(Shape)) %>% ungroup
israel_shapes_places <- israel_shapes_raw %>% as_tibble %>% select(SEMEL_YISHUV, SHEM_YISHUV, SHEM_YISHUV_ENGLISH) %>% unique

bechirot_raw <- read_csv("bechirot.csv") # downloaded from https://publicdatamarket.com/israeldata/bechirot

bechirot_wide <- bechirot_raw %>% filter(votes > 0) %>% select(SEMEL_YISHUV = place, party, propall, elections) %>% pivot_wider(id_cols = SEMEL_YISHUV, names_from = c(party, elections), values_from = propall, values_fill = NA)
bechirot_wide_places <- bechirot_wide %>% inner_join(israel_shapes_union) %>% inner_join(israel_shapes_places)

st_write(bechirot_wide_places, "bechirot_wide_places.sqlite", append=FALSE)
