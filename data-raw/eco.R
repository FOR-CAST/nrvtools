# ecodistricts --------------------------------------------------------------------------------

ecodistricts <- sf::st_read(paste0(
  "https://agriculture.canada.ca/atlas/data_donnees/nationalEcologicalFramework/",
  "data_donnees/geoJSON/ed/nef_ca_ter_ecodistrict_v2_2.geojson"
))
str(ecodistricts)

usethis::use_data(ecodistricts, overwrite = TRUE)

# ecoregions ----------------------------------------------------------------------------------

ecoregions <- sf::st_read(paste0(
  "https://agriculture.canada.ca/atlas/data_donnees/nationalEcologicalFramework/",
  "data_donnees/geoJSON/er/nef_ca_ter_ecoregion_v2_2.geojson"
))
str(ecoregions)

usethis::use_data(ecoregions, overwrite = TRUE)

# ecoprovinces --------------------------------------------------------------------------------

ecoprovinces <- sf::st_read(paste0(
  "https://agriculture.canada.ca/atlas/data_donnees/nationalEcologicalFramework/",
  "data_donnees/geoJSON/ep/nef_ca_ter_ecoprovince_v2_2.geojson"
))
str(ecoprovinces)

usethis::use_data(ecoprovinces, overwrite = TRUE)

# ecozones ------------------------------------------------------------------------------------

ecozones <- sf::st_read(paste0(
  "https://agriculture.canada.ca/atlas/data_donnees/nationalEcologicalFramework/",
  "data_donnees/geoJSON/ez/nef_ca_ter_ecozone_v2_2.geojson"
))
str(ecozones)

usethis::use_data(ecozones, overwrite = TRUE)
