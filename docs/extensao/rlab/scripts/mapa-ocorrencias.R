# ======================================================================
# Mapa de ocorrências a partir do GBIF
# R Lab - Laboratório de Ecologia e Dendrologia Tropical (LEDT/UFAM)
# https://ledtufam.github.io/extensao/rlab/rotinas/mapa-ocorrencias.html
# ======================================================================


# ---- 1. Instale os pacotes ----
install.packages(c("rgbif", "CoordinateCleaner", "leaflet", "dplyr"))


# ---- 2. Baixe os registros ----
library(rgbif)
library(dplyr)

especie <- "Ecclinusa guianensis"

res <- occ_search(
  scientificName     = especie,
  hasCoordinate      = TRUE,
  hasGeospatialIssue = FALSE,
  limit              = 5000
)

occ <- res$data |>
  select(species, decimalLongitude, decimalLatitude, countryCode,
         institutionCode, catalogNumber, year, basisOfRecord)

nrow(occ)


# ---- 3. Limpe as coordenadas ----
library(CoordinateCleaner)

flags <- clean_coordinates(
  occ,
  lon = "decimalLongitude", lat = "decimalLatitude",
  countries = "countryCode", species = "species",
  tests = c("capitals", "centroids", "equal", "gbif",
            "institutions", "zeros", "seas")
)

summary(flags)
occ_limpo <- occ[flags$.summary, ]       # mantém só os registros aprovados

# Remove duplicatas de coordenadas
occ_limpo <- distinct(occ_limpo, decimalLongitude, decimalLatitude, .keep_all = TRUE)


# ---- 4. Mapa interativo ----
library(leaflet)

leaflet(occ_limpo) |>
  addProviderTiles("Esri.WorldImagery", group = "Satélite") |>
  addTiles(group = "Mapa") |>
  addCircleMarkers(
    ~decimalLongitude, ~decimalLatitude,
    radius = 4, stroke = FALSE, fillOpacity = 0.8, color = "#f3969a",
    popup = ~paste0("<b>", institutionCode, "</b> ", catalogNumber,
                    "<br>Ano: ", year)
  ) |>
  addLayersControl(baseGroups = c("Satélite", "Mapa"))


# ---- 5. Salve os dados ----
write.csv(occ_limpo, "dados/ocorrencias_limpas.csv", row.names = FALSE)


# ---- Referências ----
