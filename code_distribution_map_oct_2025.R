# Install if needed
# install.packages(c("geobr", "sf", "ggplot2", "dplyr", "terra", "rnaturalearth", "rnaturalearthdata", "elevatr"))

library(geobr)
library(sf)
library(ggplot2)
library(dplyr)
library(terra)
library(rnaturalearth)
library(elevatr)

# 1. Load Brazil borders and states
country <- geobr::read_country(year = 2019)
states <- geobr::read_state(year = 2019)
municipalities <- geobr::read_municipality(year = 2019)

# 2. Define your city list
cities_df <- tribble(
  ~state,     ~city,
  "Ceará",     "Acarape",
  "Ceará",     "Aratuba",
  "Ceará",     "Baturité",
  "Ceará",     "Canindé",
  "Ceará",     "Guaramiranga",
  "Ceará",     "Itatira",
  "Ceará",     "Mulungu",
  "Ceará",     "Pacoti",
  "Ceará",     "Palmácia",
  "Ceará",     "Santa Quitéria",
  "Rio Grande do Norte", "Baía Formosa",
  "Rio Grande do Norte", "Espírito Santo",
  "Rio Grande do Norte", "Parnamirim",
  "Rio Grande do Norte", "São Gonçalo do Amarante",
  "Rio Grande do Norte", "Tibau do Sul",
  "Paraíba",   "Areia",
  "Paraíba",   "Conde",
  "Paraíba",   "Cruz do Espírito Santo",
  "Paraíba",   "Mamanguape",
  "Paraíba",   "Mataraca",
  "Paraíba",   "Santa Rita",
  "Pernambuco", "Araçoiaba",
  "Pernambuco", "Belo Jardim",
  "Pernambuco", "Brejão",
  "Pernambuco", "Caruaru",
  "Pernambuco", "Garanhuns",
  "Pernambuco", "Gravatá",
  "Pernambuco", "Igarassu",
  "Pernambuco", "Lagoa dos Gatos",
  "Pernambuco", "Pesqueira",
  "Pernambuco", "Pombos",
  "Pernambuco", "Taquaritinga do Norte",
  "Pernambuco", "Tracunhaém",
  "Alagoas",   "Flexeiras",
  "Alagoas",   "Quebrangulo",
  "Alagoas",   "São Sebastião",
  "Alagoas",   "Traipu",
  "Bahia",     "Andaraí",
  "Bahia",     "Barra da Estiva",
  "Bahia",     "Ibicoara",
  "Bahia",     "Iramaia",
  "Bahia",     "Ituaçu",
  "Bahia",     "Lençóis",
  "Bahia",     "Maracás",
  "Bahia",     "Morro do Chapéu",
  "Bahia",     "Mucugê",
  "Bahia",     "Palmeiras",
  "Bahia",     "Piatã",
  "Bahia",     "Seabra"
)

# 3. Get city geometries
cities_plot <- municipalities %>%
  inner_join(cities_df, by = c("name_muni" = "city", "name_state" = "state")) %>%
  st_centroid()

# 4. Get elevation raster for Brazil (z = 5 or 6 depending on resolution)
message("Downloading elevation data (may take a minute)...")
elev <- elevatr::get_elev_raster(locations = country, z = 5, clip = "locations")

# 5. Convert raster to data frame for ggplot
elev_df <- as.data.frame(terra::as.data.frame(elev, xy = TRUE))
colnames(elev_df) <- c("x", "y", "elevation")

# 6. Plot: grayscale topography + orange dots + transparent background
ggplot() +
  geom_raster(data = elev_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradient(
    low = "white", high = "black", name = "Elevation (m)"
  ) +
  geom_sf(data = states, fill = NA, color = "grey40", size = 0.2) +
  geom_sf(data = country, fill = NA, color = "black", size = 0.2) +
  geom_sf(data = cities_plot, color = "orange", size = 2.5, alpha = 0.9) +
  coord_sf() +
  theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  ) +
  labs(
    title = "Topographic Map of Brazil (Grayscale)",
    subtitle = "Orange dots represent the listed cities",
    caption = "Elevation: SRTM via elevatr | Boundaries: IBGE (geobr)"
  )
# export plot as png to folder SciComm Project in Documents
ggsave(
  filename = "brazil_topographic_map.png",
  path = "~/Documents/SciComm Project/",
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)


library(geobr)
library(sf)
library(ggplot2)
library(dplyr)
library(terra)
library(elevatr)

# 1. Load Brazil and Ceará state borders
country <- geobr::read_country(year = 2019)
states <- geobr::read_state(year = 2019)

# Filter only Ceará
ceara <- states %>%
  filter(abbrev_state == "CE") %>%
  st_transform(4326)

# 2. Download elevation raster just for Ceará
message("Downloading elevation data for Ceará (this may take a moment)...")
elev_ceara <- elevatr::get_elev_raster(
  locations = ceara,
  z = 7,                  # resolution: increase for more detail
  clip = "locations"
)

# 3. Convert raster to data frame for ggplot
elev_df <- as.data.frame(terra::as.data.frame(elev_ceara, xy = TRUE))
colnames(elev_df) <- c("x", "y", "elevation")

# 4. Plot: black & white topography map of Ceará
p <- ggplot() +
  geom_raster(data = elev_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradient(low = "white", high = "black", name = "Elevation (m)") +
  geom_sf(data = ceara, fill = NA, color = "black", size = 0.4) +
  coord_sf() +
  theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  ) +
  labs(
    title = "Topographic Map of Ceará (Grayscale)",
    subtitle = "Elevation data from SRTM via elevatr",
    caption = "Boundaries: IBGE (geobr)"
  )

# 5. Print map
print(p)

# 6. Export map as PNG
ggsave(
  filename = "ceara_topographic_map.png",
  path = "~/Documents/SciComm Project/",
  width = 8,
  height = 8,
  dpi = 300,
  bg = "white"  # use bg = "transparent" for transparent background
)
