variables
install.packages("rio")
require(tidyverse)
require(pacman)
test <- read_csv("https://github.com/iapaezg/BD_LM_02/raw/main/stores/test.csv")
train <- read_csv("https://github.com/iapaezg/BD_LM_02/raw/main/stores/train.csv")
head(test)
str(test)

# Se cre la variable sample
test<-test  %>% mutate(sample="test")
train<-train  %>% mutate(sample="train")

# Unir bases
db_ps<-rbind(test,train)
table(db_ps$sample)

# Cargar los mapas
install.packages("sf")
p_load("sf")
db_ps <- st_as_sf(
  db_ps,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)

p_load("leaflet")

pal <- colorFactor(
  palette = c('red', 'green'),
  domain = db_ps$sample
)

map<-leaflet() %>% 
  addTiles() %>%  #capa base
  addCircles(data=db_ps,col=~pal(sample)) #capa casas
map 

# Distancia del centro internacional
p_load("tmaptools") #needs to install p_load("geojsonio")
cbd <- geocode_OSM("Centro Internacional, Bogotá", as.sf=T) 
cbd

db_ps$DCBD<-st_distance(x = db_ps, y = cbd)
str(db_ps)
db_ps  %>% st_drop_geometry() %>% group_by(sample)  %>% summarize(mean(DCBD))

