#variables

pq <- c("pacman","rio","tidyverse","stargazer","skimr","caret")
if(!require(pq)) install.packages(pq) ; require(pq)
require(pacman)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       caret,
       stargazer)

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
if(!require(sf)) install.packages(sf) ; require(sf)
require(sf)
db_ps <- st_as_sf(
  db_ps,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)

require("leaflet")

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
skim(db_ps)
db_ps  %>% st_drop_geometry() %>% group_by(sample)  %>% summarize(mean(DCBD))

install.packages("pdftools")
require("pdftools")

# Limpieza
if(!require(stringi)) install.packages(stringi) ; require(stringi)
require(stringi)
# Tildes
db_ps$description <- stri_trans_general(str = db_ps$description, id = "Latin-ASCII")
# Caracteres especiales
db_ps$description <- gsub('[^A-Za-z0-9 ]+', ' ', db_ps$description)
# Minúscula
db_ps$description <- tolower(db_ps$description)
# Espacios
db_ps$description <- gsub('\\s+', ' ', db_ps$description)




x <- c("manzana", "banana", "pera")
str_view(x, "an")
str_view(x, ".a.")
texto <- as.tibble(db_ps$description)
tex_prueba <- texto %>% slice(1:5)
str_view(tex_prueba,"[0-9]+\\shabit[a-z]+")
str_detect(tex_prueba,"[0-9]+\\shabit[a-z]+")

str_view(tex_prueba,"de?posi[a-z]+")
str_count(tex_prueba,"de?posi[a-z]+")

bano <- str_detect(tex_prueba,"ba?no")
palabras[str_detect(palabras, "x$")]
#> [1] "ex"
str_subset(palabras, "x$")
#> [1] "ex"

x <- c("A and B", "A, B and C", "A, B, C and D", "foobar")
m <- gregexpr("\\([^)]*\\)", x)
m
