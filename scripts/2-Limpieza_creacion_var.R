rm(list = ls())
install.packages("rio")
install.packages("pacman")
install.packages("tidyverse")
install.packages("sf")
install.packages(leaflet)
install.packages("osmdata")
install.packages("rgeos")
install.packages("devtools")
install.packages("skimr")
install.packages("stringr")
require("rgeos")
require("osmdata")
require("leaflet")
require("pacman")
require("tidyverse")
require("sf")
require("devtools")
require(skimr)
require(stringr)
p_load(skimr,pacman,tidyverse,sf,devtools,leaflet,rio,osmdata,rgeos)


# Carga datos -------------------------------------------------------------

test <- read_csv("https://github.com/iapaezg/BD_LM_02/raw/main/stores/test.csv")
train <- read_csv("https://github.com/iapaezg/BD_LM_02/raw/main/stores/train.csv")
skim(train) # Visualizar datos
skim(test)

## Se crea la variable sample
test<-test  %>% mutate(sample="test")
train<-train  %>% mutate(sample="train")

# Unir bases
db_ps<-rbind(test,train)
table(db_ps$sample)


# Cargar mapas ------------------------------------------------------------

db_ps <- st_as_sf(
  db_ps,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)
st_crs(db_ps) <- 4326

pal <- colorFactor(
  palette = c('red', 'green'),
  domain = db_ps$sample
)

map<-leaflet() %>% 
  addTiles() %>%  #capa base
  addCircles(data=db_ps,col=~pal(sample)) #capa casas
map 

glimpse(db_ps)


# Limpieza de texto -------------------------------------------------------

# Eliminar caracteres que pueden afectar la base para la variable descripción
db_ps$description <- tolower(db_ps$description)
db_ps$description <- iconv(db_ps$description,from="UTF-8",
                         to="ASCII//TRANSLIT")
db_ps$description <- str_replace_all(db_ps$description,"[^[:alnum:]]"," ") # Elimina caract raros
db_ps$description <- gsub("\\s+"," ",db_ps$description) # Elimina dos espcios y deja uno
db_ps$description <- str_trim(db_ps$description) # Peluquea los extremos de espacios

## Ejemplo para filtrar
# Generar filtros para indexar
#filtro_area  <- grepl("area",db_ps$description)
#filtro_rea <- grepl("area",db_ps$description)
#filtro_metros  <- grepl("metros",db_ps$description)
#filtro_mts  <- grepl("mts",db_ps$description)
#filtro_m2 <- grepl("m2",db_ps$description)
#rea_detect <- str_subset(db_ps$description,"[a-z]?rea")
#head(rea_detect)


# Area var continua -------------------------------------------------------

# Extraer para visualizar el área del predio
a_select <- str_extract(db_ps$description, "[0-9]{2,} m[a-z0-9]+")
head(a_select)
db_ps <- db_ps %>% 
  mutate(area_d=str_extract(a_select,"[0-9]{2,}"))
db_ps$area_d <- as.numeric(db_ps$area_d)
skim(db_ps)
db_ps <- db_ps %>% # Se escoge el valor máximo de las tres variables
  mutate(area_f=pmax(db_ps$surface_covered,db_ps$surface_total,db_ps$area_d,na.rm = TRUE)) 
boxplot(db_ps$area_f)


# Baño var discont -------------------------------------------------------

# Extraer para visualizar baños
b_select <- str_extract(db_ps$description, "\\s? [0-9]+ ba[^lsrh][^cd]")
print(b_select)
skim(b_select)
b_conteo <- str_count(db_ps$description, "ba[^lsrh][^cd]")
skim(b_conteo)
db_ps <- db_ps %>% 
  mutate(bano_d1=str_extract(b_select,"[0-9]+")) %>% 
  mutate(bano_d2=str_count(db_ps$description, "ba[^lsrh][^cd]"))
db_ps$bano_d1 <- as.numeric(db_ps$bano_d1)
db_ps$bano_d2 <- as.numeric(db_ps$bano_d2)
skim(db_ps)
table(db_ps$bano_d1)
db_ps <- db_ps %>% # Se escoge el valor máximo de las variables y se toma un corte de 20 baños luego de inspeccion
  mutate(bano_f=pmax(db_ps$bano_d1,db_ps$bano_d2,db_ps$bathrooms,na.rm = TRUE))
db_ps <- db_ps %>%
  mutate(bano_f=ifelse(bano_f>20,pmax(db_ps$bano_d2,db_ps$bathrooms,na.rm = TRUE),db_ps$bano_f)) 
skim(db_ps)
hist(db_ps$bano_f)


# Habitaciones var discont ------------------------------------------------

# Visualizar los resultados
h_select <- str_extract(db_ps$description, "[0-9]+ h?a[bv]ita[cs]ion[a-z]*")
print(h_select)
skim(h_select)

q_select <- str_extract(db_ps$description, "[0-9]+ c[aeiou]+rto?s?")
print(q_select)
skim(q_select)

al_select <- str_extract(db_ps$description, "[0-9]+ h?alco?[bv]a?s?")
print(al_select)
skim(al_select)

# Se adiciona la variables para cada caso habitacion cuarto alcoba
db_ps <- db_ps %>% 
  mutate(h_select=str_extract(db_ps$description, "[0-9]+ h?a[bv]ita[cs]ion[a-z]*")) %>% 
  mutate(h_select=str_extract(h_select,"[0-9]+")) %>%
  mutate(h_select=as.numeric(h_select))
db_ps <- db_ps %>%
  mutate(h_select=ifelse(h_select>21,0,db_ps$h_select))
db_ps <- db_ps %>%
  mutate(q_select=str_extract(db_ps$description, "[0-9]+ c[aeiou]+rto?s?")) %>% 
  mutate(q_select=str_extract(q_select,"[0-9]+")) %>%
  mutate(q_select=as.numeric(q_select))
db_ps <- db_ps %>%
  mutate(q_select=ifelse(q_select>21,0,db_ps$q_select))
db_ps <- db_ps %>%
  mutate(al_select=str_extract(db_ps$description, "[0-9]+ h?alco?[bv]a?s?")) %>% 
  mutate(al_select=str_extract(al_select,"[0-9]+")) %>%
  mutate(al_select=as.numeric(al_select))
db_ps <- db_ps %>%
  mutate(al_select=ifelse(al_select>21,0,db_ps$al_select))
db_ps <- db_ps %>%
  mutate(room_db=pmax(db_ps$rooms,db_ps$bedrooms,na.rm = TRUE))
db_ps <- db_ps %>%
  mutate(bed_f=ifelse(room_db==0,pmax(al_select,q_select,h_select,na.rm = TRUE),room_db))
skim(db_ps)
hist(db_ps$bed_f)


# Deposito var dummy ----------------------------------------------------

# Se crea la variable dummy bodega, deposito
bodega <- str_extract(db_ps$description,"[bv]o?de?[gj]a?")
skim(bodega)
bodega <- str_detect(db_ps$description,"[bv]o?de?[gj]a?")
db_ps <- db_ps %>% 
  mutate(bod_dp=str_detect(db_ps$description,"[bv]o?de?[gj]a?")) %>% 
  mutate(dep_dp=str_detect(db_ps$description,"de?po?[cs]ito?s?")) %>%
  mutate(dep_f=ifelse((bod_dp==TRUE | dep_dp==TRUE),1,0))

# Ascensor var dummy ----------------------------------------------------

# Se crea la variable dummy ascensor
asce <- str_extract(db_ps$description,"a[cs]+ensore?s?")
asce
skim(asce)
elev <- str_extract(db_ps$description,"el[ae]?[bv]ado?re?s?")
elev
skim(elev)

db_ps <- db_ps %>% 
  mutate(asc_dp=str_detect(db_ps$description,"a[cs]+ensore?s?")) %>% 
  mutate(ele_dp=str_detect(db_ps$description,"el[ae]?[bv]ado?re?s?")) %>%
  mutate(asc_f=ifelse((asc_dp==TRUE | ele_dp==TRUE),1,0))

# Exterior var dummy ----------------------------------------------------

# Se crea la variable dummy exterior = terraza patio balcon
ter <- str_extract(db_ps$description,"te?r+a[sz]as?")
ter
pat <- str_extract(db_ps$description,"pati?os?")
pat
bal <- str_extract(db_ps$description,"[bv]al?cone?s?")
bal

db_ps <- db_ps %>% 
  mutate(ter_dp=str_detect(db_ps$description,"te?r+a[sz]as?")) %>% 
  mutate(pat_dp=str_detect(db_ps$description,"pati?os?")) %>%
  mutate(bal_dp=str_detect(db_ps$description,"[bv]al?cone?s?")) %>%
  mutate(ext_f=ifelse((ter_dp==TRUE | pat_dp==TRUE | bal_dp==TRUE),1,0))

# Exterior var dummy ----------------------------------------------------

# Se crea la variable dummy GARAJE = terraza patio balcon
gar <- str_extract(db_ps$description,"gara[gj]es?")
gar
par <- str_extract(db_ps$description,"parqu?e?a?deros?")
par

db_ps <- db_ps %>% 
  mutate(gar_dp=str_detect(db_ps$description,"gara[gj]es?")) %>% 
  mutate(par_dp=str_detect(db_ps$description,"parqu?e?a?deros?")) %>%
  mutate(par_f=ifelse((gar_dp==TRUE | par_dp==TRUE),1,0))





db_ps <- db_ps %>% 
  mutate(bano_d1=str_extract(b_select,"[0-9]+")) %>% 
  mutate(bano_d2=str_count(db_ps$description, "ba[^lsrh][^cd]"))
db_ps$bano_d1 <- as.numeric(db_ps$bano_d1)
db_ps$bano_d2 <- as.numeric(db_ps$bano_d2)
skim(db_ps)
table(db_ps$bano_d1)
db_ps <- db_ps %>% # Se escoge el valor máximo de las variables y se toma un corte de 20 baños luego de inspeccion
  mutate(bano_f=pmax(db_ps$bano_d1,db_ps$bano_d2,db_ps$bathrooms,na.rm = TRUE)) %>% 
  mutate(bano_f=ifelse(bano_f>20,pmax(db_ps$bano_d2,db_ps$bathrooms,na.rm = TRUE),db_ps$bano_f)) 
skim(db_ps)
hist(db_ps$bano_f)


# Variables finales internas -------------------------------------------------
ls(db_ps)
# area_f asc_f bano_f bed_f dep_f ext_f par_f property_type sample


# Melissa

#Verificar si alguna de las propiedadno tiene coordenadas
filtro_nocord<- is.na(db_ps$geometry) 
sum(is.na(db_ps$geometry))
print(db_ps)
glimpse(db_ps)

##Ubicar los puntos en los que se encuentran los parques
available_tags("leisure")
parques <- opq(bbox=getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="leisure",value="park")
print(parques)

parques_sf <- osmdata_sf(parques)
parques_geometria <- parques_sf$osm_polygons %>%
  select(osm_id,name)
print(parques_sf)

#Mapa de la ubicación de los parques
leaflet() %>%
  addTiles() %>%
  addPolygons(data=parques_geometria,col="green",
              opacity=0.8,popup=parques_geometria$name)
db_ps <- st_as_sf(db_ps,coords=c("lon","lat"))

#Determinar los centroides de los parques
centroides_parques <- gCentroid(as(parques_geometria$geometry,"Spatial"),byid=FALSE)
centroides_sf_parques <- st_as_sf(centroides_parques,coords=c("x","y"))

#Medir las distancias de las propiedades a los parques
dist_matrix_parques <- st_distance(x=db_ps,y=centroides_sf_parques)
dim(dist_matrix_parques)
db_ps$distancia_minima_parque <- apply(dist_matrix,1,min)

view(db_ps$distancia_minima_parque)
ls(db_ps)


##Ubicar los puntos en los que se encuentran los hospitales
available_tags("amenity")
hospitales <- opq(bbox=getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity",value="hospital")

hospitales_sf <- osmdata_sf(hospitales)
hospitales_geometria <- hospitales_sf$osm_polygons %>%
  select(osm_id,)
print(hospitales_sf)

# Mapa de la ubicación de los hospitales 
leaflet() %>%
  addTiles() %>%
  addPolygons(data=hospitales_geometria,col="green",
              opacity=0.8)

#Determinar los centroides de los hospitales
centroides_hospitales <- gCentroid(as(hospitales_geometria$geometry,"Spatial"),byid=FALSE)
centroides_sf_hospitales <- st_as_sf(centroides_hospitales,coords=c("x","y"))

dist_matrix_hospitales <- st_distance(x=db_ps,y=centroides_sf_hospitales)
dim(dist_matrix_hospitales)
db_ps$distancia_minima_hospitales <- apply(dist_matrix_hospitales,1,min)

view(db_ps$distancia_minima_hospitales)
ls(db_ps)


##Ubicar los puntos en los que se encuentran las estaciones de buses - Nota: OSM saca estacion de bus como estacion de transmilenio

estacion_bus <- opq(bbox=getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity",value="bus_station")

estacion_bus_sf <- osmdata_sf(estacion_bus)
estacion_bus_geometria <- estacion_bus_sf$osm_polygons %>%
  select(osm_id,)

# Mapa de la ubicación de las estaciones de bus
leaflet() %>%
  addTiles() %>%
  addPolygons(data=estacion_bus_geometria,col="green",
              opacity=0.8)

#Determinar los centroides de las estaciones de bus
centroides_estacion_bus <- gCentroid(as(estacion_bus_geometria$geometry,"Spatial"),byid=FALSE)
centroides_sf_estacion_bus <- st_as_sf(centroides_estacion_bus,coords=c("x","y"))

dist_matrix_estacion_bus <- st_distance(x=db_ps,y=centroides_sf_estacion_bus)
dim(dist_matrix_estacion_bus)
db_ps$distancia_minima_estacion_bus <- apply(dist_matrix_estacion_bus,1,min)


##Ubicar los puntos en los que se encuentran las universidades

universidades <- opq(bbox=getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity",value="university")

universidades_sf <- osmdata_sf(universidades)
universidades_geometria <- universidades_sf$osm_polygons %>%
  select(osm_id,)

# Mapa de la ubicación de las universidades 
leaflet() %>%
  addTiles() %>%
  addPolygons(data=universidades_geometria,col="green",
              opacity=0.8)

#Determinar los centroides de las universidades
centroides_universidades <- gCentroid(as(universidades_geometria$geometry,"Spatial"),byid=FALSE)
centroides_sf_universidades <- st_as_sf(centroides_universidades,coords=c("x","y"))

dist_matrix_universidades <- st_distance(x=db_ps,y=centroides_sf_universidades)
dim(dist_matrix_universidades)
db_ps$distancia_minima_universidades <- apply(dist_matrix_universidades,1,min)


predic_ext <- db_ps %>%
  select(distancia_minima_universidades, distancia_minima_parque, distancia_minima_hospitales, distancia_minima_estacion_bus,)

print(predic_ext)
ls(db_ps)












# Distancia del centro internacional
p_load("tmaptools") #needs to install p_load("geojsonio")
cbd <- geocode_OSM("Centro Internacional, Bogotá", as.sf=T) 
cbd

db_ps$DCBD<-st_distance(x = db_ps, y = cbd)
str(db_ps)
db_ps  %>% st_drop_geometry() %>% group_by(sample)  %>% summarize(mean(DCBD))

head(db_ps)
summary(db_ps$DCBD)

