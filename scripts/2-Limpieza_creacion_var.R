rm(list = ls())
#install.packages("rio")
#install.packages("pacman")
#install.packages("tidyverse")
#install.packages("sf")
#install.packages(leaflet)
#install.packages("osmdata")
#install.packages("osmdata_sf")
#install.packages("rgeos")
#install.packages("devtools")
#install.packages("skimr")
#install.packages("stringr")
require("rgeos")
require("osmdata")
require("osmdata_sf")
require("leaflet")
require("pacman")
require("tidyverse")
require("sf")
require("devtools")
require(skimr)
require(stringr)
require(ggplot2)
p_load(skimr,pacman,tidyverse,sf,devtools,leaflet,rio,osmdata,rgeos,vtable,stargazer,spatialsample)

# Corrida paralelo
p_load(doParallel)
detectCores()
registerDoParallel(2)

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
db_ps <- db_ps %>% # Se escoge el valor máximo de las variables y se toma un corte
                  # de 20 baños luego de inspeccion
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

# Se crea la variable dummy GARAJE/ parqueadero = terraza patio balcon
gar <- str_extract(db_ps$description,"gara[gj]es?")
gar
par <- str_extract(db_ps$description,"parqu?e?a?deros?")
par

db_ps <- db_ps %>% 
  mutate(gar_dp=str_detect(db_ps$description,"gara[gj]es?")) %>% 
  mutate(par_dp=str_detect(db_ps$description,"parqu?e?a?deros?")) %>%
  mutate(par_f=ifelse((gar_dp==TRUE | par_dp==TRUE),1,0))

# Variables finales internas -------------------------------------------------
ls(db_ps)
# area_f asc_f bano_f bed_f dep_f ext_f par_f property_type sample

# Se imputan los valores area para NA tomando la media de cada muestra
skim(db_ps)
db <- db_ps
db %>% select(sample,area_f) %>% tail()
db <-  db %>% 
  group_by(sample) %>% 
  mutate(media_area = mean(area_f,na.rm=T)) %>% 
  mutate(area_f = ifelse(test = is.na(area_f)==T,
                         yes = media_area,
                         no = area_f))

db %>% select(sample,area_f,media_area) %>% tail()
skim(db)

db <-  db %>% 
  group_by(sample) %>% 
  mutate(med_asc = median(asc_f,na.rm=T)) %>% 
  mutate(asc_f = ifelse(test = is.na(asc_f)==T,
                         yes = med_asc,
                         no = asc_f))
db <-  db %>% 
  group_by(sample) %>% 
  mutate(med_bed = median(bed_f,na.rm=T)) %>% 
  mutate(bed_f = ifelse(test = is.na(bed_f)==T,
                        yes = med_bed,
                        no = bed_f))
db <-  db %>% 
  group_by(sample) %>% 
  mutate(med_dep = median(dep_f,na.rm=T)) %>% 
  mutate(dep_f = ifelse(test = is.na(dep_f)==T,
                        yes = med_dep,
                        no = dep_f))
db <-  db %>% 
  group_by(sample) %>% 
  mutate(med_bano = median(bano_f,na.rm=T)) %>% 
  mutate(bano_f = ifelse(test = is.na(bano_f)==T,
                        yes = med_bano,
                        no = bano_f))
db <-  db %>% 
  group_by(sample) %>% 
  mutate(med_ext = median(ext_f,na.rm=T)) %>% 
  mutate(ext_f = ifelse(test = is.na(ext_f)==T,
                         yes = med_ext,
                         no = ext_f))
db <-  db %>% 
  group_by(sample) %>% 
  mutate(med_par = median(par_f,na.rm=T)) %>% 
  mutate(par_f = ifelse(test = is.na(par_f)==T,
                         yes = med_par,
                         no = par_f))
db_ps <- db

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

# Creación de variables externas -----------------------------------------------------------------------------------------------

#Verificar si alguna de las propiedades no tiene coordenadas
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
db_ps$distancia_minima_parque <- apply(dist_matrix_parques,1,min)

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


##Ubicar los puntos en los que se encuentran las estaciones de buses - 
##Nota: OSM saca estacion de bus como estacion de transmilenio

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

#Variables externas finales -----------------------------------------------------------
predic_ext <- db_ps %>%
  select(distancia_minima_universidades, distancia_minima_parque, distancia_minima_hospitales, distancia_minima_estacion_bus,)

print(predic_ext)
ls(db_ps)

## Base final para análisis --------
data_final<-db_ps %>%
  select(property_id, price, area_f, asc_f, bano_f, bed_f, dep_f, par_f, ext_f, property_type,
         distancia_minima_estacion_bus, distancia_minima_hospitales,
         distancia_minima_parque, distancia_minima_universidades, sample,
         geometry)
glimpse (data_final)
skim(data_final)

df_sf <- st_as_sf(data_final,coords=c("lon","lat"), crs = 4326)

# UPZ Bogota --------------------------------------------------------------
# Se obtiene la informacion de UPZ de Bogotá de IDECA
getwd()
upz <- sf::st_read("../stores/upz-bogota.shp") %>% 
  st_transform(4326)
upz_df <- st_join(df_sf,upz,join=st_within)
glimpse(upz_df)
ls(upz_df)
skim(upz_df)
#write_csv2(upz_df,"../stores/upz.csv")
faltantes <- read.csv2("../stores/upz_FALTANTES.csv",header = T)
ls(upz_df)
ls(faltantes)
upz_df1 <- full_join(upz_df,faltantes,"property_id")
head(upz_df1)
ls(upz_df1)
upz_df1 <- upz_df1 %>% 
  mutate(nom_upz.x=ifelse(is.na(nom_upz.x),nom_upz.y,nom_upz.x)) %>% 
  select(-c(sample.y,nom_upz.y)) %>% 
  rename(sample=sample.x) %>% 
  rename(nom_upz=nom_upz.x) %>% 
  filter(!is.na(sample))
upz_df <- upz_df1
skim(upz_df)

# Se eliminan observaciones sin UPZ en el train
upz_df <- upz_df %>% 
  group_by(sample) %>% 
  filter(!is.na(nom_upz)) %>% 
  select(-c(cod_loc,area_urbana,poblacion_u,densidad_ur,cod_upz,nomb_loc))

table(upz_df$nom_upz) # Verificar UPZ

# Buffer ------------------------------------------------------------------
# Se desarrollará LLOCV por UPZ
set.seed(2023)
df_train <- upz_df %>% 
  filter(sample=="train")
df_test <- upz_df %>% 
  filter(sample=="test")

glimpse(df_train)
skim(df_train)

# Se hace LLOCV para train
location_folds <- spatial_leave_location_out_cv( #Divide la meustra en grupos de igual tamaño
  df_train,
  group=nom_upz
)
autoplot(location_folds)

p_load("purrr")
p_load(caret)

folds_train<-list()
for(i in 1:length(location_folds$splits)){
  folds_train[[i]]<- location_folds$splits[[i]]$in_id
}
fitControl_tp<-trainControl(method ="cv",
                            number=5)
ls(df_train)
skim(df_train)

## Cambiar el tipo de variable PENDIENTE
ls(df_train)
as_factor(c(df_train$bano_f,df_train$dep_f,df_train$bed_f,df_train$asc_f,
          df_train$par_f,df_train$ext_f))
skim(df_train)

as_character(df_train$bano_f)

(c(bano_f,dep_f,par_f,asc_f,bed_f,ext_f))


df_train <- df_train %>% 
  st_drop_geometry()


## Modelo 0 --------

reg0 <- lm(log(price) ~ area_f + bano_f + bed_f + property_type + 
            distancia_minima_estacion_bus + distancia_minima_hospitales + 
            distancia_minima_parque + distancia_minima_universidades,
          data=df_train)
stargazer(reg,type="text")
str(reg0)
df_test$log_price_hat0 <- predict(reg0,newdata=df_test)
head(df_test %>% select(log_price_hat0) %>% st_drop_geometry())
df_test <- df_test %>% 
  mutate(price_hat0=exp(log_price_hat0)) %>% 
  mutate(price_hat0=round(price_hat0,-5))
head(df_test %>% select(log_price_hat0,price_hat0) %>% st_drop_geometry())

intento0 <- df_test %>% 
  select(property_id,price_hat0) %>% 
  rename(price=price_hat0) %>% 
  st_drop_geometry()
write.csv(intento0,"intento0.csv",row.names = FALSE)

# Calcular MAE MAPE
y_hat_insample0 <- predict(reg,df_train)
p_load(MLmetrics)
MAE(y_pred=y_hat_insample0,y_true=log(df_train$price))
#0.2997065
exp(0.2997065)
#1.349463
MAPE(y_pred=y_hat_insample0,y_true=log(df_train$price))
#0.01478867

#Modelo1 -------
EN_tp<-train(log(price) ~ area_f + bano_f + bed_f + property_type + 
               distancia_minima_estacion_bus + distancia_minima_hospitales + 
               distancia_minima_parque + distancia_minima_universidades,
             data=df_train,
             method = 'glmnet', 
             trControl = fitControl_tp,
             metric="MAE",
             tuneGrid = expand.grid(alpha =seq(0,1,length.out = 20),
                                    lambda = seq(0.001,0.2,length.out = 50))
)
EN_tp
EN_tp$bestTune
df_test$log_price_hat1 <- predict(EN_tp,newdata=df_test)
head(df_test %>% select(log_price_hat1) %>% st_drop_geometry())
df_test <- df_test %>% 
  mutate(price_hat1=exp(log_price_hat1)) %>% 
  mutate(price_hat1=round(price_hat1,-5))
head(df_test %>% select(log_price_hat1,price_hat1) %>% st_drop_geometry())

intento1 <- df_test %>% 
  select(property_id,price_hat1) %>% 
  rename(price=price_hat1) %>% 
  st_drop_geometry()
write.csv(intento1,"intento1.csv",row.names = FALSE)

# Calcular MAE MAPE
y_hat_insample1 <- predict(EN_tp,df_train)
p_load(MLmetrics)
MAE(y_pred=y_hat_insample1,y_true=log(df_train$price))
#0.3000676
exp(0.3000676)
#1.34995
MAPE(y_pred=y_hat_insample1,y_true=log(df_train$price))
#0.01480671


# MODELO 2 ------
reg <- lm(log(price) ~ par_f + ext_f + bano_f + bed_f + property_type + 
            distancia_minima_estacion_bus + distancia_minima_hospitales + 
            distancia_minima_parque + distancia_minima_universidades,
          data=df_train)
stargazer(reg,type="text")
str(reg)
df_test$log_price_hat2 <- predict(reg,newdata=df_test)
head(df_test %>% select(log_price_hat2) %>% st_drop_geometry())
df_test <- df_test %>% 
  mutate(price_hat2=exp(log_price_hat2)) %>% 
  mutate(price_hat2=round(price_hat2,-5))
head(df_test %>% select(log_price_hat2,price_hat2) %>% st_drop_geometry())

intento2 <- df_test %>% 
  select(property_id,price_hat2) %>% 
  rename(price=price_hat2) %>% 
  st_drop_geometry()
write.csv(intento2,"intento2.csv",row.names = FALSE)

# Calcular MAE MAPE
y_hat_insample2 <- predict(reg,df_train)
p_load(MLmetrics)
MAE(y_pred=y_hat_insample2,y_true=log(df_train$price))
#0.2973535
exp(0.2973535)
#1.346291
MAPE(y_pred=y_hat_insample2,y_true=log(df_train$price))
#0.01467218

#Modelo3-----
EN_tp0<-train(log(price) ~ par_f + ext_f + bano_f + bed_f + property_type + 
               distancia_minima_estacion_bus + distancia_minima_hospitales + 
               distancia_minima_parque + distancia_minima_universidades,
             data=df_train,
             method = 'glmnet', 
             trControl = fitControl_tp,
             metric="MAE",
             tuneGrid = expand.grid(alpha =seq(0,1,length.out = 20),
                                    lambda = seq(0.001,0.2,length.out = 50))
)
EN_tp0
EN_tp0$bestTune
df_test$log_price_hat3 <- predict(EN_tp0,newdata=df_test)
head(df_test %>% select(log_price_hat3) %>% st_drop_geometry())
df_test <- df_test %>% 
  mutate(price_hat3=exp(log_price_hat3)) %>% 
  mutate(price_hat3=round(price_hat3,-5))
head(df_test %>% select(log_price_hat3,price_hat3) %>% st_drop_geometry())

intento3 <- df_test %>% 
  select(property_id,price_hat3) %>% 
  rename(price=price_hat3) %>% 
  st_drop_geometry()
write.csv(intento3,"intento3.csv",row.names = FALSE)
# Calcular MAE MAPE
y_hat_insample3 <- predict(EN_tp0,df_train)
p_load(MLmetrics)
MAE(y_pred=y_hat_insample3,y_true=log(df_train$price))
#0.2976548
exp(0.2976548)
#1.346697
MAPE(y_pred=y_hat_insample3,y_true=log(df_train$price))
#0.01468722

# Model 4 --------
reg4 <- lm(log(price) ~ asc_f + ext_f + bano_f + bed_f + property_type + 
            distancia_minima_estacion_bus + distancia_minima_hospitales + 
            distancia_minima_parque + distancia_minima_universidades,
          data=df_train)
stargazer(reg4,type="text")
str(reg4)
df_test$log_price_hat4 <- predict(reg4,newdata=df_test)
head(df_test %>% select(log_price_hat4) %>% st_drop_geometry())
df_test <- df_test %>% 
  mutate(price_hat4=exp(log_price_hat4)) %>% 
  mutate(price_hat4=round(price_hat4,-5))
head(df_test %>% select(log_price_hat4,price_hat4) %>% st_drop_geometry())

intento4 <- df_test %>% 
  select(property_id,price_hat4) %>% 
  rename(price=price_hat4) %>% 
  st_drop_geometry()
write.csv(intento4,"intento4.csv",row.names = FALSE)

# Calcular MAE MAPE
y_hat_insample4 <- predict(reg4,df_train)
p_load(MLmetrics)
MAE(y_pred=y_hat_insample4,y_true=log(df_train$price))
#0.2964306
exp(0.2964306)
#1.345049
MAPE(y_pred=y_hat_insample4,y_true=log(df_train$price))
#0.01462549

# Model 5 --------
reg5 <- lm(log(price) ~ asc_f + ext_f + factor(bano_f) + factor(bed_f) + 
             property_type + distancia_minima_estacion_bus + 
             distancia_minima_hospitales + 
             distancia_minima_parque + distancia_minima_universidades,
           data=df_train)
stargazer(reg5,type="text")
str(reg5)
df_test$log_price_hat5 <- predict(reg5,newdata=df_test)
head(df_test %>% select(log_price_hat5) %>% st_drop_geometry())
df_test <- df_test %>% 
  mutate(price_hat5=exp(log_price_hat5)) %>% 
  mutate(price_hat5=round(price_hat5,-5))
head(df_test %>% select(log_price_hat5,price_hat5) %>% st_drop_geometry())

intento5 <- df_test %>% 
  select(property_id,price_hat5) %>% 
  rename(price=price_hat5) %>% 
  st_drop_geometry()
write.csv(intento5,"intento5.csv",row.names = FALSE)

# Calcular MAE MAPE
y_hat_insample5 <- predict(reg5,df_train)
p_load(MLmetrics)
MAE(y_pred=y_hat_insample5,y_true=log(df_train$price))
#0.2655674
exp(0.2655674)
#1.304171
MAPE(y_pred=y_hat_insample5,y_true=log(df_train$price))
#0.01310394

# Arboles -----
require("pacman")
p_load("tidyverse","ggplot2")
p_load("caret")
fitControl <- trainControl(method="cv", number=5)
set.seed(2023)

## Arbol 1 (10) ----
ls(df_train)
tree <- train(
  log(price) ~ asc_f + ext_f + factor(bano_f) + factor(bed_f) + 
    property_type + distancia_minima_estacion_bus + 
    distancia_minima_hospitales + 
    distancia_minima_parque + distancia_minima_universidades,
  data=df_train,
  method = "rpart",
  metric="MAE",
  trControl = fitControl,
  tuneLength=10
)
tree
# Predecir el precio
df_test$tree_logprice1 <- predict(tree,df_test)
df_test <- df_test %>% 
  st_drop_geometry()  %>%
  mutate(pt_1=exp(tree_logprice1)) %>% 
  mutate(pt_1=round(pt_1,-5))
int_tree1 <- df_test %>%
  ungroup() %>% 
  select(property_id,pt_1) %>% 
  rename(price=pt_1)
write.csv(int_tree1,"intento6.csv",row.names = FALSE)
# MAE / MAPE
y_hat_insample6 <- predict(tree,df_train)
p_load(MLmetrics)
MAE(y_pred=y_hat_insample6,y_true=log(df_train$price))
#0.2807288
exp(0.2807288)
#1.324094
MAPE(y_pred=y_hat_insample6,y_true=log(df_train$price))
#0.01385536

# Mirar árboles bonitos
p_load(rattle)
tree$finalModel
fancyRpartPlot(tree$finalModel)

## Arbol 2 (50) ----
ls(df_train)
tree2 <- train(
  log(price) ~ asc_f + ext_f + factor(bano_f) + factor(bed_f) + 
    property_type + distancia_minima_estacion_bus + 
    distancia_minima_hospitales + 
    distancia_minima_parque + distancia_minima_universidades,
  data=df_train,
  method = "rpart",
  metric="MAE",
  trControl = fitControl,
  tuneLength=50
)
tree2
# Predecir el precio
df_test$tree_logprice2 <- predict(tree2,df_test)
df_test <- df_test %>% 
  st_drop_geometry()  %>%
  mutate(pt_2=exp(tree_logprice2)) %>% 
  mutate(pt_2=round(pt_2,-5))
int_tree2 <- df_test %>%
  ungroup() %>% 
  select(property_id,pt_2) %>% 
  rename(price=pt_2)
write.csv(int_tree2,"intento7.csv",row.names = FALSE)
# MAE / MAPE
y_hat_insample7 <- predict(tree2,df_train)
p_load(MLmetrics)
MAE(y_pred=y_hat_insample7,y_true=log(df_train$price))
#0.2509776
exp(0.2509776)
#1.285281
MAPE(y_pred=y_hat_insample7,y_true=log(df_train$price))
#0.01238467

# Mirar árboles bonitos
p_load(rattle)
tree2$finalModel
fancyRpartPlot(tree2$finalModel)

## Arbol 3 (100) ----
ls(df_train)
tree3 <- train(
  log(price) ~ asc_f + ext_f + factor(bano_f) + factor(bed_f) + 
    property_type + distancia_minima_estacion_bus + 
    distancia_minima_hospitales + 
    distancia_minima_parque + distancia_minima_universidades,
  data=df_train,
  method = "rpart",
  metric="MAE",
  trControl = fitControl,
  tuneLength=100
)
tree3
# Predecir el precio
df_test$tree_logprice3 <- predict(tree3,df_test)
df_test <- df_test %>% 
  st_drop_geometry()  %>%
  mutate(pt_3=exp(tree_logprice3)) %>% 
  mutate(pt_3=round(pt_3,-5))
int_tree3 <- df_test %>%
  ungroup() %>% 
  select(property_id,pt_3) %>% 
  rename(price=pt_3)
write.csv(int_tree3,"intento8.csv",row.names = FALSE)
# MAE / MAPE
y_hat_insample8 <- predict(tree3,df_train)
p_load(MLmetrics)
MAE(y_pred=y_hat_insample8,y_true=log(df_train$price))
#0.2363406
exp(0.2363406)
#1.266606
MAPE(y_pred=y_hat_insample8,y_true=log(df_train$price))
#0.01166248

# Random forest -------
p_load("ranger")
set.seed(2023)
tunegrid_rf <- expand.grid(
  min.node.size=c(10,30,50,70,100),
  mtry=c(2,4,6), #columnas
  splitrule=c("variance")
)
tree_ranger <- train(
  log(price) ~ asc_f + ext_f + factor(bano_f) + factor(bed_f) + 
    property_type + distancia_minima_estacion_bus + 
    distancia_minima_hospitales + 
    distancia_minima_parque + distancia_minima_universidades,
  data=df_train,
  method="ranger",
  trControl=fitControl,
  metric="MAE",
  maximize=F,
  tuneGrid=tunegrid_rf
)
tree_ranger
plot(tree_ranger)
tree_ranger$finalModel

# Predecir el precio
df_test$ran_log <- predict(tree_ranger,df_test)
df_test <- df_test %>% 
  st_drop_geometry()  %>%
  mutate(random=exp(ran_log)) %>% 
  mutate(random=round(random,-5))
int_random <- df_test %>%
  ungroup() %>% 
  select(property_id,random) %>% 
  rename(price=random)
write.csv(int_random,"intento9.csv",row.names = FALSE)
# MAE / MAPE
y_hat_insample9 <- predict(tree_ranger,df_train)
p_load(MLmetrics)
MAE(y_pred=y_hat_insample9,y_true=log(df_train$price))
#0.1982735
exp(0.1982735)
#1.219296
MAPE(y_pred=y_hat_insample9,y_true=log(df_train$price))
#0.00978447

# Modelo final EN
EN_tpf<-train(log(price) ~ asc_f + ext_f + factor(bano_f) + factor(bed_f) + 
                property_type + distancia_minima_estacion_bus + 
                distancia_minima_hospitales + 
                distancia_minima_parque + distancia_minima_universidades,
              data=df_train,
              method = 'glmnet', 
              trControl = fitControl_tp,
              metric="MAE",
              tuneGrid = expand.grid(alpha =seq(0,1,length.out = 20),
                                     lambda = seq(0.001,0.2,length.out = 50))
)
EN_tpf
EN_tpf$bestTune
df_test$log_price_ENf <- predict(EN_tpf,newdata=df_test)
head(df_test %>% select(log_price_ENf) %>% st_drop_geometry())
df_test <- df_test %>% 
  mutate(price_ENf=exp(log_price_ENf)) %>% 
  mutate(price_ENf=round(price_ENf,-5))
head(df_test %>% select(log_price_ENf,price_ENf) %>% st_drop_geometry())

intento10 <- df_test %>% 
  ungroup() %>% 
  select(property_id,price_ENf) %>% 
  rename(price=price_ENf) %>% 
  st_drop_geometry()
write.csv(intento10,"intento10.csv",row.names = FALSE)
# Calcular MAE MAPE
y_hat_insample10 <- predict(EN_tpf,df_train)
p_load(MLmetrics)
MAE(y_pred=y_hat_insample10,y_true=log(df_train$price))
#0.2662445
exp(0.2662445)
#1.305054
MAPE(y_pred=y_hat_insample10,y_true=log(df_train$price))
#0.01313765

