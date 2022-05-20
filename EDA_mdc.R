### Data whisperers. EDA #####
## Marvel vs. DC ############
#   Por Linfray Porras. Correo: linfray16@gmail.com


#--------- Instalacion de librerias para EDA 
# install.packages("tidyverse")
# install.packages("DataExplorer")
# install.packages("corrplot")
# install.packages("gridExtra")


#---------- Lectura de librerias y la base de datos 
library(tidyverse)
library(DataExplorer)
library(corrplot)
library(gridExtra)
mdc <- read.csv("mdc.csv")

### -------- Primer vistazo
View(mdc) #viendo data
nrow(mdc)# num de obs.
ncol(mdc)# num
dim(mdc)# dimesiones del data frame
str(mdc) #descripcion de la data
names(mdc) #nombres de las variables


### -------------Un poco de limpieza y preparacion
### Quitando Columna repetida y columnas que no usaremos
mdc<-mdc%>%
  select(-c(id,director,stars,description,crit_consensus)) %>% 
  data.table::setnames(old = "X",new="id") 

#Convirtiendo algunas variables en factores
columnasf<-c("genre","mpa_rating","entity")
mdc<-mdc %>% 
  mutate(across(all_of(columnasf),as.factor))


### algunas observaciones
summary(mdc)
sum(duplicated(mdc))
which(is.na(mdc))
plot_missing(mdc)


###------ Analsisis de variables cuantitativas
#Data  numerica
num_mdc<-mdc %>% 
  select_if(is.numeric) 

###Estadisticos de centralidad y dispercion

summary(num_mdc)
#options(scipen=999)
apply(num_mdc,2,var)#varianza
apply(num_mdc,2,sd)#deviacion standar
#options(scipen=0)

#Visalizacion de dichas medidas y deteccion de outliers
names(num_mdc)
par(mfrow=c(2,4))
hist(num_mdc$runtime,main = "Histograma de Runtime",xlab ="Duracion (mins)",ylab = "Frecuencia")
hist(num_mdc$imdb_rating,main = "Histograma IMDB Ratings",xlab ="Clasificacion (1-10)",ylab = "Frecuencia")
hist(num_mdc$imdb_votes,main = "Histograma IMDB Votos",xlab ="Cant. de votos",ylab = "Frecuencia ")
hist(num_mdc$imdb_gross,main = "Histograma Ingreso bruto",xlab ="Ingreso (USD$)",ylab = "Frecuencia ")
boxplot(num_mdc$runtime,main = "Distribucion de Runtime",ylab = "Duracion (mins)")
boxplot(num_mdc$imdb_rating,main = "Distribucion IMDB Ratings",ylab = "Clasificacion)")
boxplot(num_mdc$imdb_votes,main = "Distribucion IMDB Votos",ylab = "Cant. de votos")
boxplot(num_mdc$imdb_gross,main = "Distribucion Ingreso bruto",ylab = "Ingreso (USD$)")

hist(num_mdc$tomato_meter,main = "Histograma Tomatometro",xlab ="% Crititicas Positivas pro",ylab = "Frecuencia")
hist(num_mdc$tomato_review,main = "Histograma Tomato-Reviews",xlab ="Cant. de criticas pro",ylab = "Frecuencia")
hist(num_mdc$tom_aud_score,main = "Histograma Audience score ",xlab ="% Crititicas Positivas audencia",ylab = "Frecuencia")
hist(num_mdc$tom_ratings,main = "Histograma Audience Reviews",xlab ="Cant. de criticas de audiencia",ylab = "Frecuencia")
boxplot(num_mdc$tomato_meter,main = "Distribucion de Tomatometro")
boxplot(num_mdc$tomato_review,main = "Distribucion Tomato-Reviews",ylab ="Cant. de criticas pro")
boxplot(num_mdc$tom_aud_score,main = "Distribucion Audience score ",ylab ="% Crititicas Positivas audencia")
boxplot(num_mdc$tom_ratings,main = "Distribucion Audience Reviews",ylab ="Cant. de criticas de audiencia")

#### Al hacer este tipo de ejercicios, nos surgen algunas preguntas:

# que peliculas tiene el  peor IMDB raitings (oulier)
mdc %>% 
  filter(imdb_rating<=5.7-1.5*IQR(imdb_rating)) %>% 
  select(title,year,entity)


# que peliculas tienen el mayor ingreso bruto
mdc %>% 
  filter(imdb_gross>=324972500 +1.5*IQR(imdb_gross)) %>% 
  select(title,year,entity)

#### En el aspecto multivariante, nos interesa saber como  las variables  cuantitativas que poseemos  se relacionasn entre si

# Anslisis multivariante (correlacion)
par(mfrow=c(1,1))
cm<-cor(num_mdc)
cm
corrplot.mixed(cm)
pairs(num_mdc)

#### Analsisis de variables categoricas


#no numeric data
nonum_mdc<-mdc %>% 
  select(!is.numeric) 
names(nonum_mdc)

# Conteo de peliculas
par(mfrow=c(1,1))
barplot(table(mdc$entity),main = "Conteo: Marvel vs Dc",ylab="cant. de peliculas",col=c("blue","red"))


#stacked barchar entity vc mpa rating
mdc %>% 
  ggplot(aes(x=entity,fill=mpa_rating))+
  geom_bar(position = "stack")+
  labs(title="Peliculas por casa de comic y clasificacion",ylab="conteo")

#Colores para otras graficas
coloresm<-c("DC"="blue","MARVEL"="red")

#barras separadas
mdc %>% 
  ggplot(aes(x=mpa_rating,fill=entity))+
  scale_fill_manual(values = coloresm)+
  geom_bar(position = "dodge")+
  labs(title = "Numero de peliculas por calisivicacion en cada casa de comic")


#Ahora podemos ver alguos patrones de nuestras variables cuantitativas en base a nuestras variables cualitativas

bp1<-mdc %>% 
  ggplot(aes(x=entity,y=runtime))+
  geom_boxplot(fill=c("blue","red"))+
  labs(title = "Distribucion de duracion por  entidad")

bp2<-mdc %>% 
  ggplot(aes(x=entity,y=imdb_rating))+
  geom_boxplot(fill=c("blue","red"))+
  labs(title = "Distribucion IMDB raiting por  entidad")

bp3<-mdc %>% 
  ggplot(aes(x=entity,y=tomato_meter))+
  geom_boxplot(fill=c("blue","red"))+
  labs(title = "Distribucion Tomatometro por  entidad")

bp4<-mdc %>% 
  ggplot(aes(x=entity,y=tomato_meter))+
  geom_boxplot(fill=c("blue","red"))+
  labs(title = "Distribucion Tomatometro por  entidad")

bp5<-mdc %>% 
  ggplot(aes(x=entity,y=tom_aud_score))+
  geom_boxplot(fill=c("blue","red"))+
  labs(title = "Distribucion %Score de audiencia por  entidad")

bp6<-mdc %>% 
  ggplot(aes(x=imdb_gross))+
  geom_bar(aes(fill=entity))+
  scale_fill_manual(values=coloresm) +
  scale_x_binned()+
  facet_wrap(~entity,ncol = 1)+
  labs(title = "Distribucion IMDB Ingreso Bruto por  entidad")

##Colocando todos los plots juntos
grid.arrange(bp1,bp2,bp3,bp4,bp5,bp6,ncol=3,nrow=2)

#Scarplots

#Los siguientes graficos permitiran el comportmiento del ingreso bruto de las peliculas respecto al tomatometro
#
mdc %>% 
  ggplot(aes(x=tomato_meter,y=imdb_gross)) +
  geom_point()+
  labs(title = "Tomatometro VS Ingreso Bruto",
       x="% Criticas positvas de pros",
       y="Ingreso Bruto")


mdc %>% 
  ggplot(aes(x=tomato_meter,y=imdb_gross,color=entity)) +
  geom_point(size = 3,alpha=.6)+
  scale_color_manual(values = coloresm)+
  labs(title = "Tomatometro VS Ingreso Bruto por entidad",
       x="% Criticas positvas de pros",
       y="Ingreso Bruto")


mdc %>% 
  ggplot(aes(x=tomato_meter,y=imdb_gross,color=entity,shape=mpa_rating)) +
  geom_point(size = 3,alpha=.6)+
scale_color_manual(values = coloresm)+
  labs(title = "Tomatometro VS Ingreso Bruto por entidad y clasificacion",
       x="% Criticas positvas de pros",
       y="Ingreso Bruto")

  
#un leve vistzado a las relaciones entre pares de pariables segun la casa de comic
pairs(num_mdc,col=c("blue","Red")[unclass(mdc$entity)])


