

#### Trabajo de Prediccion ########################################


# MÓDULO 9: Mineria de Datos	      
# NOMBRE PROFESOR    : Juan Carlos Herrera -
# NOMBRE DEL ESTUDIANTES: Daniel Contreras 
#                         Valentina Fernandez
#                         Emilio Mun~oz
#                         Ignacio Olave
                          

#### Instalacion de Librerias #### 

library(psych)
library(dplyr)
library(caret)
library(corrplot)
library(rpart)
library(rpart.plot)
library(ROCR)
library(ggplot2)
library(caTools)

#### Lectura de Base de datos #### 

# Lectura via descarga de archivo

base <- read.csv("~/Downloads/adult.data", header=FALSE)



# Lectura via enlace de base de datos

url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data'

datos <-  read.table(url,sep=",",
                     header=FALSE)

# Verificacion de similitud 

#datos == base
#V1   V2   V3   V4   V5   V6   V7   V8   V9  V10  V11  V12  V13  V14
#[1,] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

# El anterior es solo una extraccion de forma de ejemplo

# Aun falta agregar los nombres de las columnas a la base
# la Informacion esta contenida en https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names


colnames(datos) <-  c('age', 'workclass', 'fnlwgt', 'education', 'education-num', 'marital-status', 'occupation',
                      'relationship', 'race', 'sex', 'capital-gain', 'capital-loss', 'hours-per-week', 'native-country', 'label')

View(datos)

class(datos)

# "data.frame"


#### Describir Base de datos ####

dim(datos)
# 32561    15

# Se observan 32 561 registros, y 15 variables, entre ella, la de objetivo.

str(datos)


#'data.frame':	32561 obs. of  15 variables:
#$ age           : int  39 50 38 53 28 37 49 52 31 42 ...
#$ workclass     : chr  " State-gov" " Self-emp-not-inc" " Private" " Private" ...
#$ fnlwgt        : int  77516 83311 215646 234721 338409 284582 160187 209642 45781 159449 ...
#$ education     : chr  " Bachelors" " Bachelors" " HS-grad" " 11th" ...
#$ education-num : int  13 13 9 7 13 14 5 9 14 13 ...
#$ marital-status: chr  " Never-married" " Married-civ-spouse" " Divorced" " Married-civ-spouse" ...
#$ occupation    : chr  " Adm-clerical" " Exec-managerial" " Handlers-cleaners" " Handlers-cleaners" ...
#$ relationship  : chr  " Not-in-family" " Husband" " Not-in-family" " Husband" ...
#$ race          : chr  " White" " White" " White" " Black" ...
#$ sex           : chr  " Male" " Male" " Male" " Male" ...
#$ capital-gain  : int  2174 0 0 0 0 0 0 0 14084 5178 ...
#$ capital-loss  : int  0 0 0 0 0 0 0 0 0 0 ...
#$ hours-per-week: int  40 13 40 40 40 40 16 45 50 40 ...
#$ native-country: chr  " United-States" " United-States" " United-States" " United-States" ...
#$ label         : chr  " <=50K" " <=50K" " <=50K" " <=50K"


List_var <- data.frame(nombre = names(datos), 
                       tipo = sapply(datos, class))

table(List_var$tipo)

#character   integer 
#    9         6 

# Obtenemos que de las 15 variables, 9 son categoricas. 

var_cont <- unlist(subset(List_var, tipo == "integer", select = "nombre"), use.names = FALSE)
var_cat <- unlist(subset(List_var, tipo == "character", select = "nombre"), use.names = FALSE)

# Convertir a factor 

datos$workclass <- as.factor(datos$workclass)
datos$education <- as.factor(datos$education)
datos$marital-status <- as.factor(datos$marital-status)
datos$occupation <- as.factor(datos$occupation)
datos$relationship <- as.factor(datos$relationship)
datos$race <- as.factor(datos$race)
datos$sex <- as.factor(datos$sex)
datos$`native-country` <- as.factor(datos$`native-country`)
datos$label <- as.factor(datos$label)

glimpse(datos)

#Rows: 32,561
#Columns: 15
#$ age              <int> 39, 50, 38, 53, 28, 37, 49, 52, 31, 42, 37, 30, 23,…
#$ workclass        <fct>  State-gov,  Self-emp-not-inc,  Private,  Private, …
#$ fnlwgt           <int> 77516, 83311, 215646, 234721, 338409, 284582, 16018…
#$ education        <fct>  Bachelors,  Bachelors,  HS-grad,  11th,  Bachelors…
#$ `education-num`  <int> 13, 13, 9, 7, 13, 14, 5, 9, 14, 13, 10, 13, 13, 12,…
#$ `marital-status` <chr> " Never-married", " Married-civ-spouse", " Divorced…
#$ occupation       <fct>  Adm-clerical,  Exec-managerial,  Handlers-cleaners…
#$ relationship     <fct>  Not-in-family,  Husband,  Not-in-family,  Husband,…
#$ race             <fct>  White,  White,  White,  Black,  Black,  White,  Bl…
#$ sex              <fct>  Male,  Male,  Male,  Male,  Female,  Female,  Fema…
#$ `capital-gain`   <int> 2174, 0, 0, 0, 0, 0, 0, 0, 14084, 5178, 0, 0, 0, 0,…
#$ `capital-loss`   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#$ `hours-per-week` <int> 40, 13, 40, 40, 40, 40, 16, 45, 50, 40, 80, 40, 30,…
#$ `native-country` <fct>  United-States,  United-States,  United-States,  Un…
#$ label            <fct>  <=50K,  <=50K,  <=50K,  <=50K,  <=50K,  <=50K,  <=…


#### Analisis Descriptivo ####








