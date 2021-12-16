

#### Trabajo de Clasificacion  ########################################

# MÓDULO 9: Mineria de Datos	Aplicada      
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
library(factoextra)
library(pca3d)
library(modelsummary)

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
datos$`marital-status` <- as.factor(datos$`marital-status`)
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
#$ `marital-status` <fct>  Never-married,  Married-civ-spouse,  Divorced,  Ma…
#$ occupation       <fct>  Adm-clerical,  Exec-managerial,  Handlers-cleaners…
#$ relationship     <fct>  Not-in-family,  Husband,  Not-in-family,  Husband,…
#$ race             <fct>  White,  White,  White,  Black,  Black,  White,  Bl…
#$ sex              <fct>  Male,  Male,  Male,  Male,  Female,  Female,  Fema…
#$ `capital-gain`   <int> 2174, 0, 0, 0, 0, 0, 0, 0, 14084, 5178, 0, 0, 0, 0,…
#$ `capital-loss`   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#$ `hours-per-week` <int> 40, 13, 40, 40, 40, 40, 16, 45, 50, 40, 80, 40, 30,…
#$ `native-country` <fct>  United-States,  United-States,  United-States,  Un…
#$ label            <fct>  <=50K,  <=50K,  <=50K,  <=50K,  <=50K,  <=50K,  <=…

any(!complete.cases(datos))

#[1] FALSE

map_dbl(datos, .f = function(x){sum(is.na(x))})

#age      workclass         fnlwgt      education  education-num 
#0              0              0              0              0 
#marital-status     occupation   relationship           race            sex 
#0              0              0              0              0 
#capital-gain   capital-loss hours-per-week native-country          label 
#0              0              0              0              0 

# Como se observa no hay valores Na, al leer la documentacion se expone que el uso de ? se expresa
# en valores desconocidos.


#### Analisis Descriptivo ####

# Usamos summary dado a que tambien nos permite conocer informacion tanto de variables categoricas
# como variables numericas.

summary (datos)

# age                    workclass             fnlwgt       
#Min.   :17.00    Private         :22696   Min.   :  12285  
#1st Qu.:28.00    Self-emp-not-inc: 2541   1st Qu.: 117827  
#Median :37.00    Local-gov       : 2093   Median : 178356  
#Mean   :38.58    ?               : 1836   Mean   : 189778  
#3rd Qu.:48.00    State-gov       : 1298   3rd Qu.: 237051  
#Max.   :90.00    Self-emp-inc    : 1116   Max.   :1484705  
#                 (Other)          :  981   

#education            education-num            marital-status 
#HS-grad     :10501   Min.   : 1.00    Divorced             : 4443  
#Some-college: 7291   1st Qu.: 9.00    Married-AF-spouse    :   23  
#Bachelors   : 5355   Median :10.00    Married-civ-spouse   :14976  
#Masters     : 1723   Mean   :10.08    Married-spouse-absent:  418  
#Assoc-voc   : 1382   3rd Qu.:12.00    Never-married        :10683  
#11th        : 1175   Max.   :16.00    Separated            : 1025  
#(Other)      : 5134                   Widowed              :  993  

# occupation                 relationship                    race      
#Prof-specialty :4140    Husband       :13193    Amer-Indian-Eskimo:  311  
#Craft-repair   :4099    Not-in-family : 8305    Asian-Pac-Islander: 1039  
#Exec-managerial:4066    Other-relative:  981    Black             : 3124  
#Adm-clerical   :3770    Own-child     : 5068    Other             :  271  
#Sales          :3650    Unmarried     : 3446    White             :27816  
#Other-service  :3295    Wife          : 1568                              
#(Other)         :9541 

#sex              capital-gain    capital-loss     hours-per-week 
#Female:10771    Min.   :    0    Min.   :   0.0   Min.   : 1.00  
#Male  :21790    1st Qu.:    0    1st Qu.:   0.0   1st Qu.:40.00  
#Median :    0   Median :   0.0   Median :40.00  
#Mean   : 1078   Mean   :  87.3   Mean   :40.44  
#3rd Qu.:    0   3rd Qu.:   0.0   3rd Qu.:45.00  
#Max.   :99999   Max.   :4356.0   Max.   :99.00  

#  native-country          label      
#United-States:29170    <=50K:24720  
#Mexico       :  643    >50K : 7841  
#?            :  583                 
#Philippines  :  198                 
#Germany      :  137                 
#Canada       :  121                 
#(Other)       : 1709        

#### Tablas de Informacion ####


table (datos$label,datos$sex)   

#        Female  Male
#<=50K    9592  15128
#>50K     1179   6662

proportions(table(datos$label,datos$sex), margin = 1)

#        Female      Male
#<=50K 0.3880259 0.6119741
#>50K  0.1503635 0.8496365

# En relacion a la distribucion por genero, se observa que hay un mayor % de hombres dentro
# de las personas que se encuentran dentro del tramo de mayor ingreso. Ello, pudiera deberse a que
# en la muestra                  

proportions(table(datos$label,datos$sex), margin = 2)

#        Female      Male
#<=50K 0.8905394 0.6942634
#>50K  0.1094606 0.3057366

# Ahora al profundizar en el a nivel de genero
# se presencia que un % menor de mujeres integran tal clase con respecto a sus pares. 

# Tal elemento, nos pudiera dar primeras luces de la relacion de genero y el nivel de ingreso
# Dado el periodo del censo utilizado 1994, pudiera ser intuituvo esta relacion, dado la menor tasa 
# de insercion laboral del genero femenino. 








