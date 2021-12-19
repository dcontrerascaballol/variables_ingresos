
#### Trabajo de Clasificacion  ########################################

# MÓDULO 9: Mineria de Datos	Aplicada      
# NOMBRE PROFESOR    : Juan Carlos Herrera - Jose Yan~ez
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

#var_cont <- unlist(subset(List_var, tipo == "integer", select = "nombre"), use.names = FALSE)
#var_cat <- unlist(subset(List_var, tipo == "character", select = "nombre"), use.names = FALSE)

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
#                Median :    0    Median :   0.0   Median :40.00  
#                Mean   : 1078    Mean   :  87.3   Mean   :40.44  
#                3rd Qu.:    0    3rd Qu.:   0.0   3rd Qu.:45.00  
#                Max.   :99999    Max.   :4356.0   Max.   :99.00  

#  native-country          label      
#United-States:29170    <=50K:24720  
#Mexico       :  643    >50K : 7841  
#?            :  583                 
#Philippines  :  198                 
#Germany      :  137                 
#Canada       :  121                 
#(Other)       : 1709        

#### Analisis de Variables continuas####

summary(datos$`hours-per-week`)

#Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
#1.00   40.00   40.00   40.44   45.00   99.00 

# Se debe considerar el amplio rango de datos, y que el max, pudiera implicar un 
# nivel que incluye incluso horas nocturnas y fin de semana

describe(datos$age)


#    vars  n   mean    sd   median trimmed mad  min max  range skew kurtosis
#X1    1 32561 38.58 13.64     37   37.69 14.83  17  90    73 0.56    -0.17
#    se
#X1 0.08

# Considerar que el minimo es bajo la edad legal de trabajo (debiera tener permiso paternal), 


#### Graficos Variables Continuas ### 

#Distribucion Education Num
par(mfrow=c(2,3))
boxplot(datos$`education-num`, data=datos)
title('Education Num')

#Distribucion Capital Gain'
boxplot(datos$`capital-gain`, data=datos)
title('Capital Gain')

#Distribucion Capital Loss
boxplot(datos$`capital-loss`, data=datos)
title('Capital Loss')

#Distribucion Hours per week
boxplot(datos$`hours-per-week`, data=datos)
title('Hours per week')

#Distribucion Age
boxplot(datos$age, data=datos)
title('Age')

### Relaciones entre Variables continuas ###


par(mfrow=c(2,3))
# Relacion grafica Hours per Week y Education Num
plot(datos$`hours-per-week`~ datos$`education-num`, data=datos)
title('Hours per week v/s Education Num')

# Relacion grafica Capital Gain y Education Num
plot(datos$`education-num`~ datos$`capital-gain`, data=datos)
title('Education Num v/s Capital Gain')

# Relacion grafica Capital Loss y Education Num
plot(datos$`education-num`~datos$`capital-loss`, data=datos)
title('Education Num v/s Capital Loss')

# Relacion grafica Hours oer Week y Capital Gain
plot(datos$`hours-per-week` ~ datos$`capital-gain`,data=datos)
title('Hours per week v/s Capital Gain')

# Relacion grafica Age y Education Num
plot(datos$`education-num` ~  datos$age, data=datos)
title('Education Num v/s Age')


#### Analisis Variables Categoricas ####

table (datos$label)  
#<=50K   >50K 
#24720   7841 

proportions(table(datos$label))
#<=50K      >50K 
#0.7591904 0.2408096 

# Como se observa los que ganan sobre 50 mil dolares (dado la base de Censo US), son casi 24%
# gran parte de los registros de la base tienen un ingreso menor a tal. Lo que nos indica, que existe 
# una nivel de observaciones desbalanceada a nivel de ingreso superiores a >50K. 


# Ingreso por Genero

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

# Ingreso por Workclass

table (datos$label,datos$workclass)  


#        ?      Federal-gov  Local-gov  Never-worked  Private  Self-emp-inc
#<=50K  1645          589       1476             7    17733           494
#>50K    191          371        617             0     4963           622

#           Self-emp-not-inc  State-gov  Without-pay
#<=50K              1817        945           14
#>50K                724        353            0


proportions(table(datos$label,datos$workclass), margin = 2)

#           ?      Federal-gov  Local-gov  Never-worked   Private
#<=50K 0.8959695    0.6135417  0.7052078     1.0000000 0.7813271
#>50K  0.1040305    0.3864583  0.2947922     0.0000000 0.2186729

#       Self-emp-inc  Self-emp-not-inc  State-gov  Without-pay
#<=50K     0.4426523         0.7150728  0.7280431    1.0000000
#>50K      0.5573477         0.2849272  0.2719569    0.0000000


# Ahora con respecto a categorias de trabajo, se visualiza que en comparacion con sus pares
# el trabajador autonomo (emprendedor) es el que presenta una mayor proporcion, en terminos absolutos
# son los que poseen cargos en el sector privado.

proportions(table(datos$label,datos$workclass), margin = 1)

#           ?       Federal-gov    Local-gov  Never-worked      Private
#<=50K 0.0665453074 0.0238268608 0.0597087379  0.0002831715 0.7173543689
#>50K  0.0243591379 0.0473153934 0.0786889427  0.0000000000 0.6329549802

#         Self-emp-inc  Self-emp-not-inc State-gov  Without-pay
#<=50K  0.0199838188      0.0735032362 0.0382281553 0.0005663430
#>50K   0.0793266165      0.0923351613 0.0450197679 0.0000000000


# Con respecto, de la composicion de las observaciones que componen la condicion de altos inresos,
# se identifica que un alto porcentaje de ellos, 63%, provienen del componente privado (debiese ser mayor al sumar otros 
# componente).

# Ingreso por Education

table (datos$label,datos$education)  

#       10th  11th  12th  1st-4th  5th-6th  7th-8th  9th    Assoc-acdm
#<=50K   871  1115   400      162      317      606  487         802
#>50K     62    60    33        6       16       40   27         265

#         Assoc-voc  Bachelors  Doctorate  HS-grad  Masters  Preschool
#<=50K       1021       3134        107     8826      764         51
#>50K         361       2221        306     1675      959          0

#           Prof-school  Some-college
#<=50K          153          5904
#>50K           423          1387


proportions(table(datos$label,datos$education), margin = 2)

#         10th       11th       12th      1st-4th    5th-6th    7th-8th
#<=50K 0.93354770 0.94893617 0.92378753 0.96428571 0.95195195 0.93808050
#>50K  0.06645230 0.05106383 0.07621247 0.03571429 0.04804805 0.06191950

#         9th       Assoc-acdm  Assoc-voc  Bachelors  Doctorate  HS-grad
#<=50K 0.94747082  0.75164011 0.73878437 0.58524743 0.25907990 0.84049138
#>50K  0.05252918  0.24835989 0.26121563 0.41475257 0.74092010 0.15950862

#         Masters  Preschool  Prof-school  Some-college
#<=50K 0.44341265 1.00000000   0.26562500    0.80976546
#>50K  0.55658735 0.00000000   0.73437500    0.19023454

# Como se observa al comparar intra grupos con respecto a la clase de ingreso, se observa
# que los doctorados y profesionales (ej. abogados, medicos, otros, fuente: https://guides.lib.uw.edu/bothell/gradschool/gradprof)
# con cerca de 74% y 73% respectivamente, le siguen los master con 56%. 
# A nivel de frecuencias absolutas se manifiesta que los bachiller (bachelor)

# Ingreso por Occupation

table (datos$label,datos$occupation)  

#       ?        Adm-clerical  Armed-Forces  Craft-repair  Exec-managerial
#<=50K 1652          3263             8          3170             2098
#>50K   191           507             1           929             1968

#               Farming-fishing  Handlers-cleaners  Machine-op-inspct
#<=50K              879               1284               1752
#>50K               115                 86                250

#             Other-service  Priv-house-serv  Prof-specialty  Protective-serv
#<=50K           3158              148            2281              438
#>50K             137                1            1859              211

#       Sales      Tech-support     Transport-moving
#<=50K   2667           645              1277
#>50K     983           283               320

proportions(table(datos$label,datos$occupation), margin = 2)

#           ?        Adm-clerical  Armed-Forces  Craft-repair
#<=50K 0.896364623   0.865517241   0.888888889   0.773359356
#>50K  0.103635377   0.134482759   0.111111111   0.226640644

#         Exec-managerial  Farming-fishing  Handlers-cleaners
#<=50K      0.515986227      0.884305835        0.937226277
#>50K       0.484013773      0.115694165        0.062773723

#          Machine-op-inspct  Other-service  Priv-house-serv  Prof-specialty
#<=50K        0.875124875    0.958421851      0.993288591     0.550966184
#>50K         0.124875125    0.041578149      0.006711409     0.449033816

#      Protective-serv       Sales     Tech-support  Transport-moving
#<=50K      0.674884438 0.730684932   0.695043103       0.799624296
#>50K       0.325115562 0.269315068   0.304956897       0.200375704

# Al observar los datos, nos percatamos que con respecto a su grupo los Gerentes o Gerentas (Exec-managerial)
# con un 48% sobre el nivel de ingresos planteado de estudio, le sigue especialistas profesionales. 

# Con respecto a las variables marital-status y relationship, dado a que atienden a un elemento eventualmente
# similar de categorizar vinculos, parece pertienente agrupar algunos labels internos (ej. pareja versus solteria)
# y posteriormente, solo utilizar una.

# Ingreso por Raza

table (datos$label,datos$race)  

#       Amer-Indian-Eskimo  Asian-Pac-Islander  Black  Other  White
#<=50K                 275                 763   2737    246  20699
#>50K                   36                 276    387     25   7117


proportions(table(datos$label,datos$race), margin = 2)

#     Amer-Indian-Eskimo     Asian-Pac-Islander     Black      Other
#<=50K          0.88424437          0.73435996 0.87612036 0.90774908
#>50K           0.11575563          0.26564004 0.12387964 0.09225092

#        White
#<=50K 0.74414006
#>50K  0.25585994

# Ahora, con respecto a la variable raza, se observa, presumiblemente por la composicion de la base de datos
# al profundizar con respecto al porcentaje de cada labels con respecto al nivel de ingreso
# se aprecia que los originarios de Asia-Pacifico, tienen un 27% de su raza por sobre alto nivel de ingreso.
# le sigue 'blancos' con 26%. 

#### Analisis Cruzado de Variables####

ingreso_carac <- datos %>% 
  group_by(label) %>%
  summarise(edad_prom= mean(age, na.rm=TRUE),
            educ_prom= mean(`education-num`, na.rm=TRUE),
            gain_prom=mean(`capital-gain`, na.rm=TRUE),
            loss_prom=mean(`capital-loss`, na.rm=TRUE))

#   label    edad_prom educ_prom gain_prom loss_prom
#    <fct>      <dbl>     <dbl>     <dbl>     <dbl>
#1 " <=50K"      36.8     9.60      149.      53.1
#2 " >50K"       44.2     11.6      4006.     195. 

# Hacer tabla 
opciones_forma <- c('striped', "bordered", 'hover', 'condensed', 'responsive')

knitr::kable(ingreso_carac, booktabs = TRUE, caption = 'Edad Promedio de Sufragio por Genero') %>% 
  kable_styling(bootstrap_options = opciones_forma, full_width = FALSE, font_size = 12)

# Como se observa, el promedio de edad y de tiempo educacional es mayor en quienes
# tienen un mayor nivel de ingresos, lo mismo que la ganancia y perdida de capitales.

datos %>% 
  group_by(label,sex) %>%
  summarise(edad_prom= mean(age, na.rm=TRUE),
            educ_prom= mean(`education-num`, na.rm=TRUE),
            gain_prom=mean(`capital-gain`, na.rm=TRUE),
            loss_prom=mean(`capital-loss`, na.rm=TRUE))


#    label     sex       edad_prom educ_prom gain_prom loss_prom
#     <fct>    <fct>        <dbl>     <dbl>     <dbl>     <dbl>
#1 " <=50K" " Female"      36.2      9.82      122.      47.4
#2 " <=50K" " Male"        37.1      9.45      166.      56.8
#3 " >50K"  " Female"      42.1      11.8      4200.     174. 
#4 " >50K"  " Male"        44.6      11.6      3972.     199. 

# Como se aprecia, al agregar la variable de sexo (hoy seria genero), se observa
# que a nivel del tramo de mejores ingresos hombres tienen mayor promedio de edad y perdida de capital,
# por su parte, mujeres, tienen mayor ganancia promedio de capital, asi como, un leve mayor promedio
# en el tiempo de formacion educacional. 

# Asi mismo, en el tramo de menor ingreso, mujeres tambien poseen un mayor promedio en an~os de educacion,
# mientras que hombres presentan mayor promedio en edad y ganancia y perdida de capital. 


datos %>% 
  group_by(label,race) %>%
  summarise(edad_prom= mean(age, na.rm=TRUE),
            educ_prom= mean(`education-num`, na.rm=TRUE),
            gain_prom=mean(`capital-gain`, na.rm=TRUE),
            loss_prom=mean(`capital-loss`, na.rm=TRUE),
            hours_prom=mean(`hours-per-week`, na.rm=TRUE))


#     label    race            edad_prom educ_prom gain_prom loss_prom hours_prom
#     <fct>    <fct>               <dbl>     <dbl>     <dbl>     <dbl>      <dbl>
#1  " <=50K" " Amer-Indian-…      36.8     9.06      207.       20.4       39.4
#2  " <=50K" " Asian-Pac-Is…      36.1     10.4      114.       58.6       38.4
#3  " <=50K" " Black"             36.9     9.26      113.       44.4       37.6
#4  " <=50K" " Other"             32.6     8.56      72.2       59.6       38.9
#5  " <=50K" " White"             36.8     9.63      155.       54.5       39.0
#6  " >50K"  " Amer-Indian-…      39.7     11.2      3821.      140.       44.9
#7  " >50K"  " Asian-Pac-Is…      42.4     12.4      5249.      204.       44.8
#8  " >50K"  " Black"             43.7     11.1      4121.      173.       44.4
#9  " >50K"  " Other"             41.4     11.6      9422.      75.5       44.9
#10 " >50K"  " White"             44.4     11.6      3934.      197.       45.6

# Cabe destacar que los originarios de Asia Pacifico, presentan el mayor promedio de tiempo 
# en formacion educacional en el mayor tramo de ingresos, superior, incluso al manifestado, al ver 
# tal metrica a nivel de genero. Como se observa en los datos contiguos, tanto para genero femenino,
# como masculino, los originarios de Asia Pacifico poseen mayor promedio de an~os educacionales. 
# Otro Elementos es que parece haber un cambio en la misma tendencia entre el promdedio de an~os
# de educacion y las horas promedio de trabajo. 

datos %>% 
  group_by(sex,race) %>%
  summarise(educ_prom= mean(`education-num`, na.rm=TRUE))

#      sex        race                educ_prom
#      <fct>     <fct>                     <dbl>
#1 " Female"  " Amer-Indian-Eskimo"      9.70
#2 " Female"  " Asian-Pac-Islander"      10.4 
#3 " Female"  " Black"                   9.55
#4 " Female"  " Other"                   8.90
#5 " Female"  " White"                   10.1 
#6 " Male"    " Amer-Indian-Eskimo"      9.07
#7 " Male"    " Asian-Pac-Islander"      11.2 
#8 " Male"    " Black"                   9.42
#9 " Male"    " Other"                   8.80
#10 " Male"   " White"                   10.1 



datos %>% 
  group_by(label,workclass) %>%
  summarise(edad_prom= mean(age, na.rm=TRUE),
            educ_prom= mean(`education-num`, na.rm=TRUE),
            gain_prom=mean(`capital-gain`, na.rm=TRUE),
            loss_prom=mean(`capital-loss`, na.rm=TRUE),
            hours_prom=mean(`hours-per-week`, na.rm=TRUE))


#      label    workclass           edad_prom educ_prom gain_prom loss_prom hours_prom
#     <fct>    <fct>                 <dbl>     <dbl>     <dbl>     <dbl>      <dbl>
#1 " <=50K" "Gov"                    39.6     10.6       160.     69.3      39.0
#2 " <=50K" " Never-worked"          20.6     7.43        0       0         28.4
#3 " <=50K" " Private"               35.1     9.45      137.      49.9       38.8
#4 " <=50K" " Self-emp-inc"          43.2     10.2      178.      54.4       47.1
#5 " <=50K" " Self-emp-not-inc"      44.4     9.70      221.      62.6       43.5
#6 " <=50K" " Unknown"               39.3     9.02      159.      48.0       31.4
#7 " <=50K" " Without-pay"           47.8     9.07      488.      0         32.7
#8 " >50K"  "Gov"                    44.9     12.2      2290.     177.       43.9
#9 " >50K"  " Private"               42.8     11.4      3575.     187.       45.5
#10 " >50K"  " Self-emp-inc"         48.2     11.9      8607.     235.       50.2
#11 " >50K"  " Self-emp-not-inc"     46.4     11.6      6065.     252.       46.7
#12 " >50K"  " Unknown"              55.6     11.3      4462.     171.       36.1


#### Limpieza y recodificacion de variables para modelo predictivo de arbol ####

# Dado a los elementos expuestos los pasos en esta etapa son:

## Modificar los signos de ? por el Unknown que la documentacion asume era el valor origina

datos$occupation = gsub("?", "Unknown", datos$occupation, fixed = T )
datos$occupation = as.factor(datos$occupation)


datos$workclass = gsub("?", "Unknown", datos$workclass, fixed = T )
datos$workclass = as.factor(datos$workclass )

table(datos$workclass)

datos$`native-country` = gsub("?", "Unknown", datos$`native-country`, fixed = T )
datos$`native-country` = as.factor(datos$`native-country` )


### se verifica el cambio

## Recodificaremos algunas levels de manera de poder mejorar la capacidad de clasificacion

### En clases el profe mostro la alternativa

#data_balanced_under$job_c <- ifelse(data_balanced_under$job %in% c("blue-collar", "entrepreneur", "housemaid", "services", "unknown"), 1,
#                                    ifelse(data_balanced_under$job %in% c("self-employed", "technician"), 2,
#                                    ifelse(data_balanced_under$job %in% c("admin."), 3,
#                                    ifelse(data_balanced_under$job %in% c("management", "unemployed"), 4,5))))


# Pero en ejemplos encontrados, nos parece pertinente usar (dado a que nos permite ir conociendo los script de manera continua):

### Recodificacion de variable `marital-status`

levels(datos$`marital-status`)

#[1] " Divorced"             
#[2] " Married-AF-spouse"    
#[3] " Married-civ-spouse"   
#[4] " Married-spouse-absent"
#[5] " Never-married"        
#[6] " Separated"            
#[7] " Widowed" 

levels(datos$`marital-status`)[c(2,3,4)] = 'Married'
levels(datos$`marital-status`)[c(1,3,4,5)] = 'No Married'

table(datos$`marital-status`)

# Se configuran dos levels de la variable de estatus marital, de manera de facilitar la interpretacion para la clasificacion

proportions(table(datos$label, datos$`marital-status`), margin=2)

#     No Married    Married
#<=50K 0.93554596 0.56307972
#>50K  0.06445404 0.43692028

# Se observa que casi el 40% de los 'casados' tienen un ingreso por sobre el tope de analisis 

proportions(table(datos$label, datos$`marital-status`), margin=1)

#     No Married   Married
#<=50K  0.6488269 0.3511731
#>50K   0.1409259 0.8590741

#De los que ganan por sobre >50K un casi 86% son casados



### Recodificacion de variable workclass

levels(datos$workclass)

#[1] " Federal-gov"      " Local-gov"        " Never-worked"    
#[4] " Private"          " Self-emp-inc"     " Self-emp-not-inc"
#[7] " State-gov"        " Unknown"          " Without-pay" 

levels(datos$workclass)[c(1,2,7)] = 'Gov'
levels(datos$workclass)[c(3:5)] = 'Private sector'

# Los "Never Worked" si bien manifiestan horas de empleo se dejan fuera del sector privado lo mismo que los desconocidos,
# y los sin pago. 


table(datos$workclass)

#Gov   Never-worked Private sector        Unknown    Without-pay 
#4351              7          26353           1836             14 

proportions(table(datos$label, datos$workclass), margin=2)

#         Gov      Never-worked Private sector  Unknown  Without-pay
#<=50K 0.6917950     1.0000000      0.7605965 0.8959695    1.0000000
#>50K  0.3082050     0.0000000      0.2394035 0.1040305    0.0000000

proportions(table(datos$label, datos$workclass), margin=1)

#       Gov         Never-worked    Private sector  Unknown  Without-pay
#<=50K 0.1217637540  0.0002831715   0.8108414239 0.0665453074 0.0005663430
#>50K  0.1710241041  0.0000000000   0.8046167581 0.0243591379 0.0000000000


# Como se observa si bien, los empleados fiscales o publicos, tienen una proporcion mas alta con respecto a su clase dentro de los mejores ingresos
# al mirar quienes componen el tramo de ingresos >50K , son principalmente personas que provienen de empleos del sector publico.


### Recodificacion de variable native country

# Si bien hasta el momento no se ha usado por su alto nivel de diversidad de observaciones, la documentacion sugiere utilizar esta variable por 
# sobre race, identificando dos opciones 

table(datos$`native-country`)


#Cambodia                   Canada                    China 
#19                         121                          75 
#Columbia                    Cuba          Dominican-Republic 
#59                          95                          70 
#Ecuador                 El-Salvador                     England 
#28                         106                          90 
#France                     Germany                      Greece 
#29                         137                          29 
#Guatemala                  Haiti              Holand-Netherlands 
#64                          44                           1 
#Honduras                    Hong                     Hungary 
#13                          20                          13 
#India                        Iran                     Ireland 
#100                          43                          24 
#Italy                     Jamaica                       Japan 
#73                          81                          62 
#Laos                      Mexico                   Nicaragua 
#18                         643                          34 
#Outlying-US(Guam-USVI-etc) Peru                 Philippines 
#14                          31                         198 
#Poland                    Portugal                 Puerto-Rico 
#60                          37                         114 
#Scotland                    South                      Taiwan 
#12                          80                          51 
#Thailand             Trinadad&Tobago               United-States 
#18                          19                       29170 
#Unknown                     Vietnam                 Yugoslavia 
#583                          67                          16 


levels(datos$`native-country`)[c(39)] = 'United-States'
levels(datos$`native-country`)[c(1:38,41:42)] = 'Non-U.S.'
levels(datos$`native-country`)


table(datos$`native-country`)

#Non-U.S.  United-States       Unknown 
#2808         29170           583 

proportions(table(datos$label, datos$`native-country`), margin=2)

#        Non-U.S. United-States Unknown
#<=50K 0.8133903     0.7541652 0.7495712
#>50K  0.1866097     0.2458348 0.2504288


proportions(table(datos$label, datos$`native-country`), margin=1)

#       Non-U.S.  United-States    Unknown
#<=50K 0.09239482    0.88992718 0.01767799
#>50K  0.06682821    0.91455172 0.01862007






#### Base para aplicar modelo predictivo de arbol ####

# Se elimina variable relationship, al estimar que recogemos informacion desde marital status, lo mismo pasa con native country y race.
# Si bien, en algun momento se considero su uso (por ello recodificacion), occupation, se descarta en este modelo, esperamos usarlo en el futuro. 
# Conjuntamente, education se descarta por education num, y fnlwgt. 

data_final <- datos%>% 
              select(-relationship, -race, -occupation, -education, -fnlwgt)

