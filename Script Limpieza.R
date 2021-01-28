(rm(list=ls()))
#LIBRERIAS
library(ggplot2)
library(plyr)
library(readxl)
library(fastDummies)
library(dplyr)
library(lubridate)
library(forcats)

base.dir <- "C:/Users/emili/Desktop/ORT Analitica/Proyecto"

data.dir <- file.path(base.dir, "datos")
report.dir <- file.path(base.dir, "reportes")
graf.dir <- file.path(base.dir, "graficos")
models.dir <- file.path(base.dir, "modelos")

setwd(data.dir)
###################
##LIMPIEZA DE DATOS
###################
route <- paste (data.dir,"/", sep = "")
######
#DATA
######
#Tablas con 0 ó 1 linea por partner:
nps <- read_xlsx(paste0(route, "nps.xlsx"))
delivery <- read_xlsx(paste0(route, "PartnersOnline_FirstDateOnline_DeliveryType_ORT.xlsx"))
productos <- read_xlsx(paste0(route, "QtyProducts_Pics_BusinessType_ORT.xlsx"))
reviews <- read_xlsx(paste0(route, "Reviews_ORT.xlsx"))
city <- read_xlsx(paste0(route, "City_ORT.xlsx"))
chats <- read_xlsx(paste0(route, "Chats_ORT.xlsx"))
caract <- read_xlsx(paste0(route, "Dispatch_Rating_Discount_Shipping_ORT.xlsx"))
logistics <- read_xlsx(paste0(route, "Logistics_ORT.xlsx"))
names(logistics)[1]<- 'Restaurant Restaurant ID'


partners_total <-join_all(list(nps,delivery,productos,reviews,city,chats,caract,logistics), by='Restaurant Restaurant ID', type='left')
# Verificación
dim(partners_total)

#Importacion de tabla food: unica con más de una fila por partner (no una por mes)
food <- read_xlsx(paste0(route,"FoodCategory_ORT.xlsx"))
summary(food)
food <- food %>% na.exclude()
food$`Restaurant Category Food Category Name`<- as.factor(food$`Restaurant Category Food Category Name`)
summary(food)
food <- food[order(decreasing = TRUE, food$`Restaurant Restaurant ID`, food$`Restaurant Category Food Category Percentage`),]
food <- food[!duplicated(food$`Restaurant Restaurant ID`),]
# Se genera una columna categoria segun criterio de tipo de Restaurante
tablacateg <- read_excel("tablacateg.xlsx")
food <- inner_join(food, tablacateg)
food$`Categoria agrupada` <- as.factor(food$`Categoria agrupada`)
table(food$`Restaurant Category Food Category Name`, food$`Categoria agrupada`)
food <- food %>% select(1,4)

#Tablas con 1 linea por partner por mes:
ordenes <- read_xlsx(paste0(route, "Ordenes_Rechazos_Confirmadas_ORT.xlsx"))
triggers <- read_xlsx(paste0(route, "Triggers_CVR_Sessions_ORT.xlsx"))

#Ordenes Agrupada por RestauranteID
ordenes <- ordenes %>% group_by(`Restaurant Restaurant ID`) %>% 
  summarise(Confirmadas = sum(`Restaurant Order Confirmed Orders`), Rechazadas = sum(`Restaurant Order Rejected Orders`), OrdTotales = sum(`Restaurant Order Total Orders`), OrdenesConVoucher = sum(`Restaurant Order Total Orders with voucher discount`))

dim(ordenes)

#Triggers Agrupados 
triggers <- triggers %>% group_by(`Restaurant Restaurant ID`) %>% 
  summarise("Restaurant Total Triggers" = mean(`Restaurant Triggers Total Triggers`), "restaurant Visits Sessions Profile by Partner" = sum(`restaurant Visits Sessions Profile by Partner`)) 

dim(triggers)

#Tablas mensuales de ordenes con voucher

Abril_Voucher <- read_xlsx(paste0(route,"Abril_Voucher_ORT.xlsx"))
Mayo_Voucher <- read_xlsx(paste0(route,"Mayo_Voucher_ORT.xlsx"))
Junio_Voucher <- read_xlsx(paste0(route,"Junio_Voucher_ORT.xlsx"))
Julio_Voucher <- read_xlsx(paste0(route,"Julio_Voucher_ORT.xlsx"))
Agosto_Voucher <- read_xlsx(paste0(route,"Agosto_Voucher_ORT.xlsx"))
Setiembre_Voucher <- read_xlsx(paste0(route,"Setiembre_Voucher_ORT.xlsx"))
#Octubre_Voucher <- read_xlsx(paste0(route,"Octubre_Voucher_ORT.xlsx"))
names (Abril_Voucher)[2] = "Voucher.Disc.Abril"
names (Agosto_Voucher)[2] = "Voucher.Disc.Agosto"
names (Julio_Voucher)[2] = "Voucher.Disc.Julio"
names (Junio_Voucher)[2] = "Voucher.Disc.Junio"
names (Mayo_Voucher)[2] = "Voucher.Disc.Mayo"
#names (Octubre_Voucher)[2] = "Voucher.Disc.Octubre"
names (Setiembre_Voucher)[2] = "Voucher.Disc.Setiembre"

#Union de tablas mensuales

vouchers_total <-join_all(list(Abril_Voucher, Mayo_Voucher, Junio_Voucher, Julio_Voucher, Agosto_Voucher, Setiembre_Voucher), by='Restaurant Restaurant ID', type='left')
dim(vouchers_total)
summary(vouchers_total)

#Creacion de campos maximo, minimo y dispersion de ordenes, Total de Vouchers en el Período

vouchers_total$max_voucher <- apply(vouchers_total[,2:7], 1, max)
vouchers_total$min_voucher <- apply(vouchers_total[,2:7], 1, min)
vouchers_total <- vouchers_total %>% mutate(dispersion_voucher = max_voucher-min_voucher)
vouchers_total$TotalVouchers <- rowSums(vouchers_total[, 2:7])

ordenes_extra <-select(vouchers_total,'Restaurant Restaurant ID','max_voucher', 'min_voucher', 'dispersion_voucher')

#Union final

db <-join_all(list(partners_total, food, ordenes, triggers, ordenes_extra), by='Restaurant Restaurant ID', type='left')
dim(db)

#Adecuacion del tipo de variables
j <- sapply(db, is.character)
db[j] <- lapply(db[j], as.factor)
db$`Restaurant First Date Online Date`<- as.Date(db$`Restaurant First Date Online Date`)
summary(db)

#Eliminar filas sin valor NPS
db <- db[!is.na(db$NPS),]
dim(db)

#Sustitucion de NA
#Chats
db$`Chats Total Chats`[is.na(db$`Chats Total Chats`)] <- 0
db$`Chats Total Chats Transfered`[is.na(db$`Chats Total Chats Transfered`)] <- 0

#Creacion de nuevas variables
##Proporcion de ventas con voucher
db <- db %>% mutate(Voucher_rate = ((OrdenesConVoucher/OrdTotales)))
##Proporcion de triggers respecto a las ventas
names (db)[33] = "total_triggers"
db <- db %>% mutate(peso_triggers = (((total_triggers)/OrdTotales)))
##Fail rate
db <- db %>% mutate(fail_rate = ((Rechazadas/OrdTotales)))
## Proporcion de Chats
db <- db %>% mutate(chats_rate = ((`Chats Total Chats`/OrdTotales)))
db <- db %>% mutate(chatstransf_rate = ((`Chats Total Chats Transfered`/OrdTotales)))
## Proporcion de reviews
db <- db %>% mutate(reviews_rate = ((`Orders Reviews Total Reviews`/OrdTotales)))
## Ratio de Conversión
db <- db %>% mutate(conversion_rate = ((OrdTotales/`restaurant Visits Sessions Profile by Partner`)))
##Total encuestas
db <- db %>% mutate(encuestas = Promoters+Passives+Dectractors)
##% promotores
db <- db %>% mutate(peso_promotores = (Promoters/encuestas)*100)
db$peso_promotores <- round(db$peso_promotores,2)
##% detractores
db <- db %>% mutate(peso_detractores = (Dectractors/encuestas)*100)
db$peso_detractores <- round(db$peso_detractores,2)
## Max vouchers sobre el total de ordenes
db$max_voucher = db$max_voucher/db$OrdTotales
## Min vouchers sobre el total de ordenes
db$min_voucher = db$min_voucher/db$OrdTotales
## Dispercion vouchers sobre el total de ordenes
db$dispersion_voucher = db$dispersion_voucher/db$OrdTotales

#Eliminacion de 199 observaciones con NA en todas las variables yes/no de tabla caract
completeFun <- function(db, desiredCols) {
  completeVec <- complete.cases(db[, desiredCols])
  return(db[completeVec, ])
}

db<-completeFun(db,c(16,18:24))

#Elimiacion de variables que no seran contempladas (Fecha de chats)
db<- db[,-13]
summary(db)

#Criterios funcionales para el univero de la base da datos. Se utilizan variables que no son activables pr parte de los equipos de PY.
#Universo de los datos
#Al menos 20 respuestas a NPS
db<- db %>% filter(encuestas>19)
#Nos centramos en Resaurantes 
db <- db %>% filter(`Restaurant Business Name`== "Restaurant")
#Con más de 500 ordenes
db <- db %>% filter(OrdTotales>500)

summary(db)

#Mutamos la variable fecha para obener la cantidad de dias desde la primera vez online (Antiguedad del partner)

db$Antiguedad <- ymd(20191001)-db$`Restaurant First Date Online Date`

#generamos una variables segun la cantidad de restauranes que hay por ciudad
## Cantidad de Rstaurantes por ciudad
ciudades <- db %>% group_by(`Restaurant City - Name`) %>% summarise(cantidad=dplyr::n()) %>% arrange(desc(`cantidad`)) 

summary(ciudades)
quantile(ciudades$cantidad, probs = c( 66, 97)/100)
## Graficar el precio ordenado 
ciudades %>%
  mutate(`Restaurant City - Name` = fct_reorder(`Restaurant City - Name`, cantidad, .desc = TRUE)) %>%
  ggplot(aes(x = `Restaurant City - Name`, y = cantidad)) +
  geom_bar(stat = 'identity', aes(color = `Restaurant City - Name`), show.legend = FALSE) + coord_flip() +
  geom_hline(yintercept = 41, color='coral', size=1)  +
  geom_hline(yintercept = 230, color='coral', size=1) 

## Generar las categorías generadas sobre el precio promedio de m2 por barrio
ciudades <- ciudades %>%
  mutate(Atencion_Ciudad = case_when(cantidad > 230 ~ 'Alta',
                                     cantidad > 41 ~ 'Media',
                                     TRUE ~ 'Baja'))

db <- inner_join(db, ciudades[-2])
db$Atencion_Ciudad <- as.factor(db$Atencion_Ciudad)
db$Antiguedad <- as.numeric(db$Antiguedad)
summary(db)

#Las conversiones mayores al 100% se pasan a 100%
db$conversion_rate <- ifelse(db$conversion_rate > 1, 1, db$conversion_rate)
summary(db$conversion_rate)

#Eliminamos las variables que no vamos a utilizar en el Análisis
#Todas las relativas a órdenes en niveles. Los valores de encuestas en niveles.
db <- db %>% select(-c(2:6, 8, 11:14, 28:33, 44))
summary(db)

##################################
setwd(route)
saveRDS(db,file="datos_finales.rds")

