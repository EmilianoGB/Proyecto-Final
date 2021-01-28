########################################
##EXPLORACION Y ANALISIS DESCRIPTIVO
########################################
rm(list = ls())
#LIBRERIAS
library(smbinning)
library(DT)
library(DataExplorer)
library(skimr)
library(tidyverse)
library(ggpubr)
# library(univariateML)
# library(GGally)
library(purrr)

base.dir <- "C:/Users/emili/Desktop/ORT Analitica/Proyecto"

data.dir <- file.path(base.dir, "datos")
report.dir <- file.path(base.dir, "reportes")
graf.dir <- file.path(base.dir, "graficos")
models.dir <- file.path(base.dir, "modelos")
route <- paste (data.dir,"/", sep = "")

setwd(data.dir)

#Importacion del dataset final
datos<- readRDS(file.path(route,"datos_finales.rds"))
summary(datos)

###########################################################

###########################################################
#Análisis exploratorio
###########################################################
#Tabla Introductoria
skim(datos) 

#Analisis de datos faltantes (analisis de exclusion)
plot_missing(
  data    = datos, 
  title   = "Porcentaje de valores ausentes",
  ggtheme = theme_bw(),
  theme_config = list(legend.position = "none")
)

#VARIABLES CUANTITATIVAS

#ANALISIS UNIVARIADO
# Variable de respuesta Peso Promotores
p1 <- ggplot(data = datos, aes(x = peso_promotores)) +
  geom_density(fill = "steelblue", alpha = 0.8) +
  #geom_rug(alpha = 0.1) +
  #scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribución Peso Promotores") +
  theme_bw()

# Variable de respuesta Peso Promotores con Trnasformación Logarítmica
p1.1 <- ggplot(data = datos, aes(x = log(peso_promotores))) +
  geom_density(fill = "steelblue", alpha = 0.8) +
  #geom_rug(alpha = 0.1) +
  #scale_x_continuous(labels = scales::comma) +
  labs(title = "Transf. Logaritmica Peso Promotores") +
  theme_bw() 

# Variable de respuesta Peso Detractores

p2 <- ggplot(data = datos, aes(x = peso_detractores)) +
  geom_density(fill = "steelblue", alpha = 0.8) +
  #geom_rug(alpha = 0.1) +
  #scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribución Peso Detractores") +
  theme_bw()

# Variable de respuesta Peso Detractores con transformación logarítmica

p2.2 <- ggplot(data = datos, aes(x = log(peso_detractores)+0.1)) +
  geom_density(fill = "steelblue", alpha = 0.8) +
  #geom_rug(alpha = 0.1) +
  #scale_x_continuous(labels = scales::comma) +
  labs(title = "Transf. Logarítmica Peso Detractores") +
  theme_bw()

#Distribución univariada de Variables Continuas candidatas a predictores

plot_density(
  data    = datos %>% dplyr::select(-c(1,28:29)),
  ncol    = 3,
  nrow    = 3,
  title   = "Distribución variables continuas",
  ggtheme = theme_bw(),
  theme_config = list(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(colour = "black", size = 12, face = 2)
  )
)

#ANÁLISIS BIVARIADO
#Relacion de las variables continuas contra el Peso Promotores
#Funcion para generar los graficos
custom_corr_plot <- function(variable1, variable2, df, alpha=0.3){
  p <- df %>%
    mutate(
      #  título estilo facet
      title = paste(toupper(variable2), "vs", toupper(variable1))
    ) %>%
    ggplot(aes(x = !!sym(variable1), y = !!sym(variable2))) + 
    geom_point(alpha = alpha) +
    # Tendencia no lineal
    #geom_smooth(se = FALSE, method = "gam", formula =  y ~ splines::bs(x, 3)) +
    # Tendencia lineal
    geom_smooth(se = FALSE, method = "lm", color = "firebrick") +
    facet_grid(. ~ title) +
    theme_bw() +
    theme(strip.text = element_text(colour = "black", size = 10, face = 2),
          axis.title = element_blank())
  return(p)
}

#Predictores
predictores <- datos %>% select_if(is.numeric) %>% dplyr::select(-c(1,18:19)) %>% names()

PlotsContinuaPromotores <- map(
                          .x = predictores,
                          .f = custom_corr_plot,
                          variable2 = "peso_promotores",
                          df = datos#[-c(615,6406),]
)

ggarrange(plotlist = PlotsContinuaPromotores, ncol = 3, nrow = 6) %>%
  annotate_figure(
    top = text_grob("Correlación con Peso Promotor", face = "bold", size = 16,
                    x = 0.16)
  )

PlotsContinuaDetractores <- map(
  .x = predictores,
  .f = custom_corr_plot,
  variable2 = "peso_detractores",
  df = datos[datos$peso_detractores > 0,]
)

ggarrange(plotlist = PlotsContinuaDetractores, ncol = 3, nrow = 6) %>%
  annotate_figure(
    top = text_grob("Correlación con Peso Detractor", face = "bold", size = 16,
                    x = 0.16)
  )
#Correlacion de a pares entre variables continuas
plot_correlation(
  data = datos[-1],
  cor_args = list("use" = "pairwise.complete.obs"),
  type = "continuous",
  title = "Matriz de correlación variables continuas",
  theme_config = list(legend.position = "none",
                      plot.title = element_text(size = 12, face = "bold"),
                      axis.title = element_blank(),
                      axis.text.x = element_text(angle = -45, hjust = +0.1)
  )
)

# VARIABLES CUALITATIVAS

#Análsis univariado
plot_bar(
  datos,
  ncol    = 3,
  nrow = 4,
  title   = "Número de observaciones por grupo",
  ggtheme = theme_bw(),
  theme_config = list(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(colour = "black", size = 12, face = 2),
    legend.position = "none"
  )
)


#Análisis Bivariado
#funcion para graficos
custom_box_plot <- function(variable1, variable2, df, alpha=0.3){
  p <- df %>%
    mutate(
      # Truco para que se ponga el título estilo facet
      title = paste(toupper(variable2), "vs", toupper(variable1))
    ) %>%
    ggplot(aes(x = !!sym(variable1), y = !!sym(variable2))) + 
    #geom_violin(alpha = alpha) +
    geom_boxplot() +
    facet_grid(. ~ title) +
    theme_bw() +
    theme(strip.text = element_text(colour = "black", size = 10, face = 2),
          axis.title = element_blank())
  return(p)
}

predictorescuali <- datos %>% select_if(is.factor) %>% names()

plots <- map(
  .x = predictorescuali,
  .f = custom_box_plot,
  variable2 = "peso_promotores",
  df = datos#[-c(615,6406),]
)

ggarrange(plotlist = plots, ncol = 3, nrow = 4) %>%
  annotate_figure(
    top = text_grob("Correlación con Peso promotor", face = "bold", size = 16,
                    x = 0.16)
  )

plotsdet <- map(
  .x = predictorescuali,
  .f = custom_box_plot,
  variable2 = "peso_detractores",
  df = datos[datos$peso_detractores > 0,]
)

ggarrange(plotlist = plotsdet, ncol = 3, nrow = 4) %>%
  annotate_figure(
    top = text_grob("Correlación con Peso detractor", face = "bold", size = 16,
                    x = 0.16)
  )

#A partir del análisis exploraorio se generan 2 datasets indpendientes para la etapa de modelado según el tipo de partner.
DatosLogistic <- datos %>% filter(`Restaurant Is Logistic Market Place`== "Logistic")
DatosMarketplace <- datos %>% filter(`Restaurant Is Logistic Market Place`== "Marketplace")
DatosMarketplace <- DatosMarketplace %>% select(-c(14:16))

saveRDS(DatosLogistic,file="datos_ModeladoL.rds")
saveRDS(DatosMarketplace,file="datos_ModeladoM.rds")
