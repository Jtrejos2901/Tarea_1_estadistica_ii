library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(univariateML)
library(rriskDistributions) #encuentra que distribuciónn de probabilidades es la
#que ajusta mejor con una colección de datos.
library(fitdistrplus)
library(metRology)
library(boot)
library(ks)
library(plot3D)

BD <- read_csv2("BaseSalarios.csv")[-6]
BD <- BD %>% rename("Salario" = "U. Salario", "Cuotas" = "Coutas" )
BD <- BD[BD$Salario <= 10000000, ]

#------------------Parte II-----------------------------------------------------

#---Histrograma de los salarios--|

salarios <- BD$Salario


ggplot(BD, aes(x = Salario)) +
  geom_histogram(fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de Salarios", x = "Salario", y = "Frecuencia")


#---Densidad de los salarios por kernel---|

#Biweight
densidad_biweigth <- density(salarios, kernel = "biweight", bw = "ucv")
#Normal
densidad_normal <- density(salarios)
#Epanechnikov
densidad_epanechnikov <- density(salarios, kernel = "epanechnikov",  bw = "ucv")
#Coseno
densidad_coseno <- density(salarios, kernel = "cosine",  bw = "ucv")
#Rectangular
densidad_rectangular <- density(salarios, kernel = "rectangular",  bw = "ucv")
#Triangular
densidad_triangular <- density(salarios, kernel = "triangular",  bw = "ucv")


#------------------Parte III----------------------------------------------------

#-----Análisis AIC-----|

#Comparación densidades paramétricas por el criterio AIC

model_select(salarios, models = univariateML_models, criterion = "aic",
             na.rm = FALSE)


#La densidad que más se aproxima es la t-student

#Análisis con la función fit.cont

fit.cont(salarios)
# La densidad con menor AIC es Weibull

#----Elegir distribución---|

#Comparación gráfica entre la distribución weibull y la t-student

fw <- fitdist(salarios, "weibull")
summary(fw)
ft <- fitdist(salarios, "t.scaled", start=list(df=3,mean=mean(salarios),sd= sd(salarios)))
summary(ft)

par(mfrow = c(2,2))
leyenda <-c("Weibull", "T-Student")

denscomp(list(fw, ft), legendtext = leyenda)
qqcomp(list(fw, ft), legendtext = leyenda)
cdfcomp(list(fw, ft), legendtext = leyenda)
ppcomp(list(fw, ft), legendtext = leyenda)

#-------Intervalos de confianza bootstrap----|

densidad_salario <- mlweibull(salarios)
summary(densidad_salario)

#Los parámetros para la distribución Weibull 
 
k <- 1.888755
lambda <- 1217912

lambda*gamma(1+(1/k))

densidad2 <-mlgamma(salarios)
summary(densidad2)

alfa <-2.911098
beta <- 2.694071e-06 

alfa/beta

#intervalo confianza media
bootstrapml(densidad_salario, map = function(x) x[2]*gamma(1+(1/x[1])))

#intervalo confianza desviación estándar
bootstrapml(densidad_salario, map = function(x) sqrt(x[2]^2*(gamma(1+2/x[1])-(gamma(1+1/x[1]))^2)))

#-----------------------Parte IV------------------------------------------------

#La función kde estima la densidad del kernel para datos entre 1 a 6 dimensiones.

resultado_kde <- kde(salarios)
plot(resultado_kde, main = "Estimación de Densidad de Kernel", xlab = "Salario")

#La función boot.ci permite construir intervalos de confianza mediante el método
#bootstrap no paramétrico. El tipo de intervalo deseado depende del parámetro type,
#el cual puede ser norm, basic, perc, bca, all, stud.

#---Bootstrap de la media---|

set.seed(1)
resultado_boot <- boot(salarios, statistic=function(y,indices) mean(y[indices]),
                R=1000)

media_boot <- mean(resultado_boot$t)
media <- mean(resultado_boot$t0)

#Ambas medias son bastantes similares. Por ende,
#esto permite ver que la estimación de la media por bootstrap es un buen método
#para obtener una buena aproximación de la media de los salarios a partir de los 
#datos originales.


#------Histograma bootstrap----|
plot(resultado_boot)

