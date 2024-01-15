#------------------Parte II-----------------------------------------------------

#---Densidad de los salarios por kernel---|

salarios <- BD$Salario

#Biweight
densidad_biweigth <- density(salarios, kernel = "biweight")
#Normal
densidad_normal <- density(salarios)
#Epanechnikov
densidad_epanechnikov <- density(salarios, kernel = "epanechnikov")
#Coseno
densidad_coseno <- density(salarios, kernel = "cosine")
#Rectangular
densidad_rectangular <- density(salarios, kernel = "rectangular")
#Triangular
densidad_triangular <- density(salarios, kernel = "triangular")

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

set.seed(123)  # Establece semilla para reproducibilidad


# Definir el modelo paramétrico para obtener media y desviación estándar
modelo_weibull <- function(BD, indices) {
  muestra_bootstrap <- BD$Salario[indices]
  
  # Ajusta la distribución Weibull a la muestra bootstrap usando fitdist
  ajuste_bootstrap <- fitdist(muestra_bootstrap, "weibull")
  
  # Obtiene los parámetros del ajuste
  parametros <- coef(ajuste_bootstrap)
  
  # Calcula media y desviación estándar de la muestra bootstrap
  media_bootstrap <- parametros[2] * gamma(1 + 1/parametros[1])
  desviacion_bootstrap <- sqrt(parametros[2]^2 * (gamma(1 + 2/parametros[1]) - (gamma(1 + 1/parametros[1]))^2))
  
  return(c(media_bootstrap, desviacion_bootstrap))
}

# Número de repeticiones bootstrap
n_repeticiones <- 1000

# Realiza el bootstrap paramétrico para media y desviación estándar
resultados_bootstrap_parametrico <- boot(BD, modelo_weibull, R = n_repeticiones)

# Calcula intervalo de confianza para la media y la desviación estándar (por ejemplo, al 95%)
intervalo_confianza_media <- boot.ci(resultados_bootstrap_parametrico, type = "perc", index = 1)$percent
intervalo_confianza_desviacion <- boot.ci(resultados_bootstrap_parametrico, type = "perc", index = 2)$percent

cat("Intervalo de confianza para la media:", intervalo_confianza_media, "\n")
cat("Intervalo de confianza para la desviación estándar:", intervalo_confianza_desviacion, "\n")

