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

