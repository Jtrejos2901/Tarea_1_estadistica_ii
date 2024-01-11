library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)

BD <- read_csv2("BaseSalarios.csv")[-6]
BD <- BD %>% rename("Salario" = "U. Salario", "Cuotas" = "Coutas" )


resumen<- BD %>% group_by(Sexo) %>% summarise_at(vars(Cuotas,Salario), 
                                                          list(Mínimo = min,
                                                          Media = mean,
                                                          Máximo = max,
                                                          Varianza = var))

#Gráfico boxplot
boxplot_salarios<- ggplot(BD, aes(x = factor(Sexo, labels = c("Hombre", "Mujer")), y = Salario, fill= factor(Sexo, labels = c("Hombre", "Mujer")))) + 
  geom_boxplot() + geom_boxplot() +
  labs(title = "Boxplot de los salarios por sexo",x = "Sexo", y = "Salario", 
       fill = "Sexo") + 
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal()

print(boxplot_salarios)


int_boxplot <- ggplotly(boxplot_salarios)
print(int_boxplot)

#Prueba t.test

salarios_hombres <-split(BD, BD$Sexo)[[1]][[5]]
salarios_mujeres <- split(BD, BD$Sexo)[[2]][[5]]

print(salarios_hombres)
print(salarios_mujeres)

t.test(salarios_hombres, salarios_mujeres)
