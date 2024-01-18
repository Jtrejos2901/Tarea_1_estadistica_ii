library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

BD <- read_csv2("BaseSalarios.csv")[-6]
BD <- BD %>% rename("Salario" = "U. Salario", "Cuotas" = "Coutas" ) 

BD <- BD[BD$Salario <= 10000000, ]

#Punto 1
#Histograma de los salarios 
hist_salarios <- ggplot(BD, aes(x = Salario)) +
  geom_histogram(fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de Salarios", x = "Salario", y = "Frecuencia") + 
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal()
print(hist_salarios)

#Punto 2
#Densidad de los salarios por Kernel
Kernels <- c("biweight", "gaussian", "epanechnikov", "cosine", "rectangular", 
             "triangular")

densidades <- lapply(Kernels, function(kernel) {
  density(BD$Salario, kernel = kernel, bw = "ucv")
})

crear_grafico_kernel <- function(densidad, titulo, col) {
  ggplot(data.frame(x = densidad$x, y = densidad$y), aes(x = x, y = y)) +
    geom_line(color = col, size = 1) +
    labs(title = titulo, y = "Densidad", x = "Salario") +
    scale_x_continuous(labels = scales::comma_format()) +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_minimal()
}

# Límites de los ejes y cuadrícula para todos los gráficos
xlim <- c(0, max(BD$Salario))
ylim <- c(0, max(sapply(densidades, function(d) max(d$y))))

# Crear gráficos llamando a la función
biweight <- crear_grafico_kernel(densidades[[1]], 
                                 "Densidad de Salarios con Kernel Biweight", 
                                 "blue")
print(biweight)
gaussian <- crear_grafico_kernel(densidades[[2]], 
                                 "Densidad de Salarios con Kernel Gaussiano", 
                                 "red")
print(gaussian)
epanechnikov <- crear_grafico_kernel(densidades[[3]], 
                                     "Densidad de Salarios con Kernel Epanechnikov", 
                                     "green")
print(epanechnikov)
cosine <- crear_grafico_kernel(densidades[[4]], 
                               "Densidad de Salarios con Kernel Coseno", 
                               "purple")
print(cosine)
rectangular <- crear_grafico_kernel(densidades[[5]], 
                                    "Densidad de Salarios con Kernel Rectangular", 
                                    "orange")
print(rectangular)
triangular <- crear_grafico_kernel(densidades[[6]], 
                                   "Densidad de Salarios con Kernel Triangular", 
                                   "turquoise")
print(triangular)

#Punto 3
#Resultados 
hist_salarios_densidad <- ggplot(BD, aes(x = Salario, y = after_stat(density))) +
  geom_histogram(fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Histograma y Densidades de Salarios", x = "Salario", y = "Densidad") + 
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal()

juntos <- hist_salarios_densidad +
  geom_line(data = data.frame(x = densidades[[1]]$x, y = densidades[[1]]$y, color = Kernels[1]), 
            aes(x = x, y = y, color = Kernels[1]), size = 1) +
  geom_line(data = data.frame(x = densidades[[2]]$x, y = densidades[[2]]$y, color = Kernels[2]), 
            aes(x = x, y = y, color = Kernels[2]), size = 1) +
  geom_line(data = data.frame(x = densidades[[3]]$x, y = densidades[[3]]$y, color = Kernels[3]), 
            aes(x = x, y = y, color = Kernels[3]), size = 1) +
  geom_line(data = data.frame(x = densidades[[4]]$x, y = densidades[[4]]$y, color = Kernels[4]), 
            aes(x = x, y = y, color = Kernels[4]), size = 1) +
  geom_line(data = data.frame(x = densidades[[5]]$x, y = densidades[[5]]$y, color = Kernels[5]), 
            aes(x = x, y = y, color = Kernels[5]), size = 1) +
  geom_line(data = data.frame(x = densidades[[6]]$x, y = densidades[[6]]$y, color = Kernels[6]), 
            aes(x = x, y = y, color = Kernels[6]), size = 1) +
  scale_color_manual(values = c("blue", "red", "green", "purple", "orange", 
                                "turquoise"), labels = Kernels) +
  labs(color = "Kernel")

print(juntos)



