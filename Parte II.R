library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

BD <- read_csv2("BaseSalarios.csv")[-6]
BD <- BD %>% rename("Salario" = "U. Salario", "Cuotas" = "Coutas" ) 

#Histograma de los salarios 
hist_salarios <- ggplot(BD, aes(x = Salario)) +
  geom_histogram(fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de Salarios", x = "Salario", y = "Frecuencia") + 
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal()
print(hist_salarios)

#Densidad de los salarios por Kernel
Kernels <- c("biweight", "gaussian", "epanechnikov", "cosine", "rectangular", 
             "triangular")

densidades <- lapply(Kernels, function(kernel) {
  density(BD$Salario, kernel = kernel, bw = "ucv")
})

plot(densidades[[1]], main = "Densidad de Salarios con Diferentes Kernels", 
     xlim = c(0, max(BD$Salario)), lwd = 2, ylab = "Densidad")

for (i in 2:length(Kernels)) {
  lines(densidades[[i]], col = i, lwd = 2)
}

legend("topright", legend = Kernels, col = seq(Kernels), lty = 1, lwd = 2)

