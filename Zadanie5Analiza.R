# Autor: Oliwia Zielińska 268480
library(ggplot2)
source("Zadanie5.R")

# Zbierz dane
collatz_stats <- data.frame(
  c0 = 1:1000,
  length = sapply(1:1000, function(x) length(collatz(x)))
)

# Wykres długości ciągu
ggplot(collatz_stats, aes(x = c0, y = length)) +
  geom_line(color = "steelblue") +
  labs(title = "Długość ciągu Collatza dla c₀ ∈ [1, 1000]",
       x = "Wartość początkowa c₀",
       y = "Długość ciągu") +
  theme_minimal()

