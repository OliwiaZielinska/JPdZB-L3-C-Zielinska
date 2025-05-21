# Autor: Oliwia Zielińska 268480
# Źródła, z których korzystałam przy rozwiązywaniu zadań z listy:
# SKŁADNIA: https://cran.r-project.org/doc/contrib/wprowadzenie_do_R.pdf
# KOMENTARZE DOKUMENTACYJNE: https://roxygen2.r-lib.org/
# GGPLOT2: https://ggplot2.tidyverse.org/
# WYKRES W GGPLOT2: https://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# RAMKA DANYCH: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/data.frame
# LOGIKA: swoje rozwiązania listy nr 1 napisane w języku C#, dostępne pod adresem:
# https://github.com/OliwiaZielinska/JPdZB-L1-C--Zielinska/tree/master

# Do wygenerowania wykresu wykorzystano bibliotekę ggplot2
library(ggplot2)

# Ładowanie funkcji collatz z pliku Zadanie5.R
source("Zadanie5.R")

# Wartość, dla której wygenerowany zostanie ciąg Collatza
c0 <- 6  # możesz zmienić na inną liczbę

# Wygenerowanie ciągu Collatza
ciąg <- collatz(c0)

# Utworzenie ramki danych krok = seq_along(ciąg) tworzy wektor: 1, 2, 3, ..., n,
# gdzie n to długość ciągu
df <- data.frame(Krok = seq_along(ciąg), Wartość = ciąg)

# Utworzenie wykresu za pomocą ggplot2
# aes (x=Krok, y=Wartość) - zadeklarowanie osi
ggplot(df, aes(x = Krok, y = Wartość)) +
  geom_line(color = "blue") +       # Linia ma mieć kolor niebieski
  geom_point(color = "darkred") +   # Punkt ma mieć kolor czerwony
  ggtitle(paste("Ciąg Collatza dla c0 =", c0)) +  # Tytuł wykresu
  xlab("Krok") +      # Etykieta osi X
  ylab("Wartość") +   # Etykieta osi Y
  theme_minimal()     # Minimalistyczny styl wykresu bez tła i ramki