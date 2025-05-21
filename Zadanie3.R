# Autor: Oliwia Zielińska 268480
# Źródła, z których korzystałam przy rozwiązywaniu zadań z listy:
# SKŁADNIA: https://cran.r-project.org/doc/contrib/wprowadzenie_do_R.pdf
# KOMENTARZE DOKUMENTACYJNE: https://roxygen2.r-lib.org/
# WYJĄTKI: https://stackoverflow.com/questions/1608130/equivalent-of-throw-in-r
# PĘTLA FOR: https://www.geeksforgeeks.org/for-loop-in-r/
# FUNKCJA combn(): https://www.geeksforgeeks.org/generate-all-combinations-of-xcm-in-r-programming-combn-function/
# TWORZENIE WEKTORA-funkcja c(): https://www.wawrowski.edu.pl/ppr/02-struktury
# LOGIKA: swoje rozwiązania listy nr 1 napisane w języku C#, dostępne pod adresem:
# https://github.com/OliwiaZielinska/JPdZB-L1-C--Zielinska/tree/master

#' Funkcja wypisująca wszystkie podzbiory z dwóch podanych zbiorów.
#' @param x Wektor dowolnych elementów, może zawierać liczby, znaki itd.
#' @return Lista zawierająca wszystkie możliwe podzbiory zbioru x.
#' @examples
#' podzbiory(c('a', 'b', 'c'))
#' podzbiory(1:3)

podzbiory <- function(x) {
  # Rzucanie wyjątków if(warunek do spełnienia){stop (rzucanie wyjątku)}
  if (is.null(x)) {
    stop("Zbiór nie może być NULL.")
  }
  # Obliczenie długości podanego zbioru
  n <- length(x)
  # Utworzenie pustej listy wyników
  wynik <- list()
  # Pętla od i=0 (pusty zbiór) do i=n (pełny zbiór)
  for (i in 0:n) {
    # Dodanie pierwszego pustego wektora do listy
    if (i == 0) {
      wynik <- c(wynik, list(c()))
    } else {
      # Funkcja combn() generuje wszystkie kombinacje (podzbiory) i-elementowe 
      # z x (podanego na wejściu zbioru), simplify = FALSE oznacza, że wynik
      # ma być listą, która zostanie dodana do wyniku c(wynik, combn())
      wynik <- c(wynik, combn(x, i, simplify = FALSE))
    }
  }
  wynik     # może być też return(wynik)
}