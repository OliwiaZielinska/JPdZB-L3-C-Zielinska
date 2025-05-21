# Autor: Oliwia Zielińska 268480
# Źródła, z których korzystałam przy rozwiązywaniu zadań z listy:
# SKŁADNIA: https://cran.r-project.org/doc/contrib/wprowadzenie_do_R.pdf
# KOMENTARZE DOKUMENTACYJNE: https://roxygen2.r-lib.org/
# WYJĄTKI: https://stackoverflow.com/questions/1608130/equivalent-of-throw-in-r
# PĘTLA WHILE: https://www.w3schools.com/r/r_while_loop.asp
# PROBLEM COLLATZA: https://pl.wikipedia.org/wiki/Problem_Collatza
# FUNKCJA floor(): https://datascienceparichay.com/article/r-floor-function/
# MODULO: https://www.datamentor.io/r-programming/operator
# TWORZENIE WEKTORA-funkcja c(): https://www.wawrowski.edu.pl/ppr/02-struktury
# LOGIKA: swoje rozwiązania listy nr 1 napisane w języku C#, dostępne pod adresem:
# https://github.com/OliwiaZielinska/JPdZB-L1-C--Zielinska/tree/master

#' Funkcja generująca ciąg Collatza dla danej liczby naturalnej c0.
#' @param c0 liczba naturalna większa od zera
#' @return wektor liczb tworzących ciąg Collatza zaczynając od c0.
#' @examples
#' collatz(6)  # [1] 6 3 10 5 16 8 4 2 1

collatz <- function(c0) {
  # Sprawdzenie poprawności typów i wartości - NIE JEST TO KONIECZNE
  # język R to język dynamiczny, sam konwertuje dane, ale może to prowadzić
  # do pojawienia się błędów
  # https://www.statology.org/r-check-data-type/
  # floor(c0) sprawdza, czy liczba c0 jest liczbą całkowitą
  if (!is.numeric(c0) || length(c0) != 1 || c0 <= 0 || c0 != floor(c0)) {
    stop("c0 musi być liczbą naturalną większą od zera.")
  }
  wynik <- c(c0)
  # Pętla while(), dopóki c0 będzie różne od 1 (cykl 4,2,1)
  while (c0 != 1) {
    # Gdy c0 jest parzyste, reszta z dzielenia przez 2 (modulo) jest równe 0
    if (c0 %% 2 == 0) {
      c0 <- c0 / 2
    } else {
      # c0 nieparzyste
      c0 <- 3 * c0 + 1
    }
    wynik <- c(wynik, c0)
  }
  # Zwracamy cały wektor wynik, zrzutowany na wartości int as.integer().
  return(as.integer(wynik))
}

