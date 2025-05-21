# Autor: Oliwia Zielińska 268480
# Źródła, z których korzystałam przy rozwiązywaniu listy:
# HERON: https://pl.wikipedia.org/wiki/Wz%C3%B3r_Herona
# SKŁADNIA: https://cran.r-project.org/doc/contrib/wprowadzenie_do_R.pdf
# KOMENTARZE DOKUMENTACYJNE: https://roxygen2.r-lib.org/
# WYJĄTKI: https://stackoverflow.com/questions/1608130/equivalent-of-throw-in-r
# FUNKCJA sqrt: https://www.statology.org/square-root-function-in-r-sqrt/
# LOGIKA: swoje rozwiązania listy nr 1 napisane w języku C#, dostępne pod adresem:
# https://github.com/OliwiaZielinska/JPdZB-L1-C--Zielinska/tree/master

#' Funkcja obliczająca pole trójkąta na podstawie boków a, b, c za pomocą wzoru Herona.
#' @param a Długość pierwszego boku, która musi być liczbą dodatnią.
#' @param b Długość drugiego boku, która musi także być liczbą dodatnią.
#' @param c Długość trzeciego boku, która musi być dodatnia.
#' @return Pole trójkąta obliczone na podstawie wzoru Herona.
#' @examples
#' heron(3, 4, 5) # powinno zwrócić 6
#' heron(1, 2, 3) # błąd - Trójkąt o podanych bokach nie istnieje

heron <- function(a, b, c){
  # Sprawdzenie poprawności typów i wartości - NIE JEST TO KONIECZNE
  # język R to język dynamiczny, sam konwertuje dane, ale może to prowadzić
  # do pojawienia się błędów
  # https://www.statology.org/r-check-data-type/
  if (!is.numeric(a) || !is.numeric(b) || !is.numeric(c)) {
    stop("Wszystkie podane argumenty muszą być liczbami.")
  }
  # Rzucanie wyjątków if(warunek do spełnienia){stop (rzucanie wyjątku)}
  if (a <= 0 || b <= 0 || c <= 0) {
    stop("Wszystkie boki trójkąta muszą być większe od zera.")
  }
  # Sprawdzenie warunku trójkąta
  if (a + b <= c || a + c <= b || b + c <= a) {
    stop("Trójkąt o podanych bokach nie istnieje.")
  }
  # Obliczenie połowy obwodu trójkąta 
  obwod <- .5 * (a+b+c)
  # Obliczenie pola trójkąta za pomocą wzoru Herona
  pole <- sqrt(obwod*(obwod-a)*(obwod-b)*(obwod-c))
  return(pole)  # funkcja zwraca ostatnią linijkę kodu, więc mogłoby być pole
}

