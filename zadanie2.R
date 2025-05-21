# Autor: Oliwia Zielińska 268480
# Źródła, z których korzystałam przy rozwiązywaniu zadań z listy:
# SKŁADNIA: https://cran.r-project.org/doc/contrib/wprowadzenie_do_R.pdf
# KOMENTARZE DOKUMENTACYJNE: https://roxygen2.r-lib.org/
# WYJĄTKI: https://stackoverflow.com/questions/1608130/equivalent-of-throw-in-r
# PĘTLA FOR: https://www.geeksforgeeks.org/for-loop-in-r/
# FUNKCJA match(): https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/match
# FUNKCJA is.na(): https://www.statology.org/is-na/
# TWORZENIE WEKTORA-funkcja c(): https://www.wawrowski.edu.pl/ppr/02-struktury
# LOGIKA: swoje rozwiązania listy nr 1 napisane w języku C#, dostępne pod adresem:
# https://github.com/OliwiaZielinska/JPdZB-L1-C--Zielinska/tree/master

#' Funkcja obliczająca część wspólną dwóch multizbiorów.
#' @param x wektor liczbowy reprezentujący pierwszy multizbiór
#' @param y wektor liczbowy reprezentujący drugi multizbiór
#' @return Wektor zawierający wspólne elementy obu multizbiorów
#' @examples
#' wspolne(c(1,2,2,3), c(2,2,4))
#' # wynik: c(2,2)

wspolne <- function(x, y) {
  # Sprawdzenie poprawności typów i wartości - NIE JEST TO KONIECZNE
  # język R to język dynamiczny, sam konwertuje dane, ale może to prowadzić
  # do pojawienia się błędów
  # https://www.statology.org/r-check-data-type/
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Oba argumenty muszą być wektorami liczbowymi.")
  }
  # Rzucanie wyjątków if(warunek do spełnienia){stop (rzucanie wyjątku)}
  if (length(x) == 0 || length(y) == 0) {
    stop("Multizbiory nie mogą być puste.")
  }
  # Tworzy pusty wektor numeryczny, do którego trafią elementy wspólne
  wynik <- numeric(0)
  # Kopia zbioru y, aby nie modyfikować orginalnego, podanego zbioru
  y_kopia <- y
  
  for (element in x) {
    # Szuka pierwszego indeksu pasującego elementu w y_kopia,gdy nie ma zwraca NA.
    # NA oznacza brak wartości (ang. Not Available)
    indeks <- match(element, y_kopia)
    # funkcja is.na() https://www.statology.org/is-na/
    if (!is.na(indeks)) {   # Sprawdzenie, czy są jakieś elementy wspólne, różne od NA
      wynik <- c(wynik, element)  # Funkcja c() umożliwia tworzenie wektoru - tutaj dodajemy element do wyniku
      y_kopia <- y_kopia[-indeks] # Usuwamy ten konkretny element z kopii y, aby unikąć powielenia
    }
  }
  wynik     # Lub mogłoby być return(wynik)
}

