# Autor: Oliwia Zielińska 268480
# Źródła, z których korzystałam przy rozwiązywaniu zadań z listy:
# SKŁADNIA: https://cran.r-project.org/doc/contrib/wprowadzenie_do_R.pdf
# KOMENTARZE DOKUMENTACYJNE: https://roxygen2.r-lib.org/
# WYJĄTKI: https://stackoverflow.com/questions/1608130/equivalent-of-throw-in-r
# PĘTLA FOR: https://www.geeksforgeeks.org/for-loop-in-r/
# CIĄG FIBONACCIEGO: https://pl.wikipedia.org/wiki/Ci%C4%85g_Fibonacciego
# TWORZENIE WEKTORA-funkcja c(): https://www.wawrowski.edu.pl/ppr/02-struktury
# LOGIKA: swoje rozwiązania listy nr 1 napisane w języku C#, dostępne pod adresem:
# https://github.com/OliwiaZielinska/JPdZB-L1-C--Zielinska/tree/master

#' Funkcja zwracająca listę n pierwszych elementów ciągu Fibonacciego, 
#' w sposób iteracyjny.
#' @param n liczba oznaczająca indeks ostatniego wyrazu, numeryjące je od zera
#' @return Wektor liczb ciągu Fibonacciego od 0 do n-tego wyrazu
#' @examples
#' ciag_fibonacciego(5)  # [1] 0 1 1 2 3 5

ciag_fibonacciego <- function(n) {
  # Sprawdzenie poprawności typów i wartości - NIE JEST TO KONIECZNE
  # język R to język dynamiczny, sam konwertuje dane, ale może to prowadzić
  # do pojawienia się błędów
  # https://www.statology.org/r-check-data-type/
  if (!is.numeric(n) || length(n) != 1 || n < 0) {
    stop("Podaj nieujemną liczbę całkowitą.")
  }
  # Zamiana liczb zmiennoprzecinkowych na całkowite np.: 3.0 na 3
  n <- as.integer(n)
  # Gdy n==0 od razu zwraca jeden element 0
  if (n == 0) return(c(0))
  # Gdy n==1 od razu zwraca dwa elementy: 0 i 1
  if (n == 1) return(c(0, 1))
  # Tworzy zerowy wektor liczbowy o długości n+1 (dla n=5, elementy: 0, 1, 1, 2, 3, 5).
  wynik <- numeric(n + 1)
  # Ustawienie pierwszego elementu 0 (numerujemy od 1 a nie od 0 w wektorach)
  wynik[1] <- 0
  # Ustawienie drugiego elementu na 1
  wynik[2] <- 1
  # Obliczenie kolejnych elementów ciągu od i=3 do i=n+1
  for (i in 3:(n + 1)) {
    wynik[i] <- wynik[i - 1] + wynik[i - 2]
  }
  wynik     # może też być return(wynik)
}