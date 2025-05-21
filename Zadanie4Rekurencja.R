# Autor: Oliwia Zielińska 268480
# Źródła, z których korzystałam przy rozwiązywaniu zadań z listy:
# SKŁADNIA: https://cran.r-project.org/doc/contrib/wprowadzenie_do_R.pdf
# KOMENTARZE DOKUMENTACYJNE: https://roxygen2.r-lib.org/
# WYJĄTKI: https://stackoverflow.com/questions/1608130/equivalent-of-throw-in-r
# PĘTLA FOR: https://www.geeksforgeeks.org/for-loop-in-r/
# CIĄG FIBONACCIEGO: https://pl.wikipedia.org/wiki/Ci%C4%85g_Fibonacciego
# TWORZENIE WEKTORA-funkcja c(): https://www.wawrowski.edu.pl/ppr/02-struktury
# REKURENCJA OGONOWA: https://pl.wikipedia.org/wiki/Rekurencja_ogonowa
# LOGIKA: swoje rozwiązania listy nr 1 napisane w języku C#, dostępne pod adresem:
# https://github.com/OliwiaZielinska/JPdZB-L1-C--Zielinska/tree/master

#' Funkcja zwracająca n pierwszych wyrazów ciągu Fibonacciego, bazując na
#' rekurencji ogonowej.
#' @param n liczba oznaczająca indeks ostatniego wyrazu, numerując je od zera
#' @return Wektor liczb Fibonacciego od 0 do n
#' @examples
#' ciag_fibonacciego_rek(5)  # [1] 0 1 1 2 3 5

ciag_fibonacciego_rek <- function(n) {
  # Sprawdzenie poprawności typów i wartości - NIE JEST TO KONIECZNE
  # język R to język dynamiczny, sam konwertuje dane, ale może to prowadzić
  # do pojawienia się błędów
  # https://www.statology.org/r-check-data-type/
  if (!is.numeric(n) || length(n) != 1 || n < 0) {
    stop("Podaj nieujemną liczbę całkowitą.")
  }
  # Zamiana liczb zmiennoprzecinkowych na całkowite np.: 3.0 na 3
  n <- as.integer(n)
  
  #' Funkcja budująca ciąg Fibonacciego rekurencyjnie z wykorzystaniem
  #' rekurencji ogonowej.
  #' @param i ile jeszcze elementów ciągu zostało do dodania, czyli licznik kroków
  #' @param poprzedni poprzedni element ciągu
  #' @param obecny obecny element ciągu
  #' @param ciag_obecny wektor przechowujący zbudowany do tej pory ciąg
  #' @return Wektor liczb Fibonacciego od 0 do n
  #' @examples
  #' fib_rekurencja(3, 0, 1, numeric())  # zwróci 0 1 1 2
  fib_rekurencja <- function(i, poprzedni, obecny, ciag_obecny) {
    ciag_obecny <- c(ciag_obecny, poprzedni)
    if (i == 0) {
      return(ciag_obecny)
    } else {
      return(fib_rekurencja(i - 1, obecny, poprzedni + obecny, ciag_obecny))
    }
  }
  
  return(fib_rekurencja(n, 0, 1, numeric()))
}
