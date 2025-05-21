# Autor: Oliwia Zielińska 268480
# Podczas rozwiązywania zadania i pisania testów skorzystała z:
# TESTTHAT: https://www.r-bloggers.com/2019/11/automated-testing-with-testthat-in-practice/
# LOGIKA: swoje rozwiązania listy nr 1 napisane w języku C#, dostępne pod adresem:
# https://github.com/OliwiaZielinska/JPdZB-L1-C-Test-Zielinska/tree/master

# Testy jednostkowe zostały napisane zgodnie z biblioteką testthat
library(testthat)

# Ładowanie funkcji z pliku Zadanie6.R
source("Zadanie6.R")

#' Testy funkcji komplement
#'
#' Sprawdza, czy funkcja komplement działa poprawnie na poprawnych danych wejściowych.
test_that("komplement - poprawne działanie", {
  expect_equal(komplement("ATCG"), "CGAT")
})

#' Sprawdza, czy funkcja komplement działa poprawnie dla jednej zasady.
test_that("komplement - pojedynczy nukleotyd", {
  expect_equal(komplement("A"), "T")
})

#' Sprawdza, czy funkcja komplement działa poprawnie dla długiej sekwencji.
test_that("komplement - długa sekwencja", {
  # Funkcja rep("ATCG", 1000) replikuje element "ATCG" 1000 razy.
  # https://www.statology.org/rep-function-in-r/
  # Funkcja paste(ATCG..., collapse="") skleja elementy w jeden ciąg znaków bez spacji
  # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/paste
  sekwencja <- paste(rep("ATCG", 1000), collapse = "")
  wynik <- komplement(sekwencja)
  expect_equal(nchar(wynik), 4000)
})

#' Sprawdza, czy funkcja komplement zgłasza błąd dla pustych danych wejściowych.
test_that("komplement - brak danych wejściowych", {
  expect_error(komplement(""), "Nie podano nici kodującej")
})

#' Sprawdza, czy funkcja komplement zgłasza błąd dla niepoprawnych danych wejściowych.
test_that("komplement - nieprawidłowe dane wejściowe", {
  expect_error(komplement("atcggu"), "Podana nić nie jest kodującą")
})

#' Testy funkcji transkrybuj
#'
#' Sprawdza, czy funkcja transkrybuj działa poprawnie dla poprawnych danych wejściowych.
test_that("transkrybuj - poprawne działanie", {
  expect_equal(transkrybuj("ATCG"), "CGAU")
})

#' Sprawdza, czy funkcja transkrybuj działa poprawnie dla jednej zasady.
test_that("transkrybuj - pojedynczy znak", {
  expect_equal(transkrybuj("T"), "A")
})

#' Sprawdza, czy funkcja transkrybuj zgłasza błąd dla pustej nici matrycowej.
test_that("transkrybuj - brak danych wejściowych", {
  expect_error(transkrybuj(""), "Nie podano nici matrycowej")
})

#' Sprawdza, czy funkcja transkrybuj zgłasza błąd dla niepoprawnych danych wejściowych.
test_that("transkrybuj - nieprawidłowe dane wejściowe", {
  expect_error(transkrybuj("aattccgguu"), "Podana nić nie jest matrycową")
})

#' Testy funkcji transluj
#'
#' Sprawdza, czy funkcja transluj działa poprawnie na poprawnym mRNA.
test_that("transluj - poprawne działanie", {
  expect_equal(transluj("AUGUUUUAA"), "Met-Phe")
})

#' Sprawdza, czy funkcja transluj działa poprawnie dla kodonu start i stop.
test_that("transluj - AUG i UAA", {
  expect_equal(transluj("AUGUAA"), "Met")
})

#' Sprawdza, czy funkcja transluj działa poprawnie, gdy użytkownik wpisze mRNA małymi literami.
test_that("transluj - małe litery", {
  expect_equal(transluj("auguuuuag"), "Met-Phe")
})

#' Sprawdza, czy funkcja transluj zgłasza błąd przy braku kodonu start.
test_that("transluj - brak kodonu start", {
  expect_error(transluj("UUUUAA"), "Nie znaleziono kompletnego białka")
})

#' Sprawdza, czy funkcja transluj zgłasza błąd przy braku kodonu stop.
test_that("transluj - brak kodonu stop", {
  expect_error(transluj("AUGUUU"), "Brak kodonu stop")
})

#' Sprawdza, czy funkcja transluj zgłasza błąd przy braku kodonu stop dla niepełnego kodonu.
test_that("transluj - niepełne kodony, brak stop", {
  expect_error(transluj("AUGUU"), "Brak kodonu stop")
})

#' Sprawdza poprawność działania całego łańcucha funkcji: komplement → transkrybuj → transluj.
test_that("gen - komplement + transkrybuj + transluj dla dłuższego wejścia", {
  dna <- "CATGGCCTTTAAGTAATTGGCCATGGCCTTTAAGTAA"
  wynik <- transluj(transkrybuj(komplement(dna)))
  expect_equal(wynik, "Met-Ala-Phe-Lys, Met-Ala-Phe-Lys")
})


