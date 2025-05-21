# Autor: Oliwia Zielińska 268480
# Podczas rozwiązywania zadania i pisania testów skorzystała z:
# TESTTHAT: https://www.r-bloggers.com/2019/11/automated-testing-with-testthat-in-practice/
# LOGIKA: swoje rozwiązania listy nr 1 napisane w języku C#, dostępne pod adresem:
# https://github.com/OliwiaZielinska/JPdZB-L1-C-Test-Zielinska/tree/master

# Testy jednostkowe zostały napisane zgodnie z biblioteką testthat
library(testthat)

# Ładowanie funkcji ciag_fibonacciego z pliku Zadanie4.R
source("Zadanie4.R")
# Ładowanie funkcji ciag_fibonacciego_rek z pliku Zadanie4Rekurencja.R
source("Zadanie4Rekurencja.R")

# --- A. ITERACYJNY SPOSÓB ---
#' Testy funkcji ciag_fibonacciego
#'
#' Sprawdza, czy funkcja poprawnie zwraca 10 pierwszych elementów ciągu.
test_that("ciag_fibonacciego: Poprawne działanie dla 10", {
  oczekiwane <- c(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
  expect_equal(ciag_fibonacciego(10), oczekiwane)
})

#' Sprawdza, czy funkcja poprawnie zwraca 0 pierwszych elementów ciągu.
test_that("ciag_fibonacciego: Dla 0", {
  expect_equal(ciag_fibonacciego(0), c(0))
})

#' Sprawdza, czy funkcja poprawnie zwraca elementy ciągu dla n=1.
test_that("ciag_fibonacciego: Dla 1", {
  expect_equal(ciag_fibonacciego(1), c(0, 1))
})

#' Sprawdza, czy funkcja wyrzuci błąd, gdy n < 0.
test_that("ciag_fibonacciego: Wyjątek dla wartości ujemnej", {
  expect_error(ciag_fibonacciego(-1), "nieujemną")
})

#' Sprawdza, czy funkcja wyrzuci błąd, gdy n = "a".
test_that("ciag_fibonacciego: Wyjątek dla wartości nie będącej liczbą", {
  expect_error(ciag_fibonacciego("a"), "liczbę całkowitą")
})

#' Sprawdza, czy funkcja poprawnie zwraca 5.0 pierwszych elementów ciągu.
test_that("ciag_fibonacciego: Poprawne działanie dla 5.0", {
  oczekiwane <- c(0, 1, 1, 2, 3, 5)
  expect_equal(ciag_fibonacciego(5.0), oczekiwane)
})

#' Sprawdza, czy funkcja poprawnie zamieni 5.7 na 5 i zwróci 5 pierwszych elementów ciągu.
test_that("ciag_fibonacciego: Poprawne działanie dla 5.7", {
  oczekiwane <- c(0, 1, 1, 2, 3, 5)
  expect_equal(ciag_fibonacciego(5.7), oczekiwane)
})
# --- B. REKURENCYJNY SPOSÓB ---
#' Testy funkcji ciag_fibonacciego_rek
#'
#' Sprawdza, czy funkcja poprawnie zwraca 10 pierwszych elementów ciągu.
test_that("ciag_fibonacciego_rek: Poprawne działanie dla 10", {
  oczekiwane <- c(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
  expect_equal(ciag_fibonacciego_rek(10), oczekiwane)
})

#' Sprawdza, czy funkcja poprawnie zwraca 0 pierwszych elementów ciągu.
test_that("ciag_fibonacciego_rek: Dla 0", {
  expect_equal(ciag_fibonacciego_rek(0), c(0))
})

#' Sprawdza, czy funkcja poprawnie zwraca elementy ciągu dla n=1.
test_that("ciag_fibonacciego_rek: Dla 1", {
  expect_equal(ciag_fibonacciego_rek(1), c(0, 1))
})

#' Sprawdza, czy funkcja wyrzuci błąd, gdy n < 0.
test_that("ciag_fibonacciego_rek: Wyjątek dla wartości ujemnej", {
  expect_error(ciag_fibonacciego_rek(-5), "nieujemną")
})

#' Sprawdza, czy funkcja wyrzuci błąd, gdy n = "a".
test_that("ciag_fibonacciego_rek: Wyjątek dla wartości nie będącej liczbą", {
  expect_error(ciag_fibonacciego_rek("r"), "liczbę całkowitą")
})

#' Sprawdza, czy funkcja poprawnie zwraca 3.0 pierwszych elementów ciągu.
test_that("ciag_fibonacciego_rek: Poprawne działanie dla 3.0", {
  oczekiwane <- c(0, 1, 1, 2)
  expect_equal(ciag_fibonacciego(3.0), oczekiwane)
})

#' Sprawdza, czy funkcja poprawnie zamieni 3.5 na 3 i zwróci 3 pierwsze elementów ciągu.
test_that("ciag_fibonacciego_rek: Poprawne działanie dla 3.5", {
  oczekiwane <- c(0, 1, 1, 2)
  expect_equal(ciag_fibonacciego(3.5), oczekiwane)
})