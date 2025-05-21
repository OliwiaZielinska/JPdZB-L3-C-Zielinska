# Autor: Oliwia Zielińska 268480
# Podczas rozwiązywania zadania i pisania testów skorzystała z:
# TESTTHAT: https://www.r-bloggers.com/2019/11/automated-testing-with-testthat-in-practice/
# ILOŚĆ PODZBIORÓW: https://wojcienty.com/artykul/8/matematyka/wzor_na_liczebnosc_podzbiorow_zbioru/
# LOGIKA: swoje rozwiązania listy nr 1 napisane w języku C#, dostępne pod adresem:
# https://github.com/OliwiaZielinska/JPdZB-L1-C-Test-Zielinska/tree/master

# Testy jednostkowe zostały napisane zgodnie z biblioteką testthat
library(testthat)

# Ładowanie funkcji podzbiory z pliku Zadanie3.R
source("Zadanie3.R")

#' Testy funkcji podzbiory
#'
#' Sprawdza, czy funkcja podzbiory poprawnie zwraca wszystkie podzbiory zbioru z 1 elementem.
test_that("Poprawne działanie dla 1 elementu", {
  zbior <- c(1)
  wynik <- podzbiory(zbior)
  expect_equal(length(wynik), 2^length(zbior))
})

#' Sprawdza, czy funkcja podzbiory poprawnie zwraca wszystkie podzbiory zbioru liczbowego z 2 elementami.
test_that("Poprawne działanie dla 2 elementów", {
  zbior <- c(1, 2)
  wynik <- podzbiory(zbior)
  expect_equal(length(wynik), 2^length(zbior))
})

#' Sprawdza, czy funkcja podzbiory poprawnie zwraca wszystkie podzbiory zbioru liczbowego(1,2,3).
test_that("Poprawne działanie dla 3 elementów", {
  zbior <- c(1, 2, 3)
  wynik <- podzbiory(zbior)
  expect_equal(length(wynik), 2^length(zbior))
})

#' Sprawdza, czy funkcja podzbiory poprawnie zwraca wszystkie podzbiory zbioru z literami("a", "b", "c", "d") .
test_that("Poprawne działanie dla 4 elementów", {
  zbior <- c("a", "b", "c", "d")
  wynik <- podzbiory(zbior)
  expect_equal(length(wynik), 2^length(zbior))
})

#' Sprawdza, czy funkcja podzbiory poprawnie zwraca wszystkie podzbiory zbioru 5-cio elementowego.
test_that("Poprawne działanie dla 5 elementów", {
  zbior <- c("a", "b", "c", "d", "e")
  wynik <- podzbiory(zbior)
  expect_equal(length(wynik), 2^length(zbior))
})

#' Sprawdza, czy funkcja podzbiory poprawnie zwraca wszystkie podzbiory zbioru 10-cio elementowego.
test_that("Poprawne działanie dla 10 elementów", {
  zbior <- c(1, "a", 2, "b", 3, "c", 8, "d", 10, "e")
  wynik <- podzbiory(zbior)
  expect_equal(length(wynik), 2^length(zbior))
})

#' Sprawdza, czy funkcja podzbiory rzuca wyjątek, gdy podany zbiór jest NULL
test_that("Pusty zbiór (NULL) rzuca wyjątek", {
  expect_error(podzbiory(NULL), "Zbiór nie może być NULL")
})
