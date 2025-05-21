# Autor: Oliwia Zielińska 268480
# Podczas rozwiązywania zadania i pisania testów skorzystała z:
# TESTTHAT: https://www.r-bloggers.com/2019/11/automated-testing-with-testthat-in-practice/
# LOGIKA: swoje rozwiązania listy nr 1 napisane w języku C#, dostępne pod adresem:
# https://github.com/OliwiaZielinska/JPdZB-L1-C-Test-Zielinska/tree/master

# Testy jednostkowe zostały napisane zgodnie z biblioteką testthat
library(testthat)

# Ładowanie funkcji wspolne z pliku zadanie2.R
source("zadanie2.R")

#' Testy funkcji wspolne
#'
#' Sprawdza, czy funkcja wspolnie poprawnie zwraca elementy wspólne multizbiorów.
test_that("Poprawne działanie funkcji - wspólne elementy", {
  x <- c(1, 2, 2, 3, 4)
  y <- c(2, 2, 4, 5)
  wynik <- wspolne(x, y)
  expect_equal(wynik, c(2, 2, 4))
})

#' Sprawdza, czy funkcja wspolnie poprawnie działa, gdy nie ma elementów wspólnych.
test_that("Brak wspólnych elementów", {
  x <- c(1, 3, 5)
  y <- c(2, 4, 6)
  wynik <- wspolne(x, y)
  expect_equal(wynik, numeric(0))
})

#' Sprawdza, czy funkcja wspolnie poprawnie działa, gdy są takie same multizbiory.
test_that("Takie same multizbiory", {
  x <- c(1, 2, 2, 3)
  y <- c(1, 2, 2, 3)
  wynik <- wspolne(x, y)
  expect_equal(wynik, c(1, 2, 2, 3))
})

#' Sprawdza, czy funkcja wspolnie rzuca wyjątek, gdy jeden zbiór jest pusty.
test_that("Pusty multizbiór x powinien rzucać wyjątek", {
  x <- numeric(0)
  y <- c(1, 2, 3)
  expect_error(wspolne(x, y), "puste")
})

#' Sprawdza, czy funkcja wspolnie rzuca wyjątek, gdy drugi zbiór jest pusty.
test_that("Pusty multizbiór y powinien rzucać wyjątek", {
  x <- c(1, 2, 3)
  y <- numeric(0)
  expect_error(wspolne(x, y), "puste")
})

#' Sprawdza, czy funkcja wspolnie rzuca wyjątek, gdy oba zbiory są puste.
test_that("Dwa puste multizbiory powinny rzucać wyjątek", {
  x <- numeric(0)
  y <- numeric(0)
  expect_error(wspolne(x, y), "puste")
})

#' Sprawdza, czy funkcja wspolnie rzuca wyjątek, gdy zbiór x zawiera literę.
test_that("Multizbiór x zawierający literę powinien rzucić wyjątek", {
  x <- c(1, 2, "a")
  y <- c(1, 2, 3)
  expect_error(wspolne(x, y), "wektorami liczbowymi")
})

#' Sprawdza, czy funkcja wspolnie rzuca wyjątek, gdy zbiór y zawiera litery.
test_that("Multizbiór y zawierający litery powinien rzucić wyjątek", {
  x <- c(1, 2, 3)
  y <- c(1, "a", "b")
  expect_error(wspolne(x, y), "wektorami liczbowymi")
})