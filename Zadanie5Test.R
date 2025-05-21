# Autor: Oliwia Zielińska 268480
# Podczas rozwiązywania zadania i pisania testów skorzystała z:
# TESTTHAT: https://www.r-bloggers.com/2019/11/automated-testing-with-testthat-in-practice/
# LOGIKA: swoje rozwiązania listy nr 1 napisane w języku C#, dostępne pod adresem:
# https://github.com/OliwiaZielinska/JPdZB-L1-C-Test-Zielinska/tree/master

# Testy jednostkowe zostały napisane zgodnie z biblioteką testthat
library(testthat)

# Ładowanie funkcji collatz z pliku Zadanie5.R
source("Zadanie5.R")

#' Testy funkcji collatz
#'
#' Sprawdza, czy funkcja poprawnie zwraca 6 elementów przed wpadnięciem w cykl (4,2,1).
test_that("Poprawne działanie dla 6", {
  oczekiwane <- c(11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1)
  expect_equal(collatz(11), oczekiwane)
})

#' Sprawdza, czy funkcja poprawnie zwraca 11 elementów przed wpadnięciem w cykl (4,2,1).
test_that("Poprawne działanie dla 11", {
  oczekiwane <- c(6, 3, 10, 5, 16, 8, 4, 2, 1)
  expect_equal(collatz(6), oczekiwane)
})

#' Sprawdza, czy funkcja poprawnie zwraca 1 element przed wpadnięciem w cykl (4,2,1).
test_that("Poprawne działanie dla 1", {
  oczekiwane <- c(1)
  expect_equal(collatz(1), oczekiwane)
})

#' Sprawdza, czy funkcja rzuca wyjątek, gdy c0 jest liczbą <0.
test_that("Rzuca wyjątek dla wartości ujemnych", {
  expect_error(collatz(-5), "c0 musi być liczbą naturalną")
})

#' Sprawdza, czy funkcja rzuca wyjątek, gdy c0 jest równe 0.
test_that("Rzuca wyjątek dla zera", {
  expect_error(collatz(0), "c0 musi być liczbą naturalną")
})

#' Sprawdza, czy funkcja rzuca wyjątek, gdy c0 nie jest liczbą całkowitą.
test_that("Rzuca wyjątek dla liczb niecałkowitych", {
  expect_error(collatz(1.5), "c0 musi być liczbą naturalną")
})