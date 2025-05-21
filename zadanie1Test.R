# Autor: Oliwia Zielińska 268480
# Podczas rozwiązywania zadania i pisania testów skorzystała z:
# TESTTHAT: https://www.r-bloggers.com/2019/11/automated-testing-with-testthat-in-practice/
# LOGIKA: swoje rozwiązania listy nr 1 napisane w języku C#, dostępne pod adresem:
# https://github.com/OliwiaZielinska/JPdZB-L1-C-Test-Zielinska/tree/master

# Testy jednostkowe zostały napisane zgodnie z biblioteką testthat
library(testthat)

# Ładowanie funkcji heron z pliku zadanie1.R
source("zadanie1.R")

#' Testy funkcji heron
#'
#' Sprawdza, czy funkcja heron poprawnie liczy pole trójkąta o bokach: 3, 4, 5.
test_that("Poprawne pole dla trójkąta 3, 4, 5", {
  expect_equal(heron(3, 4, 5), 6)
})

#' Sprawdza, czy funkcja heron poprawnie liczy pole trójkąta o bokach: 7, 5, 10.
test_that("Poprawne pole dla trójkąta 7, 5, 10", {
  # ZAOKRĄGLANIE LICZB: https://www.statology.org/round-in-r/
  expect_equal(round(heron(7, 5, 10), 3), 16.248)
})

#' Sprawdza, czy funkcja heron rzuca wyjątek, gdy bok a jest liczbą ujemną.
test_that("Ujemny bok a powinien rzucać wyjątek", {
  expect_error(heron(-3, 4, 2), "większe od zera")
})

#' Sprawdza, czy funkcja heron rzuca wyjątek, gdy bok b jest liczbą ujemną.
test_that("Ujemny bok b powinien rzucać wyjątek", {
  expect_error(heron(3, -4, 2), "większe od zera")
})

#' Sprawdza, czy funkcja heron rzuca wyjątek, gdy bok c jest liczbą ujemną.
test_that("Ujemny bok c powinien rzucać wyjątek", {
  expect_error(heron(3, 4, -2), "większe od zera")
})

#' Sprawdza, czy funkcja heron rzuca wyjątek, gdy jeden z boków ma długość 0.
test_that("Bok o długości 0 powinien rzucać wyjątek", {
  expect_error(heron(3, 4, 0), "większe od zera")
})

#' Sprawdza, czy funkcja heron rzuca wyjątek, gdy nie spełnione jest warunek trójkąta.
test_that("Nierówność trójkąta powinna rzucać wyjątek", {
  expect_error(heron(1, 1, 2), "nie istnieje")
})