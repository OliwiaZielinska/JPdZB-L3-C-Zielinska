# Autor: Oliwia Zielińska 268480
# Źródła, z których korzystałam przy rozwiązywaniu zadań z listy:
# SKŁADNIA: https://cran.r-project.org/doc/contrib/wprowadzenie_do_R.pdf
# KOMENTARZE DOKUMENTACYJNE: https://roxygen2.r-lib.org/
# WYJĄTKI: https://stackoverflow.com/questions/1608130/equivalent-of-throw-in-r
# PĘTLA FOR: https://www.geeksforgeeks.org/for-loop-in-r/
# TWORZENIE WEKTORA-funkcja c(): https://www.wawrowski.edu.pl/ppr/02-struktury
# FUNKCJA toupper(): https://www.statology.org/r-toupper/
# KODONY: https://pl.wikipedia.org/wiki/Kodon
# TABELA Z BIAŁKAMI: https://zpe.gov.pl/a/przeczytaj/DvjLxZDbh
# LISTA: https://www.geeksforgeeks.org/r-lists/
# LOGIKA: swoje rozwiązania listy nr 1 napisane w języku C#, dostępne pod adresem:
# https://github.com/OliwiaZielinska/JPdZB-L1-C--Zielinska/tree/master

#' Funkcja zwraca nić matrycową DNA, będącą komplementarną, odwróconą nicią do
#' nici kodującej.
#' @param nic_kodujaca_podana Ciąg znaków reprezentujący nić kodującą DNA (np. "ATCG").
#' @return Ciąg znaków reprezentujący odwróconą nić matrycową DNA.
#' @examples
#' komplement("ATGCGT")
#' # [1] "ACGCAT"
komplement <- function(nic_kodujaca_podana) {
  # Funkcja toupper do zwiększenia liter
  nic_kodujaca <- toupper(nic_kodujaca_podana)
  zasady_DNA <- c("A", "T", "C", "G")
  # Funkcja nchar zliczająca ilość znaków w podanym argumencie
  # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/nchar
  if (nchar(nic_kodujaca) == 0) {
    stop("Nie podano nici kodującej!")
    # Funkcja strsplit() dzieli ciąg na pojedyncze znaki "ACT" -> "A", "C", "T"
    # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strsplit
    # Funkcja %in% określenie czy element znajduje się w dozwolonych zasadach
    # https://www.statology.org/in-operator-in-r/
  } else if (!all(strsplit(nic_kodujaca, "")[[1]] %in% zasady_DNA)) {
    stop("Podana nić nie jest kodującą nicią DNA!")
  } else {
    # Funkcja strsplit() dzieli ciąg na pojedyncze znaki "ACT" -> "A", "C", "T"
    # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strsplit
    zasady <- strsplit(nic_kodujaca, "")[[1]]
    # Funkcja sapply() pozwala na zastosowanie funkcji dla każdej zasady
    # https://www.statology.org/a-guide-to-apply-lapply-sapply-and-tapply-in-r/
    nic_matrycowa <- sapply(zasady, function(z) {
      # Switch https://www.geeksforgeeks.org/switch-case-in-r/
      switch(z,
             "A" = "T",
             "T" = "A",
             "C" = "G",
             "G" = "C")
    })
    # Funkcja rev() odwraca kolejność
    # https://www.geeksforgeeks.org/reverse-the-values-of-an-object-in-r-programming-rev-function/
    # Funkcja paste(odwrócona nić matrycowa, collapse="") skleja elementy w jeden ciąg znaków bez spacji
    # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/paste
    return(paste(rev(nic_matrycowa), collapse = ""))
  }
}

#' Funkcja przekształcająca daną nić matrycową DNA w odpowiadającą jej nić mRNA 
#' poprzez zamianę zasad azotowych.
#' @param nic_matrycowa_podana Ciąg znaków reprezentujący nić matrycową DNA.
#' @return Ciąg znaków reprezentujący transkrybowaną nić mRNA.
#' @examples
#' transkrybuj("TACGTA")
#' # [1] "UACGUA"

transkrybuj <- function(nic_matrycowa_podana) {
  # Funkcja toupper do zwiększenia liter
  nic_matrycowa <- toupper(nic_matrycowa_podana)
  zasady_DNA <- c("A", "T", "C", "G")
  # Funkcja nchar zliczająca ilość znaków w podanym argumencie
  # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/nchar
  if (nchar(nic_matrycowa) == 0) {
    stop("Nie podano nici matrycowej DNA!")
  } # Funkcja strsplit() dzieli ciąg na pojedyncze znaki "ACT" -> "A", "C", "T"
  # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strsplit
  # Funkcja %in% określenie czy element znajduje się w dozwolonych zasadach
  # https://www.statology.org/in-operator-in-r/
   else if (!all(strsplit(nic_matrycowa, "")[[1]] %in% zasady_DNA)) {
    stop("Podana nić nie jest matrycową nicią DNA!")
  } else {
    # Funkcja strsplit() dzieli ciąg na pojedyncze znaki "ACT" -> "A", "C", "T"
    # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strsplit
    zasady <- strsplit(nic_matrycowa, "")[[1]]
    # Funkcja sapply() pozwala na zastosowanie funkcji dla każdej zasady
    # https://www.statology.org/a-guide-to-apply-lapply-sapply-and-tapply-in-r/
    sekwencja_RNA <- sapply(zasady, function(z) {
      # Switch https://www.geeksforgeeks.org/switch-case-in-r/
      switch(z,
             "A" = "U",
             "T" = "A",
             "C" = "G",
             "G" = "C")
    })
    # Funkcja rev() odwraca kolejność
    # https://www.geeksforgeeks.org/reverse-the-values-of-an-object-in-r-programming-rev-function/
    # Funkcja paste(odwrócona sekwencja RNA, collapse="") skleja elementy w jeden ciąg znaków bez spacji
    # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/paste
    return(paste(rev(sekwencja_RNA), collapse = ""))
  }
}

#' Funkcja analizująca podaną sekwencję mRNA i tłumacząca ją na łańcuch 
#' aminokwasów (białko),rozpoczynając od kodonu start (AUG) i kończąc 
#' na pierwszym kodonie stop.
#' @param mRNA_podane Ciąg znaków reprezentujący sekwencję mRNA.
#' @return Ciąg znaków reprezentujący sekwencję aminokwasów (np. "Met-Val-Stop").
#' @examples
#' transluj("AUGGCCUAA")
#' # [1] "Met-Ala"
transluj <- function(mRNA_podane) {
  kodon_do_aminokwasu <- list(
    "AUG"="Met", "UUU"="Phe", "UUC"="Phe",
    "UUA"="Leu", "UUG"="Leu", "CUU"="Leu", "CUC"="Leu", "CUA"="Leu", "CUG"="Leu",
    "AUU"="Ile", "AUC"="Ile", "AUA"="Ile",
    "GUU"="Val", "GUC"="Val", "GUA"="Val", "GUG"="Val",
    "UCU"="Ser", "UCC"="Ser", "UCA"="Ser", "UCG"="Ser",
    "CCU"="Pro", "CCC"="Pro", "CCA"="Pro", "CCG"="Pro",
    "ACU"="Thr", "ACC"="Thr", "ACA"="Thr", "ACG"="Thr",
    "GCC"="Ala", "GCU"="Ala", "GCA"="Ala", "GCG"="Ala",
    "UAU"="Tyr", "UAC"="Tyr",
    "UAA"="Stop", "UAG"="Stop", "UGA"="Stop",
    "CAU"="His", "CAC"="His",
    "CAA"="Gln", "CAG"="Gln",
    "AAU"="Asn", "AAC"="Asn",
    "AAA"="Lys", "AAG"="Lys",
    "GAU"="Asp", "GAC"="Asp",
    "GAA"="Glu", "GAG"="Glu",
    "UGU"="Cys", "UGC"="Cys",
    "UGG"="Trp",
    "CGU"="Arg", "CGC"="Arg", "CGA"="Arg", "CGG"="Arg", "AGA"="Arg", "AGG"="Arg",
    "AGU"="Ser", "AGC"="Ser",
    "GGU"="Gly", "GGC"="Gly", "GGA"="Gly", "GGG"="Gly"
  )
  # Funkcja toupper do zwiększenia liter
  mRNA <- toupper(mRNA_podane)
  zasady_RNA <- c("A", "U", "C", "G")
  # Funkcja nchar zliczająca ilość znaków w podanym argumencie
  # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/nchar
  if (nchar(mRNA) < 3) {
    stop("Sekwencja mRNA jest za krótka!")
  } # Funkcja strsplit() dzieli ciąg na pojedyncze znaki "ACT" -> "A", "C", "T"
  # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strsplit
  # Funkcja %in% określenie czy element znajduje się w dozwolonych zasadach
  # https://www.statology.org/in-operator-in-r/
  else if (!all(strsplit(mRNA, "")[[1]] %in% zasady_RNA)) {
    stop("Podana nić nie jest sekwencją mRNA!")
  }
  # Funkcja c() tworzy pusty wektor
  bialka <- c()
  mRNA_length <- nchar(mRNA)
  # Pętla for po wszystkich możliwych pozycjach kodonów (dlatego długość mRNA-2) 
  for (przesuniecie in 1:(mRNA_length - 2)) {
    # Funkcja substr() zwraca fragment łańcucha mRNA od pozycji przesuniecie do przesuniecie+2
    # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/substr
    kodon_start <- substr(mRNA, przesuniecie, przesuniecie + 2)
    if (kodon_start == "AUG") {
      aktualne_bialko <- c("Met")
      tlumaczenie_rozpoczete <- TRUE
      i <- przesuniecie + 3
      while (i <= mRNA_length - 2) {
        kodon <- substr(mRNA, i, i + 2)
        if (!is.null(kodon_do_aminokwasu[[kodon]])) {
          if (kodon_do_aminokwasu[[kodon]] == "Stop") {
            # Funkcja paste(aktualne_bialko, collapse="") skleja elementy w jeden ciąg znaków bez spacji
            # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/paste
            bialka <- c(bialka, paste(aktualne_bialko, collapse = "-"))
            tlumaczenie_rozpoczete <- FALSE
            break
          } else {
            aktualne_bialko <- c(aktualne_bialko, kodon_do_aminokwasu[[kodon]])
          }
        }
        i <- i + 3
      }
      if (tlumaczenie_rozpoczete) {
        stop("Brak kodonu stop dla jednego z białek.")
      }
    }
  }
  if (length(bialka) == 0) {
    stop("Nie znaleziono kompletnego białka.")
  }
  # Funkcja paste(białka, collapse="") skleja elementy w jeden ciąg znaków bez spacji
  # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/paste
  return(paste(bialka, collapse = ", "))
}
