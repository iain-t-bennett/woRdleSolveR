#' Filters a vector of words to ones compatible with a
#'
#' @param x a woRdleStatus
#' @param words either a vector of words or internal list will be used
#'
#' @return
#' @export
#'
#' @examples
filterWords <- function(x, words = NULL){

  if (is.null(words)){
    wordsdf <- read.csv(system.file(file = "extdata/wordlist.csv", package = "woRdleSolveR"))
    words <- wordsdf$words
  }

  assertthat::assert_that(
    class(x) == "woRdleStatus",
    msg = "x must be a woRdleStatus"
  )

  assertthat::assert_that(
    class(words) == "character",
    msg = "words must be a vector of characters"
  )

x
  df <- dplyr::tibble(Word = words) %>%
    dplyr::transmute(
      Word,
      L1 = substr(words, 1,1),
      L2 = substr(words, 2,2),
      L3 = substr(words, 3,3),
      L4 = substr(words, 4,4),
      L5 = substr(words, 5,5)
    )

  # pass 1 remove any
  invalid_L1 <- which(x[1,]==FALSE, na.)

}
