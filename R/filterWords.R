#' Filters a vector of words to ones compatible with a
#'
#' @param x a woRdleStatus
#' @param words either a vector of words or internal list will be used
#'
#' @return
#' @export
#'
#'
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

  # must include are any tries that are still positive
  must_have <- names(which(colSums(x$status) * x$tried>0))

  # basis remove any that are not possible
  valid_L1 <- names(which(x$status[1,]==TRUE))
  valid_L2 <- names(which(x$status[2,]==TRUE))
  valid_L3 <- names(which(x$status[3,]==TRUE))
  valid_L4 <- names(which(x$status[4,]==TRUE))
  valid_L5 <- names(which(x$status[5,]==TRUE))

  df <- dplyr::tibble(Word = words) %>%
    dplyr::transmute(
      Word,
      L1 = substr(words, 1,1),
      L2 = substr(words, 2,2),
      L3 = substr(words, 3,3),
      L4 = substr(words, 4,4),
      L5 = substr(words, 5,5)
    ) %>%
    dplyr::filter(L1 %in% valid_L1,
                  L2 %in% valid_L2,
                  L3 %in% valid_L3,
                  L4 %in% valid_L4,
                  L5 %in% valid_L5
                  )

    rc <- df %>%
      dplyr::group_by(Word) %>%
      dplyr::filter(all(must_have %in% c(L1,L2,L3,L4,L5))) %>%
      dplyr::ungroup()

  return(rc$Word)

}
