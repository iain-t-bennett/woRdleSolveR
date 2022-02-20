
#' Title
#'
#' @param x a woRdle object
#' @param word the word being guessed
#'
#' @return a woRdle object
#' @export
#'
updatewoRdle <- function(x, word){

  assertthat::assert_that(
    class(x) == "woRdle",
    msg = "x must be a woRdle"
  )

  word <- toupper(word)

  assertthat::assert_that(
    nchar(word)==5,
    msg = "word must be 5 letters"
  )

  # could check is in wordlist?

  # increment guess count
  rc <- x

  rc$guess_count <- rc$guess_count + 1
  this.guess <- checkword(word = word, target_word = rc$target)

  rc$guesses[[rc$guess_count]] <- this.guess

  rc$status <- updatewoRdleStatus(rc$status, this.guess)

  return(rc)

}

