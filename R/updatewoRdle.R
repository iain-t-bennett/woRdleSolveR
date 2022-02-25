
#' Title
#' One of word or guess must be provided
#'
#' @param x a woRdle object
#' @param word the word being guessed
#' @param guess a wordleGuess to apply
#'
#' @return a woRdle object
#' @export
#'
updatewoRdle <- function(x, word, guess){

  assertthat::assert_that(
    class(x) == "woRdle",
    msg = "x must be a woRdle"
  )

  # either guess or word must be provided

  assertthat::assert_that(
    sum(missing(word),missing(guess))==1,
    msg = "One and only one of word or guess must be provided"
  )

  if (!missing(word)){
    word <- toupper(word)

    assertthat::assert_that(
      nchar(word)==5,
      msg = "word must be 5 letters"
    )

    this.guess <- checkword(word = word, target_word = rc$target)
  }

  if (!missing(guess)){
    assertthat::assert_that(
      class(guess)=="woRdleGuess",
      msg = "guess must be a woRdleGuess"
    )

    this.guess <- guess

  }

  # could check is in wordlist?

  # increment guess count
  rc <- x

  rc$guess_count <- rc$guess_count + 1


  rc$guesses[[rc$guess_count]] <- this.guess

  rc$status <- updatewoRdleStatus(rc$status, this.guess)

  return(rc)

}

