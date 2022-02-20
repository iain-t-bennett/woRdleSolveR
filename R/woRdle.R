#' Class to represent a wordle game
#'
#' @param target_word
#'
#' @return a woRdle
#' @export
#'
#'
woRdle <- function(target_word = NULL){

  if (is.null(target_word)){
    wordsdf <- read.csv(system.file(file = "extdata/wordlist.csv", package = "woRdleSolveR"))
    words <- wordsdf$words

    target_word <- words[round(runif(1, min = 1, max = length(words)))]
  }

  assertthat::assert_that(
    nchar(target_word)==5,
    msg = "word must be 5 letters"
  )

  target_word <- toupper(target_word)

  rc <- new_woRdle(target_word = target_word)

  return(rc)
}

new_woRdle <- function(target_word){

  rc <- list(target = target_word,
             status = woRdleStatus(),
             guess_count = 0,
             guesses = list())

  class(rc) <- "woRdle"
  return(rc)
}

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


# internal function that checks a word against a target

checkword <- function(word, target_word){
  assertthat::assert_that(
    nchar(word)==5,
    msg = "word must be 5 letters"
  )
  assertthat::assert_that(
    nchar(word)==5,
    msg = "target word must be 5 letters"
  )

  resp <- ""

  for (i in 1:5){
    this.letter <- substr(word, i,i)
    this.target <- substr(target_word, i,i)
    # default to not
    this.resp <- "0"
    # is letter in word?
    if(grepl(this.letter, target_word, fixed = TRUE )) {
      this.resp <- "Y"
    }
    # is letter in correct place?
    if(this.letter == this.target){
      this.resp <- "G"
    }
    resp <- paste0(resp, this.resp)
  }

  rc <- woRdleGuess(word = word, response = resp)

  return(rc)
}



