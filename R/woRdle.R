#' Class to represent a wordle game
#'
#' @param target_word the game hidden word (defualt will be random)
#'
#' @return a woRdle
#' @export
#'
#'
woRdle <- function(target_word = NULL){

  if (is.null(target_word)){
    wordsdf <- read.csv(system.file(file = "extdata/wordlist.csv", package = "woRdleSolveR"))
    words <- wordsdf$words

    target_word <- words[round(stats::runif(1, min = 1, max = length(words)))]
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


#' Plotting function for woRdle S3 object
#'
#' @param x a woRdle object
#' @param ... unused. compliance with S3
#'
#' @return a plot
#' @export
#'
plot.woRdle <- function(x, ...){

  assertthat::assert_that(
    class(x) == "woRdle",
    msg = "x must be a woRdle"
  )

  # as built of classes can just plot components
  p.stat <- plot(x$status)
  p.guess <- lapply(x$guesses, plot)

  # then combine the plots
  if (x$guess_count == 0){
    rc <- p.stat
  } else  {
    rc <- p.guess[[1]]
    if (x$guess_count > 1 ){
      for (i in 2:x$guess_count){
        rc <- gridExtra::grid.arrange(rc, p.guess[[i]])
      }
    }
    rc <- gridExtra::grid.arrange(rc, p.stat)
  }

  return(rc)
}


