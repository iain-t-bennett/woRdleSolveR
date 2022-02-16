

#' Updates a woRdleStatus with a woRdleGuess
#'
#' @param x a woRdleStatus
#' @param guess a woRdleGuess
#'
#' @return a woRdleStatus
#' @export
#' @examples
#' game_0 <- woRdleStatus()
#' guess_1 <- woRdleGuess("APPLE", "YGG00")
#' game_1 <- updatewoRdleStatus(game_0, guess_1)
updatewoRdleStatus <- function(x, guess){

  assertthat::assert_that(
    class(x) == "woRdleStatus",
    msg = "x must be a woRdleStatus"
  )

  assertthat::assert_that(
    class(guess) == "woRdleGuess",
    msg = "guess must be a woRdleGuess"
  )

  rc <- x

  for (i in 1:5){
    this.letter <- guess$word[i]
    this.resp <-  guess$response[i]

    this.letterNum <- as.numeric(charToRaw(this.letter)) - 64

    if (this.resp == "G"){
      rc[i,c(1:26)[-this.letterNum]] <- FALSE
    }
    if (this.resp == "Y"){
      rc[i,this.letterNum] <- FALSE
    }
    if (this.resp == "0"){
      rc[1:5,this.letterNum] <- FALSE
    }
  }

  return(rc)
}
