#' woRdleGuess
#'
#' Response defines how good the guess was
#' G is green, Y is yellow, 0 is grey
#' e.g. G0000 means Green for postion 1, grey for the rest
#'
#' @param word 5 letter word
#' @param response is the colour update (5 char string with each char G,Y,0)
#
#' @return
#' @export
#'
#' @examples
#' x <- woRdleGuess("APPLE", "YGG00")
#' print(x)
woRdleGuess <- function(word, response){
  # check form
  assertthat::assert_that(
    nchar(word)==5,
    msg = "word must be 5 letters"
    )

  assertthat::assert_that(
    nchar(response)==5,
    msg = "response must be 5 letters"
  )

  word <- toupper(word)
  response <- toupper(response)

 rc <- new_woRdleGuess(word, response)
 return(rc)
}

# woRdleGuess

new_woRdleGuess <- function(word, response){

  wordarray <- array(data = "1", dim = 5)
  resparray <- array(data = "1", dim = 5)

  for( i in 1:5){
    wordarray[i] <- substr(word,i,i)
    resparray[i] <- substr(response,i,i)
  }

  rc <- list(word = wordarray, response = resparray)

  class(rc) <- "woRdleGuess"

  return(rc)
}

#' print a guess
#'
#' @param x a guess object
#' @param ... unused
#'
#' @return
#' @export
#'
print.woRdleGuess <- function(x,...){

  rg <- which(x$response == "G")
  ry <- which(x$response == "Y")
  r0 <- which(x$response == "0")

  rc <- paste0("Word guessed was ", paste0(x$word, collapse = ""))

  if (length(r0) == 5){
    rc <- paste(rc, "No letters correct", collapse = "\n")
  } else{
    if (length(rg)>0){
      txtg <- paste0("Correct letters are ",paste0(x$word[rg], collapse = ","),
                     " in correct positions ", paste0(rg, collapse = ","))
      rc <- paste(rc, txtg, collapse = "\n")
    }
    if (length(ry)>0){
      txty <- paste0("Correct letters are ",paste0(x$word[ry], collapse = ","),
                     " in wrong positions ", paste0(ry, collapse = ","))
      rc <- paste(rc, txty, collapse = "\n")
    }
  }

  cat(rc)

}
