
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
