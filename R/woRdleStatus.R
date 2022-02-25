# init
#
# @return woRdleStatus
#
#
new_woRdleStatus <- function(){
  # define an object that can hold state of current knowledge
  rc1 <- matrix(nrow = 5, ncol = 26, data = TRUE)
  rc2 <- rep(FALSE,26)
  # label columns as letters
  names(rc2) <- colnames(rc1) <- LETTERS

  # label rows as letter position
  rownames(rc1) <- paste0("Position",1:5)

  # can now represent game with this matrix
  # status = TRUE is right letter right place
  # status = FALSE means letter not in that place
  # letter not in word implies Position 1-5 all FALSE
  # letter in wrong place implies only that position is False

  rc <- list(status = rc1,
             tried = rc2)

  class(rc) <- "woRdleStatus"

  return(rc)
}

# init
#
# @return woRdleStatus
# @export
#
woRdleStatus <- function(){
  rc <- new_woRdleStatus()
  return(rc)
}

