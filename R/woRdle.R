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
             guesses = lapply(1:6,function(x){woRdleGuessNull()}))

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

  # get the guesses as a dataframe

  df <- expand.grid(guess_i = 1:6,
                    letter_i = 1:5)

  for (row in 1:nrow(df)){
   df$letter[row] = x$guesses[[df$guess_i[row]]]$word[df$letter_i[row]]
   df$status[row] = x$guesses[[df$guess_i[row]]]$response[df$letter_i[row]]
  }

  df <- df %>%
    dplyr::mutate(
      color = ifelse(status == "G", "Green", ifelse(status == "Y", "Yellow", "Grey")),
      color = ifelse(letter == " ", "White", color),
      ypos = (6-guess_i) * 2,
      xpos = letter_i * 2
    )

  # get the status as a dataframe
  green_pos <- which(rowSums(x$status$status)==1)
  Green <- colSums(x$status$status[green_pos,]) > 0
  Grey  <- colSums(x$status$status==FALSE, na.rm = TRUE) == 5
  Yellow <- x$status$tried * !(Green | Grey)

  df2 <- dplyr::tibble(letter = LETTERS,
                      color = ifelse(Green == TRUE, "Green", "White"),
                      xpos = NA,
                      ypos = NA ) %>%
    dplyr::mutate(color = ifelse(Grey == TRUE, "Grey", color),
                  color = ifelse(Yellow == TRUE, "Yellow", color))

  row1 <- "QWERTYUIOP"
  row2 <- "ASDFGHJKL"
  row3 <- "ZXCVBNM"

  for (i in 1:26){
    this.letter <- df2$letter[i]
    if (grepl(this.letter, row1)){
      df2$ypos[i] <- -2
      df2$xpos[i] <- as.numeric(unlist(gregexpr(this.letter, row1))) + 0.5
    }
    if (grepl(this.letter, row2)){
      df2$ypos[i] <- -3
      df2$xpos[i] <- as.numeric(unlist(gregexpr(this.letter, row2))) + 1
    }
    if (grepl(this.letter, row3)){
      df2$ypos[i] <- -4
      df2$xpos[i] <- as.numeric(unlist(gregexpr(this.letter, row3))) + 2
    }
  }

  # make a new df

  df3 <- dplyr::bind_rows(
    dplyr::transmute(df, type = "guess", xpos, ypos, letter, color),
    dplyr::transmute(df2, type = "status", xpos, ypos, letter, color),
  )


  df3$color <- factor(df3$color,
                      ordered = TRUE,
                      levels =  c("Green", "Grey", "White", "Yellow"))

  # make the plot

  rc <- ggplot2::ggplot(data = df3, ggplot2::aes(x= xpos, y = ypos, fill = color)) +
    ggplot2::geom_tile(data = function(x){dplyr::filter(x, type == "guess")},
                       linetype = 1, size = 1, color = "black") +
    ggplot2::geom_tile(data = function(x){dplyr::filter(x, type == "status")},
                       linetype = 1, size = 1, color = "black") +
    ggplot2::geom_text(ggplot2::aes(label = letter)) +
    ggplot2::theme_void() +
    ggplot2::scale_fill_manual(values = c("Green", "Grey", "White", "Yellow"),
                               drop = FALSE) +
    ggplot2::theme(legend.position = "none")

  return(rc)
}


