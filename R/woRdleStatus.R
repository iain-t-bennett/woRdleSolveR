#' init
#'
#' @return woRdleStatus
#' @export
#'
new_woRdleStatus <- function(){
  # define an object that can hold state of current knowledge
  rc <- matrix(nrow = 5, ncol = 26, data = TRUE)
  # label columns as letters
  colnames(rc) <- LETTERS
  # label rows as letter position
  rownames(rc) <- paste0("Position",1:5)

  # can now represent game with this matrix
  # status = TRUE is right letter right place
  # status = FALSE means letter not in that place
  # letter not in word implies Position 1-5 all FALSE
  # letter in wrong place implies only that position is False

  class(rc) <- "woRdleStatus"

  return(rc)
}

#' init
#'
#' @return woRdleStatus
#' @export
#'
woRdleStatus <- function(){
  rc <- new_woRdleStatus()
  return(rc)
}

#' plot.woRdleStatus
#'
#' @param x
#'
#' @return ggplot
#' @export
#'
plot.woRdleStatus <- function(x){
  # collapse status down to display
  green_pos <- which(rowSums(x)==1)
  Green <- colSums(x[green_pos,]) > 0
  Grey  <- colSums(x==FALSE, na.rm = TRUE) == 5
  yellow_pos <- rowSums(x)
  Yellow <-
  df <- data.frame(letter = LETTERS,
                   color = ifelse(Green == TRUE, "Green", "White"),
                   xpos = NA,
                   ypos = NA)
  df$color <- ifelse(Grey == TRUE, "Grey", df$color)
  df$color <- ifelse(Yellow == TRUE, "Yellow", df$color)

  row1 <- "QWERTYUIOP"
  row2 <- "ASDFGHJKL"
  row3 <- "ZXCVBNM"

  for (i in 1:26){
    this.letter <- df$letter[i]
    if (grepl(this.letter, row1)){
      df$ypos[i] <- 3
      df$xpos[i] <- as.numeric(unlist(gregexpr(this.letter, row1)))
    }
    if (grepl(this.letter, row2)){
      df$ypos[i] <- 2
      df$xpos[i] <- as.numeric(unlist(gregexpr(this.letter, row2))) + 0.5
    }
    if (grepl(this.letter, row3)){
      df$ypos[i] <- 1
      df$xpos[i] <- as.numeric(unlist(gregexpr(this.letter, row3))) + 1.5
    }
  }

  df$color <- factor(df$color,
                     ordered = TRUE,
                     levels =  c("Green", "Grey", "White", "Yellow"))

  rc <- ggplot2::ggplot(data = df, aes(x = xpos, y= ypos, fill = color)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(aes(label = letter)) +
    ggplot2::theme_void() +
    ggplot2::scale_fill_manual(values = c("Green", "Grey", "White", "Yellow"),
                               drop = FALSE) +
    ggplot2::theme(legend.position = "none")
  return(rc)
}

