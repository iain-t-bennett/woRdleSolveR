
#' algo_guess_v1
#'
#' @param x a woRdleStatus
#'
#' @return a word
#' @export
#'
#'
algo_guess_v1 <- function(x){
  assertthat::assert_that(
    class(x) == "woRdleStatus",
    msg = "x must be a woRdleStatus"
  )

  # find all valid words

  valid_words <- filterWords(x)

  # get letter stats for these words

  stats <- letterStats(valid_words)

  # select word that has most frequent letters in most frequent positions

  scored <- stats %>%
    group_by(Word) %>%
    mutate(Bscore = sum(BFreq_1, BFreq_2, BFreq_3, BFreq_4, BFreq_5, na.rm = TRUE),
           Pscore = sum(PFreq_1, PFreq_2, PFreq_3, PFreq_4, PFreq_5, na.rm = TRUE)) %>%
    arrange(desc(Bscore, Pscore)) %>%
    ungroup()

  return(scored$Word[1])
}
