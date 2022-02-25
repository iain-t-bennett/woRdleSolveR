#' letterStats
#'
#' @param words a vector of words
#'
#' @return dataframe containing statistics on frequency of letters in each word
#' @export
#'
#' @examples
#' letterStats(words = c("APPLE","PROSE","PUPPY"))
#'
letterStats <- function(words){

  assertthat::assert_that(
    class(words) == "character",
    msg = "words must be a vector of characters"
  )

  pos_count <- dplyr::tibble(Word = words) %>%
    dplyr::transmute(
      Word,
      L1 = substr(words, 1,1),
      L2 = substr(words, 2,2),
      L3 = substr(words, 3,3),
      L4 = substr(words, 4,4),
      L5 = substr(words, 5,5)
    ) %>%
    reshape2::melt(id.var = "Word") %>%
    dplyr::transmute(Word, Position = substr(variable, 2,2), Letter = value) %>%
    dplyr::add_count(Letter, Position, name = "position_count")

  # to avoid duplicates order by most frequent letter per word

  best_letter <- pos_count %>%
    dplyr::add_count(Letter, name = "overall_count") %>%
    dplyr::group_by(Word, overall_count, Letter) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(Word, best_letter = Letter, best_count = overall_count) %>%
    dplyr::arrange(Word, desc(best_count)) %>%
    dplyr::group_by(Word) %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    dplyr::ungroup()

  # cast back to merge

  pos_cast1 <- pos_count %>%
    dplyr::transmute(Word, variable = paste0("Letter_", Position), Letter) %>%
    reshape2::dcast(Word~variable, value.var = "Letter")

  pos_cast2 <- pos_count %>%
    dplyr::transmute(Word, variable = paste0("PFreq_", Position), position_count) %>%
    reshape2::dcast(Word~variable, value.var = "position_count")

  best_cast1 <- best_letter %>%
    dplyr::transmute(Word, variable = paste0("BestLetter_", id), best_letter) %>%
    reshape2::dcast(Word~variable, value.var = "best_letter")

  best_cast2 <- best_letter %>%
    dplyr::transmute(Word, variable = paste0("BFreq_", id), best_count) %>%
    reshape2::dcast(Word~variable, value.var = "best_count")

  rc <- pos_cast1 %>%
    dplyr::left_join(pos_cast2, by = "Word") %>%
    dplyr::left_join(best_cast1, by = "Word") %>%
    dplyr::left_join(best_cast2, by = "Word")

  return(rc)

}
