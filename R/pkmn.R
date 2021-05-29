#' Pokemon Stats by Generation
#'
#' This data set contains information and stats on Pokemon from Pokemon generations 1 through 7. Each Pokemon has it's own row, in which it's name, first/primary type, total base stats, and other information is presented.
#' 
#' @format A tibble with 12 columns and 751 rows.
#' @docType data
#' \describe{
#'    \item {name}{chr denoting the name of each Pokemon}
#'    \item {type_1}{fct The pokemon's first/primary type.}
#'    \item {type_level_code}{fct A numerical coding version of the first/primary type column, for data analysis}
#'    \item {total}{dbl The pokemon's total aggregate base stats}
#'    \item {hp}{dbl The base HP (Hit Points) of each pokemon}
#'    \item {attack}{dbl Base Attack points}
#'    \item {defense}{dbl Base Defense points}
#'    \item {sp_atk}{dbl Base Special Attack points}
#'    \item {sp_def}{dbl Base Special Defense points}
#'    \item {speed}{dbl Base Speed points}
#'    \item {generation}{fct Numerical factor, starting with 1, that corresponds to each Pokemon's generation}
#'    \item {legendary}{fct A factor that has levels TRUE or FALSE, indicating the stats of a pokemon as Legendary or not}
#'    }
#'    @source \url{https://www.kaggle.com/abcsds/pokemon}
#'    "pkmn"