# Re-exports -------------------------------------------------------------
# Rules are written as dplyr-style expressions, so the handful of verbs that
# appear inside almost every rule are re-exported. `library(tidyABM)` is then
# enough to write a model; `library(dplyr)` is only needed to analyse the result.

#' @importFrom dplyr if_else
#' @export
dplyr::if_else

#' @importFrom dplyr case_when
#' @export
dplyr::case_when

#' @importFrom dplyr between
#' @export
dplyr::between

#' @importFrom dplyr coalesce
#' @export
dplyr::coalesce

#' @importFrom dplyr n
#' @export
dplyr::n

#' @importFrom tibble tibble
#' @export
tibble::tibble

#' @importFrom rlang .data
#' @export
rlang::.data
