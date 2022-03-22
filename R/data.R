#'
#' This is an internally stored version of glottolog data. Use glottoget("glottolog", download = TRUE) to download the latest version
#' @keywords internal
#' @format A data frame with XX rows and XX variables:
#' \describe{
#'   \item{id}{glottocodes, a unique identifier for each languoid}
#'   ...
#' }
#' @source \url{https://glottolog.org/
#' }
#' @evalRd glottoget_remotemeta(name = "glottolog")
"glottolog"

#' WALS data
#'
#' This is an internally stored version of WALS data. Use glottoget("wals", download = TRUE) to download the latest version
#' @keywords internal
#' @format A data frame based on The World Atlas of Language Structures
#' \describe{
#'   \item{glottocode}{A unique identifier for each languoid}
#'   ...
#' }
#' @source \url{https://wals.info/
#' }
#' @evalRd glottoget_remotemeta(name = "wals")
"wals"


#'
#' This is an internally stored version of D-PLACE data. Use glottoget("dplace", download = TRUE) to download the latest version
#' @keywords internal
#' @format A data.frame based on D-PLACE, the Database of Places, Language, Culture and Environment
#' \describe{
#'   \item{glottocode}{A unique identifier for each languoid}
#'   ...
#' }
#' @source \url{https://d-place.org/
#' }
#' @evalRd glottoget_remotemeta(name = "dplace")
"dplace"
