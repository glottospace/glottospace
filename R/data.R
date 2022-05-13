#'
#' This is an internally stored version of glottolog data. Use glottoget("glottolog", download = TRUE) to download the latest version
#' @keywords internal
#' @format A data frame based on glottolog
#' @source \url{https://glottolog.org/
#' }
#' @evalRd glottoget_remotemeta(name = "glottolog")
"glottolog"

#' WALS data
#'
#' This is an internally stored version of WALS data. Use glottoget("wals", download = TRUE) to download the latest version
#' @keywords internal
#' @format A data frame based on The World Atlas of Language Structures
#' @source \url{https://wals.info/
#' }
#' @evalRd glottoget_remotemeta(name = "wals")
"wals"


#'
#' This is an internally stored version of D-PLACE data. Use glottoget("dplace", download = TRUE) to download the latest version
#' @keywords internal
#' @format A data.frame based on D-PLACE, the Database of Places, Language, Culture and Environment
#' @source \url{https://d-place.org/
#' }
#' @evalRd glottoget_remotemeta(name = "dplace")
"dplace"

#'
#' This is an internally stored version of political boundaries of the world (obtained from rnaturalearth).
#' @keywords internal
#' @format An sf object
#' @source \url{https://www.naturalearthdata.com/
#' }
"worldpol"
