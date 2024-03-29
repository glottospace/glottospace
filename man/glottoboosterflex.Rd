% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glottobooster.R
\name{glottoboosterflex}
\alias{glottoboosterflex}
\title{Enhance glottolog data (flexible options)}
\usage{
glottoboosterflex(
  glottologdata = NULL,
  space = TRUE,
  addfamname = TRUE,
  addisolates = TRUE,
  rmdialects = TRUE,
  rmfamilies = TRUE,
  addfamsize = TRUE,
  addfamsizerank = TRUE,
  rmbookkeeping = TRUE,
  rmartifam = TRUE,
  rmsignlangs = TRUE,
  rename = TRUE
)
}
\arguments{
\item{glottologdata}{data from \href{https://glottolog.org/}{glottolog}, can be downloaded with glottoget("glottolog").}

\item{space}{Return spatial object?}

\item{addfamname}{Add column with familiy names?}

\item{addisolates}{Add column to identify isolates?}

\item{rmdialects}{Remove rows with dialects?}

\item{rmfamilies}{Remove rows with families?}

\item{addfamsize}{Add column with family size?}

\item{addfamsizerank}{Add column with family size rank?}

\item{rmbookkeeping}{Remove bookkeeping rows and delete bookkeeping column?}

\item{rmartifam}{Remove rows that refer to artificial families?}

\item{rmsignlangs}{Remove rows with sign languages?}

\item{rename}{Rename columns "id" to "glottocode" and "iso639p3code" to "isocode"}
}
\value{
glottologdata object, either a spatial object (class: sf) or a data.frame.
}
\description{
It is recommended to use glottobooster, but this function is more flexible in removing/adding columns.
}
\details{
Note that the different options are additional. For example, if you set rmfamilies to TRUE and space = TRUE, most families will still be removed since they lack spatial coordinates.
Another example, depending on whether artificial families are removed, the total number of families also increases/decreases.
}
\examples{
\donttest{
glottologdata <- glottoget("glottolog")
glottologdata <- glottoboosterflex(glottologdata)
}
}
\seealso{
Other <glottobooster>: 
\code{\link{glottobooster}()}
}
\concept{<glottobooster>}
\keyword{internal}
