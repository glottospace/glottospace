% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glottoclean.R
\name{glottoclean}
\alias{glottoclean}
\title{Clean glottodata/glottosubdata}
\usage{
glottoclean(
  glottodata,
  tona = NULL,
  tofalse = NULL,
  totrue = NULL,
  id = NULL,
  glottosample = FALSE,
  one_level_drop = TRUE
)
}
\arguments{
\item{glottodata}{glottodata (either a list or a data.frame)}

\item{tona}{Optional additional values to recode to NA (besides default)}

\item{tofalse}{Optional additional values to recode to FALSE (besides default)}

\item{totrue}{Optional additional values to recode to TRUE (besides default)}

\item{id}{By default, glottoclean looks for a column named 'glottocode', if the id is in a different column, this should be specified.}

\item{glottosample}{Should the sample table be used to subset the data?}

\item{one_level_drop}{A logical value to denote whether or not to drop variables with a single value, the default value is TRUE.}
}
\value{
A cleaned-up and simplified version of the original glottodata object
}
\description{
This function cleans glottodata/glottosubdata and returns a simplified glottodata/glottosubdata object containing only the cleaned data table and a structure table.
}
\details{
This function has some built in default values that are being recoded:
For example, if column type is 'symm' or 'asymm', values such as "No" and 0 are recoded to FALSE
Values such as "?" are recoded to NA.
}
\examples{
glottodata <- glottoget("demodata", meta = TRUE)
glottodata <- glottoclean(glottodata)

glottosubdata <- glottoget("demosubdata", meta = TRUE)
glottosubdata <- glottoclean(glottosubdata)
}
