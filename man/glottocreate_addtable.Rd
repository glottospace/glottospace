% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glottocreate.R
\name{glottocreate_addtable}
\alias{glottocreate_addtable}
\title{Add a table to glottodata}
\usage{
glottocreate_addtable(glottodata, table, name)
}
\arguments{
\item{glottodata}{A glottodata table, or a list of glottodata tables}

\item{table}{A table to be added}

\item{name}{A name for the table}
}
\value{
a glottodata object with structure table added to it.
}
\description{
Add a table to glottodata
}
\examples{
glottodata <- glottoget("demodata", meta = FALSE)
structuretable <- glottocreate_structuretable(varnames = colnames(glottodata)[-1])
glottodata <- glottocreate_addtable(glottodata, table = structuretable, name = "structure")
}
\keyword{internal}
