% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rangebreak.blob.R
\name{rangebreak.blob}
\alias{rangebreak.blob}
\title{rangebreak.blob Conduct a blob rangebreak test as described in Glor and Warren 2011.}
\usage{
rangebreak.blob(species.1, species.2, env, type, f = NULL, nreps = 99,
  nback = 1000, ...)
}
\arguments{
\item{species.1}{An emtools.species object}

\item{species.2}{An enmtools.species object}

\item{env}{A RasterLayer or RasterStack object containing environmental data}

\item{type}{The type of model to construct, currently accepts "glm", "mx", "bc", "gam", or "dm"}

\item{f}{A function to use for model fitting.  Only required for GLM models at the moment.}

\item{nreps}{Number of replicates to perform}

\item{...}{Additional arguments to be passed to model fitting functions.}
}
\value{
results A list containing a replicates, models for the empirical data, and summary statistics and plots.
}
\description{
rangebreak.blob Conduct a blob rangebreak test as described in Glor and Warren 2011.
}
\examples{
rangebreak.blob(ahli, allogus, env, type = "glm", f = layer.1 + layer.2 + layer.3, nreps = 10, ...)

}
\keyword{barrier,}
\keyword{biogeography,}
\keyword{enmtools,}
\keyword{hypothesis}
\keyword{rangebreak,}
\keyword{testing}

