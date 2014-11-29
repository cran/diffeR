\name{crosstabm}
\alias{crosstabm}

\title{
create a contingency table between a comparison raster map (rows) and a reference raster map (columns)
}
\description{
create a contingency table, also called cross-tabulated matrix, between a comparison raster map (rows), or map at time \emph{t}, and a reference raster map (columns), or map at time \emph{t+1}
}
\usage{
crosstabm(comp, ref, percent=FALSE)
}

\arguments{
  \item{comp}{
object of class RasterLayer corresponding to the comparison map, or map at time \emph{t}
}
  \item{ref}{
object of class RasterLayer corresponding to the reference map, or map at time \emph{t+1}
}
  \item{percent}{
logical. If TRUE, output values are given as percentage. If FALSE, output values are given in pixel counts
}
}
\value{
a matrix showing the cross-tabulation between the comparison map (or map at time \emph{t}) and the reference map (or map at time \emph{t+1})
}
\seealso{
\code{\link{memberships}}
}
\examples{
comp <- raster(system.file("external/comparison.rst", package="diffeR"))
ref <- raster(system.file("external/reference.rst", package="diffeR"))
crosstabm(comp, ref)
}

\keyword{ spatial }
