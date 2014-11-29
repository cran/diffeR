\name{differenceMR}
\alias{differenceMR}

\title{
calculates difference metrics between a reference map and a comparison map both consecutively aggregated at multiple resolutions
}
\description{
calculates quantity, exchange and shift components of difference, as well as the overall difference, between a comparison raster map (or map at time \emph{t}), and a reference raster map (or map at time \emph{t+1}), both consecutively aggregated at multiple resolutions.  

Quantity difference is defined as the amount of difference between the reference map and a comparison map that is due to the less than maximum match in the proportions of the categories. Exchange consists of a transition from category \emph{i} to category \emph{j} in some pixels and a transition from category \emph{j} to category \emph{i} in an identical number of other pixels. Shift refers to the difference remaining after subtracting quantity difference and exchange from the overall difference.
}
\usage{
differenceMR(comp, ref, eval="multiple", percent=TRUE, fact=2)
}

\arguments{
  \item{comp}{
object of class RasterLayer corresponding to the comparison map, or map at time \emph{t}
}
  \item{ref}{
object of class RasterLayer corresponding to the reference map, or map at time \emph{t+1}
}
  \item{eval}{
default "original", return difference metrics between the input raster maps at the original resolution; if "multiple", return difference metrics at multiple resolutions aggregated according to a geometric sequence 
}
  \item{percent}{
logical. If TRUE, output value is given as percentage. If FALSE, output value is given as proportion (0 to 1)
}
  \item{fact}{
integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically). Or two integers (horizontal and vertical aggregation factor). See \code{raster} package for details
}
}
\value{
data.frame containing quantity, exchange and shift components of difference, as well as the overall difference, between the comparison map and the reference map at multiple resolutions
}
\references{
Pontius Jr., R.G., Millones, M. 2011. \emph{Death to Kappa: birth of quantity disagreement and allocation disagreement for accuracy assessment}. International Journal of Remote Sensing 32 (15), 4407-4429.

Pontius Jr., R.G., Santacruz, A. 2014. \emph{Quantity, exchange and shift components of difference in a square contingency table}. International Journal of Remote Sensing 35 (21), 7543-7554.
}
\seealso{
\code{\link{overallQtyD}}
}
\examples{
\dontrun{
comp <- raster(system.file("external/comparison.rst", package="diffeR"))
ref <- raster(system.file("external/reference.rst", package="diffeR"))
differenceMR(comp, ref, eval="original")
differenceMR(comp, ref, eval="multiple", fact=2)
}
}

\keyword{ spatial }
