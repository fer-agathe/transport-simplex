% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transport-simplex.R
\name{transport_simplex}
\alias{transport_simplex}
\title{Transport compositional data from group 0 to group 1}
\usage{
transport_simplex(X0, X1, isomorphism = c("clr", "alr", "ilr"), n_interp = 1)
}
\arguments{
\item{X0}{Data frame with observations in group 0.}

\item{X1}{Data frame with observations in group 1.}

\item{isomorphism}{Isomorphism used to map the composition data from the
simplex (\eqn{S_d}) to a \eqn{d-1} Euclidean space. Three possibilities: \code{"alr"}
(additive log ratio transform), \code{"clr"} (centered log ratio transform),
and \code{"ilr"} (isometric log ratio transform).}

\item{n_interp}{Number of steps in the interpolation (default to 1: no
interpolation).}
}
\value{
A tibble with the transported values. If \code{n_interp} is larger than
1, the result also contains a list with the interpolated values, in the
\code{"interpolated"} attribute ; else, the attribute is \code{NULL}. The attribute
\code{"ot_mapping"} stores the mapping.
}
\description{
Transport compositional data from group 0 to group 1
}
\examples{
# First three columns: probabilities of being of class A, B, or C.
# Last column: group (0 or 1)
data(toydataset)
X0 <- toydataset[toydataset$group == 0, c("A", "B", "C")]
X1 <- toydataset[toydataset$group == 1, c("A", "B", "C")]

# Transport only, from group 0 to group 1, using centered log ratio transform:
transp <- transport_simplex(X0 = X0, X1 = X1, isomorphism = "clr")
head(transp)

# If we want to transport new points:
new_obs <- data.frame(A = c(.2, .1), B = c(.6, .5), C = c(.2, .4))
transport_simplex_new(transport = transp, newdata = new_obs)

# If we want to get interpolated values using McCann (1997) displacement
# interpolation: (here, with 5 intermediate points)
transp_with_interp <- transport_simplex(
  X0 = X0, X1 = X1, isomorphism = "clr", n_interp = 5
)
interpolated(transp_with_interp)[[1]] # first obs
interpolated(transp_with_interp)[[2]] # second obs

# And displacement interpolation for the new obs:
transp_new_obs_with_interp <- transport_simplex_new(
  transport = transp, newdata = new_obs, n_interp = 5
)
interpolated(transp_new_obs_with_interp)[[1]] # first new obs
interpolated(transp_new_obs_with_interp)[[1]] # second new obs
}
\references{
McCann, Robert J. 1997. "A Convexity Principle for Interacting
Gases." Advances in Mathematics 128 (1): 153–79.
}
\seealso{
\code{\link[=transport_simplex_new]{transport_simplex_new()}}
}
