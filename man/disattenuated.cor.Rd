\name{disattenuated.cor}
\alias{disattenuated.cor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for disattentuated correlation}
\description{
This function is used to calculate the disattentuated correlation between two measures given the corresponding test reliabilities.
}
\usage{
disattenuated.cor(r.xy, r.xx, new.r.xx = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r.xy}{ The correlation between test x and test y }
  \item{r.xx}{ Each tests' reliability }
  \item{new.r.xx}{  A new reliability for each test (optional) }
}
\details{
The data given in r.xy may be a single value or a matrix. A matrix is assumed to be a correlation matrix (square, symmetric).

The data given in r.xx should be a vector, with one reliability for each instrument involved in the correlation, r.xy.

The new.r.xx represents a new reliability for each measure. If these values are less than 1, the returned correlation is the value that would be expected with the new reliability. 
}
\value{
If r.xy is a single value a single value is returned. If r.xy is a matrix then a matrix is returned with the reliabilities on the diagonal, the disattenuated correlations in the upper triangle and the original correlations in the lower triangle.
}
\references{ 
Spearman, C. (1904). The proof and measurement of association between two things. American Journal of Psychology, 15, 72-101. \cr
Gulliksen, H. (1950). Theory of Mental Tests. New York: John Wiley & Sons, Inc.
 }
\author{ John T. Willse, Zhan Shu }

\examples{

# r.xy=0.6, r.xx=0.7,r.yy=0.8
disattenuated.cor(0.6,c(0.7,0.8))

# if r.xy is a matrix:
cor1 <- matrix(c(1.0000000, 0.24391288, 0.2812319, 0.05251050,
                 0.2439129, 1.00000000, 0.1652985, 0.08126448,
                 0.2812319, 0.16529850, 1.0000000, 0.27971630,
                 0.0525105, 0.08126448, 0.2797163, 1.00000000), byrow=TRUE,
                 ncol=4)

rxx1 <- c(0.8,0.8,0.81,0.9)     # reliability of each test 
new.rxx1 <- c(0.9,0.97,0.8,0.7) # projected new reliability of those tests

disattenuated.cor(cor1, rxx1, new.rxx1)
}
\keyword{misc}
