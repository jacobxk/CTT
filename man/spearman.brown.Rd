\name{spearman.brown}
\alias{spearman.brown}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Functions for Spearman-Brown "Prophecy" Formula}
\description{
 This function calculates either a predicted reliability for a measure given the original reliability and a new test length, or the function calculates the required test length to achieve a desired level of reliability. }
\usage{
spearman.brown(r.xx, input = 2, n.or.r = "n")
}
\arguments{
  \item{r.xx}{ The original relibility }
  \item{input}{ The new test length or a desired level of reliability, depending on n.or.r}
  \item{n.or.r}{ If n.or.r="n", the function will return a new reliability; if n.or.r="r", the function will return the factor by which the test length must change to achieve a desired level of reliability.}
}
\details{
 If n.or.r="n", the function will return a new reliability and input should be the factor by which the test length is to be changed.
 If n.or.r="r", the function will return the factor by which the test length must change to achieve a desired level of reliability (provided in input). 
}
\references{   Spearman, C. (1910). Correlation calculated with faulty data. British Journal of Psychology, 3,
271-295.\cr \cr
   Brown,W. (1910). Some experimental results in the correlation of mental abilities. British Journal
of Psychology, 3, 296-322.
}
\author{ John Willse, Zhan Shu }
\examples{

# old relibility is 0.6, if the measure is lengthened by a factor of 2, the relibility of new test is:
spearman.brown(0.6,2,"n")

# old relibility is 0.5, if we want a new measure to be 0.8, the new test length is:
spearman.brown(0.5, 0.8, "r")
}
\keyword{misc}