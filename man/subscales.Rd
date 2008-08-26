\name{subscales}
\alias{subscales}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to create subscales based on a design matrix}
\description{
  This convenience function is provided to facilitate extracting subscales from a single set of item responses. 
}
\usage{
subscales(items, scales, scale.names = NA, score.items = FALSE, check.reliability = FALSE, key=NA)
}
\arguments{
  \item{items}{ The item response (scored or not) }
  \item{scales}{ A design matrix, with items represented in rows and separate subscales represented in columns.
  An item may appear in more than one subscale.}
  \item{scale.names}{ Optional vector of names for the subscales }
  \item{score.items}{ If responses are not scored, they may be scored using score.items=TRUE (key must be provided) }
  \item{check.reliability}{ If check.reliability=TRUE, the reliability for each subscale will be calculted }
  \item{key}{ Optional key, required only if score.scales=TRUE.}
}
\details{
  This function provides an easy way to create new datasets from a single set of item responses. 
This function is also a front end for score and reliability, enabling the item responses to be partitioned into separate scales, scored, and reliability analyses performed using this one function.
}
\value{
  A list is returned. Results for each subscale (i.e., column in the scales matrix) are provided as sparate objects in that list.
  \item{score}{Each examinee's score on the associated subscale}
  \item{reliablity}{Reliability results (if requested) for the associated subscale}
  \item{scored}{The scored item responses (if required) for each respondent for the associated subscale}
}
\author{ John Willse, Zhan Shu}
\seealso{ reliability, score }
\examples{
# Example data included with package
data(CTTdata)
data(CTTkey)

# design matrix
q <- matrix(c(1,0,
              1,0,
              1,0,
              1,0,
              1,0,
              1,0,
              1,0,
              1,0,
              1,0,
              1,1,
              0,1,
              0,1,
              0,1,
              0,1,
              0,1,
              0,1,
              0,1,
              0,1,
              0,1,
              0,1), ncol=2, byrow=TRUE)
subscales(CTTdata,q,c("T1","T2"),TRUE,TRUE,CTTkey)
}
\keyword{misc}
