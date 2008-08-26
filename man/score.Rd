\name{score}
\alias{score}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to score the response files }
\description{
  This function can score multiple choice item responses. 
  This function can also call and return results from function reliability.
}
\usage{
score(items, key = NA, output.scored = FALSE, ID = NA, rel = FALSE)
}
\arguments{
  \item{items}{ The item responses to be scored }
  \item{key}{ The answer key }
  \item{output.scored}{ If output.scored=FALSE (the default) only a vector of scores is returned. 
                        If output.scored=TRUE a matrix containing scored items is returned.  }
  \item{ID}{ If respondent IDs are provided scores are labeled appropriately.}
  \item{rel}{ If rel=TRUE, the function will call the function reliability and provide that output as well. }
}
\author{ John T. Willse, Zhan Shu }
\seealso{ reliability }
\examples{
# Example data provided with package
data(CTTdata)
data(CTTkey)

# Scores for each preson 
score(CTTdata,CTTkey)

# Scores, scored file, and relibility
score(CTTdata,CTTkey,output.scored=TRUE,rel=TRUE)
}
\keyword{misc}
