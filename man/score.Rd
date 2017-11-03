\name{score}
\alias{score}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to score the response files }
\description{
  This function can score multiple choice item responses. 
  This function can also call and return results from function reliability.
}
\usage{
score(items,key,output.scored=FALSE,ID=NA,rel=FALSE,multiKeySep="none",
         multiKeyScore=c("or","poly"))
}
\arguments{
  \item{items}{ The item responses to be scored }
  \item{key}{ The answer key }
  \item{output.scored}{ If output.scored=FALSE (the default) only a vector of scores is returned. 
                        If output.scored=TRUE a matrix containing scored items is returned.  }
  \item{ID}{ If respondent IDs are provided scores are labeled appropriately.}
  \item{rel}{ If rel=TRUE, the function will call the function reliability and provide that output as well. }
  \item{multiKeySep}{ If a value other than "none" is provided (e.g., ","), the key and the raw items will be reviewed for the provided delimiter. Using this otion allows for multiple correct responses.}
  \item{multiKeyScore}{ The first value controls how multiple keys are handled. If "or" any correct response results in score of 1. If "and" all responses must be correct. If the second value is "poly" the returned score is the sum of correct responses. If the second value is "dich" a maximum score of 1 is returned. If the respondent can only provide one response, use "or". If the responded can provide multiple responses and you use c("and", "poly") the score will be 0 or max score.}
}
\author{ John T. Willse }
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
