\name{score.transform}
\alias{score.transform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for transforming scores onto different scales }
\description{
  The function transforms the score metric by setting new scales' mean, standard deviation, and normalizing the distribution.
}
\usage{
score.transform(scores, mu.new = 0, sd.new = 1, normalize = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scores}{ Vector for examinee scores }
  \item{mu.new}{ Desired mean of the scale }
  \item{sd.new}{ Desired standard deviation of scales }
  \item{normalize}{ If normailize=True, the score will be normalized applying the inverse of the cumulative distribution function 
of the normal distribution to the respondents percentile score.}
}
\value{
  The function returns a list with two vectors: new.scores is the transformed score 
and p.scores is the percentile rank of every examinee. If normalize=TRUE than percentile scores are used to create a roughly normal distribution by applying an inverse cumulative normal distribution function to the p.scores.  
}
\author{John T. Willse, Zhan Shu }
\examples{

# Example data provided with package
data(CTTdata)
data(CTTkey)

# Data scored to demonstrate function
scores <- score(CTTdata,CTTkey)$score # obtain the scores

# the targeted mean=3, standard deviation=1

score.transform(scores,3,1)

# the score should be transformed by normalized precentile
score.transform(scores,3,1,TRUE)
}
\keyword{misc}