\name{reliability}
\alias{reliability}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for item reliability analysis  }
\description{
 This function performs reliability analyses, providing coefficient alpha and item statistics.
}
\usage{
reliability(items, itemal = TRUE, NA.Delete = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{items}{ The scored response file with "0" (wrong) and "1" (correct) or Likert type data }
  \item{itemal}{ If itemal=FALSE (the default) no item analyses are conducted. 
   If itemal=TRUE, the function will provide item means, item total correlations, and alpha if item is removed.}
  \item{NA.Delete}{If NA.Delete=TRUE (the default) records are deleted listwise if there are missing responses. 
                   If NA.Delete=FALSE all NA values are changed to 0s. }
}
\details{
The input files must be scored files with "0" and "1" or numeric scales (e.g., Likert Type scales). 
Only basic scale information is returned to the screen. Use str() to view additional statistics that are available. }
\value{
  \item{nItem}{The number of items}
  \item{nPerson}{The sample size used in calculating the values}
  \item{alpha}{Crobach's alpha}
  \item{scaleMean}{Average total sum socre}
  \item{scaleSD}{Standard deviation of total sum score}
  \item{alphaIfDeleted}{Cronbach's alpha if the corresponding item were deleted}
  \item{pBis}{The item total correlation, with the item's contribution removed from the total}
  \item{bis}{The item total biserial (or polyserial) correlation, with the item's contribution removed from the total'}
  \item{itemMean}{Average of each item}
}
\references{ Cronbach, L. J. (1951). Coefficient alpha and the internal structure of tests. Psychometika, 16, 297-334. }
\author{ John T. Willse, Zhan Shu }
\seealso{ score }
\examples{

# Scored input (data frame is preferred)
 x<-data.frame(matrix(c(0,0,0,0,0,
                        0,0,0,0,0,
                        0,0,0,0,1,
                        0,0,0,1,1,
                        0,0,1,1,1,
                        0,1,1,1,1,
                        1,1,1,1,1,
                        1,0,1,1,1,
                        0,0,0,1,1,
                        0,1,1,1,1),nrow=10,ncol=5,byrow=TRUE,
                        dimnames=list(c(paste("P",c(1:10),sep="")),c(paste("I",c(1:5),sep="")))))
reliability(x, itemal=TRUE)

# To see more item statisitics
str(reliability(x,itemal=TRUE))
}
\keyword{misc}
