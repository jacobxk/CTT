\name{itemAnalysis}
\alias{itemAnalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for item reliability analysis  }
\description{
 This function performs reliability analyses, providing coefficient alpha and classical item statistics. This function improves and replaces the function reliability from previous versions.
}
\usage{
itemAnalysis(items, itemReport=TRUE, NA.Delete=TRUE, rBisML=FALSE, hardFlag, 
             easyFlag, pBisFlag, bisFlag, flagStyle = c("X",""))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{items}{ The scored response file with "0" (wrong) and "1" (correct) or Likert type data }
  \item{itemReport}{ If itemReport=TRUE (the default) item analyses are conducted. The function will provide a dataframe containing item names, item means, item total correlations, and alpha if item is removed.}
  \item{NA.Delete}{If NA.Delete=TRUE (the default) records are deleted listwise if there are missing responses. 
                   If NA.Delete=FALSE all NA values are changed to 0s. }
  \item{rBisML}{A logical variable indicating whether the biserial correlation is calculated using a formal maximum likelihood estimator or an ad hoc estimator (default, speeds up analysis with many items).}
  \item{hardFlag}{ If a numeric value is provided, a flag is added to itemReport for each item with a mean less than the value. itemReport=TRUE must also be set.}
  \item{easyFlag}{ If a numeric value is provided, a flag is added to itemReport for each item with a mean greater than the value. itemReport=TRUE must also be set.}
  \item{pBisFlag}{ If a numeric value is provided, a flag is added to itemReport for each item with a point-biserial correlation less than the value. itemReport=TRUE must also be set.}
  \item{bisFlag}{ If a numeric value is provided, a flag is added to itemReport for each item with a biserial correlation less than the value. itemReport=TRUE must also be set.}
  \item{flagStyle}{ Determines the values to be used for item flagging. Default uses an "X" when an item is flagged and "" when not. Any value, including booleans can be used.}
}
\details{
The input files must be scored files with "0" and "1" or numeric scales (e.g., Likert Type scales). 
Only basic scale information is returned to the screen. Use str() to view additional statistics that are available. If itemReport is used (preferred) item statistics are provided as part of a dataframe called itemReport. Use function reliability with option itemal (being phased out), for output pre 2.2.}
\value{
  \item{nItem}{The number of items}
  \item{nPerson}{The sample size used in calculating the values}
  \item{alpha}{Crobach's alpha}
  \item{scaleMean}{Average total sum socre}
  \item{scaleSD}{Standard deviation of total sum score}
  \item{itemReport}{Returned if itemReport = TRUE. Returns a data frame with key item analysis results: item mean (itemMean), point-biserial (pBis), biserial (bis), Cronbach's alpha if item removed, and any item flags indicated in the function call.}
}
\references{ Cronbach, L. J. (1951). Coefficient alpha and the internal structure of tests. Psychometika, 16, 297-334. }
\author{ John T. Willse }
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
itemAnalysis(x)

# To see an item report with flags.
iA <- itemAnalysis(x, hardFlag=.25, pBisFlag=.15)
iA$itemReport

# To see more item statisitics
str(itemAnalysis(x))

}
\keyword{misc}
