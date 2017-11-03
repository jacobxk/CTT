\name{distractor.analysis}
\alias{distractor.analysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for item distractor analysis }
\description{
  This function is deprecated. Use \code{distractorAnalysis} for a more complete distractor analysis.
}
\usage{
distractor.analysis(items, key, scores, p.table = FALSE, write.csv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{items}{ The unscored item response from a multiple-choice test }
  \item{key}{ The answer key for the items }
  \item{scores}{ An optional set of person scores associated with the item data. If scores are not provided (default) the scores are calculated using the item data and key.  }
  \item{p.table}{ If p.table=FALSE  (the default) the function returns the counts of examinees who provide each answer.  
                  If p.table=TRUE the function returns the proportion of examinees who provide each answer.  }
  \item{write.csv}{ If the optional file name is provided the function will save a .csv file with the results. }
}
\details{
 The scores are used to split respondents into terciles. The number (or proportion if p.table=TRUE) of examinees in each tercile giving each response is reported. 
 The correct answer is indicated with an "*".
}
\value{
 If p.table=F counts of respondents in each tercile who chose each answer is returned as a list of tables. Each item is a separate element in the list. 
 If p.table=T the tables contain the proportion of respondents who chose each corresponding answer. }
\references{ Allen, M. J. & Yen, W. M. (1979). Introduction to Measurement Theory. Lon Grove, Illinois: Waveland Press, INC. }
\author{ John T. Willse, Zhan Shu}
\seealso{
   \code{\link{distractorAnalysis}}
   }
\examples{

# Example data provided with package
data(CTTdata)
data(CTTkey)

distractor.analysis(CTTdata,CTTkey)

# Results provided in a .csv file.
distractor.analysis(CTTdata,CTTkey,p.table=TRUE,write.csv="Hello.csv")
}
\keyword{misc}