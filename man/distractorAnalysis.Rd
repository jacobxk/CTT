\name{distractorAnalysis}
\alias{distractorAnalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for item distractor analysis }
\description{
  This function provides a distractor analysis.
}
\usage{
distractorAnalysis(items, key, scores, nGroups=4, multiKeySep="none",
         multiKeyScore=c("or","poly"), validResp, csvReport, pTable=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{items}{ The unscored item response from a multiple-choice test }
  \item{key}{ The answer key for the items }
  \item{scores}{ An optional set of person scores associated with the item data. If scores are not provided (default) the scores are calculated using the item data and key.  }
  \item{nGroups}{ Determines the number of groups into which scores are discretized. For example, nGroups=4 (default) performs and analysis based on quartiles.  }
    \item{multiKeySep}{ If a value other than "none" is provided (e.g., ","), the key and the raw items will be reviewed for the provided delimiter. Using this otion allows for multiple correct responses.}
  \item{multiKeyScore}{ The first value controls how multiple keys are handled. If "or" any correct response results in score of 1. If "and" all responses must be correct. If the second value is "poly" the returned score is the sum of correct responses. If the second value is "dich" a maximum score of 1 is returned. If the respondent can only provide one response, use "or". If the responded can provide multiple responses and you use c("and", "poly") the score will be 0 or max score.}
  \item{validResp}{ A list of vectors providing valid responses for the distractor tables. If no value is provided, valid responses are determined from the data and assumed to be the same across items. If "fromItem" is provided, values are determined from item responses and NOT assumed to be the same across items.}
  \item{pTable}{ If pTable=FALSE the function returns the counts of examinees who provide each answer.
                  If pTable=TRUE (default) the function returns the proportion of examinees who provide each answer.  }
  \item{csvReport}{ If an optional file name is provided the function will save a .csv file with the results. }
}
\details{
 The scores are used to split respondents into groups, with number determine by nGroups. The proportion (or number if pTable=FALSE) of examinees in each group giving each response is reported. 
 The correct answer is indicated with an "*". Additional item statistics are provided. Descriptors of each item are returned as separate elements in a list.  
}
\value{
  \item{correct}{An "*" indicates the correct response}
  \item{key}{The response option being described}
  \item{n}{The number of responsdents choosing that option}
  \item{rspP}{The proportion of respondents with that response}
  \item{pBis}{The point-biserial correlation between that reponse and the total score with that item removed}
  \item{discrim}{The upper proportion minus the lower proportion}
  \item{lower}{The proportion of respondents choosing that response that are from the lowest score group}
  \item{upper}{The proportion of respondents choosing that response that are from the highest score group}
 }
\references{ Allen, M. J. & Yen, W. M. (1979). Introduction to Measurement Theory. Lon Grove, Illinois: Waveland Press, INC. }
\author{ John T. Willse}
\examples{

# Example data provided with package
data(CTTdata)
data(CTTkey)

distractorAnalysis(CTTdata,CTTkey)

# Results provided in a .csv file.
distractorAnalysis(CTTdata,CTTkey,csvReport="Hello.csv")
}
\keyword{misc}