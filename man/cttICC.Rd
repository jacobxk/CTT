\name{cttICC}
\alias{cttICC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for producing theoretical and empirical item characteristic curves. }
\description{
  This function produces empirical item characteristic curves.
}

\usage{
cttICC(scores, itemVector, xlim, ylim, plotTitle, xlab, ylab, 
     col = c("black","white"), colTheme, gDevice, file, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scores}{ A total measure score, for creating expected mean values of the item. }
  \item{itemVector}{ Observed item responses for the item ICC. }
  \item{xlim}{ A vector overriding default limits for the x axis. }
  \item{ylim}{ A vector overriding default limits for the y axis. }
  \item{plotTitle}{ Controls the main plot title.}
  \item{xlab}{ The label for the x axis. }
  \item{ylab}{ The label for the y axis. }
  \item{col}{ A vector of the colors to be used in the plot. The first color will be used for item labels. The second color will be used for shading the area of rejection. }
  \item{colTheme}{ Four color themes ("cavaliers", "dukes", "spartans", "greys") are provided. If you provide a color theme, it will override the col paramater. }
  \item{gDevice}{ Controls graphics device. Options are "screen" (default), "jpg", or "png". }
  \item{file}{ The name of the output file if a device other than "screen" is chosen. }
  \item{...}{ Additional parameters passed to the plot command.}
}
\details{ The function produces an item characterisic curve plot. The empirical ICC is created by calculating the item mean in between 2 and 20 bins. There must be at least 15 observations per bin, or a smaller number of bins is used.
}

\author{ John T. Willse }

\examples{

library(CTT)# Example data provided with package
data(CTTdata)
data(CTTkey)

# Scores for each preson 
myScores <- score(CTTdata,CTTkey, output.scored=TRUE)

# ICC for item 1
cttICC(myScores$score, myScores$scored[,1], colTheme="spartans", cex=1.5)
}
\keyword{ misc }
