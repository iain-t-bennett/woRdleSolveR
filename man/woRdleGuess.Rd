% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/woRdleGuess.R
\name{woRdleGuess}
\alias{woRdleGuess}
\title{woRdleGuess}
\usage{
woRdleGuess(word, response)
}
\arguments{
\item{word}{5 letter word}

\item{response}{is the colour update (5 char string with each char G,Y,0)}
}
\value{

}
\description{
Response defines how good the guess was
G is green, Y is yellow, 0 is grey
e.g. G0000 means Green for postion 1, grey for the rest
}
\examples{
x <- woRdleGuess("APPLE", "YGG00")
print(x)
}
