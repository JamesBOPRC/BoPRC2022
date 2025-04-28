\name{Convert.Columns}
\alias{Convert.Columns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Change column classes}
\description{
This function coverts columns within data frames to different classes. These classes are; numeric, character, factor or integer. }
\usage{
Convert.Columns(data, class)
}
\arguments{
  \item{data}{
Data frame to convert column classes
}
\item{class}{
Vector specifying column classes in order
}
}
\section{Error Messages}{
"Incorrect class type"

This error occurs when at least one of the inputted class types is not numeric, integer, character or factor. It will also be caused by the class types not being in quotation marks (correctly shown in the example below), or a spelling mistake.
}

\seealso{
\code{\link{as.numeric}},  \code{\link{as.integer}},   \code{\link{as.character}}    , \code{\link{as.factor}}
}
\examples{
# To convert a data frame with 3 columns to numeric, factor and character classes (from left to right)

Convert.Columns(data, c("numeric","factor","character"))
}
