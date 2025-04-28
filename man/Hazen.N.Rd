\name{Hazen.N}
\alias{Hazen.N}
\title{
Determine number of data points needed for Hazen Percentile calculation
}
\description{
This function will output the minimum number of data points needed for a Hazen Percentile calculation based off the percentile value. }
\usage{
Hazen.N(percentile)
}
\arguments{
  \item{percentile}{
Percentile value as a whole number
}
}

\examples{
# to find out how many data points are needed for a 95th percentile Hazen calculation
Hazen.N(95)
}
