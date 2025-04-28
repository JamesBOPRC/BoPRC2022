\name{Hazen.Percentile}
\alias{Hazen.Percentile}
\title{
Hazen Percentile Calculation
}
\description{
This function calculates percentiles using the Hazen method. The Hazen method formula is 1/2+pn, where p is the percentile fraction and n is the number of observations.  This calculation can also be performed through the Ministry for the Environment's Excel file that can be accessed through their website.
}
\usage{
Hazen.Percentile(data, percentile)
}
\arguments{
  \item{data}{
Data to be used for the Hazen Percentile calculation
}
\item{percentile}{
The desired percentile as a whole number
}
}

\section{Error Messages}{
"Data class must be either numeric or integer"


This error occurs when the class of the data is not numeric or integer.
To check what the class of the data input is, run:
class(data)
If the output is not numeric or integer, use the following code to transform the data into the correct class.

Data<-as.numeric(data)

Additionaly, ensure that the data is not in quotation marks (eg. "c(1:10)" is classed as a character, whereas c(1:10) is classed as an integer) and if a data frame is being used, that it is correctly subsetted (eg. Df[,x] , where x is the column of interest)

"Percentile class must be either numeric or integer"

This error occurs when the class of the percentile is not numeric or integer. To check what the class of the data input is, run:
class(data)
If the output is not numeric or integer, use the following code to transform the data into the correct class.

Data<-as.numeric(data)

Additionaly, ensure that the data is not in quotation marks (eg. "98" is classed as a character, whereas 98 is classed as numeric)

"Not enough data"

Depending on the desired percentile, there are differing number of data points required. To find this number, run: Hazen.N(percentile)

}

\examples{
#Create a dummy dataset
data<-rnorm(10000, mean=50,sd=10)
# Calculate the Hazen 95th percentile
Hazen.Percentile (data, 95)
