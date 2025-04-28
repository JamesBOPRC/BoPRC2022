\name{NOFLakesRiversCyanobacteria}
\alias{NOFLakesRiversCyanobacteria}
\title{
NOF Assessment - Cyanobacteria (Lakes and lake fed rivers)}
\description{
This function will output the National Objectives Framework attribute bandings for Cyanobacteria from a given data set. This is calculated using a combination of Total Cyanobacteria and Potentially Toxic Cyanobacteria("Total Cyanobacteria" and "Potentially Toxic Cyanobacteria" WQ parameter - AQUARIUS) . Calculations are made using a subset of the data, based on the system time of the users computer. This subset is the last completed 3 year period from Jun to July. Alternate timeframes can be used through the 'time', 'start' and 'end' arguments.  Outputted attribute band value can be interpreted through the National Policy Statement for Freshwater Management 2020 or within the values section below.
}
\usage{
NOFLakesRiversCyanobacteria(data, ... time=Sys.Date(), start="", end="")
}
\arguments{
  \item{data}{
A data frame in the format; (site ID, name, time, value). Where value is "c("Total Cyanobacteria", "Potentially Toxic Cyanobacteria")".
}

\item{time}{
Creates a subset timeframe based on the last completed 3 year period (from Jun-July) from the inputted time.  Eg: time="2015-08-07" would create a subset time frame of 2012-07-01 to 2015-06-30. Optional.
}

\item{start}{
Allows a subset timeframe to be tested by specifying a start date. This argument must be used in conjunction with an 'end' date.  Optional
}

\item{end}{
Allows a subset timeframe to be tested by specifying an end date. This argument must be used in conjunction with a 'start' date. Optional
}
}

\value{
The outputted value reflects NOF attribute bandings with the following descriptions
\item{A}{
Risk exposure from cyanobacteria is no different to that in natural conditions (from any contact with freshwater).
}

\item{B}{
Low risk of health effects from exposure to cyanobacteria (from any contact with freshwater).
}

\item{C}{
Moderate risk of health effects from exposure to cyanobacteria (from any contact with freshwater).
}

\item{D}{
High health risks (for example, respiratory, irritation and allergy symptoms) exist from exposure to cyanobacteria  (from any contact with freshwater).
}

}

\section{Error Messages}{
"Incorrect dataframe"

This error occurs when the inputted data frame does not have 5 columns. Ensure that the data frame has 4 columns; Site ID, Site Name, Time and  2 Value columns, in that order.

"Incorrect parameter used"

This function uses the AQUARIUS WQ Parameters "Total Cyanobacteria" and "Potentially Toxic Cyanobacteria" when using the AQMultiExtract function. The data frame created should contain 2 columns with the titles "Total Cyanobacteria (mm^3/l)" and "Potentially Toxic Cyanobacteria (mm^3/l)". If it does not, this error will occur.

}

\examples{
#create a dataframe for site
Df<-AQMultiExtract("FL168384", c("Total Cyanobacteria","Potentially Toxic Cyanobacteria"))
#do a NOF Cyanobacteria assessment on the data frame, for the most recent 3 year period jun-july
NOFLakesRiversCyanobacteria(df)
}
