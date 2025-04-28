\name{NOFRiversNO3}
\alias{NOFRiversNO3}
\title{
NOF Assessment - Nitrate Toxicity (Rivers)
}
\description{
This function will output the National Objectives Framework attribute bandings for nitrate toxicity from a given data set. This is calculated using nitrate toxicity ("Nitrite Nitrate (as N)" WQ parameter - AQUARIUS) . Calculations are made using a subset of the data, based on the system time of the users computer. This subset is the last completed 1 year period from Jan to Dec. Alternate timeframes can be used through the 'time', 'start' and 'end' arguments. The outputted attribute band value can be interpreted through the National Policy Statement for Freshwater Management 2020 or within the values section below.
}
\usage{
NOFRiversNO3(data,...time=Sys.Date(),start="",end="")
}
\arguments{
  \item{data}{
  A data frame in the format; (site ID, name, time, value). Where value is "Nitrite Nitrate (as N)".
}

\item{time}{
Creates a subset timeframe based on the last completed 1 year period (from Jan-Dec) from the inputted time.  Eg: time="2015-08-07" would create a subset time frame of 2014-01-01 to 2014-12-31. Optional.
}

\item{start}{
Allows a subset timeframe to be tested by specifying a start date. This argument must be used in conjunction with an 'end' date.  Optional
}

\item{end}{
Allows a subset timeframe to be tested by specifying an end date. This argument must be used in conjunction with a 'start' date. Optional
}
}


\value{
\item{A}{
High conservation value system. Unlikely to be effects even on sensitive species
}

\item{B}{
Some growth affect on up to 5 percent of species
}

\item{C}{
Growth effects on up to 20 percent of species (mainly sensitive species such as fish)
}

\item{D}{
Impacts on growth of multiple species, and starts approaching acute impact level (that is, risk of death) for sensitive species at higher concentrations (>20mg/L)
}
}

\section{Error Messages}{
"Incorrect data frame"

This error occurs when the inputted data frame does not have 4 columns. Ensure that the data frame has 4 columns; Site ID, Site Name, Time , Value in that order.

"Incorrect parameter used"

This function uses the AQUARIUS WQ Parameter "Nitrite Nitrate (as N)" when using the AQMultiExtract function. The data frame created should contain a column with the title "Nitrite Nitrate (g/m^3)" . If it does not, this error will occur.
}



\examples{
#create a dataframe
Df<-AQMultiExtract("DP784306", "Nitrite Nitrate (as N)")
#do a NOF Ammonia Toxicity assessment on the data frame, for the most recent 1 year period Jan-Dec
NOFRiversNO3 (Df)
}
