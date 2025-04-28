\name{NOFRiversSFS}
\alias{NOFRiversSFS}
\title{
NOF Assessment - Suspended Fine Sediment (Rivers)
}
\description{
This function will output National Objective Framework attribute bandings for suspended fine sediment from a given dataset. This is calculated using "Water Clarity" WQ parameter - AQUARIUS) . Calculations are made using a subset of the data, based on the system time of the users computer. This subset is the last completed 5 year period from Jan to Dec. Alternate timeframes can be used through the "time", "start" and "end" arguments.  The outputted attribute band value can be interpreted through the National Policy Statement for Freshwater Management 2020 or within the values section below.
}
\usage{
NOFRiversSFS(data, class,.. time=Sys.Date(), start="", end="")
}

\arguments{
  \item{data}{
A data frame in the format ; (site ID, name, time, value).  where value is "Water Clarity"
}

\item{class}{
Define a suspended sediment class (1, 2, 3, 4)
}

\item{time}{
Creates a subset timeframe based on the last completed 5 year period (from Jan-dec) from the inputted time.  Eg: time="2015-08-07" would create a subset time frame of 2010-01-01 to 2014-12-31. Optional.
}

\item{start}{
Allows a subset timeframe to be tested by specifying a start date. This argument must be used in conjunction with an 'end' date.  Optional
}

\item{end}{
Allows a subset timeframe to be tested by specifying an end date. This argument must be used in conjunction with a 'start' date. Optional
}
}

\value{
The outputted attribute band value can be interpreted as;
\item{A}{
Minimal impact of suspended sediment on instream biota. Ecological communities are similar to those observed in natural reference conditions
}

\item{B}{
Low to moderate impact of suspended sediment on instream biota. Abundance of sensitive fish species may be reduced
}

\item{C}{
Moderate to high impact of suspended sediment on instream biota. Sensitive fish species may be lost.
}

\item{D}{
High impact of suspended sediment on instream biota. Ecological communities are significantly altered and sensitive fish and macroinvertebrates species are lost or at high risk of being lost.
}
}
\section{Error Messages}{
"Incorrect data frame"

This error occurs when the inputted data frame does not have 4 columns. Ensure that the data frame has 4 columns; Site ID, Site Name, Time , Value in that order.

"Incorrect parameter used"

This function uses the AQUARIUS WQ Parameter "Water Clarity" when using the AQMultiExtract function. The data frame created should contain a column with the title "Water Clarity (m)" . If it does not, this error will occur.

}

\examples{
#create a dataframe
Df<-AQMultiExtract("LI953392", "Water Clarity")
#do a NOF Suspended fine sediment assessment on the data frame, for the most recent 5 year period jan-dec
NOFRiversSFS(Df)
}
