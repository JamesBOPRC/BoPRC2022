\name{NOFLakesPhytoplankton}
\alias{NOFLakesPhytoplankton}
\title{
NOF Assessment - Phytoplankton (Lakes)
}
\description{
This function will output the National Objectives Framework attribute bandings for Phytoplankton from a given dataset. This is calculated using milligrams chlorophyll-a per cubic meter ("Chloro a" WQ parameter - AQUARIUS).  The final attribute band reflects the lowest of the two calculated median and maximum bands for each site.  Calculations are made using a subset of the data, based on the system time of the users computer. This subset is the last completed 1 year period from Jun to July. Alternate timeframes can be used through the 'time', 'start' and 'end' arguments. The outputted attribute band value can be interpreted through the National Policy Statement for Freshwater Management 2020 or the value section below.}
\usage{
NOFLakesPhytoplankton(data, ... time=Sys.Date(), start="", end="")
}
\arguments{
  \item{data}{
A data frame in the format; (site ID, name, time, value). Where value is "Chloro a".
}

\item{time}{
Creates a subset timeframe based on the last completed 1 year period (from Jun-July) from the inputted time.  Eg: time="2015-08-07" would create a subset time frame of 2014-07-01 to 2015-06-30. Optional.
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
Lake ecological communities are healthy and resilient, similar to natural reference conditions
}

\item{B}{
Lake ecological communities are slightly impacted by additional algal and/or plant growth arising from nutrient levels that are elevated above natural reference conditions.
}

\item{C}{
Lake ecological communities are moderately impacted by additional algal and plant growth arising from nutrient levels that are elevated well above natural reference conditions. Reduced water clarity is likely to affect habitat available for native macrophytes

}
}

\section{Error Messages}{
" Incorrect dataframe"

This error occurs when the inputted data frame does not have 4 columns. Ensure that the data frame has 4 columns; Site ID, Site Name, Time and Value in that order.

"Incorrect parameter used"

This function uses the AQUARIUS WQ Parameter "Chloro a", when using the AQMultiExtract function. The data frame created should have the 4th column name as "Chloro a (mg/m^3") .If it does not, this error will occur.
}




\examples{
#create a dataframe
Df<-AQMultiExtract("FJ295574", “Chloro a”)

#do a NOF Phytoplankton assessment on the dataframe, for the most recent 1 year period jun-july
NOFLakesPhytoplankton(Df)
}
