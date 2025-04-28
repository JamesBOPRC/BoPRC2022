\name{NOFLakesTN}
\alias{NOFLakesTN}
\title{
NOF Assessment - Total Nitrogen (Lakes)
}
\description{
This function will output the National Objectives Framework attribute bandings for total nitrogen from a given data set. This is calculated using total nitrogen concentration ("N (Tot)" WQ parameter - AQUARIUS) . This be applied to either seasonally stratified (default) and brackish lakes or polymictic lakes. . The final attribute band is calculated using the median.  Calculations are made using a subset of the data, based on the system time of the users computer. This subset is the last completed 1 year period from Jun to July. Alternate timeframes can be used through the 'time', 'start' and 'end' arguments.   The outputted attribute band value can be interpreted through the National Policy Statement for Freshwater Management 2020 or within the values section below.
}
\usage{
NOFLakesTN(data,...time=Sys.Date(),start="",end="",laketype="Stratified")
}
\arguments{
  \item{data}{
A data frame in the format; (site ID, name, time, value). Where value is "N (Tot)".
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

\item{laketype}{
Used to specify whether the lake type is stratified (default) or polymictic
}
}

\value{
The outputted value reflects NOF attribute bandings with the following descriptions

\item{A}{
Lake ecological communities are healthy and resilient, similar to natural reference conditions.
}

\item{B}{
Lake ecological communities are slightly impacted by additional algal and/or plant growth arising from nutrient levels that are elevated above natural reference conditions.
}

\item{C}{
Lake ecological communities are moderately impacted by additional algal and plant growth arising from nutrient levels that are elevated well above natural reference conditions.
}

\item{D}{
Lake ecological communities have undergone or are at high risk of a regime shift to a persistent, degraded state (without native macrophyte/seagrass cover), due to impacts of elevated nutrients leading to excessive algal and/or plant growth, as well as from losing oxygen in bottom waters of deep lakes.
}

}

\section{Error Messages}{
"Incorrect data frame"

This error occurs when the inputted data frame does not have 4 columns. Ensure that the data frame has 4 columns; Site ID, Site Name, Time and  Value, in that order.

"Incorrect parameter used"

This function uses the AQUARIUS WQ Parameter "N (Tot)" when using the AQMultiExtract function. The data frame created should contain a column with the title "N (Tot) (g/m^3)" . If it does not, this error will occur.

"Incorrect lake type used"

This function will only accept the following lake types;

-	Stratified (default)

-	stratified

-	Polymictic

-	polymitic

Ensure you are using the correct lake type
}


\examples{
#create a dataframe
Df<-AQMultiExtract("ML715056", "N (Tot)")
#do a NOF Total nitrogen assessment on the data frame, for the most recent 1 year period jun-july
NOFLakesRiversTN(df)
}
