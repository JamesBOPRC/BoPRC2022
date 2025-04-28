\name{NOFRiversDRP}
\alias{NOFRiversDRP}
\title{
NOF Assessment - Dissolved Reactive Phosphorus
}
\description{
This function will output National Objective Framework attribute bandings for dissolved reactive phosphorus from a given dataset. This is calculated using "DRP" WQ parameter - AQUARIUS) . Calculations are made using a subset of the data, based on the system time of the users computer. This subset is the last completed 5 year period from Jan to Dec. Alternate timeframes can be used through the 'time', 'start' and 'end' arguments.  The outputted attribute band value can be interpreted through the National Policy Statement for Freshwater Management 2020 or within the values section below.
}
\usage{
NOFRiversDRP(data,...time=Sys.Date(), time="",start="",end="")
}

\arguments{
  \item{data}{
  a data frame in the format ; (site ID, name, time, value).  where value is "DRP"
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
Ecological communities and ecosystem processes are similar to those of natural reference conditions. No adverse effects attributable to dissolved reactive phosphorus enrichment are expected
}

\item{B}{
Ecological communities are slightly impacted by minor DRP elevation above natural reference conditions. If other conditions also favour eutrophication, sensitive ecosystems may experience additional algal and plant growth, loss of sensitive macroinvertebrate taxa, and higher respiration and decay rates
}

\item{C}{
Ecological communities are impacted by moderate DRP elevation above natural reference conditions. If other conditions also favour eutrophication, DRP enrichment may cause increased algal and plant growth, loss of sensitive macro-invertebrate and fish taxa, and high rates of respiration and decay
}

\item{D}{
 Ecological communities impacted by substantial DRP elevation above natural reference conditions. IN combination with other conditions favouring eutrophication, DRP enrichment drives excessive primary production and significant changes in macroinvertebrate and fish communities, as taxa sensitive to hypoxia are lost.
}
}

\section{Error messages}{
"Incorrect data frame"

This error occurs when the inputted data frame does not have 4 columns. Ensure that the data frame has 4 columns; Site ID, Site Name, Time , Value in that order.


"Incorrect parameter used"

This function uses the AQUARIUS WQ Parameter "DRP" when using the AQMultiExtract function. The data frame created should contain a column with the title "DRP (g/m^3)" . If it does not, this error will occur.

}


\examples{
#create a dataframe
Df<-AQMultiExtract("FN834668", "DRP")
#do a NOF Periphyton assessment on the data frame, for the most recent 5 year period jan-dec
NOFRiversDRP(Df)
}
