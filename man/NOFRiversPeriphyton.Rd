\name{NOFRiversPeriphyton}
\alias{NOFRiversPeriphyton}
\title{
NOF Assessment -Periphyton (Rivers)
}
\description{
This function will output National Objective Framework attribute bandings for Periphyton from a given dataset. This is calculated using milligrams of chloropyhll-a per square meter ("Chloro Periphyton" WQ parameter - AQUARIUS). Either default, or productive class can be used, Calculations are made using a subset of the data, based on the system time of the users computer. This subset is the last completed 3 year period from Jan to Dec. Alternate timeframes can be used through the 'time', 'start' and 'end' arguments.  The outputted attribute band value can be interpreted through the National Policy Statement for Freshwater Management 2020 or within the values section below.}
\usage{
NOFRiversPeriphyton(data,...time=Sys.Date(),start="",end="",class="default")
}
\arguments{
  \item{data}{
  A data frame in the format ; (site ID, name, time, value).  where value is "Chloro Periphyton"
  }

\item{time}{
Creates a subset timeframe based on the last completed 3 year period (from Jun- July) from the inputted time.  Eg: time="2015-08-07" would create a subset time frame of 2012-01-01 to 2014-12-31. Optional.
}

\item{start}{
Allows a subset timeframe to be tested by specifying a start date. This argument must be used in conjunction with an 'end' date.  Optional
}

\item{end}{
Allows a subset timeframe to be tested by specifying an end date. This argument must be used in conjunction with a 'start' date. Optional
}

\item{class}{
Used to specify between Default class (default) or Productive.
}
}


\value{
The outputted attriubute band value can be interpreted as;
\item{A}{
Rare blooms reflecting negligible nutrient enchricement and/or alteration of the natural flow regime or habitat
}

\item{B}{
Occasional blooms reflecting low nutrient enrichment and/or alteration of the natural flow regime or habitat
}

\item{C}{
Periodic short- duration nuisance blooms reflecting moderate nutrient enrichment and/or moderate alteration of the natural flow regime or habitat
}

\item{D}{
Regular and/or extended-duration nuisance blooms reflecting high nutrient enrichment and/or significant alteration of the natural flow regime or habitat
}
}

\section{Error Messages}{
"Incorrect data frame"

This error occurs when the inputted data frame does not have 4 columns. Ensure that the data frame has 4 columns; Site ID, Site Name, Time , Value in that order.

"Incorrect parameter used"

This function uses the AQUARIUS WQ Parameter "Chloro Periphyton" when using the AQMultiExtract function. The data frame created should contain a column with the title "Chloro Periphyton (mg/m^2)" . If it does not, this error will occur.

"Incorrect class"

This function will only accept;

-	Productive

-	productive

-	Default

-	default

}

\examples{
#create a dataframe
Df<-AQMultiExtract("EO451883", "Chloro Periphyton")
#do a NOF Periphyton assessment on the data frame, for the most recent 3 year period jan-dec
NOFRiversPeriphyton (Df)
}
