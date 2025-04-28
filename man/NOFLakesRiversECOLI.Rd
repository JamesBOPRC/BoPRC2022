\name{NOFLakesRiversECOLI}
\alias{NOFLakesRiversECOLI}
\title{
NOF Assessment - E.Coli (Lakes and Rivers)}
\description{
This function will output the National Objectives Framework attribute bandings for E. Coli  from a given data set. This is calculated using the "E Coli"  WQ parameter  from AQUARIUS. The final attribute band is calculated using the percent exceedances over 540/100mL, percent exceedances over 260/100mL, median concentration /100mL and 95th percentile of E. Coli/100mL.   This attribute state is determined by satisfying all numeric attribute states .Calculations are made using a subset of the data, based on the system time of the users computer. This subset is the last completed 1 year period from Jan to Dec. Alternate timeframes can be used through the 'time', 'start' and 'end' arguments.The outputted attribute band value can be interpreted through the National Policy Statement for Freshwater Management 2020 or within the values section below.  This function also outputs a swim ability status.
}
\usage{
NOFLakesRiversECOLI(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
 A data frame in the format; (site ID, name, time, value). Where value is "E Coli".}

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
The predicted average infection risk is 1 percent
}

\item{B}{
The predicted average infection risk is 2 percent
}

\item{C}{
The predicted average infection risk is 3 percent
}

\item{D}{
The predicted average infection risk is >3 percent
}

\item{E}{
The predicted average infection risk >7 percent
}

}

\section{Error Messages}{
"Incorrect data frame"

This error occurs when the inputted data frame does not have 4 columns. Ensure that the data frame has 4 columns; Site ID, Site Name, Time and  Value, in that order.

"Incorrect parameter used"

This function uses the AQUARIUS WQ Parameter "E coli" when using the AQMultiExtract function. The data frame created should contain a column with the title "E.Coli (cfy/100ml)" . If it does not, this error will occur.

}


\examples{
#create a dataframe
Df<-AQMultiExtract("RN123610", "E coli")
#do a NOF Total phosphurus assessment on the data frame, for the most recent 1 year period jan-dec
NOFLakesRiversECOLI(Df)
