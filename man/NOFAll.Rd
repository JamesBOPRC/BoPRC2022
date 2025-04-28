\name{NOFAll}
\alias{NOFAll}
\title{
Simultaneously compute all NOF bands for a site}
\description{
This function will output all applicable NOF bands for a given site. The applicable NOF assessments are determined by whether the inputted site is within the NERM_River() or lakelist() functions. Outputted bands are calculated with the default arguments of the individual NOF functions, however these can be altered within the function. For more information on the NOF values see the help files for the NOF functions. }
\usage{
NOFAll(site,... time=Sys.Date(),start="",end="", periclass="default". TNLakeType="stratified", AmonAdjust=True, SFSClass=1)
}
\arguments{
  \item{site}{
AQUARIUS site ID of interest, in quotation marks
}

\item{time}{
The time argument allows the subsetted end date to be altered whilst maintaining the default intervals of each function.
}

\item{start}{
Allows a subset timeframe to be tested by specifying a start date. This argument must be used in conjunction with an 'end' date and will be applied to all NOF bands  Optional
}

\item{end}{
Allows a subset timeframe to be tested by specifying an end date. This argument must be used in conjunction with a 'start' date and will be applied to all nof bands.    Optional
}

\item{periclass}{
Define the class used for the NOFRiversPeriphyton function, optional
}

\item{TNLakeType}{
Define the lake type used in the NOFLakesTN function, optional
}

\item{AmonAdjust}{
Define whether pH values should be adjusted within the NOFLakesRiversNH4 function, optional
}

\item{SFSClass}{
Define the suspended sediment class to be used in the NOFRiversSFS function, optional
}
}

\section{Error messages}{
"Incorrect site input"

Site input must be of class character, this means it has to be in quotation marks

"Site not found"

This function works by searching for the inputted site name within the NERMN_River() and lakelist() functions. If this error has occurred, the inputted site was not found. If you believe that a site should be within this function, contact James or EDS.

}
