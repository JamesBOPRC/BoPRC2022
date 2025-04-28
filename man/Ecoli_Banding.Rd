\name{Ecoli_Banding}
\alias{Ecoli_Banding}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
NOF Assessment - Ec.Coli banding using alternate methods
}
\description{
This function will output the National Objectives Framework attribute bandings for E. Coli from inputted values. This function is to be used if an error occurs with the NOFLakesRiversECOLI function, due to a site occupying multiple bands at once. Using either mean, max or 95th percentile methods, the function will compute attribute bands using the percent exceedances over 540/100mL, percent exceedances over 260/100mL, median concentration /100mL and 95th percentile of E. Coli/100mL. The default method is maximum, which is used in the NOFLakesRiversECOLI function.   The outputted attribute band value can be interpreted through the National Policy Statement for Freshwater Management 2020 or within the values section below. }
\usage{
Ecoli_Banding(PercentExcced540,PercentExceed260,Median,Percenitle95, method="max")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PercentExceed540}{
  This value represents the percentage of Ecoli exceedances over 540/100mL. It can be obtained from the NOFLakesRiversECOLI function output.
}

\item{PercentExceed260}{
This value represents the percentage of Ecoli exceedances over 260/100mL. It can be obtained from the NOFLakesRiversECOLI function output.
}

\item{Median}{
This value represents the median Ecoli value. It can be obtained from the NOFLakesRiversECOLI function output
}

\item{Percentile95}{
This value represents the 95th percentile of Ecoli. It can be obtained from the NOFLakesRiversECOLI function outputs
}

\item{method}{
Allows ECOLI attribute banding to be calculated using alternate methods ("95th", "mean", "max") with max being the default method.
}
}


\value{
The Proposed Band output is representative of NOF Bandings with the following descriptions

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
The predicted average infection risk is >7 percent
}
}

\section{Error Messages}{
"Incorrect Method"

This function only accepts the methods max, mean and 95th. These can be only written as;

-	max

-	MAX

-	Max

-	mean

-	MEAN

-	Mean

-	95th

-	95TH
}

