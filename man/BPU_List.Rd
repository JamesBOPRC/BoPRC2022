\name{BPU_List}
\alias{BPU_List}
\title{
Get Site ID list for biophysical unit}
\description{
This function will output a list of site IDs that correspond to an inputted biophysical unit. The biophysical unit types are; non-volcanic steep slope ("Non_VA/Steep"), non-volcanic gentle slope ("Non_VA/Gentle") , volcanic steep slope ("VA/Steep") , and volcanic gentle slope ("VA/Gentle").}
\usage{
BPU_List(BPU)
}
\arguments{
  \item{BPU}{
The biophysical unit of interest. ("Non_VA/Steep","Non_VA/Gentle", "VA/Steep","VA/Gentle")
}
}
\value{
A list of AQUARIUS site IDs corresponding to the biophysical unit.
}

\section{Error Messages}{
"Biophysical unit not recognised"

If this error has occurred, BPU argument (above) is incorrect. Ensure you are inputting one of the biophysical units listed, with the correct spelling and in quotation marks.

}
