\name{Write.Excel}
\alias{Write.Excel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert data frame into Excel format

}
\description{
This function will copy a given dataframe on to the user clipboard, in a format that can
be pasted directly into an Excel table}
\usage{
Write.Excel(data, ... row.names=FALSE, col.names=TRUE)
}
\arguments{
\item{data}{
The data frame to be converted into Excel format'
}
\item{row.names}{
If row.names=TRUE, the table pasted into Excel will have numbered rows.
}
\item{col.names}{
If col.names = FALSE, the table pasted into Excel will not have column titles. Any titles will be consistent with the data frame
}
}
