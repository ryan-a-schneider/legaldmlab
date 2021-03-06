% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general_functions.R
\name{reformat_Qualtrics_datetime}
\alias{reformat_Qualtrics_datetime}
\title{Remove the time stamps from a Qualtrics data files by mutating the start and end date columns}
\usage{
reformat_Qualtrics_datetime(df, remove_timestamps = TRUE)
}
\arguments{
\item{df}{A survey import that has the columns "start_date" and "end_date", both of which will be subjected to mutation to reformat their respective times.}

\item{remove_timestamps}{If this option is set to TRUE, the time will be removed from both columns and the formatted columns will contain the date only. Setting it to FALSE leaves the time and reformats the column from a character structure to a date-time structure.}
}
\description{
Remove the time stamps from a Qualtrics data files by mutating the start and end date columns
}
\examples{
reformat_Qualtrics_datetime()
}
