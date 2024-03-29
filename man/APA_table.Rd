% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general_functions.R
\name{APA_table}
\alias{APA_table}
\title{Apply some standard APA format options to a flextable}
\usage{
APA_table(table_object, table_title, include_note)
}
\arguments{
\item{table_object}{The table object you want to turn into an APA-style table. Can be a data frame or a tibble.}

\item{table_title}{The title you want to add to the table}

\item{include_note}{Option to add a note to the end of the table as a footer. Can either be a string of text, or FALSE if you do not wish to add any notes.}
}
\description{
Quickly set the font size to 11; the font style to Times New Roman; add two header lines with the table number and title; and auto fit all columns.
}
