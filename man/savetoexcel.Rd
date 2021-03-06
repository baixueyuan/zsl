% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zslSaveToExcel.R, R/zslSaveToExcelP.R
\name{SaveToExcel}
\alias{SaveToExcel}
\alias{zslSaveToExcel}
\alias{zslSaveToExcelP}
\title{Save zsl Result to the Excel File}
\usage{
zslSaveToExcel(data, output.file = "zsl.xlsx", assign = FALSE)

zslSaveToExcelP(data, wb, output.file = "zsl.xlsx")
}
\arguments{
\item{data}{the data to be saved to Excel file.}

\item{output.file}{the file path of the ouput, if missed, it is set to
"zsl.xlsx" in the working directory.}

\item{assign}{logical, default is FALSE, if TRUE, the workbook object
generated by \code{XLConnect} package will be assigned to the Global
environment, named "wb".}

\item{wb}{the \code{workbook} object, used in \code{zslSaveToExcel} to add
worksheet, it can be missed and generate a new Excel file with template.}
}
\value{
No obvious return, the data will be saved to Excel file.
}
\description{
Save the data to Excel file which is generated by \code{zslTwoDayDiff} and
other functions giving the comparison result.
}
\details{
For the convenience of viewing the result, the function changed somewhere to
Chinese, including the "mark" column and the column names.

The funcion uses \code{XLConnect} package to manipulate the Excel file. For
the format, the style action is set to "NAME_PREFIX" and the style is already
adjusted well in the "template.xlsx".

Besides, using param \code{assign} can assign the workbook object to the
Global environment. This is uesful for generating multiple sheets Excel file.

The function \code{zslSaveToExcelP} saves the data returned by
\code{zslPeriodDiff}.
}

