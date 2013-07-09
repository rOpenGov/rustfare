\name{ScrapeData_skip1}
\alias{ScrapeData_skip1}
\title{Scrape the raw html-table data (skipping the 1st row)}
\usage{
  ScrapeData_skip1(indicator)
}
\arguments{
  \item{indicator}{Character string. Select indicator from
  the \code{indicator} column of data frame returned by
  function \code{RosstatIndicator}.}
}
\value{
  data frame with raw data from html-table
}
\description{
  Scrape the raw html-table data from Rosstat Regional
  Statistics for tables with rowspan used on first row.
}
\examples{
# raw.html.data <- ScrapeData_skip1("")
}
\author{
  Markus Kainu <markuskainu(at)gmail.com>
}

