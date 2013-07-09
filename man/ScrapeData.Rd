\name{ScrapeData}
\alias{ScrapeData}
\title{Scrape the raw html-table data}
\usage{
  ScrapeData(indicator = "mortality_rate")
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
  Statistics
}
\examples{
# raw.html.data <- ScrapeData("")
}
\author{
  Markus Kainu <markuskainu(at)gmail.com>
}

