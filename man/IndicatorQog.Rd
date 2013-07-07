\name{IndicatorQog}
\alias{IndicatorQog}
\title{List the currently available
indicators from Quality of Governance database}
\usage{
  IndicatorQog(urli = "http://www.qog.pol.gu.se/digitalAssets/1358/1358067_qog_tsd_csv_v6apr11.csv")
}
\arguments{
  \item{urli}{is the url address to the data}
}
\value{
  string of names of available indicators
}
\description{
  List the currently available indicators from Quality of
  Governance database
  \url{http://www.qog.pol.gu.se/data/datadownloads/qogstandarddata/}
}
\examples{
# available.indicators <- IndicatorQog()
}
\author{
  Markus Kainu <markuskainu(at)gmail.com>
}

