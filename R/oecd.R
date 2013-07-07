# This file is part of the rustfare program (https://github.com/muuankarski/rustfare)

# Copyright (C) 2012-2013 Markus Kainu <markuskainu@gmail.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


#' List the currently available 
#' indicators from OECD Country statistical profile
#'
#' List the currently available 
#' indicators from OECD Country statistical profile at:
#' \url{http://www.oecd-ilibrary.org/economics/country-statistical-profile-russian-federation_20752288-table-rus}
#'
#' @param urli is the url address to the data
#'
#' @return data frame with indicator names
#' 
#' @export
#' @examples # available.indicators <- IndicatorOecd()
#' @author Markus Kainu <markuskainu(at)gmail.com> 

IndicatorOecd <- function(urli = "http://research.muuankarski.org/rustfare/data/IndicatorOecd.RData") {
  load(url(urli))
  IndicatorOecd
}

#' Download data from OECD Country statistical profile: Russian Federation
#' 
#' Download data from OECD Country statistical profile: Russian Federation
#' \url{http://www.oecd-ilibrary.org/economics/country-statistical-profile-russian-federation_20752288-table-rus}
#'
#' @param indicator Character string. Select indicator from the \code{indicator} column 
#' of data frame returned by function \code{OecdIndicator}. More than one indicator 
#' is NOT allowed.
#'  
#' @return data frame with 4 columns
#' \itemize{
#'  \item \code{indicator} Name of the indicator
#'  \item \code{unit} unit the indicator
#'  \item \code{value} Value of the indicator
#'  \item \code{year} Year of observation
#'  }
#' 
#' @export
#' @examples # dat <- GetOecd("public_expenditure_on_health")
#' @author Markus Kainu <markuskainu(at)gmail.com> 


GetOecd <- function(measure="public_expenditure_on_health") {
  library(XML)
  library(stringr)
  library(reshape2)
  
  url <- "http://www.oecd-ilibrary.org/previewsites/csp-rus-table-2013-1-en/index.html?webPreview=true"
  tables <- readHTMLTable(url, header=TRUE, trim=TRUE)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  df <- tables[[which.max(n.rows)]]
  df <- df[5:124,2:11]
  colnames(df) <- c("indicator","unit","2004","2005","2006",
                    "2007","2008","2009","2010","2011")
  df.long <- melt(df, id.vars=c("indicator","unit"))
  df.long$value <- str_replace_all(df.long$value, " ", "")
  df.long$value <- str_extract(df.long$value, "[0-9]+.[0-9]")
  df.long$indicator <- str_replace_all(df.long$indicator, " ", "_")
  df.long$indicator <- tolower(df.long$indicator)
  df.long$indicator <- factor(df.long$indicator)
  df.long$year <- df.long$variable
  df.long$variable <- NULL
  oecd.data <- subset(df.long, indicator == measure)
  oecd.data
}