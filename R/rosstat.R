# This file is part of the rustfare program (https://github.com/muuankarski/rustfare)

# Copyright (C) 2012-2013 Markus Kainu <markuskainu@gmail.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


#' List the currently available 
#' indicators from Rosstat Regional Statistics
#' 
#' RosstatIndicator loads data frame that lists 
#' currently available indicators from Rosstat Regional Statistics
#'
#' @param urli is the url address to the data
#'
#' @return data frame with indicator name, url, levels of aggregation and metadata in Russian and in English
#' 
#' @export
#' @examples # available.indicators <- IndicatorRosstat()
#' @author Markus Kainu <markuskainu(at)gmail.com> 

IndicatorRosstat <- function(urli = "http://research.muuankarski.org/rustfare/data/RosstatIndicatorData.RData") {
  load(url(urli))
  RosstatIndicatorData
}

#' Scrape the raw html-table data
#' 
#' Scrape the raw html-table data from Rosstat Regional Statistics
#'
#' @param indicator Character string. Select indicator from the \code{indicator} column 
#' of data frame returned by function \code{RosstatIndicator}.
#'
#' @return data frame with raw data from html-table
#' 
#' @export
#' @examples # raw.html.data <- ScrapeData("")
#' @author Markus Kainu <markuskainu(at)gmail.com> 

ScrapeData <- function(indicator="mortality_rate"){
  library(XML)
  load(url("http://research.muuankarski.org/rustfare/data/RosstatIndicatorData.RData"))
  condition <- indicator
  urldata <- subset(RosstatIndicatorData, indicator == condition)
  url <- as.character(urldata[1,2])
  x <- readHTMLTable(url, header = TRUE, dec=",")
  x=x[[1]]
  x
}

#' Scrape the raw html-table data (skipping the 1st row)
#' 
#' Scrape the raw html-table data from Rosstat Regional Statistics
#' for tables with rowspan used on first row.
#'
#' @param indicator Character string. Select indicator from the \code{indicator} column 
#' of data frame returned by function \code{RosstatIndicator}.
#'
#' @return data frame with raw data from html-table
#' 
#' @export
#' @examples # raw.html.data <- ScrapeData_skip1("")
#' @author Markus Kainu <markuskainu(at)gmail.com> 

ScrapeData_skip1 <- function(indicator){
  library(XML)
  load(url("http://research.muuankarski.org/rustfare/data/RosstatIndicatorData.RData"))
  condition <- indicator
  urldata <- subset(RosstatIndicatorData, indicator == condition)
  url <- as.character(urldata[1,2])
  x <- readHTMLTable(url, header = TRUE, dec=",", skip.rows=1)
  x=x[[1]]  
  x
}



#' Download data from Rosstat Regional Statistics
#' 
#' Download data based on indicator and aggregation level 
#' from Rosstat Regional Statistics
#'
#' @param indicator Character string. Select indicator from the \code{indicator} column 
#' of data frame returned by function \code{RosstatIndicator}. More than one indicator is NOT allowed.
#' @param level Character string. Defines the administrative level of aggregation 
#' \itemize{
#'   \item \code{federal} at the federal level
#'   \item \code{federal_district} at the federal district level (8 regions)
#'   \item \code{region} at the regional level (83 regions)
#' } More than one level is NOT allowed.
#'  
#' @return data frame with 7 columns
#' \itemize{
#'  \item \code{region} Region name in Russian
#'  \item \code{region_en} Region name in English
#'  \item \code{year} Year of observation
#'  \item \code{value} Value of the indicator
#'  \item \code{level} Aggregation level
#'  \item \code{id_shape} id number for mergin with shapefile from GADM
#'  \item \code{indicator} name of the indicator
#' }
#' 
#' @export
#' @examples # dat <- GetRosstat("life_expectancy_men","federal_district")
#' @author Markus Kainu <markuskainu(at)gmail.com> 

GetRosstat <- function(indicator = "mortality_rate", level = "federal_district"){
  library(XML)
  library(reshape2)
  library(stringr)
  # extract columns with TOTAL values
  #   if (indicator %in% c("sports_halls",
  #                        "swimming_pools",
  #                        "planar_sports_facilities_(playgrounds_and_fields)",
  #                        "stadiums_with_stands_for_1500_seats_or_more",
  #                        "life_expectancy_total",
  #                        "life_expectancy_men",
  #                        "life_expectancy_women",
  #                        "population_total",
  #                        "population_urban",
  #                        "population_rural")) {
  #     load(url("http://research.muuankarski.org/rustfare/data/RosstatIndicatorData.RData"))
  #     condition <- indicator
  #     urldata <- subset(RosstatIndicatorData, indicator == condition)
  #     url <- as.character(urldata[1,2])
  #     x <- readHTMLTable(url, header = TRUE, dec=",")
  #     x=x[[1]]
  #     x  
  #   }  
  #   else {
  #     load(url("http://research.muuankarski.org/rustfare/data/RosstatIndicatorData.RData"))
  #     condition <- indicator
  #     urldata <- subset(RosstatIndicatorData, indicator == condition)
  #     url <- as.character(urldata[1,2])
  #     x <- readHTMLTable(url, header = TRUE, dec="," , skip.rows=1
  #                        )
  #     x=x[[1]]  
  #     x
  #   }
  ##########################################
  # extracting the right columns from non-harmonized html-tables
  
  if (indicator == "average_percapita_income") {
    x <- suppressWarnings(ScrapeData(indicator))
    y <- x[,c(1,3:14)]
    names(y) <- c("region","x1995","x2000","x2001",
                  "x2002","x2003","x2004","x2005",
                  "x2006","x2007","x2008","x2009","x2010")
    y.long <- melt(y, id.vars="region")
    
  }
  if (indicator == "gross_regional_product") {
    x <- suppressWarnings(ScrapeData_skip1(indicator))  
    y <- x
    names(y) <- c("region","x2000","x2002","x2003","x2004","x2005",
                  "x2006","x2007","x2008","x2009","x2010")
    y.long <- melt(y, id.vars="region")
  }
  ######################################
  # Population
  if (indicator == "population_total") {
    x <- suppressWarnings(ScrapeData_skip1(indicator))  
    y <- x[,c(1:4)]
    names(y) <- c("region","x1989","x2002","x2010")
    y.long <- melt(y, id.vars="region")
  }
  
  if (indicator == "population_urban") {
    x <- suppressWarnings(ScrapeData_skip1(indicator))  
    y <- x[,c(1,5:7)]
    names(y) <- c("region","x1989","x2002","x2010")
    y.long <- melt(y, id.vars="region")
  }
  
  if (indicator == "population_rural") {
    x <- suppressWarnings(ScrapeData_skip1(indicator))  
    y <- x[,c(1,8:10)]
    names(y) <- c("region","x1989","x2002","x2010")
    y.long <- melt(y, id.vars="region")
  }
  ######################################
  # Life expectancy
  if (indicator == "life_expectancy_total") {
    x <- suppressWarnings(ScrapeData_skip1(indicator))  
    y <- x[,c(1,2,5,8)]
    names(y) <- c("region","x2000","x2005","x2009")
    y.long <- melt(y, id.vars=c("region"))
  }
  if (indicator == "life_expectancy_men") {
    x <- suppressWarnings(ScrapeData_skip1(indicator))  
    y <- x[,c(1,3,6,9)]
    names(y) <- c("region","x2000","x2005","x2009")
    y.long <- melt(y, id.vars=c("region"))
  }
  if (indicator == "life_expectancy_women") {
    x <- suppressWarnings(ScrapeData_skip1(indicator))  
    y <- x[,c(1,4,7,10)]
    names(y) <- c("region","x2000","x2005","x2009")
    y.long <- melt(y, id.vars=c("region"))
  }
  ######################################
  # Demography
  if (indicator %in% c("mortality_rate",
                       "infant_mortality_rate",
                       "crude_birth_rate")) {
    x <- suppressWarnings(ScrapeData(indicator))
    y <- x[,c(1:14)]
    names(y) <- c("region","x1990","x1995","x2000","x2001",
                  "x2002","x2003","x2004","x2005",
                  "x2006","x2007","x2008","x2009","x2010")
    y.long <- melt(y, id.vars="region")
  }
  
  if (indicator %in% c("average_size_of_pensions",
                       "average_nominal_monthly_salary")) {
    x <- suppressWarnings(ScrapeData(indicator))
    y <- x[,c(1,3:14)]
    names(y) <- c("region","x2000","x2001",
                  "x2002","x2003","x2004","x2005",
                  "x2006","x2007","x2008","x2009","x2010",
                  "x2011")
    y.long <- melt(y, id.vars="region")
  }
  ######################################
  ## Culture & sports
  if (indicator == "number_of_theater_goers_per_1000_population") {
    x <- suppressWarnings(ScrapeData(indicator))  
    y <- x[,c(1:14)]
    names(y) <- c("region","x1990","x1995","x2000","x2001",
                  "x2002","x2003","x2004","x2005",
                  "x2006","x2007","x2008","x2009","x2010")
    y.long <- melt(y, id.vars="region")
  }
  if (indicator == "number_of_visits_to_museums_per_1000_population") {
    x <- suppressWarnings(ScrapeData(indicator))  
    y <- x[,c(1:14)]
    names(y) <- c("region","x1990","x1995","x2000","x2001",
                  "x2002","x2003","x2004","x2005",
                  "x2006","x2007","x2008","x2009","x2010")
    y.long <- melt(y, id.vars="region")
  }
  if (indicator == "publication_of_newspapers_per_1000_people") {
    x <- suppressWarnings(ScrapeData(indicator))  
    y <- x[,c(1:14)]
    names(y) <- c("region","x1990","x1995","x2000","x2001",
                  "x2002","x2003","x2004","x2005",
                  "x2006","x2007","x2008","x2009","x2010")
    y.long <- melt(y, id.vars="region")
  }
  if (indicator == "stadiums_with_stands_for_1500_seats_or_more") {
    x <- suppressWarnings(ScrapeData_skip1(indicator))  
    y <- x[,c(1,2:5)]
    names(y) <- c("region","x1995",
                  "x2005",
                  "x2009","x2010")
    y.long <- melt(y, id.vars="region")
  }
  if (indicator == "planar_sports_facilities_(playgrounds_and_fields)") {
    x <- suppressWarnings(ScrapeData_skip1(indicator))  
    y <- x[,c(1,6:9)]
    names(y) <- c("region","x1995",
                  "x2005",
                  "x2009","x2010")
    y.long <- melt(y, id.vars="region")
  }
  if (indicator == "sports_halls") {
    x <- suppressWarnings(ScrapeData_skip1(indicator))  
    y <- x[,c(1,10:13)]
    names(y) <- c("region","x1995",
                  "x2005",
                  "x2009","x2010")
    y.long <- melt(y, id.vars="region")
  }
  if (indicator == "swimming_pools") {
    x <- suppressWarnings(ScrapeData_skip1(indicator))  
    y <- x[,c(1,14:17)]
    names(y) <- c("region","x1995",
                  "x2005",
                  "x2009","x2010")
    y.long <- melt(y, id.vars="region")
  }
  ######################################
  ######################################
  # Operations for all datasets
  
  # remove cases with missing values
  y.long <- y.long[!is.na(y.long$value),]
  # change variable into numeric years (x2009 <- 2009)
  y.long$variable <- str_replace_all(y.long$variable, "x","")
  # change comma to dot in value
  y.long$value <- str_replace_all(y.long$value, ",",".")
  # make both variables into numerical
  y.long$value <- factor(y.long$value)
  y.long$variable <- factor(y.long$variable)
  
  y.long$value <- suppressWarnings(as.numeric(levels(y.long$value))[y.long$value])
  y.long$variable <- as.numeric(levels(y.long$variable))[y.long$variable]
  
  y.long$list <- str_extract_all(y.long$region, "[а-яА-я]+")
  y.long$one <- sapply(y.long$list, function(x){as.character(x[1])})
  y.long$two <- sapply(y.long$list, function(x){as.character(x[2])})
  y.long$three <- sapply(y.long$list, function(x){as.character(x[3])})
  y.long$four <- sapply(y.long$list, function(x){as.character(x[4])})
  y.long$five <- sapply(y.long$list, function(x){as.character(x[5])})
  y.long$six <- sapply(y.long$list, function(x){as.character(x[6])})
  y.long$x <- paste(y.long$one,
                    y.long$two,
                    y.long$three,
                    y.long$four,
                    y.long$five,
                    y.long$six,
                    sep="")
  y.long$only_txt <- str_replace_all(y.long$x,
                                     "NA",
                                     "")
  y.long$name_begin <- str_sub(y.long$only_txt, 1, 7)
  y.long$name_end <- str_sub(y.long$only_txt, -5)
  y.long$id_name <- tolower(paste(y.long$name_begin,
                                  y.long$name_end,
                                  sep=""))
  y.long$year <- y.long$variable
  regiodata <- subset(y.long, select=c("region",
                                       "year",
                                       "value",
                                       "id_name"))
  
  # translation
  if (level == "federal") {
    load(url("http://research.muuankarski.org/rustfare/data/RosstatRegionKey.RData"))
    key <- subset(RosstatRegionKey, level == "federal")
    regiodata <- merge(regiodata,key, 
                       by="id_name")
    regiodata$indicator <- indicator
    subset(regiodata, select=c("region","region_en",
                               "year","value","level",
                               "id_shape","indicator"))
  }
  if (level == "federal_district") {
    load(url("http://research.muuankarski.org/rustfare/data/RosstatRegionKey.RData"))
    key <- subset(RosstatRegionKey, level == "federal_district")
    regiodata <- merge(regiodata,key, 
                       by="id_name")
    regiodata$indicator <- indicator
    subset(regiodata, select=c("region","region_en",
                               "year","value","level",
                               "id_shape","indicator"))
  }
  
  if (level == "region") {
    load(url("http://research.muuankarski.org/rustfare/data/RosstatRegionKey.RData"))
    key <- subset(RosstatRegionKey, level == "region")
    regiodata <- merge(regiodata,key, 
                       by="id_name")
    regiodata$indicator <- indicator
    subset(regiodata, select=c("region","region_en",
                               "year","value","level",
                               "id_shape","indicator"))
  } else {
    regiodata
  }
  
}
