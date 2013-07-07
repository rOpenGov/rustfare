# This file is part of the rustfare program (https://github.com/muuankarski/rustfare)

# Copyright (C) 2012-2013 Markus Kainu <markuskainu@gmail.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Print map using ggplot2
#' 
#' @param level accepts four values \code{federal}, \code{federal_district}, \code{region} and \code{rayon}
#'
#' @return shapefile at required level
#' 
#' @export
#' @examples # shapefile <- GetRusGADM("federal")
#' @author Markus Kainu <markuskainu(at)gmail.com> 


RosstatMapPlot <- function(indicator="average_percapita_income",vuosi=2008,level="federal_district") {
  library(rgdal)
  library(rgeos)
  library(ggplot2)
  library(rustfare)
  library(utils)
  library(maptools)
  map <- GetRusGADM(level)
  map@data$id <- rownames(map@data)
  map.points <- fortify(map, region="id")
  map.df <- merge(map.points, map@data, by="id")
  df <- suppressWarnings(GetRosstat(indicator,level))
  df.year <- subset(df, year == vuosi)
  choro <- merge(map.df,df.year, by.x="ID_1",by.y="id_shape")
  #  choro$rate <- cut_interval(choro$value, 5)
  choro <- choro[order(choro$order),]
  
  ggplot(choro, aes(long,lat,group=group)) +
    geom_polygon(aes(fill = value)) +
    #scale_fill_brewer(palette="Oranges", name = indicator) + # colour scheme
    geom_polygon(data = map.df, aes(long,lat), fill=NA, color = "white") + # white borders
    #     geom_text(data=cnames, aes(long, lat, label = english, group=english), size=3, 
    #               color="black") + # country names
    coord_map(project="orthographic", xlim=c(25,170),
              ylim=c(45,70)) + # projection
    labs(title=paste(indicator," at the ",level," level in ",vuosi,sep=""))
}