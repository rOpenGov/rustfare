# This file is part of the rustfare program (https://github.com/muuankarski/rustfare)

# Copyright (C) 2012-2013 Markus Kainu <markuskainu@gmail.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Get Russian map data in GADM format
#' 
#' @param level accepts four values \code{federal}, \code{federal_district}, \code{region} and \code{rayon}
#'
#' @return shapefile at required level
#' 
#' @export
#' @examples # shapefile <- GetRusGADM("federal")
#' @author Markus Kainu <markuskainu(at)gmail.com> 


GetRusGADM <- function(level) {
  if (level == "federal") {
    load(url("http://gadm.org/data/rda/RUS_adm0.RData"))
  }
  if (level == "federal_district") {
    load(url("http://research.muuankarski.org/rustfare/data/shape/ShapeFederalDistrict.RData"))
    gadm <- ShapeFederalDistrict
  }
  if (level == "region") {
    load(url("http://research.muuankarski.org/rustfare/data/shape/ShapeRegion.RData"))
    gadm <- ShapeRegion
  }
  if (level == "rayon") {
    load(url("http://gadm.org/data/rda/RUS_adm2.RData"))
  }
  map <- gadm
}