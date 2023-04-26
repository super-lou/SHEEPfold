# Copyright 2022 Louis Héraut (louis.heraut@inrae.fr)*1,
#                Éric Sauquet (eric.sauquet@inrae.fr)*1
#
# *1   INRAE, France
#
# This file is part of dataSheep R package.
#
# dataSheep R package is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# dataSheep R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with dataSheep R package.
# If not, see <https://www.gnu.org/licenses/>.


#' @title Info panel
#' @export
panel_info_region = function(meta,
                             regionLight=NULL,
                             Shapefiles=NULL,
                             to_do='all') {

    if (!is.null(Shapefiles)) {
        # Computes the map associated to the station
        map =  panel_mini_map(meta,
                              Shapefiles=Shapefiles,
                              regionLight=regionLight)
        # Otherwise
    } else {
        # Puts it blank
        map = void()
    }

    
    # Gets the metadata about the station
    meta_region = meta[substr(meta$Code, 1, 1) == regionLight,]

    if ('title' %in% to_do | 'all' %in% to_do) {
        # Extracts the name
        text1 = paste0("<b>", meta_region$Region_Hydro[1], "</b>",
                       " - ", regionLight)
        # Converts all texts to graphical object in the right position
        gtext1 = richtext_grob(text1,
                               x=0, y=1,
                               margin=unit(c(t=0, r=5, b=0, l=0),
                                           "mm"),
                               hjust=0, vjust=1,
                               gp=gpar(col=INRAEcyan, fontsize=14))
    } else {
        gtext1 = void()
    }

    # Subitle info
    if ('subtitle' %in% to_do | 'all' %in% to_do) {
        text2 = paste0("<b>", length(meta_region$Code),
                       " stations de référence", "</b>")
        gtext2 = richtext_grob(text2,
                               x=0, y=1,
                               margin=unit(c(t=0, r=0, b=0, l=0),
                                           "mm"),
                               hjust=0, vjust=1,
                               gp=gpar(col=INRAEcyan, fontsize=8))
    } else {
        gtext2 = void()
    }

    # Spatial info about station
    if ('spatial' %in% to_do | 'all' %in% to_do) {
        text3 = paste0(
            "Superficie minimale : ",
            min(meta_region$Surface_km2), " km<sup>2</sup><br>",
            "Superficie maximale : ",
            max(meta_region$Surface_km2), " km<sup>2</sup><br>",
            "Altitude minimale (station) : ",
            min(meta_region$Altitude_m), " m<br>",
            "Altitude maximale (station) : ",
            max(meta_region$Altitude_m), " m")
        gtext3 = richtext_grob(text3,
                               x=0, y=1,
                               margin=unit(c(t=0, r=0, b=0, l=0),
                                           "mm"),
                               hjust=0, vjust=1,
                               gp=gpar(col=IPCCgrey13, fontsize=9))
    } else {
        gtext3 = void()
    }

    # Makes a list of all plots
    P = list(gtext1, gtext2, gtext3, void(), map)
    
    # Creates the matrix layout
    LM = matrix(c(1, 1, 1, 5,
                  2, 2, 4, 5,
                  3, 3, 4, 5,
                  3, 3, 4, 5),
                nrow=4, 
                byrow=TRUE)
    # And sets the relative height of each plot
    heights = rep(1, times=nrow(LM))
    heights[1] = 0.57
    heights[2] = 0.65

    # Arranges all the graphical objetcs
    plot = grid.arrange(grobs=P,
                        layout_matrix=LM,
                        heights=heights)
    # Return the plot object
    return(plot)
}  
