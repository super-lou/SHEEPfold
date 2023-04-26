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
panel_info_regime = function(QM_code,
                             regimeLight="",
                             meta=NULL,
                             Code_regime=NULL,
                             Shapefiles=NULL,
                             to_do='all') {

    # If there is a data serie for the given code
    if (!is.null(QM_code)) {
        if (length(QM_code) == 2) {
            # Computes the hydrograph
            hyd1 = panel_hydrograph(
                QM_code[[1]],
                names(QM_code)[1],
                ratio_title=1/7,
                margin_title=margin(t=0, r=0, b=0, l=14,
                                    unit="mm"),
                margin_hyd=margin(t=1, r=0, b=0, l=5,
                                  unit="mm"))
            hyd2 = panel_hydrograph(
                QM_code[[2]],
                names(QM_code)[2],
                ratio_title=1/7,
                margin_title=margin(t=0, r=0, b=0, l=14,
                                    unit="mm"),
                margin_hyd=margin(t=1, r=0, b=0, l=5,
                                  unit="mm"))
        } else {
            if (!is.list(QM_code)) {
                QM_code = list(QM_code)
            }
            hyd1 = void()
            hyd2 = panel_hydrograph(
                QM_code[[1]],
                names(QM_code)[1],
                ratio_title=1/7,
                margin_title=margin(t=0, r=0, b=0, l=14,
                                    unit="mm"),
                margin_hyd=margin(t=1, r=0, b=0, l=5,
                                  unit="mm"))
        }
    # Otherwise
    } else {
        # Puts it blank
        hyd1 = void()
        hyd2 = void()
    }

    meta_regime = meta[meta$Code %in% Code_regime,]
    
    if (!is.null(Shapefiles)) {
        # Computes the map associated to the station
        map =  panel_mini_map(meta_regime,
                              Shapefiles=Shapefiles,
                              regimeCodeLight=Code_regime)
        # Otherwise
    } else {
        # Puts it blank
        map = void()
    }

    
    # # Gets the metadata about the station
    # meta_regime = meta[meta$typologie_regimeHydro == regimeLight,]

    if ('title' %in% to_do | 'all' %in% to_do) {
        # Extracts the name
        text1 = paste0("<b>", gsub(" [-]", "</b> -", regimeLight))
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
        text2 = paste0("<b>", length(meta_regime$Code),
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

    # print(meta_regime$Code[is.na(meta_regime$Surface_km2)])

    # Spatial info about station
    if ('spatial' %in% to_do | 'all' %in% to_do) {
        text3 = paste0(
            "Superficie minimale : ",
            min(meta_regime$Surface_km2, na.rm=TRUE),
            " km<sup>2</sup><br>",
            "Superficie maximale : ",
            max(meta_regime$Surface_km2, na.rm=TRUE),
            " km<sup>2</sup><br>",
            "Altitude minimale (station) : ",
            min(meta_regime$Altitude_m, na.rm=TRUE), " m<br>",
            "Altitude maximale (station) : ",
            max(meta_regime$Altitude_m, na.rm=TRUE), " m")
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
    P = list(gtext1, gtext2, gtext3, hyd1, hyd2, map)
    
    # Creates the matrix layout
    LM = matrix(c(1, 1, 1, 6,
                  2, 4, 5, 6,
                  3, 4, 5, 6,
                  3, 4, 5, 6),
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
