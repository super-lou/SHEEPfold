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
                legend="CM",
                ratio_title=1/7,
                margin_title=margin(t=0, r=0, b=0, l=4,
                                    unit="mm"),
                margin_hyd=margin(t=1, r=0, b=0, l=3,
                                  unit="mm"))
            hyd2 = panel_hydrograph(
                QM_code[[2]],
                names(QM_code)[2],
                legend="CM",
                ratio_title=1/7,
                margin_title=margin(t=0, r=0, b=0, l=4,
                                    unit="mm"),
                margin_hyd=margin(t=1, r=0, b=0, l=3,
                                  unit="mm"))
        } else {
            if (!is.list(QM_code)) {
                QM_code = list(QM_code)
            }
            hyd1 = void()
            hyd2 = panel_hydrograph(
                QM_code[[1]],
                names(QM_code)[1],
                legend="CM",
                ratio_title=1/7,
                margin_title=margin(t=0, r=0, b=0, l=4,
                                    unit="mm"),
                margin_hyd=margin(t=1, r=0, b=0, l=3,
                                  unit="mm"))
        }
    # Otherwise
    } else {
        # Puts it blank
        hyd1 = void()
        hyd2 = void()
    }

    meta_regime = meta[meta$code %in% Code_regime,]
    
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
                               gp=gpar(col=refCOL, fontsize=14))
    } else {
        gtext1 = void()
    }

    # Subitle info
    if ('subtitle' %in% to_do | 'all' %in% to_do) {
        text2 = paste0("<b>", length(meta_regime$code),
                       " stations de référence", "</b>")
        gtext2 = richtext_grob(text2,
                               x=0, y=1.1,
                               margin=unit(c(t=0, r=0, b=0, l=0),
                                           "mm"),
                               hjust=0, vjust=1,
                               gp=gpar(col=refCOL, fontsize=8))
    } else {
        gtext2 = void()
    }

    # print(meta_regime$code[is.na(meta_regime$surface_km2)])

    # Spatial info about station
    if ('spatial' %in% to_do | 'all' %in% to_do) {
        if (all(is.na(meta_regime$surface_km2))) {
            surface_min = "inconnue"
            surface_max = "inconnue"
        } else {
            surface_min = paste0(round(min(meta_regime$surface_km2,
                                           na.rm=TRUE)),
                                 " km<sup>2</sup>")
            surface_max = paste0(round(max(meta_regime$surface_km2,
                                           na.rm=TRUE)),
                                 " km<sup>2</sup>")
        }
        if (all(is.na(meta_regime$altitude_m))) {
            altitude_min = "inconnue"
            altitude_max = "inconnue"
        } else {
            altitude_min = paste0(round(min(meta_regime$altitude_m,
                                            na.rm=TRUE)), " m")
            altitude_max = paste0(round(max(meta_regime$altitude_m,
                                            na.rm=TRUE)), " m")
        }

        text3 = paste0(
            "Superficie minimale : ", surface_min, "<br>",
            "Superficie maximale : ", surface_max, "<br>",
            "Altitude minimale (station) : ", altitude_min, "<br>",
            "Altitude maximale (station) : ", altitude_max)
        gtext3 = richtext_grob(text3,
                               x=0, y=1,
                               margin=unit(c(t=0, r=0, b=0, l=0),
                                           "mm"),
                               hjust=0, vjust=0.98,
                               gp=gpar(col=IPCCgrey13, fontsize=9))
    } else {
        gtext3 = void()
    }

    plan = matrix(c("text1", "text1", "text1", "map",
                    "text2", "hyd1", "hyd2", "map",
                    "text3", "hyd1", "hyd2", "map",
                    "text3", "hyd1", "hyd2", "map"),
                  nrow=4, 
                  byrow=TRUE)

    herd = bring_grass(verbose=verbose)
    herd = plan_of_herd(herd, plan,
                        verbose=verbose)

    herd = add_sheep(herd,
                     sheep=gtext1,
                     id="text1",
                     height=0.35,
                     width=1,
                     verbose=verbose)
    herd = add_sheep(herd,
                     sheep=gtext2,
                     id="text2",
                     height=0.35,
                     width=1,
                     verbose=verbose)
    herd = add_sheep(herd,
                     sheep=gtext3,
                     id="text3",
                     height=1,
                     width=1.07,
                     verbose=verbose)
    herd = add_sheep(herd,
                     sheep=hyd1,
                     id="hyd1",
                     height=1,
                     width=1,
                     verbose=verbose)
    herd = add_sheep(herd,
                     sheep=hyd2,
                     id="hyd2",
                     height=1,
                     width=1,
                     verbose=verbose)
    herd = add_sheep(herd,
                     sheep=map,
                     id="map",
                     height=1,
                     width=1,
                     verbose=verbose)

    return (herd)
}  
