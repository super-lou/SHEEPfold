# Copyright 2022-2024 Louis Héraut (louis.heraut@inrae.fr)*1,
#                     Éric Sauquet (eric.sauquet@inrae.fr)*1,
#                     Jean-Philippe Vidal (jean-philippe.vidal@inrae.fr)*1
#
# *1   INRAE, UR RiverLy, Villeurbanne, France
#
# This file is part of SHEEPfold R toolbox.
#
# SHEEPfold R toolbox is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# SHEEPfold R toolbox is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with SHEEPfold R toolbox.
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
    meta_region = meta[substr(meta$code, 1, 1) == regionLight,]

    if ('title' %in% to_do | 'all' %in% to_do) {
        # Converts all texts to graphical object in the right position
        gtext1 = richtext_grob(
            paste0("<b style='font-size:14pt; color:", refCOL, "'>",
                   meta_region$hydrological_region[1], "</b>",
                   nbsp(1),
                   "<span style='font-size:14pt; color:", refCOL, "'>", "-", "</span>",
                   nbsp(1),
                   "<span style='font-size:14pt; color:", refCOL, "'>",
                   regionLight, "</span>"),
            x=0, y=1,
            margin=unit(c(t=0, r=5, b=0, l=0),
                        "mm"),
            hjust=0, vjust=1)

        
    } else {
        gtext1 = void()
    }

    # Subitle info
    if ('subtitle' %in% to_do | 'all' %in% to_do) {
        text2 = paste0("<b>", length(meta_region$code),
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

    # Spatial info about station
    if ('spatial' %in% to_do | 'all' %in% to_do) {
        if (all(is.na(meta_region$surface_km2))) {
            surface_min = "inconnue"
            surface_max = "inconnue"
        } else {
            surface_min = paste0(round(min(meta_region$surface_km2,
                                           na.rm=TRUE)),
                                 " km<sup>2</sup>")
            surface_max = paste0(round(max(meta_region$surface_km2,
                                           na.rm=TRUE)),
                                 " km<sup>2</sup>")
        }
        if (all(is.na(meta_region$altitude_m))) {
            altitude_min = "inconnue"
            altitude_max = "inconnue"
        } else {
            altitude_min = paste0(round(min(meta_region$altitude_m,
                                            na.rm=TRUE)), " m")
            altitude_max = paste0(round(max(meta_region$altitude_m,
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
                    "text2", "text2", "text2", "map",
                    "text3", "text3", "text3", "map",
                    "text3", "text3", "text3", "map"),
                  nrow=4, 
                  byrow=TRUE)
    
    herd = bring_grass(verbose=verbose)
    herd = plan_of_herd(herd, plan,
                        verbose=verbose)

    herd = add_sheep(herd,
                     sheep=gtext1,
                     id="text1",
                     height=0.35,
                     verbose=verbose)
    herd = add_sheep(herd,
                     sheep=gtext2,
                     id="text2",
                     height=0.35,
                     verbose=verbose)
    herd = add_sheep(herd,
                     sheep=gtext3,
                     id="text3",
                     height=1,
                     verbose=verbose)
    herd = add_sheep(herd,
                     sheep=map,
                     id="map",
                     height=1,
                     verbose=verbose)
    
    return (herd)
}  
