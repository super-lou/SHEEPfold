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
panel_info_station = function(data_code,
                              QM_code=NULL,
                              regimeLight="",
                              meta=NULL,
                              Shapefiles=NULL,
                              codeLight=NULL,
                              to_do='all',
                              zone_to_show='France') {
    
    # If there is a data serie for the given code
    if (!is.null(QM_code)) {
        # Computes the hydrograph
        hyd = panel_hydrograph(QM_code,
                               regimeLight,
                               ratio_title=1/7,
                               margin_title=margin(t=0, r=0, b=0, l=14,
                                                   unit="mm"),
                               margin_hyd=margin(t=1, r=0, b=0, l=5,
                                                 unit="mm"))
    # Otherwise
    } else {
        # Puts it blank
        hyd = void()
    }

    if (!is.null(Shapefiles)) {
        # Computes the map associated to the station
        map =  panel_mini_map(meta,
                              Shapefiles=Shapefiles,
                              codeLight=codeLight)
    # Otherwise
    } else {
        # Puts it blank
        map = void()
    }

    # Gets the metadata about the station
    meta_code = meta[meta$Code == codeLight,]

    if ('title' %in% to_do | 'all' %in% to_do) {
        # Extracts the name
        Nom = meta_code$Nom
        # Corrects some errors about the formatting of title with dash
        Nom = gsub("-", "-&nbsp;", Nom)
        # Name of the datasheet
        text1 = paste(
            "<b>", codeLight, '</b>  -  ', Nom,
            sep='')
        # Converts all texts to graphical object in the right position
        gtext1 = richtext_grob(text1,
                               x=0, y=1,
                               margin=unit(c(t=0, r=5, b=0, l=0),
                                           "mm"),
                               hjust=0, vjust=1,
                               gp=gpar(col=INRAEcyan, fontsize=14))
    } else if ('short_title' %in% to_do) {
        # Name of the datasheet
        text1 = paste(
            "<b>", codeLight, '</b>',
            sep='')
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
        text2 = paste(
            "<b>",
            "Gestionnaire : ", meta_code$Gestionnaire, "<br>",
            "Région hydrographique : ", meta_code$Region_Hydro,
            "</b>",
            sep='')
        gtext2 = richtext_grob(text2,
                               x=0, y=1.1,
                               margin=unit(c(t=0, r=0, b=0, l=0),
                                           "mm"),
                               hjust=0, vjust=1,
                               gp=gpar(col=INRAEcyan, fontsize=8))
    } else {
        gtext2 = void()
    }

    # Spatial info about station
    if ('spatial' %in% to_do | 'all' %in% to_do) {
        if (is.na(meta_code$Surface_km2)) {
            if (is.na(meta_code$surface_km2_IN)) {
                surface = "inconnue"
            } else {
                surface = paste0(round(meta_code$surface_km2_IN),
                                 " km<sup>2</sup>")
            }
        } else {
            surface = paste0(round(meta_code$Surface_km2),
                             " km<sup>2</sup>")
        }
        if (is.na(meta_code$Altitude_m)) {
            if (is.na(meta_code$altitude_m_IN)) {
                altitude = "inconnue"
            } else {
                altitude = paste0(round(meta_code$altitude_m_IN), " m")
            }
        } else {
            altitude = paste0(round(meta_code$Altitude_m), " m")
        }

        text3 = paste0(
            "Superficie : ", surface, "<br>",
            "Altitude : ", altitude, "<br>",
            "X = ", round(meta_code$XL93_m), "  m (Lambert93)<br>",
            "Y = ", round(meta_code$YL93_m), "  m (Lambert93)")
        gtext3 = richtext_grob(text3,
                               x=0, y=0.98,
                               margin=unit(c(t=0, r=0, b=0, l=0),
                                           "mm"),
                               hjust=0, vjust=1,
                               gp=gpar(col=IPCCgrey13, fontsize=9))
    } else {
        gtext3 = void()
    }

    # Time info about station
    if ('temporal' %in% to_do | 'all' %in% to_do) {
        # Computes the time span of data, the start and the end
        duration = round(as.numeric(max(data_code$Date) -
                                    min(data_code$Date))/365.25)
        debut = format(min(data_code$Date), "%d/%m/%Y")
        fin = format(max(data_code$Date), "%d/%m/%Y")

        text4 = paste0(
            "Date de début : ", debut, "<br>",
            "Date de fin : ", fin, "<br>",
            "Disponibilité : ", duration, " ans", "<br>",
            "Taux de lacunes : ", signif(meta_code$tLac_pct, 2),
            " %")
        gtext4 = richtext_grob(text4,
                               x=0, y=0.96,
                               margin=unit(c(t=0, r=0, b=0, l=0),
                                           "mm"),
                               hjust=0, vjust=1,
                               gp=gpar(col=IPCCgrey13, fontsize=9))
    } else {
        gtext4 = void()
    }


    # # Makes a list of all plots
    # P = list(gtext1, gtext2, gtext3, gtext4, hyd, map)
    
    # # Creates the matrix layout
    # LM = matrix(c(1, 1, 1, 6,
    #               2, 2, 5, 6,
    #               3, 4, 5, 6,
    #               3, 4, 5, 6),
    #             nrow=4, 
    #             byrow=TRUE)
    # # And sets the relative height of each plot
    # heights = rep(1, times=nrow(LM))
    # # heights[2] = 0.1
    # heights[2] = 0.8

    # # Arranges all the graphical objetcs
    # plot = grid.arrange(grobs=P,
    #                     layout_matrix=LM,
    #                     heights=heights)


    plan = matrix(c("text1", "text1", "text1", "map",
                    "text2", "text2", "hyd", "map",
                    "text3", "text4", "hyd", "map",
                    "text3", "text4", "hyd", "map"),
                  nrow=4, 
                  byrow=TRUE)
    
    flock = bring_grass()
    flock = plan_of_flock(flock, plan)

    flock = add_sheep(flock,
                      sheep=gtext1,
                      # sheep=contour(),
                      id="text1",
                      height=0.4)
    flock = add_sheep(flock,
                      sheep=gtext2,
                      # sheep=contour(),
                      id="text2",
                      height=0.5)
    flock = add_sheep(flock,
                      sheep=gtext3,
                      id="text3",
                      height=1)
    flock = add_sheep(flock,
                      sheep=gtext4,
                      id="text4",
                      height=1)
    flock = add_sheep(flock,
                      sheep=hyd,
                      id="hyd",
                      height=1)
    flock = add_sheep(flock,
                      sheep=map,
                      id="map",
                      height=1)    

    # print(flock)
    
    flock = shear_sheeps(flock)

    # print(flock)
    
    return (flock)
}  
