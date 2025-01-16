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
panel_info_station = function(data_code,
                              QM_code=NULL,
                              regimeLight="",
                              meta=NULL,
                              nProjections=NULL,
                              projection_legend=NULL,
                              Shapefiles=NULL,
                              codeLight=NULL,
                              to_do='all',
                              if_NA_unkowned=TRUE,
                              subtitle=NULL,
                              subtitle_height=0.35,
                              show_regime_info=TRUE,
                              zoom=NULL,
                              map_limits=NULL,
                              x_echelle_pct=62,
                              y_echelle_pct=5,
                              echelle=c(0, 50, 100, 250),
                              echelle_size=2.6,
                              echelle_km_size=2.5,
                              echelle_tick_height=1.3,
                              size_codeLight=1.4,
                              stroke_codeLight=0.3,
                              margin_map=margin(t=0, r=0, b=0, l=0, unit="mm"),
                              verbose=FALSE) {

    # If there is a data serie for the given code
    if (!is.null(QM_code) & ('regime' %in% to_do | 'all' %in% to_do) &
        !('projection' %in% to_do)) {
        # Computes the hydrograph
        if (show_regime_info) {
            ratio_title = 1/7
        } else {
            ratio_title = 0
        }
        hyd = panel_hydrograph(QM_code,
                               regimeLight,
                               ratio_title=ratio_title,
                               margin_title=margin(t=0, r=0, b=0, l=14,
                                                   unit="mm"),
                               margin_hyd=margin(t=1, r=0, b=0, l=5,
                                                 unit="mm"),
                               verbose=verbose)
    } else {
        hyd = void()
    }

    if (!is.null(Shapefiles) & ('map' %in% to_do | 'all' %in% to_do)) {
        # Computes the map associated to the station
        map =  panel_mini_map(meta,
                              Shapefiles=Shapefiles,
                              codeLight=codeLight,
                              zoom=zoom,
                              map_limits=map_limits,
                              x_echelle_pct=x_echelle_pct,
                              y_echelle_pct= y_echelle_pct,
                              echelle=echelle,
                              echelle_size=echelle_size,
                              echelle_km_size=echelle_km_size,
                              echelle_tick_height=echelle_tick_height,
                              size_codeLight=size_codeLight,
                              stroke_codeLight=stroke_codeLight,
                              margin_map=margin_map)
    # Otherwise
    } else {
        # Puts it blank
        map = void()
    }

    # Gets the metadata about the station
    meta_code = meta[meta$code == codeLight,]

    
    if ('title' %in% to_do | 'all' %in% to_do) {
        # Converts all texts to graphical object in the right position
        # gtext1 = richtext_grob(
        #     paste0("<b style='font-size:14pt; color:", refCOL, "'>",
        #            codeLight, "</b>",
        #            nbsp(1),
        #            "<span style='font-size:14pt; color:", refCOL, "'>", "-", "</span>",
        #            nbsp(1),
        #            "<span style='font-size:14pt; color:", refCOL, "'>",
        #            gsub(" [[]", paste0(nbsp(1), "["), meta_code$name), "</span>"),
        #     x=0, y=1,
        #     gp=gpar(fontfamily="Lato"),
        #     margin=unit(c(t=0, r=5, b=0, l=0),
        #                 "mm"),
        #     hjust=0, vjust=1)

        title = paste0("\\textbf{", codeLight, "} - ", meta_code$name)
        
        gtext1 = ggplot() + theme_void_Lato() +
            theme(plot.margin=margin(t=0, r=5,
                                     b=0, l=0, "mm")) + 
            annotate("text",
                     x=0, y=0.94,
                     label=TeX(title),
                     color=refCOL,
                     size=5, hjust=0, vjust=1,
                     family="Lato",
                     color=IPCCgrey23) +
            scale_x_continuous(limits=c(0, 1),
                               expand=c(0, 0)) +
            scale_y_continuous(limits=c(0, 1),
                               expand=c(0, 0))
        

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
                               gp=gpar(col=refCOL,
                                       fontfamily="Lato",
                                       fontsize=14))
    } else {
        gtext1 = void()
    }

    # Subitle info
    if ('subtitle' %in% to_do | 'all' %in% to_do) {
        if (is.null(subtitle)) {
            subtitle = paste0("Région hydrographique : ", meta_code$hydrological_region)
        }
        text2 = paste0("<b>", subtitle, "</b>")
        gtext2 = richtext_grob(text2,
                               x=0, y=1,
                               margin=unit(c(t=0, r=0, b=0, l=0),
                                           "mm"),
                               hjust=0, vjust=1,
                               gp=gpar(col=refCOL,
                                       fontfamily="Lato",
                                       fontsize=8))
    } else {
        gtext2 = void()
    }

    # Spatial info about station
    if ('spatial' %in% to_do | 'all' %in% to_do) {
        if (is.na(meta_code$surface_km2) & if_NA_unkowned) {
            surface = paste0("Superficie : inconnue<br>")
        } else if (is.na(meta_code$surface_km2) & !if_NA_unkowned) {
            surface = NULL
        } else {
            surface = paste0("Superficie : ",
                             round(meta_code$surface_km2),
                             " km<sup>2</sup><br>")
        }
        
        if ("altitude_m" %in% names(meta_code)) {
            if (is.na(meta_code$altitude_m) & if_NA_unkowned) {
                altitude = paste0("Altitude : inconnue<br>")
            } else if (is.na(meta_code$altitude_m) & !if_NA_unkowned) {
                altitude = NULL
            } else {
                altitude = paste0("Altitude : ",
                                  round(meta_code$altitude_m),
                                  " m<br>")
            }
        } else {
            if (if_NA_unkowned) {
                altitude = paste0("Altitude : inconnue<br>")
            } else {
                altitude = NULL
            } 
        }

        if ('projection' %in% to_do & !is.null(nProjections) | !is.null(nProjections)) {
            projection = paste0(
                "<br>Nombre de projections sous RCP 8.5 : ", nProjections, "<br>",
                "Nombre de modèles hydrologiques : ", meta_code$n)
        } else {
            projection = NULL
        }

        text3 = paste0(
            surface,
            altitude,
            "X = ", round(meta_code$XL93_m), " m (Lambert93)<br>",
            "Y = ", round(meta_code$YL93_m), " m (Lambert93)",
            projection)
        gtext3 = richtext_grob(text3,
                               x=0, y=1,
                               margin=unit(c(t=0, r=0, b=0, l=0),
                                           "mm"),
                               hjust=0, vjust=0.98,
                               gp=gpar(col=IPCCgrey13,
                                       fontfamily="Lato",
                                       fontsize=9))
    } else {
        gtext3 = void()
    }

    # Time info about station
    if ('temporal' %in% to_do | 'all' %in% to_do) {
        # Computes the time span of data, the start and the end
        duration = round(as.numeric(max(data_code$date) -
                                    min(data_code$date))/365.25)
        debut = format(min(data_code$date), "%d/%m/%Y")
        fin = format(max(data_code$date), "%d/%m/%Y")

        text4 = paste0(
            "Date de début : ", debut, "<br>",
            "Date de fin : ", fin, "<br>",
            "Disponibilité : ", duration, " ans", "<br>",
            "Taux de lacunes : ", signif(meta_code$tLac_pct, 2),
            " %")
        gtext4 = richtext_grob(text4,
                               x=0, y=1,
                               margin=unit(c(t=0, r=0, b=0, l=0),
                                           "mm"),
                               hjust=0, vjust=1,
                               gp=gpar(col=IPCCgrey13,
                                       fontfamily="Lato",
                                       fontsize=9))

    } else if ('projection' %in% to_do & !is.null(projection_legend)) {
        gtext4 = projection_legend
        
    } else {
        gtext4 = void()
    }

    if ('projection' %in% to_do) {
        plan = matrix(c("text1", "text1", "text1", "map",
                        "text2", "text2", "text2", "map",
                        "text3", "text4", "text4", "map",
                        "text3", "text4", "text4", "map"),
                      nrow=4, 
                      byrow=TRUE)
        width_text3 = 1.4
        width_text4 = 2
        width_map = 0.9
    } else {
        plan = matrix(c("text1", "text1", "text1", "map",
                        "text2", "text2", "hyd", "map",
                        "text3", "text4", "hyd", "map",
                        "text3", "text4", "hyd", "map"),
                      nrow=4, 
                      byrow=TRUE)
        width_text3 = 1
        width_text4 = 1
        width_map = 1.1
    }
    
    herd = bring_grass(verbose=verbose)
    herd = plan_of_herd(herd, plan,
                        verbose=verbose)

    herd = add_sheep(herd,
                     sheep=gtext1,
                     id="text1",
                     height=0.3,
                     width=1,
                     verbose=verbose)
    herd = add_sheep(herd,
                     sheep=gtext2,
                     id="text2",
                     height=subtitle_height,
                     width=1,
                     verbose=verbose)
    herd = add_sheep(herd,
                     sheep=gtext3,
                     id="text3",
                     height=1,
                     width=width_text3,
                     verbose=verbose)
    herd = add_sheep(herd,
                     sheep=gtext4,
                     id="text4",
                     height=1,
                     width=width_text4,
                     verbose=verbose)
    herd = add_sheep(herd,
                     sheep=hyd,
                     id="hyd",
                     height=1,
                     width=1,
                     verbose=verbose)
    herd = add_sheep(herd,
                     sheep=map,
                     id="map",
                     height=1,
                     width=width_map,
                     verbose=verbose)    
    
    return (herd)
}  
