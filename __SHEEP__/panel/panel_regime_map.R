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


## 1. MAP PANEL ______________________________________________________
# Generates a map plot of the tendancy of a hydrological variable
#' @title Mini map panel
#' @export
panel_regime_map = function (meta,
                             Shapefiles=Shapefiles,
                             margin=margin(t=0, r=0, b=0, l=0, "cm"),
                             verbose=verbose) {

    meta = meta[!is.na(meta$Regime_hydro_id),]
    
    is_numeric_logical = function (x) {
        return (is.logical(x) | is.numeric(x))
    }

    # Extract shapefiles
    france = Shapefiles$france
    basinHydro = Shapefiles$basinHydro
    regionHydro = Shapefiles$regionHydro
    secteurHydro = Shapefiles$secteurHydro
    entiteHydro = Shapefiles$entiteHydro
    river = Shapefiles$river

    # Stores the coordonate system 
    cf = coord_fixed()
    # Makes it the default one to remove useless warning
    cf$default = TRUE

    # Open a new plot with the personalise theme
    map = ggplot() + theme_void() + cf +
        theme(plot.margin=margin)        
    
    xlim = c(90000, 1250000)
    ylim = c(6040000, 7120000)
    
    xmin = gpct(62, xlim, shift=TRUE)
    xint = c(0, 50*1E3, 100*1E3, 250*1E3)
    ymin = gpct(5, ylim, shift=TRUE)
    ymax = ymin + gpct(1.3, ylim)
    size = 2.6
    sizekm = 2.5
    linewidth = 0.4

    map = map +
        # Adds the base line of the scale
        geom_line(aes(x=c(xmin, max(xint)+xmin),
                      y=c(ymin, ymin)),
                  color=IPCCgrey50, size=linewidth,
                  lineend="round") +
        # Adds the 'km' unit
        annotate("text",
                 x=max(xint)+xmin+gpct(1, xlim), y=ymin,
                 vjust=0, hjust=0, label="km",
                 color=IPCCgrey50, size=sizekm)
    # For all graduations
    for (x in xint) {
        map = map +
            # Draws the tick
            annotate("segment",
                     x=x+xmin, xend=x+xmin, y=ymin, yend=ymax,
                     color=IPCCgrey50, size=linewidth,
                     lineend="round") +
            # Adds the value
            annotate("text",
                     x=x+xmin, y=ymax+gpct(0.5, ylim),
                     vjust=0, hjust=0.5, label=x/1E3,
                     fontface="bold",
                     color=IPCCgrey50, size=size)
    }

    # if (is_secteur) {
    #     dataEXind_var =
    #         dplyr::summarise(dplyr::group_by(dataEXind_var,
    #                                          Secteur=substr(Code,
    #                                                         1, 2)),
    #                          !!var:=median(get(var), na.rm=TRUE),
    #                          .groups="drop")
    # }
    
    map = map +
        geom_sf(data=france,
                color=NA,
                fill=IPCCgrey99)

    map = map +
        geom_sf(data=basinHydro,
                color=IPCCgrey85,
                fill=NA,
                size=0.2)

    if (!is.null(river)) {
        map = map +
            geom_sf(data=river,
                    color=INRAElightcyan,
                    alpha=1,
                    fill=NA,
                    linewidth=0.3,
                    na.rm=TRUE)
    }

    # if (is_secteur) {
    #     dataEXind_var = dplyr::rename(dataEXind_var,
    #                                   CdSecteurH=Secteur)
    #     secteurHydro = dplyr::inner_join(secteurHydro,
    #                                      dataEXind_var,
    #                                      by="CdSecteurH")
    #     map = map +
    #         geom_sf(data=secteurHydro,
    #                 color=IPCCgrey99,
    #                 size=0.1,
    #                 fill=secteurHydro$fill)

    #     map = map +
    #         geom_sf(data=france,
    #                 color=IPCCgrey99,
    #                 fill=NA,
    #                 linewidth=0.8)
    # }
    
    map = map +
        geom_sf(data=france,
                color=IPCCgrey50,
                fill=NA,
                linewidth=0.35)

    Regime_hydro_id_fill = list(EXPLORE2pmc=c("02", "03"),
                                EXPLORE2pc=c("05", "06"),
                                EXPLORE2pn = "07",
                                EXPLORE2np = c("08", "09"),
                                EXPLORE2nng=c("11", "12"))

    get_fill = function (x, Regime_hydro_id_fill) {
        get(names(Regime_hydro_id_fill)[sapply(lapply(Regime_hydro_id_fill,
                                                      "%in%", x), any)])
    }    
    
    meta$fill = sapply(meta$Regime_hydro_id, get_fill, Regime_hydro_id_fill)

    RH_id = levels(factor(meta$Regime_hydro_id))
    RH_typology = unique(meta$Regime_hydro_typology_2[order(meta$Regime_hydro_id)])
    RH_typology = RH_typology[!is.na(RH_typology)]
    
    # if (!is_secteur) {
    for (id in RH_id) {        
        meta_id = meta[meta$Regime_hydro_id == id,]
        map = map +
            geom_point(data=meta_id,
                       aes(x=XL93_m, y=YL93_m),
                       color="grey30",
                       fill=meta_id$fill,
                       shape=21,
                       size=3,
                       stroke=0.5)
    }
    # }
    
    map = map +
        coord_sf(xlim=xlim, ylim=ylim,
                 expand=FALSE)

    plan = matrix(c("map", "ca",
                    "map", "map"),
                  ncol=2)

    herd = bring_grass(verbose=verbose)
    herd = plan_of_herd(herd, plan, verbose=verbose)

    herd = add_sheep(herd,
                     sheep=map,
                     id="map",
                     height=0.7,
                     verbose=verbose)

    ca = panel_colorbar_circle(seq(0, 1,
                                   length.out=length(RH_typology)),
                               sapply(names(Regime_hydro_id_fill),
                                      get),
                               size_circle=2.9,
                               d_line=0.015,
                               linewidth=0.35,
                               d_space=0,
                               d_text=0.1,
                               text_size=2.2,
                               label=RH_typology,
                               on_circle=TRUE,
                               margin=margin(t=5.1, r=0,
                                             b=0.4, l=0.1,
                                             "cm"))
    herd = add_sheep(herd,
                     sheep=ca,
                     id="ca",
                     height=0.4,
                     verbose=verbose)
    
    return (herd)
}

