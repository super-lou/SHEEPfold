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
panel_stationnarity_map = function (trendEX_variable,
                                    metaEX_variable,
                                    meta,
                                    is_secteur=FALSE,
                                    show_MK=TRUE,
                                    zoom=NULL,
                                    map_limits=NULL,
                                    x_echelle_pct=62,
                                    y_echelle_pct=5,
                                    echelle=c(0, 50, 100, 250),
                                    Shapefiles=NULL,
                                    margin_map=margin(t=0, r=0,
                                                      b=0, l=0, "cm"),
                                    margin_shape=margin(t=0.2, r=0.5,
                                                        b=0.2, l=2, "cm"),
                                    margin_fill=margin(t=0.3, r=0.5,
                                                       b=2.1, l=5.5, "cm"),
                                    verbose=FALSE) {

    is_numeric_logical = function (x) {
        return (is.logical(x) | is.numeric(x))
    }

    if (any(names(metaEX_variable) == "variable")) {
        variable_to_display = metaEX_variable$variable
    } else {
        variable_to_display = variable
    }
    variable = metaEX_variable$variable_en
    unit = metaEX_variable$unit_fr
    is_date = metaEX_variable$is_date
    to_normalise = metaEX_variable$to_normalise
    Palette = metaEX_variable$palette
    Palette = unlist(strsplit(Palette, " "))
    sampling_period = metaEX_variable$sampling_period_en
    sampling_period = unlist(strsplit(sampling_period, ", "))

    
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
        theme(plot.margin=margin_map)        

    if (!is.null(zoom)) {
        xlim = c(min(meta$XL93_m)*(1-zoom[1]), max(meta$XL93_m)*(1+zoom[2]))
        ylim = c(min(meta$YL93_m)*(1-zoom[3]), max(meta$YL93_m)*(1+zoom[4]))
    } else if (!is.null(map_limits)) {
        xlim = map_limits[1:2]
        ylim = map_limits[3:4]
    } else {
        xlim = c(90000, 1250000)
        ylim = c(6040000, 7120000)
    }
    
    xmin = gpct(x_echelle_pct, xlim, shift=TRUE)
    xint = echelle*1E3
    ymin = gpct(y_echelle_pct, ylim, shift=TRUE)
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

    print(variable)

    # by secteur or not
    if (is_secteur) {
        # get_for_coord = function (i, meta) {
        #     st_point(as.numeric(meta[i, ]))
        # }
        # points = do.call("st_sfc",
        #                  c(lapply(1:nrow(meta), 
        #                           get_for_coord,
        #                           meta=meta[which(grepl("L93",
        #                                                 names(meta)))]),
        #                    list("crs"=2154)))

        # get_name = function (col) { 
        #     secteurHydro[which(col), ]$CdSecteurH
        # }
        # meta$Secteur = apply(st_intersects(secteurHydro,
        #                                    points,
        #                                    sparse=FALSE),
        #                      2, get_name)

        # dataEX_criteria_variable = dplyr::left_join(dataEX_criteria_variable,
        #                                        dplyr::select(meta,
        #                                                      c("Code",
        #                                                        "Secteur")),
        #                                        by="Code")
        # dataEX_criteria_variable =
        #     dplyr::summarise(dplyr::group_by(dataEX_criteria_variable, Secteur),
        #                      !!variable:=median(get(variable), na.rm=TRUE),
        #                      .groups="drop")

    } else {
        trendEX_variable =
            dplyr::left_join(trendEX_variable,
                             dplyr::select(meta,
                                           c("code",
                                             "XL93_m",
                                             "YL93_m")),
                             by="code")
    }

    
    
    Palette_level = 1:(length(Palette)/2)
    Palette_level = c(rev(Palette_level), Palette_level)
    

    res = compute_colorBin(trendEX_variable$a_normalise_min,
                           trendEX_variable$a_normalise_max,
                           colorStep=length(Palette),
                           center=0,
                           include=FALSE)
    bin = res$bin
    upBin = res$upBin
    lowBin = res$lowBin

    trendEX_variable$fill = get_colors(trendEX_variable$a_normalise,
                                       upBin=upBin,
                                       lowBin=lowBin,
                                       Palette=Palette)

    palette_match = match(trendEX_variable$fill, Palette)
    palette_matchNoNA = palette_match[!is.na(palette_match)]

    trendEX_variable$H[is.na(trendEX_variable$H)] = FALSE
    trendEX_variable$level = Palette_level[palette_match]
    trendEX_variable$level[!trendEX_variable$H] = trendEX_variable$level[!trendEX_variable$H]/10

    trendEX_variable$stroke = 0.6
    trendEX_variable$color = IPCCgrey40
    if (show_MK) {
        trendEX_variable$color[!trendEX_variable$H] = IPCCgrey67
    }
    trendEX_variable$size = 3
    if (show_MK) {
        trendEX_variable$size[trendEX_variable$H] = 4
    }
    trendEX_variable$shape = 21
    if (show_MK) {
        trendEX_variable$shape[trendEX_variable$H & trendEX_variable$a > 0] = 24
        trendEX_variable$shape[trendEX_variable$H & trendEX_variable$a < 0] = 25
    }
    
    trendEX_variable$color[is.na(trendEX_variable$fill)] = NA
    # trendEX_variable$color[is.na(trendEX_variable$fill)] = "grey75"
    # trendEX_variable$level[is.na(trendEX_variable$fill)] = 0
    
    level = as.numeric(levels(factor(trendEX_variable$level)))

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

    if (is_secteur) {
        trendEX_variable = dplyr::rename(trendEX_variable,
                                    CdSecteurH=Secteur)
        secteurHydro = dplyr::full_join(secteurHydro,
                                        trendEX_variable,
                                        by="CdSecteurH")
        map = map +
            geom_sf(data=secteurHydro,
                    color=IPCCgrey99,
                    size=0.1,
                    fill=secteurHydro$fill)

        map = map +
            geom_sf(data=france,
                    color=IPCCgrey99,
                    fill=NA,
                    linewidth=0.8)
    }
    
    map = map +
        geom_sf(data=france,
                color=IPCCgrey50,
                fill=NA,
                linewidth=0.35)
    
    if (!is_secteur) {
        for (l in level) {
            trendEX_variable_tmp = dplyr::filter(trendEX_variable,
                                              level==l)
            map = map +
                geom_point(data=trendEX_variable_tmp,
                           aes(x=XL93_m, y=YL93_m),
                           color=trendEX_variable_tmp$color,
                           fill=trendEX_variable_tmp$fill,
                           shape=trendEX_variable_tmp$shape,
                           size=trendEX_variable_tmp$size,
                           stroke=trendEX_variable_tmp$stroke)
        }
    }

    map = map +
        coord_sf(xlim=xlim, ylim=ylim,
                 expand=FALSE)


    return (map)
}

