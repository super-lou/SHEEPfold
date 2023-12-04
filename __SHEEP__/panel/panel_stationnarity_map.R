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
panel_stationnarity_map = function (trendEX_var,
                                    metaEX_var,
                                    meta,
                                    min_var=NULL,
                                    max_var=NULL,
                                    prob=0.1,
                                    is_secteur=FALSE,
                                    zoom=NULL,
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

    var = metaEX_var$var
    unit = metaEX_var$unit[metaEX_var$var == var]
    is_date = metaEX_var$is_date[metaEX_var$var == var]
    normalize = metaEX_var$normalize[metaEX_var$var == var]
    Palette = unlist(strsplit(metaEX_var$palette[metaEX_var$var == var], " "))

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

    print(var)

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

        # dataEX_criteria_var = dplyr::left_join(dataEX_criteria_var,
        #                                        dplyr::select(meta,
        #                                                      c("Code",
        #                                                        "Secteur")),
        #                                        by="Code")
        # dataEX_criteria_var =
        #     dplyr::summarise(dplyr::group_by(dataEX_criteria_var, Secteur),
        #                      !!var:=median(get(var), na.rm=TRUE),
        #                      .groups="drop")

    } else {
        trendEX_var =
            dplyr::left_join(trendEX_var,
                             dplyr::select(meta,
                                           c("Code",
                                             "XL93_m",
                                             "YL93_m")),
                             by="Code")
    }

    
    
    Palette_level = 1:(length(Palette)/2)
    Palette_level = c(rev(Palette_level), Palette_level)
    
    # Palette = get_IPCC_Palette(name, reverse=reverse)
    if (is.null(min_var)) {
        min_var = quantile(trendEX_var$trend,
                           prob, na.rm=TRUE)
    }
    if (is.null(max_var)) {
        max_var = quantile(trendEX_var$trend,
                           1-prob, na.rm=TRUE)
    }

    res = compute_colorBin(min_var,
                           max_var,
                           colorStep=length(Palette),
                           center=0,
                           include=FALSE)
    bin = res$bin
    upBin = res$upBin
    lowBin = res$lowBin
    
    trendEX_var$fill = get_colors(trendEX_var$trend,
                                  upBin=upBin,
                                  lowBin=lowBin,
                                  Palette=Palette)

    palette_match = match(trendEX_var$fill, Palette)
    palette_matchNoNA = palette_match[!is.na(palette_match)]

    trendEX_var$H[is.na(trendEX_var$H)] = FALSE
    trendEX_var$level = Palette_level[palette_match]
    trendEX_var$level[!trendEX_var$H] = trendEX_var$level[!trendEX_var$H]/10

    trendEX_var$stroke = 0.6
    trendEX_var$color = IPCCgrey40
    trendEX_var$color[!trendEX_var$H] = IPCCgrey67
    trendEX_var$size = 4
    trendEX_var$size[!trendEX_var$H] = 3
    trendEX_var$shape = 21
    trendEX_var$shape[trendEX_var$H & trendEX_var$a > 0] = 24
    trendEX_var$shape[trendEX_var$H & trendEX_var$a < 0] = 25

    trendEX_var$color[is.na(trendEX_var$fill)] = NA
    # trendEX_var$color[is.na(trendEX_var$fill)] = "grey75"
    # trendEX_var$level[is.na(trendEX_var$fill)] = 0
    
    level = as.numeric(levels(factor(trendEX_var$level)))

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
        trendEX_var = dplyr::rename(trendEX_var,
                                    CdSecteurH=Secteur)
        secteurHydro = dplyr::full_join(secteurHydro,
                                        trendEX_var,
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
            trendEX_var_tmp = dplyr::filter(trendEX_var,
                                              level==l)
            map = map +
                geom_point(data=trendEX_var_tmp,
                           aes(x=XL93_m, y=YL93_m),
                           color=trendEX_var_tmp$color,
                           fill=trendEX_var_tmp$fill,
                           shape=trendEX_var_tmp$shape,
                           size=trendEX_var_tmp$size,
                           stroke=trendEX_var_tmp$stroke)
        }
    }

    map = map +
        coord_sf(xlim=xlim, ylim=ylim,
                 expand=FALSE)


    # plan = matrix(c("map", "fill",
                    # "map", "fill",
                    # "map", "shape"),
                  # nrow=2, byrow=TRUE)

    # herd = bring_grass(verbose=verbose)
    # herd = plan_of_herd(herd, plan, verbose=verbose)

    # herd = add_sheep(herd,
                     # sheep=map,
                     # id="map",
                     # height=0.7,
                     # verbose=verbose)

    # shape = panel_colorbar_circle(c(0, 0.5, 1),
                               # c("transparent",
                                 # "transparent",
                                 # "transparent"),
                               # size_circle=2.2,
                               # d_line=0.1,
                               # linewidth=0.35,
                               # d_space=0,
                               # d_text=0.5,
                               # text_size=2.8,
                               # stroke=c(0.5, 0.5, 0.5),
                               # color=c("grey30",
                                       # "grey30",
                                       # "grey30"),
                               # label=c("Baisse significative à 10%",
                                       # "Non significatif à 10%",
                                       # "Hausse significative à 10%"),
                               # shape=c(25, 21, 24),
                               # on_circle=TRUE,
                               # margin=margin_shape)
    # herd = add_sheep(herd,
                     # sheep=contour(),
                     # id="shape",
                     # height=0.2,
                     # verbose=verbose)
    
    # fill = panel_colorbar_circle(bin*100,
                               # Palette,
                               # size_circle=3.3,
                               # d_line=0.2,
                               # linewidth=0.35,
                               # d_space=0.15,
                               # d_text=0.5,
                               # text_size=3,
                               # label=NULL,
                               # ncharLim=4,
                               # colorText=IPCCgrey50,
                               # colorLine=IPCCgrey50,
                               # on_circle=FALSE,
                               # margin=margin_fill)
    
    # herd = add_sheep(herd,
                     # sheep=contour(),
                     # id="fill",
                     # height=1,
                     # verbose=verbose)

    return (map)
}
