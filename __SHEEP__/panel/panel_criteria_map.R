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
panel_criteria_map = function (dataEXind_model_var,
                               metaEXind,
                               meta,
                               min_var,
                               max_var,
                               Shapefiles=Shapefiles,
                               margin=margin(t=0, r=0, b=0, l=0, "cm"),
                               verbose=verbose) {

    is_numeric_logical = function (x) {
        return (is.logical(x) | is.numeric(x))
    }

    Model = levels(factor(dataEXind_model_var$Model))
    
    var = names(dataEXind_model_var)[which(sapply(dataEXind_model_var,
                                                  is_numeric_logical))[1]]
    
    unit = metaEXind$unit[metaEXind$var == var]
    is_date = metaEXind$is_date[metaEXind$var == var]
    normalize = metaEXind$normalize[metaEXind$var == var]
    reverse_palette = metaEXind$reverse_palette[metaEXind$var == var]

    
    # Extract shapefiles
    france = Shapefiles$france
    basinHydro = Shapefiles$basinHydro
    regionHydro = Shapefiles$regionHydro
    entiteHydro = Shapefiles$entiteHydro
    river = Shapefiles$river

    # Stores the coordonate system 
    cf = coord_fixed()
    # Makes it the default one to remove useless warning
    cf$default = TRUE

    # Open a new plot with the personalise theme
    map = ggplot() + theme_void() + cf +
        
        theme(plot.margin=margin) +
        
        # Plot the background of France
        geom_sf(data=france,
                color=NA,
                fill=IPCCgrey99)

    map = map +
        # Plot the hydrological basin
        geom_sf(data=basinHydro,
                color=IPCCgrey85,
                fill=NA,
                size=0.2)
    
    # If the river shapefile exists
    if (!is.null(river)) {
        # Plot the river
        # map = map +
        #     geom_sf(data=river,
        #             color="white",
        #             alpha=1,
        #             fill=NA,
        #             linewidth=0.4,
        #             na.rm=TRUE)
        map = map +
            geom_sf(data=river,
                    color=INRAElightcyan,
                    alpha=1,
                    fill=NA,
                    linewidth=0.3,
                    na.rm=TRUE)
    }

    map = map +
        geom_sf(data=france,
                color=IPCCgrey50,
                fill=NA,
                linewidth=0.35)
    
    xlim = c(90000, 1250000)
    ylim = c(6040000, 7120000)
    
    xmin = gpct(62, xlim, shift=TRUE)
    xint = c(0, 50*1E3, 100*1E3, 250*1E3)
    ymin = gpct(4, ylim, shift=TRUE)
    ymax = ymin + gpct(1, ylim)
    size = 2.1
    sizekm = 1.6


    map = map +
        # Adds the base line of the scale
        geom_line(aes(x=c(xmin, max(xint)+xmin),
                      y=c(ymin, ymin)),
                  color=IPCCgrey40, size=0.2) +
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
                     color=IPCCgrey40, size=0.2) +
            # Adds the value
            annotate("text",
                     x=x+xmin, y=ymax+gpct(0.5, ylim),
                     vjust=0, hjust=0.5, label=x/1E3,
                     color=IPCCgrey40, size=size)
    }

    print(var)

    if (!grepl("(RAT)|(HYP)", var)) {
        dataEXind_var =
            dplyr::summarise(dplyr::group_by(dataEXind_model_var, Code),
                             !!var:=median(get(var), na.rm=TRUE))
    } else {
        if (length(Model) == 1) {
            dataEXind_var = dataEXind_model_var
        } else {
            return (void())
        }
    }

    dataEXind_var =
        dplyr::left_join(dataEXind_var,
                         dplyr::select(meta,
                                       c("Code",
                                         "XL93_m",
                                         "YL93_m")),
                         by="Code")

    if (is.null(reverse_palette)) {
        reverse_palette = FALSE
    }
    
    if (grepl("(KGE)|(NSE)", var)) {
        Palette_level = c(5, 4, 3, 2, 1)
        bin = c(-Inf, 0, 0.25, 0.5, 0.75, 1)
        upBin = c(0, 0.25, 0.5, 0.75, 1)
        lowBin = c(-Inf, 0, 0.25, 0.5, 0.75)
        Palette = get_IPCC_Palette("rainbow")[4:8]

    } else if (!grepl("(RAT)|(HYP)", var)) {
        if (grepl("(Biais)|(^Q)|(^moyQ)|(^V)|(^BF)|(^med[{]v)|(^med[{]t)|(^med[{]debut)|(^med[{]centre)|(^med[{]fin)|(^med[{]dt)", var)) {
            center = 0
        }
        if (grepl("(Rc)|(^epsilon)|(^alpha)|(STD)", var)) {
            center = 1
        }

        if (grepl("(Biais)|(^Q)|(^alpha)|(^moyQ)|(^V)|(^BF)|(^med[{]v)", var)) {
            reverse = FALSE
            name = "ground_short"
        }
        if (grepl("(Rc)|(^epsilon)|(^med[{]t)|(^med[{]debut)|(^med[{]centre)|(^med[{]fin)|(^med[{]dt)|(STD)", var)) {
            reverse = TRUE
            name = "rainbow"
        }
        
        Palette_level = c(4, 3, 2, 1, 1, 2, 3, 4)
        Palette = get_IPCC_Palette(name, reverse=reverse)
        if (is.null(min_var)) {
            min_var = quantile(dataEXind_model_var[[var]],
                               0.1, na.rm=TRUE)
        }
        if (is.null(max_var)) {
            max_var = quantile(dataEXind_model_var[[var]],
                               0.9, na.rm=TRUE)
        }
        
        res = compute_colorBin(min_var,
                               max_var,
                               colorStep=8,
                               center=center,
                               include=FALSE)
        bin = res$bin
        upBin = res$upBin
        lowBin = res$lowBin
    }

    if (grepl("(KGE)|(NSE)", var)) {
        lim = c(0.5, 1)
    } else if (grepl("Biais", var)) {
        lim = c(-0.2, 0.2)
    } else if (grepl("(^Q)|(^med[{]t)", var)) {
        lim = c(-1, 1)
    } else if (grepl("(^epsilon)|(^alpha)", var)) {
        lim = c(0.5, 2)
    } else if (!grepl("(RAT)|(HYP)", var)) {
        lim = c(center-1, center+1)
    }
    
    if (grepl("(RAT)|(HYP)", var)) {
        Palette_level = c(1, 2)
        Palette = get_IPCC_Palette("OrangePurple",
                                   reverse=TRUE)[c(2, 5)]
        RAT = dataEXind_var[[var]]
        RAT[is.na(RAT)] = FALSE
        fills = rep(Palette[2], length(RAT))
        fills[RAT] = Palette[1]
        dataEXind_var$fill = fills
    } else {
        dataEXind_var$fill = get_colors(dataEXind_var[[var]],
                                        upBin=upBin,
                                        lowBin=lowBin,
                                        Palette=Palette)
    }

    palette_match = match(dataEXind_var$fill, Palette)
    palette_matchNoNA = palette_match[!is.na(palette_match)]
    dataEXind_var$level = Palette_level[palette_match]
    
    dataEXind_var$color = "grey30"
    dataEXind_var$stroke = 0.5
    if (!grepl("(RAT)|(HYP)", var)) {
        dataEXind_var$color[lim[1] <= dataEXind_var[[var]] &
                            dataEXind_var[[var]] <= lim[2]] = "grey75"
        dataEXind_var$stroke[lim[1] <= dataEXind_var[[var]] &
                             dataEXind_var[[var]] <= lim[2]] = 0.4
    } else {
        dataEXind_var$color[dataEXind_var[[var]]] = "grey75"
        dataEXind_var$stroke[dataEXind_var[[var]]] = 0.4
    }
    
    dataEXind_var$color[is.na(dataEXind_var$fill)] = NA

    level = as.numeric(levels(factor(Palette_level)))

    for (l in level) {
        dataEXind_var_tmp = dplyr::filter(dataEXind_var,
                                          level==l)
        map = map +
            geom_point(data=dataEXind_var_tmp,
                       aes(x=XL93_m, y=YL93_m),
                       color=dataEXind_var_tmp$color,
                       fill=dataEXind_var_tmp$fill,
                       shape=21, size=3,
                       stroke=dataEXind_var_tmp$stroke)
    }


    map = map +
        # Allows to crop shapefile without graphical problem
        coord_sf(xlim=xlim, ylim=ylim,
                 expand=FALSE)


    plan = matrix(c("map", "map", "map",
                    "ca", "cb", "map"),
                  ncol=2)

    herd = bring_grass(verbose=verbose)
    herd = plan_of_herd(herd, plan, verbose=verbose)

    herd = add_sheep(herd,
                     sheep=map,
                     id="map",
                     height=0.7,
                     verbose=verbose)

    ca = panel_colorbar_circle(c(0, 1),
                               c("transparent",
                                 "transparent"),
                               size_circle=2.2,
                               d_line=0.1,
                               linewidth=0.35,
                               d_space=0,
                               d_text=0.5,
                               text_size=2.2,
                               stroke=c(0.4, 0.5),
                               color=c("grey75",
                                       "grey30"),
                               label=c("Aucun avertissement",
                                       "Avertissement"),
                               on_circle=TRUE,
                               margin=margin(t=0.2, r=0.5, b=0.5, l=2.5,
                                             "cm"))
    herd = add_sheep(herd,
                     sheep=ca,
                     id="ca",
                     height=0.2,
                     verbose=verbose)
    
    
    if (grepl("KGE", var)) {
        label = bin
        on_circle = FALSE
        d_space = 0.1
        margin = margin(t=1.5, r=0.5, b=2.5, l=5.5, "cm")

    } else if (grepl("(RAT)|(HYP)", var)) {
        bin = c(0, 1)
        label = c("Test validé",
                  "Test non validé")
        on_circle = TRUE
        d_space = 0
        margin = margin(t=3, r=0.5, b=4, l=4.3, "cm")

    } else {
        label = NULL
        on_circle = FALSE
        d_space = 0.1
        margin = margin(t=0.8, r=0.5, b=1.6, l=5.5, "cm")
    }

    cb = panel_colorbar_circle(bin,
                               Palette,
                               size_circle=2.7,
                               d_line=0.2,
                               linewidth=0.35,
                               d_space=d_space,
                               d_text=0.5,
                               text_size=2.2,
                               label=label,
                               on_circle=on_circle,
                               margin=margin)
    
    herd = add_sheep(herd,
                     sheep=cb,
                     id="cb",
                     height=1,
                     verbose=verbose)
    
    return (herd)
}

