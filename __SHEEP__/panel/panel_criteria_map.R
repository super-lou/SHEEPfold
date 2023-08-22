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
                               is_secteur=FALSE,
                               is_warning=FALSE,
                               model_by_shape=FALSE,
                               remove_warning_lim=FALSE,
                               Shapefiles=Shapefiles,
                               margin=margin(t=0, r=0, b=0, l=0, "cm"),
                               verbose=verbose) {

    is_numeric_logical = function (x) {
        return (is.logical(x) | is.numeric(x))
    }

    Model = levels(factor(dataEXind_model_var$Model))
    nModel = length(Model)
    
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

    print(var)

    ## center
    if (grepl("(Biais)|(^Q)|(^moyQ)|(^V)|(^BF)|(^med[{]v)|(^med[{]t)|(^med[{]debut)|(^med[{]centre)|(^med[{]fin)|(^med[{]dt)", var)) {
        center = 0
    }
    if (grepl("(Rc)|(^epsilon)|(^alpha)|(^a)|(STD)", var)) {
        center = 1
    }

    ## lim
    if (grepl("(KGE)|(NSE)|(r)", var)) {
        lim = c(0.5, 1)
    } else if (grepl("^Biais$", var)) {
        lim = c(-0.2, 0.2)
    } else if (grepl("^Biaismoy$", var)) {
        lim = c(-2, 2)
    } else if (grepl("^Q10$", var)) {
        lim = c(-0.2, 0.2)
    } else if (grepl("^Q90$", var)) {  
        lim = c(-0.8, 0.8)
    } else if (grepl("^med[{]t", var)) {
        lim = c(-1, 1)
    } else if (grepl("(^epsilon)|(^alpha)|(^a)", var)) {
        lim = c(0.5, 2)
    } else if (!grepl("(RAT)|(HYP)", var)) {
        lim = c(center-1, center+1)
    }

    
    ## count warning or agreagate for multi model
    if (is_warning) {
        dataEXind_var = dataEXind_model_var
        if (!grepl("(RAT)|(HYP)", var)) {
            dataEXind_var[[var]] = !(lim[1] < dataEXind_var[[var]] & dataEXind_var[[var]] < lim[2])
        } else {
            dataEXind_var[[var]] = !dataEXind_var[[var]]
        }
        
    } else {
        if (model_by_shape) {
            dataEXind_var = dataEXind_model_var
            
        } else {
            if (!grepl("(RAT)|(HYP)", var)) {
                dataEXind_var =
                    dplyr::summarise(dplyr::group_by(dataEXind_model_var, Code),
                                     !!var:=median(get(var), na.rm=TRUE))
            } else {
                if (nModel == 1) {
                    dataEXind_var = dataEXind_model_var
                } else {
                    return (void())
                }
            } 
        }
    }


    # by secteur or not
    if (is_secteur | is_warning) {
        if (is_warning) {
            dataEXind_var =
                dplyr::summarise(
                           dplyr::group_by(dataEXind_var,
                                           Secteur=substr(Code,
                                                          1, 2)),
                           !!var:=sum(get(var),
                                      na.rm=TRUE)/dplyr::n()*100,
                           .groups="drop")
        } else {
            dataEXind_var =
                dplyr::summarise(dplyr::group_by(dataEXind_var,
                                                 Secteur=substr(Code,
                                                                1, 2)),
                                 !!var:=median(get(var), na.rm=TRUE),
                                 .groups="drop")
        }

    } else {
        dataEXind_var =
            dplyr::left_join(dataEXind_var,
                             dplyr::select(meta,
                                           c("Code",
                                             "XL93_m",
                                             "YL93_m")),
                             by="Code")
    }

    
    if (!is_warning) {
        if (is.null(reverse_palette)) {
            reverse_palette = FALSE
        }
        
        if (grepl("(KGE)|(NSE)|(r)", var)) {
            Palette_level = c(5, 4, 3, 2, 1)
            bin = c(-Inf, 0, 0.25, 0.5, 0.75, 1)
            upBin = c(0, 0.25, 0.5, 0.75, 1)
            lowBin = c(-Inf, 0, 0.25, 0.5, 0.75)
            Palette = get_IPCC_Palette("rainbow_8")[4:8]

        } else if (!grepl("(RAT)|(HYP)", var)) {

            if (grepl("(Biais)|(^Q)|(^alpha)|(^a)|(^moyQ)|(^V)|(^BF)|(^med[{]v)", var)) {
                reverse = FALSE
                name = "ground_8"
            }
            if (grepl("(Rc)|(^epsilon)|(^med[{]t)|(^med[{]debut)|(^med[{]centre)|(^med[{]fin)|(^med[{]dt)|(STD)", var)) {
                reverse = TRUE
                name = "rainbow_8"
            }
            
            Palette_level = c(4, 3, 2, 1, 1, 2, 3, 4)
            Palette = get_IPCC_Palette(name, reverse=reverse)
            if (is.null(min_var)) {
                min_var = quantile(dataEXind_var[[var]],
                                   0.1, na.rm=TRUE)
            }
            if (is.null(max_var)) {
                max_var = quantile(dataEXind_var[[var]],
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
        
        if (!remove_warning_lim) {
            if (!grepl("(RAT)|(HYP)", var)) {
                dataEXind_var$color[lim[1] <= dataEXind_var[[var]] &
                                    dataEXind_var[[var]] <= lim[2]] = "grey75"
                dataEXind_var$stroke[lim[1] <= dataEXind_var[[var]] &
                                     dataEXind_var[[var]] <= lim[2]] = 0.4
            } else {
                dataEXind_var$color[dataEXind_var[[var]]] = "grey75"
                dataEXind_var$stroke[dataEXind_var[[var]]] = 0.4
            }
        }


    } else {
        colorStep = 10
        min_var = 0
        max_var = 100
        Palette_level = 0:colorStep
        Palette = c(IPCCfreshblue,
                    get_IPCC_Palette("red_ramp",
                                     colorStep=colorStep,
                                     reverse=TRUE))
        
        res = compute_colorBin(min_var,
                               max_var,
                               colorStep=colorStep+1,
                               center=NULL,
                               include=c(FALSE, TRUE))
        bin = res$bin
        upBin = res$upBin
        lowBin = res$lowBin
        
        dataEXind_var$fill = get_colors(
            dataEXind_var[[var]],
            upBin=upBin,
            lowBin=lowBin,
            Palette=Palette,
            include_min=c(FALSE, TRUE, rep(FALSE, 9)),
            include_max=c(TRUE, FALSE, rep(TRUE, 9)))
        dataEXind_var$color = "grey75"
        dataEXind_var$stroke = 0.4
    }

    dataEXind_var$shape = 21
    if (model_by_shape) {
        shape = c(21, 22, 23, 24, 25)

        Model = levels(factor(dataEXind_var$Model))
        nModel = length(Model)

        if (nModel > length(shape)) {
            stop ("too many model")
        }
        for (i in 1:nModel) {
            dataEXind_var$shape[dataEXind_var$Model == Model[i]] = shape[i]
        }
    }


    dataEXind_var$color[is.na(dataEXind_var$fill)] = NA

    level = as.numeric(levels(factor(Palette_level)))

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

    if (is_secteur | is_warning) {
        dataEXind_var = dplyr::rename(dataEXind_var,
                                      CdSecteurH=Secteur)
        secteurHydro = dplyr::inner_join(secteurHydro,
                                         dataEXind_var,
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
    
    if (!is_secteur & !is_warning) {
        for (l in level) {
            dataEXind_var_tmp = dplyr::filter(dataEXind_var,
                                              level==l)
            map = map +
                geom_point(data=dataEXind_var_tmp,
                           aes(x=XL93_m, y=YL93_m),
                           color=dataEXind_var_tmp$color,
                           fill=dataEXind_var_tmp$fill,
                           shape=dataEXind_var_tmp$shape,
                           size=3,
                           stroke=dataEXind_var_tmp$stroke)
        }
    }


    map = map +
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

    if (!is_secteur & !is_warning &
        !remove_warning_lim & !model_by_shape) {
        ca = panel_colorbar_circle(c(0, 1),
                                   c("transparent",
                                     "transparent"),
                                   size_circle=2.2,
                                   d_line=0.1,
                                   linewidth=0.35,
                                   d_space=0,
                                   d_text=0.5,
                                   text_size=2.8,
                                   stroke=c(0.4, 0.5),
                                   color=c("grey75",
                                           "grey30"),
                                   label=c("Aucun avertissement",
                                           "Avertissement"),
                                   on_circle=TRUE,
                                   margin=margin(t=0.2, r=0.5,
                                                 b=0.5, l=2.5,
                                                 "cm"))
    } else if (model_by_shape & nModel > 1) {
        ca = panel_colorbar_circle(c(0, 0.5, 1),
                                   c("transparent",
                                     "transparent",
                                     "transparent"),
                                   size_circle=2.2,
                                   d_line=0.1,
                                   linewidth=0.35,
                                   d_space=0,
                                   d_text=0.5,
                                   text_size=2.8,
                                   stroke=c(0.5, 0.5, 0.5),
                                   color=c("grey30",
                                           "grey30",
                                           "grey30"),
                                   label=Model,
                                   shape=shape[1:nModel],
                                   on_circle=TRUE,
                                   margin=margin(t=0.2, r=0.5,
                                                 b=0.2, l=2.5,
                                                 "cm"))
        
    } else {
        ca = void()
    }
    herd = add_sheep(herd,
                     sheep=ca,
                     id="ca",
                     height=0.2,
                     verbose=verbose)
    
    
    if (grepl("KGE|(NSE)|(r)", var)) {
        label = bin
        text_size = 3
        on_circle = FALSE
        d_space = 0.15
        margin = margin(t=1.5, r=0.5, b=2.5, l=5.5, "cm")

    } else if (grepl("(RAT)|(HYP)", var) & !is_warning) {
        bin = c(0, 1)
        text_size = 2.7
        ###########################################
        label = c("Modèle robuste",
                  "Modèle non robuste")
        ###########################################
        on_circle = TRUE
        d_space = 0
        margin = margin(t=3.5, r=0, b=4.8, l=4, "cm")

    } else {
        label = NULL
        text_size = 3
        on_circle = FALSE
        d_space = 0.15
        margin = margin(t=0.3, r=0.5, b=2.1, l=5.5, "cm")
    }

    cb = panel_colorbar_circle(bin,
                               Palette,
                               size_circle=3.3,
                               d_line=0.2,
                               linewidth=0.35,
                               d_space=d_space,
                               d_text=0.5,
                               text_size=text_size,
                               label=label,
                               colorText=IPCCgrey50,
                               colorLine=IPCCgrey50,
                               on_circle=on_circle,
                               margin=margin)
    
    herd = add_sheep(herd,
                     sheep=cb,
                     id="cb",
                     height=1,
                     verbose=verbose)
    
    return (herd)
}

