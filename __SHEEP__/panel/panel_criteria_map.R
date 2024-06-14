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
panel_criteria_map = function (dataEX_criteria_hm_variable,
                               metaEX_criteria,
                               meta,
                               min_variable,
                               max_variable,
                               prob=0.1,
                               is_secteur=FALSE,
                               is_warning=FALSE,
                               hm_by_shape=FALSE,
                               remove_warning_lim=FALSE,
                               Shapefiles=Shapefiles,
                               margin=margin(t=0, r=0, b=0, l=0, "cm"),
                               verbose=verbose) {

    is_numeric_logical = function (x) {
        return (is.logical(x) | is.numeric(x))
    }

    HM = levels(factor(dataEX_criteria_hm_variable$HM))
    nHM = length(HM)
    
    variable = names(dataEX_criteria_hm_variable)[which(sapply(dataEX_criteria_hm_variable,
                                                  is_numeric_logical))[1]]
    
    unit = metaEX_criteria$unit_fr[metaEX_criteria$variable_en == variable]
    is_date = metaEX_criteria$is_date[metaEX_criteria$variable_en == variable]
    is_normalise = metaEX_criteria$is_normalise[metaEX_criteria$variable_en == variable]
    reverse_palette = metaEX_criteria$reverse_palette[metaEX_criteria$variable_en == variable]

    
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
    map = ggplot() + theme_void_Lato() + cf +
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

    print(variable)

    ## center
    if (grepl("(Biais)|(^Q)|(^moyQ)|(^V)|(^BF)|(^med[{]v)|(^med[{]t)|(^med[{]debut)|(^med[{]centre)|(^med[{]fin)|(^med[{]dt)|([_]ratio$)", variable)) {
        center = 0
    } else if (grepl("(Rc)|(^epsilon)|(^alpha)|(^a)|(STD)|(CR)", variable)) {
        center = 1
    } else {
        center = 0
    }


    ## lim
    if (grepl("(KGE)|(NSE)|(^r$)", variable)) {
        lim = c(0.5, 1)
    } else if (grepl("^Biais$", variable)) {
        lim = c(-0.2, 0.2)
    } else if (grepl("^Biaismoy$", variable)) {
        lim = c(-2, 2)
    } else if (grepl("^Q10$", variable)) {
        lim = c(-0.2, 0.2)
    } else if (grepl("^Q90$", variable)) {  
        lim = c(-0.8, 0.8)
    } else if (grepl("^med[{]t", variable)) {
        lim = c(-1, 1)
    } else if (grepl("(^epsilon)|(^alpha)|(^a)", variable)) {
        lim = c(0.5, 2)
    } else if (grepl("(RAT)|(HYP)", variable)) {
        lim = NULL
    } else {
        remove_warning_lim = TRUE
    }

    
    ## count warning or agreagate for multi hm
    if (is_warning) {
        dataEX_criteria_variable = dataEX_criteria_hm_variable
        if (!grepl("(RAT)|(HYP)", variable)) {
            dataEX_criteria_variable[[variable]] = !(lim[1] < dataEX_criteria_variable[[variable]] & dataEX_criteria_variable[[variable]] < lim[2])
        }
        # else {
            # dataEX_criteria_variable[[variable]] = !dataEX_criteria_variable[[variable]]
        # }
        
    } else {
        if (hm_by_shape) {
            dataEX_criteria_variable = dataEX_criteria_hm_variable
            
        } else {
            if (!grepl("(RAT)|(HYP)", variable)) {
                dataEX_criteria_variable =
                    dplyr::summarise(dplyr::group_by(dataEX_criteria_hm_variable, code),
                                     !!variable:=median(get(variable), na.rm=TRUE))
            } else {
                if (nHM == 1) {
                    dataEX_criteria_variable = dataEX_criteria_hm_variable
                } else {
                    return (void())
                }
            } 
        }
    }


    # by secteur or not
    if (is_secteur | is_warning) {

        get_for_coord = function (i, meta) {
            st_point(as.numeric(meta[i, ]))
        }
        points = do.call("st_sfc",
                         c(lapply(1:nrow(meta), 
                                  get_for_coord,
                                  meta=meta[which(grepl("L93",
                                                        names(meta)))]),
                           list("crs"=2154)))

        get_name = function (col) { 
            secteurHydro[which(col), ]$CdSecteurH
        }
        meta$Secteur = apply(st_intersects(secteurHydro,
                                           points,
                                           sparse=FALSE),
                             2, get_name)

        dataEX_criteria_variable = dplyr::left_join(dataEX_criteria_variable,
                                         dplyr::select(meta,
                                                       c("code",
                                                         "Secteur")),
                                         by="code")
        
        if (is_warning) {
            dataEX_criteria_variable =
                dplyr::summarise(
                           dplyr::group_by(dataEX_criteria_variable, Secteur),
                           !!variable:=sum(get(variable),
                                      na.rm=TRUE)/dplyr::n()*100,
                           .groups="drop")
        } else {
            dataEX_criteria_variable =
                dplyr::summarise(dplyr::group_by(dataEX_criteria_variable, Secteur),
                                 !!variable:=median(get(variable), na.rm=TRUE),
                                 .groups="drop")
        }

    } else {
        dataEX_criteria_variable =
            dplyr::left_join(dataEX_criteria_variable,
                             dplyr::select(meta,
                                           c("code",
                                             "XL93_m",
                                             "YL93_m")),
                             by="code")
    }

    
    if (!is_warning) {
        if (is.null(reverse_palette)) {
            reverse_palette = FALSE
        }
        
        if (grepl("(KGE)|(NSE)|(^r$)", variable)) {
            Palette_level = c(5, 4, 3, 2, 1)
            bin = c(-Inf, 0, 0.25, 0.5, 0.75, 1)
            upBin = c(0, 0.25, 0.5, 0.75, 1)
            lowBin = c(-Inf, 0, 0.25, 0.5, 0.75)
            Palette = get_IPCC_Palette("rainbow_8")[4:8]

        } else if (!grepl("(RAT)|(HYP)", variable)) {

            if (grepl("(Biais)|(^Q)|(^alpha)|(^a)|(^moyQ)|(^V)|(^BF)|(^med[{]v)|(CR)|([_]ratio$)|(moyRA)", variable)) {
                reverse = FALSE
                name = "ground_8"
            } else if (grepl("(Rc)|(^epsilon)|(^med[{]t)|(^med[{]debut)|(^med[{]centre)|(^med[{]fin)|(^med[{]dt)|(STD)", variable)) {
                reverse = TRUE
                name = "rainbow_8"
            } else {
                reverse = TRUE
                name = "rainbow_8"
            }
            
            Palette_level = c(4, 3, 2, 1, 1, 2, 3, 4)
            Palette = get_IPCC_Palette(name, reverse=reverse)
            if (is.null(min_variable)) {
                min_variable = quantile(dataEX_criteria_variable[[variable]],
                                   prob, na.rm=TRUE)
            }
            if (is.null(max_variable)) {
                max_variable = quantile(dataEX_criteria_variable[[variable]],
                                   1-prob, na.rm=TRUE)
            }

            


            
            res = compute_colorBin(min_variable,
                                   max_variable,
                                   colorStep=8,
                                   center=center,
                                   include=FALSE)
            bin = res$bin
            upBin = res$upBin
            lowBin = res$lowBin

            # print(dataEX_criteria_variable)
            # print(min_variable)
            # print(max_variable)
            # print(center)
            # print(lowBin)
            # print(upBin)
            # print(bin)
            # print("")

            # print(bin)
            # print(length(bin))
            # print(length(Palette))
            # print("")
            
            
            
        }
        
        if (grepl("(RAT)|(HYP)", variable)) {
            Palette_level = c(1, 2)
            Palette = get_IPCC_Palette("OrangePurple",
                                       reverse=TRUE)[c(2, 5)]
            RAT = dataEX_criteria_variable[[variable]]
            RAT[is.na(RAT)] = TRUE
            fills = rep(Palette[1], length(RAT))
            fills[RAT] = Palette[2]
            dataEX_criteria_variable$fill = fills
        } else {
            dataEX_criteria_variable$fill = get_colors(dataEX_criteria_variable[[variable]],
                                            upBin=upBin,
                                            lowBin=lowBin,
                                            Palette=Palette)
        }

        palette_match = match(dataEX_criteria_variable$fill, Palette)
        palette_matchNoNA = palette_match[!is.na(palette_match)]
        dataEX_criteria_variable$level = Palette_level[palette_match]
        
        dataEX_criteria_variable$color = "grey30"
        dataEX_criteria_variable$stroke = 0.5
        
        if (!remove_warning_lim) {
            if (!grepl("(RAT)|(HYP)", variable)) {
                dataEX_criteria_variable$color[lim[1] <= dataEX_criteria_variable[[variable]] &
                                    dataEX_criteria_variable[[variable]] <= lim[2]] = "grey75"
                dataEX_criteria_variable$stroke[lim[1] <= dataEX_criteria_variable[[variable]] &
                                     dataEX_criteria_variable[[variable]] <= lim[2]] = 0.4
            } else {
                dataEX_criteria_variable$color[!dataEX_criteria_variable[[variable]]] = "grey75"
                dataEX_criteria_variable$stroke[!dataEX_criteria_variable[[variable]]] = 0.4
            }
        }


    } else {
        colorStep = 10
        min_variable = 0
        max_variable = 100
        Palette_level = 0:colorStep
        Palette = c(IPCCfreshblue,
                    get_IPCC_Palette("red_ramp",
                                     colorStep=colorStep,
                                     reverse=TRUE))
        
        res = compute_colorBin(min_variable,
                               max_variable,
                               colorStep=colorStep+1,
                               center=NULL,
                               include=c(FALSE, TRUE))
        bin = res$bin
        upBin = res$upBin
        lowBin = res$lowBin

        dataEX_criteria_variable$fill = get_colors(
            dataEX_criteria_variable[[variable]],
            upBin=upBin,
            lowBin=lowBin,
            Palette=Palette,
            include_min=c(FALSE, TRUE, rep(FALSE, 9)),
            include_max=c(TRUE, FALSE, rep(TRUE, 9)))
        dataEX_criteria_variable$color = "grey75"
        dataEX_criteria_variable$stroke = 0.4
    }

    dataEX_criteria_variable$shape = 21
    if (hm_by_shape) {
        shape = c(21, 22, 23, 24, 25)

        HM = levels(factor(dataEX_criteria_variable$HM))
        nHM = length(HM)

        if (nHM > length(shape)) {
            stop ("too many hm")
        }
        for (i in 1:nHM) {
            dataEX_criteria_variable$shape[dataEX_criteria_variable$HM == HM[i]] = shape[i]
        }
    }


    # dataEX_criteria_variable$color[is.na(dataEX_criteria_variable$fill)] = NA
    dataEX_criteria_variable$color[is.na(dataEX_criteria_variable$fill)] = "grey75"
    dataEX_criteria_variable$level[is.na(dataEX_criteria_variable$fill)] = 0
    
    
    level = as.numeric(levels(factor(dataEX_criteria_variable$level)))

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
        dataEX_criteria_variable = dplyr::rename(dataEX_criteria_variable,
                                      CdSecteurH=Secteur)
        # secteurHydro = dplyr::filter(secteurHydro,
                                     # CdSecteurH %in%
                                     # dataEX_criteria_variable$CdSecteurH)
        secteurHydro = dplyr::full_join(secteurHydro,
                                        dataEX_criteria_variable,
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
            dataEX_criteria_variable_tmp = dplyr::filter(dataEX_criteria_variable,
                                              level==l)
            map = map +
                geom_point(data=dataEX_criteria_variable_tmp,
                           aes(x=XL93_m, y=YL93_m),
                           color=dataEX_criteria_variable_tmp$color,
                           fill=dataEX_criteria_variable_tmp$fill,
                           shape=dataEX_criteria_variable_tmp$shape,
                           size=3,
                           stroke=dataEX_criteria_variable_tmp$stroke)
        }
    }

    # map = map +
    #     geom_point(data=meta,
    #                aes(x=XL93_m, y=YL93_m),
    #                fill="black",
    #                alpha=0.7,
    #                stroke=0,
    #                size=1)

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
        !remove_warning_lim & !hm_by_shape) {
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
    } else if (hm_by_shape & nHM > 1) {
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
                                   label=HM,
                                   shape=shape[1:nHM],
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
    
    
    if (grepl("KGE|(NSE)|(^r$)", variable) & !is_warning) {
        label = bin
        text_size = 3
        on_circle = FALSE
        d_space = 0.15
        margin = margin(t=1.5, r=0.5, b=2.5, l=5.3, "cm")

    } else if (grepl("(RAT)|(HYP)", variable) & !is_warning) {
        bin = c(0, 1)
        text_size = 2.6
        ###########################################
        label = c("Modèle robuste",
                  "Modèle non robuste")
        ###########################################
        on_circle = TRUE
        d_space = -0.05
        margin = margin(t=3.7, r=0, b=4.9, l=3.9, "cm")

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
                               ncharLim=4,
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

