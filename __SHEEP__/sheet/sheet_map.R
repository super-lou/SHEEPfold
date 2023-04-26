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
#' @title Map panel
#' @export
sheet_map = function (list_df2plot, meta, shapefile_list,
                      idPer_trend=1, trend_period, mean_period,
                      colorForce=FALSE, exQprob=0.01, codeLight=NULL,
                      mapType='trend', margin=NULL, showSea=TRUE,  
                      foot_note=FALSE, foot_height=0,
                      logo_path=NULL, zone_to_show='France', mode="",
                      df_page=NULL, outdirTmp_pdf='',
                      outdirTmp_png='', verbose=TRUE) {
    
    # Extract shapefiles
    france = shapefile_list$france
    basin = shapefile_list$basin
    subBasin = shapefile_list$subBasin
    codeBasin = shapefile_list$codeBasin
    river = shapefile_list$river

    # Number of variable/plot
    if (is.null(list_df2plot)) {
        nVar = 1
    } else {
        nVar = length(list_df2plot)
    }
    
    # Get all different stations code
    Code = rle(data$Code)$value
    nCode = length(Code)

    if (mapType == 'trend' & !is.null(trend_period)) {
        # Convert 'trend_period' to list
        trend_period = as.list(trend_period)
        # Number of trend period
        nPeriod_trend = length(trend_period)
        
        # Extracts the min and the max of the mean trend
        # for all the station
        res = get_valueExtremes(list_df2plot, Code, nPeriod_trend,
                                nVar, nCode, valueType="trend",
                                colorForce=colorForce,
                                minQprob=exQprob, maxQprob=1-exQprob)
        minTrendX = res$min
        maxTrendX = res$max
    }

    # If there is a 'mean_period'
    if (mapType == 'mean' & !is.null(mean_period)) {
        # Convert 'mean_period' to list
        mean_period = as.list(mean_period)
        # Number of mean period
        nPeriod_mean = length(mean_period)

        res = get_valueExtremes(list_df2plot, Code,
                                nPeriod_mean, nVar, nCode,
                                valueType="break",
                                minQprob=exQprob, maxQprob=1-exQprob)
        minBreakX = res$min
        maxBreakX = res$max
        breakX_code = res$value

        nMap = nPeriod_mean - 1
        
    } else {
        nMap = 1
    }

    if (mapType == 'regime') {
        regimeColorSample = c('#005249',
                              '#3e8baa',
                              '#a9c0cb')
        names(regimeColorSample) = c('Pluvial',
                                     'Transition',
                                     'Nival Glaciaire')
        nRegime = length(regimeColorSample)
        regimeColor = c()
        for (code in Code) {
            regime = meta$regimeHydro[meta$Code == code]
            color = regimeColorSample[regime]
            regimeColor = c(regimeColor, color)
        }
    }
    
    # Number of ticks for the colorbar
    colorStep = 10

    for (j in 1:nMap) {
        # For all variable
        for (i in 1:nVar) {
            # If there is a specified station code to highlight (mini map)
            # and there has already been one loop
            condition = (i > 1 | j > 1 ) & (mapType == 'mini' | mapType == 'regime')
            if (condition) {
                # Stop the for loop over the variable
                break
            }
            
            # Extracts the variable of the plot
            var = list_df2plot[[i]]$var
            # Extracts the type of variable of the plot
            type = list_df2plot[[i]]$type
            unit = list_df2plot[[i]]$unit
            # Explanations about the variable
            glose = list_df2plot[[i]]$glose
            
            # Creates a name for the map
            if (mapType == 'trend') {
                outname = paste('map_', var, sep='')
            } else if (mapType == 'mean') {
                outname = paste('map_d', var, sep='')
            } else if (mapType == 'regime') {
                outname = paste('map_regime', sep='')
            }

            # If there is the verbose option
            if (verbose) {
                
                if (mapType == 'trend') {
                    mapName = 'tendence'
                } else if (mapType == 'mean') {
                    mapName = 'difference'
                }

                # Prints the name of the map
                print(paste('Map of ', mapName, ' for : ', var,
                            "   (",
                            round(i/nVar*100, 0),
                            " %)", 
                            sep=''))
            }

            # If there is no specified station code to highlight
            # (mini map)
            if (mapType != 'mini') {
                # Sets the size of the countour
                sizefr = 0.45
                sizebs = 0.4
                sizecbs = 0.5
                sizerv = 0.3
            } else {
                sizefr = 0.35
                sizebs = 0.3
                sizecbs = 0.4
                sizerv = 0.2
            }

            # Stores the coordonate system 
            cf = coord_fixed()
            # Makes it the default one to remove useless warning
            cf$default = TRUE

            if (mode == "dark") {
                FRfill_color = NA
                RV_color = "grey40"
                HBcontour_color = NA
                FRcontour_color = "#FAFAFA"
            } else {
                FRfill_color = "grey97"
                RV_color = "grey80"
                HBcontour_color = "grey70"
                FRcontour_color = "grey40"
            }
            
            # Open a new plot with the personalise theme
            map = ggplot() + theme_void() +
                
                # theme(panel.background=element_rect(color = "#EC4899")) + 
                
                # Fixed coordinate system (remove useless warning)
                cf +
                # Plot the background of France
                geom_sf(data=france,
                        color=NA,
                        fill=FRfill_color)
            
            # If the river shapefile exists
            if (!is.null(river)) {
                # Plot the river
                map = map +
                    geom_sf(data=river,
                            color=RV_color,
                            fill=NA,
                            size=sizerv)
            }
            
            map = map +
                # Plot the hydrological basin
                geom_sf(data=basin,
                        color=HBcontour_color,
                        fill=NA,
                        size=sizebs)
            
            if (zone_to_show == 'Adour-Garonne') {
                map = map +
                    # Plot the hydrological sub-basin
                    geom_sf(data=subBasin,
                            color=HBcontour_color,
                            fill=NA,
                            size=sizebs)
            }
            
            map = map +
                # Plot the countour of France
                geom_sf(data=france,
                        color=FRcontour_color,
                        fill=NA,
                        size=sizefr)
            
            if (mapType == 'regime') {
                # color = regimeColor[match(df_codeBasin$Code, Code)]
                color = 'grey20'
                map = map +
                    # Plot the hydrological code basins
                    geom_sf(data=codeBasin,
                            color=color,
                            fill=NA,
                            size=sizecbs)
            }
            
            if (zone_to_show == 'Adour-Garonne') {
                if (mapType != 'mini') {
                xBasin = c(410000, 520000, 630000,
                           620000, 510000, 450000,
                           390000, 390000)
                yBasin = c(6280000, 6290000, 6320000,
                           6385000, 6450000, 6530000,
                           6365000, 6353000)
                nameBasin = c('Adour', 'Garonne', 'Tarn-Aveyron',
                              'Lot', 'Dordogne', 'Charente',
                              'Fleuves-', 'Côtiers')
                nBasin = length(xBasin)

                plot_basin = tibble(x=xBasin, y=yBasin, label=nameBasin)
                
                map = map +
                    geom_shadowtext(data=plot_basin,
                                    aes(x=x, y=y, label=label),
                                    fontface="bold",
                                    color="grey80",
                                    bg.colour="grey97",
                                    hjust=0.5, vjust=0.5, size=5)
                }
                            
                # If the sea needs to be shown
                if (showSea) {
                    # Leaves space around the France
                    xlim = c(295000, 790000)
                    ylim = c(6125000, 6600000)
                    # Otherwise
                } else {
                    # Leaves minimal space around France
                    xlim = c(305000, 790000)
                    ylim = c(6135000, 6600000)
                }

                # If there is no specified station code to
                # highlight (mini map)
                if (mapType != 'mini') {
                    # Sets a legend scale start
                    xmin = gpct(4, xlim, shift=TRUE)
                    # Sets graduations
                    xint = c(0, 10*1E3, 50*1E3, 100*1E3)
                    # Sets the y postion
                    ymin = gpct(5, ylim, shift=TRUE)
                    # Sets the height of graduations
                    ymax = ymin + gpct(1, ylim)
                    # Size of the value
                    size = 3
                    # Size of the 'km' unit
                    sizekm = 2.5
                    # If there is a specified station code
                } else {
                    # Same but with less graduation and smaller size
                    xmin = gpct(2, xlim, shift=TRUE)
                    xint = c(0, 100*1E3)
                    ymin = gpct(1, ylim, shift=TRUE)
                    ymax = ymin + gpct(3, ylim)
                    size = 2
                    sizekm = 1.5
                }
                
            } else if (zone_to_show == 'France') {
                # Leaves space around the France
                xlim = c(90000, 1250000)
                ylim = c(6040000, 7120000)

                # If there is no specified station code to
                # highlight (mini map)
                if (mapType != 'mini') {
                    # Sets a legend scale start
                    xmin = gpct(4, xlim, shift=TRUE)
                    # Sets graduations
                    xint = c(0, 50*1E3, 100*1E3, 250*1E3)
                    # Sets the y postion
                    ymin = gpct(5, ylim, shift=TRUE)
                    # Sets the height of graduations
                    ymax = ymin + gpct(1, ylim)
                    # Size of the value
                    size = 3
                    # Size of the 'km' unit
                    sizekm = 2.5
                    # If there is a specified station code
                } else {
                    # Same but with less graduation and smaller size
                    xmin = gpct(2, xlim, shift=TRUE)
                    xint = c(0, 250*1E3)
                    ymin = gpct(1, ylim, shift=TRUE)
                    ymax = ymin + gpct(3, ylim)
                    size = 2
                    sizekm = 1.5
                }
            }

            if (mapType != "minimal") {
                map = map +
                    # Adds the base line of the scale
                    geom_line(aes(x=c(xmin, max(xint)+xmin),
                                  y=c(ymin, ymin)),
                              color="grey40", size=0.2) +
                    # Adds the 'km' unit
                    annotate("text",
                             x=max(xint)+xmin+gpct(1, xlim), y=ymin,
                             vjust=0, hjust=0, label="km",
                             color="grey40", size=sizekm)
                # For all graduations
                for (x in xint) {
                    map = map +
                        # Draws the tick
                        annotate("segment",
                                 x=x+xmin, xend=x+xmin, y=ymin, yend=ymax,
                                 color="grey40", size=0.2) +
                        # Adds the value
                        annotate("text",
                                 x=x+xmin, y=ymax+gpct(0.5, ylim),
                                 vjust=0, hjust=0.5, label=x/1E3,
                                 color="grey40", size=size)
                }
            }
            
            map = map +
                # Allows to crop shapefile without graphical problem
                coord_sf(xlim=xlim, ylim=ylim,
                         expand=FALSE)
            
            # If there is no margins specified
            if (is.null(margin)) {
                # Sets all margins to 0
                map = map + 
                    theme(plot.margin=margin(t=0, r=0, b=0, l=0,
                                             unit="mm"))
                # Otherwise
            } else {
                # Sets margins to the given ones
                map = map + 
                    theme(plot.margin=margin)
            }

            # Blank vector to store data about station
            lon = c()
            lat = c()
            fill = c()
            shape = c()
            X = c()
            OkVal = c()
            # For all code
            for (k in 1:nCode) {
                # Gets the code
                code = Code[k]

                if (mapType == 'mean') {
                    value = breakX_code[j+1, i, k]
                    minX = minBreakX[j+1, i]
                    maxX = maxBreakX[j+1, i]
                    pVal = 0
                    
                } else if (mapType == 'trend') {

                    # Extracts the data corresponding to the
                    # current variable
                    data = list_df2plot[[i]]$data
                    # Extracts the trend corresponding to the
                    # current variable
                    trend = list_df2plot[[i]]$trend
                    # Gets the risk of the test
                    level = list_df2plot[[i]]$level
                    # Extracts the data corresponding to the code
                    data_code = data[data$Code == code,]

                    # Extracts the trend corresponding to the code
                    trend_code = trend[trend$Code == code,]
                    
                    # Extract start and end of trend periods
                    Start = trend_code$start[idPer_trend]
                    End = trend_code$end[idPer_trend]

                    # Extracts the corresponding data for the period
                    data_code_per =
                        data_code[data_code$Date >= Start 
                                     & data_code$Date <= End,]
                    # Same for trend
                    trend_code_per = 
                        trend_code[trend_code$start == Start 
                                      & trend_code$end == End,]

                    # Computes the number of trend analysis selected
                    Ntrend = nrow(trend_code_per)
                    # If there is more than one trend on the same period
                    if (Ntrend > 1) {
                        # Takes only the first because they are similar
                        trend_code_per = trend_code_per[1,]
                    }

                    # If it is a flow variable
                    if (unit == 'm^{3}' | unit == 'm^{3}.s^{-1}') {
                        # Computes the mean of the data on the period
                        dataMean = mean(data_code_per$X,
                                        na.rm=TRUE)
                        # Normalises the trend value by the mean
                        # of the data
                        value = trend_code_per$a / dataMean
                        # If it is a date variable
                    } else if (unit == 'jour' | unit == "jour de l'année") {
                        value = trend_code_per$a
                    }

                    minX = minTrendX[idPer_trend, i]
                    maxX = maxTrendX[idPer_trend, i]
                    pVal = trend_code_per$p

                } else {
                    value = NA
                    minX = NULL
                    maxX = NULL
                    pVal = 0
                }
                
                # Computes the color associated to the trend
                color_res = get_color(value,
                                      minX,
                                      maxX,
                                      Palette=Palette_ground(),
                                      colorStep=colorStep,
                                      reverse=FALSE)
                
                if (mapType == 'trend') {
                    # If it is significative
                    if (pVal <= level){
                        # The computed color is stored
                        filltmp = color_res
                        # If the mean tend is positive
                        if (value > 0) {
                            # Uses a triangle up for the shape
                            # of the marker
                            shapetmp = 24
                            # If negative
                        } else {
                            # Uses a triangle down for the shape
                            # of the marker
                            shapetmp = 25
                        }
                    } else if (pVal > level & colorForce) {
                        # The computed color is stored
                        filltmp = color_res
                        # The marker is a circle
                        shapetmp = 21 
                        # If it is not significative
                    } else {
                        # The fill color is grey
                        filltmp = 'grey97'
                        # The marker is a circle
                        shapetmp = 21 
                    }
                } else {
                    # The computed color is stored
                    filltmp = color_res
                    # The marker is a circle
                    shapetmp = 21
                }

                # Extracts the localisation of the current station
                lontmp =
                    meta$XL93_m[meta$Code == code]           
                lattmp =
                    meta$YL93_m[meta$Code == code]
                
                # Stores all the parameters
                lon = c(lon, lontmp)
                lat = c(lat, lattmp)
                fill = c(fill, filltmp)
                shape = c(shape, shapetmp)
                X = c(X, value)
                # If the trend analysis is significative a TRUE is stored
                OkVal = c(OkVal, pVal <= level)
            }
            
            # Creates a tibble to stores all the data to plot
            plot_map = tibble(lon=lon, lat=lat, fill=fill,
                              shape=shape, Code=Code, OkVal=OkVal)

            # If there is no specified station code to highlight
            # (mini map)
            if (mapType == 'trend' | mapType == 'mean') {

                plot_map_NOk = plot_map[!plot_map$OkVal,]
                plot_map_Ok = plot_map[plot_map$OkVal,]

                if (nrow(plot_map_NOk) > 0) {
                    map = map +
                        # Plots the point that are not
                        # significant first
                        geom_point(data=plot_map_NOk,
                                   aes(x=lon, y=lat),
                                   shape=shape[!OkVal],
                                   size=5, stroke=1,
                                   color='grey50', fill=fill[!OkVal])
                }
                if (nrow(plot_map_Ok) > 0) {
                    map = map +
                        # Plots the point that are significant last
                        geom_point(data=plot_map_Ok,
                                   aes(x=lon, y=lat),
                                   shape=shape[OkVal], size=5, stroke=1,
                                   color='grey50', fill=fill[OkVal])
                }

                # Computes the colorbar info
                palette_res = compute_colorBin(minX,
                                               maxX,
                                               Palette=Palette_ground(),
                                               colorStep=colorStep,
                                               reverse=FALSE)

                bin = palette_res$bin
                upBin = palette_res$upBin
                lowBin = palette_res$lowBin
                
                midBin = (bin[2:(colorStep-1)] + bin[1:(colorStep-2)])/2
                dBin = mean(diff(midBin))
                midBin = c(midBin[1]-dBin, midBin, midBin[(colorStep-2)]+dBin)
                midBinNorm = (midBin - min(midBin)) / (max(midBin) - min(midBin))
                
                color = palette_res$Palette
                
                # Spreading of the colorbar
                valNorm = colorStep * 2.65
                base = 70.5 - valNorm
                # Normalisation of the position of ticks
                Ypal = midBinNorm * valNorm + base
                
                # X position of ticks all similar
                Xpal = rep(1.62, times=colorStep)

                # Computes the label of the tick of the colorbar
                nCharLim = 4
                if (unit == 'm^{3}' | unit == 'm^{3}.s^{-1}') {
                    labelRaw = bin*100
                } else if (unit == 'jour' | unit == "jour de l'année") {
                    labelRaw = bin
                }
                label2 = signif(labelRaw, 2)
                label2[label2 >= 0] = paste0("+", label2[label2 >= 0])
                label1 = signif(labelRaw, 1)
                label1[label1 >= 0] = paste0("+", label1[label1 >= 0])
                label = label2        
                label[nchar(label2) > nCharLim] = label1[nchar(label2) > nCharLim]
                label = gsub("[+]", "  ", label)                
                label = c("\\downarrow ", label, "\\uparrow ")
                
                # X position of ticks all similar
                Xlab = rep(1, times=colorStep+1)
                dY = mean(diff(Ypal))
                Ylab = Ypal - dY/2
                Ylab = c(Ylab, Ylab[colorStep] + dY/2 + dY/3)
                Ylab[1] = Ylab[1] + dY/3
                
                # Creates a tibble to store all parameters of colorbar
                plot_palette = tibble(Xpal=Xpal, Ypal=Ypal,
                                      color=color)
                
                nbLine = as.integer(nchar(glose)/40) + 1

                nbNewline = 0
                nbLim = 43
                gloseName = glose
                nbChar = nchar(gloseName)
                while (nbChar > nbLim) {
                    nbNewline = nbNewline + 1
                    posSpace = which(strsplit(gloseName, "")[[1]] == " ")
                    idNewline = which.min(abs(posSpace - nbLim * nbNewline))
                    posNewline = posSpace[idNewline]
                    gloseName = paste(substring(gloseName,
                                                c(1, posNewline + 1),
                                                c(posNewline,
                                                  nchar(gloseName))),
                                      collapse="\n")
                    Newline = substr(gloseName,
                                     posNewline + 2,
                                     nchar(gloseName))
                    nbChar = nchar(Newline)
                }

                Yline = 86.8 + 2.6*nbNewline
                Ytitle = Yline + 0.8
   
                # New plot with void theme
                leg = ggplot() + theme_void() +
                    
                    # Plots separation lines
                    geom_line(aes(x=c(0, 9.7), y=c(84.4, 84.4)),
                              size=0.6, color="#00A3A8") +
                    geom_line(aes(x=c(0, 9.7), y=c(Yline, Yline)),
                              size=0.6, color="#00A3A8") +
                    # Writes title
                    geom_shadowtext(data=tibble(x=0, y=Ytitle,
                                                label=var),
                                    aes(x=x, y=y, label=label),
                                    fontface="bold",
                                    color="#00A3A8",
                                    bg.colour="white",
                                    hjust=0, vjust=0, size=10) +
                    
                    # Writes glose
                    geom_shadowtext(data=tibble(x=0, y=85,
                                                label=gloseName),
                                    aes(x=x, y=y, label=label),
                                    fontface="bold",
                                    color="#00A3A8",
                                    bg.colour="white",
                                    hjust=0, vjust=0, size=3) +
    
                    # Plots the point of the colorbar
                    geom_point(data=plot_palette,
                               aes(x=Xpal, y=Ypal),
                               shape=21, size=5, stroke=1,
                               color='white', fill=color)
                
                if (mapType == 'trend') {
                    periodName_trend = paste(
                    format(as.Date(trend_period[[idPer_trend]][1]),
                           '%Y'),
                    format(as.Date(trend_period[[idPer_trend]][2]),
                           '%Y'),
                    sep='-')
                                    
                    XName1 = "Tendances observées"
                    XName2 = paste("sur la période ",
                                       periodName_trend, sep='')
                    # If it is a flow variable
                    if (unit == 'm^{3}' | unit == 'm^{3}.s^{-1}') {
                        unitLeg = bquote(bold("(% par an)"))
                        # If it is a date variable
                    } else if (unit == "jour") {
                        unitLeg = bquote(bold("(jour par an)"))
                    } else if (unit == "jour de l'année") {
                        unitLeg = bquote(bold("(jour de l'année par an)"))
                    }
                    
                } else if (mapType == 'mean') {
                    periodName1_mean = paste(
                    format(as.Date(mean_period[[1]][1]),
                           '%Y'),
                    format(as.Date(mean_period[[1]][2]),
                           '%Y'),
                    sep='-')
                periodName2_mean = paste(
                    format(as.Date(mean_period[[2]][1]),
                           '%Y'),
                    format(as.Date(mean_period[[2]][2]),
                           '%Y'),
                    sep='-')
                                    
                    XName1 = "Écarts observés entre"
                    XName2 = paste(periodName1_mean,
                                       " et ",
                                       periodName2_mean,
                                       sep='')
                    # If it is a flow variable
                    if (unit == 'm^{3}' | unit == 'm^{3}.s^{-1}') {
                        unitLeg = bquote(bold("(%)"))
                        # If it is a date variable
                    } else if (unit == 'jour') {
                        unitLeg = bquote(bold("(jour)"))
                    } else if (unit == "jour de l'année") {
                        unitLeg = bquote(bold("(jour de l'année)"))
                    }
                }

                leg = leg +
                    # Name of the colorbar
                    annotate('text',
                             x=0, y=81.5,
                             label=XName1,
                             hjust=0, vjust=0.5,
                             size=6, color='grey40') +
                    # Second line
                    annotate('text',
                             x=0, y=78.8,
                             label=XName2,
                             hjust=0, vjust=0.5,
                             size=6, color='grey40') +
                    # Unit legend of the colorbar
                    annotate('text',
                             x=0, y=75.6,
                             label=unitLeg,
                             hjust=0, vjust=0.5,
                             size=4, color='grey40')
                
                # For all the ticks
                for (id in 1:(colorStep+1)) {
                    leg = leg +
                        # Adds the value
                        annotate('text', x=Xlab[id],
                                 y=Ylab[id],
                                 label=TeX(
                                     paste0("\\textbf{", label[id], "}")),
                                 hjust=1, vjust=0.75, 
                                 size=3, color='grey40')
                }

                if (mapType == 'trend') {
                    upLabel = bquote(bold("Hausse significative à 10%"))
                    noneLabel = bquote(bold("Non significatif à 10%"))
                    downLabel = bquote(bold("Baisse significative à 10%"))

                    yUp = 38
                    yNone = 35.6
                    yDown = 32.7
                    
                    leg = leg +
                        # Up triangle in the marker legend
                        geom_point(aes(x=0.7, y=yUp),
                                   shape=24, size=4, stroke=1,
                                   color='grey50', fill='grey97') +
                        # Up triangle text legend
                        annotate('text',
                                 x=1.4, y=yUp,
                                 label=upLabel,
                                 hjust=0, vjust=0.5,
                                 size=3, color='grey40')

                    leg = leg +
                        # Circle in the marker legend
                        geom_point(aes(x=0.7, y=yNone),
                                   shape=21, size=4, stroke=1,
                                   color='grey50', fill='grey97') +
                        # Circle text legend
                        annotate('text',
                                 x=1.4, y=yNone,
                                 label=noneLabel,
                                 hjust=0, vjust=0.7,
                                 size=3, color='grey40')
                    
                    leg = leg +
                        # Down triangle in the marker legend
                        geom_point(aes(x=0.7, y=yDown),
                                   shape=25, size=4, stroke=1,
                                   color='grey50', fill='grey97') +
                        # Down triangle text legend
                        annotate('text',
                                 x=1.4, y=yDown,
                                 label=downLabel,
                                 hjust=0, vjust=0.5,
                                 size=3, color='grey40')
                }
                

                # Takes only the significative ones
                yXOk = X[OkVal]
                yXNOk = X[!OkVal]

                # Histogram distribution
                # Computes the histogram of values
                res_hist = hist(yXOk,
                                breaks=c(-Inf, bin, Inf),
                                plot=FALSE)
                # Extracts the number of counts per cells
                countsOk = res_hist$counts
                # Extracts middle of cells 
                mids = res_hist$mids
                mids = mids[2:(colorStep-1)]
                dM = mean(diff(mids))
                mids = c(mids[1] - dM, mids, mids[colorStep-2] + dM)

                # Histogram distribution
                # Computes the histogram of values
                res_hist = hist(yXNOk,
                                breaks=c(-Inf, bin, Inf),
                                plot=FALSE)
                # Extracts the number of counts per cells
                countsNOk = res_hist$counts
               
                counts = countsOk + countsNOk

                # Blank vectors to store position of points of
                # the distribution to plot
                xX = c()
                yX = c()
                color = c()
                shape = c()
                # Start X position of the distribution
                start_hist = 2.5

                # X separation bewteen point
                hist_sep = 0.35

                # Gets the maximun number of point of the distribution
                maxCount = max(counts, na.rm=TRUE)
                # Limit of the histogram
                lim_hist = 8.4
                len_hist = maxCount * hist_sep + start_hist
                # If the number of point will exceed the limit
                if (len_hist > lim_hist) {
                    # Computes the right amount of space between points
                    hist_sep = (lim_hist - start_hist) / maxCount
                    len_hist = lim_hist
                }
                
                # For all cells of the histogram
                for (ii in 1:length(mids)) {
                    # If the count in the current cell is not zero
                    if (counts[ii] != 0) {
                        # Stores the X positions of points of the 
                        # distribution for the current cell
                        xX = c(
                            xX,
                            seq(start_hist,
                                start_hist+(counts[ii]-1)*hist_sep,
                                by=hist_sep))
                    }
                    # Stores the Y position which is the middle of the
                    # current cell the number of times it has been counted
                    yX = c(yX, rep(mids[ii],
                                           times=counts[ii]))
                    
                    color = c(color, rep('grey50',
                                         times=countsOk[ii]))
                    color = c(color, rep('grey80',
                                         times=countsNOk[ii]))

                    if (mapType == 'trend') {
                        if (mids[ii] < 0) {
                            shapetmp = 25
                        } else {
                            shapetmp = 24
                        }
                        shape = c(shape, rep(shapetmp,
                                             times=countsOk[ii]))
                        shape = c(shape, rep(21,
                                             times=countsNOk[ii]))
                    } else if (mapType == 'mean') {
                        shape = 21
                    }
                }

                yXNorm =
                    (yX - min(midBin)) / (max(midBin) - min(midBin)) * valNorm + base - 0.2
                
                # Makes a tibble to plot the distribution
                plot_value = tibble(xX=xX, yX=yXNorm)

                if (nCode <= 60) {
                    leg = leg +
                        # Plots the point of the distribution
                        geom_point(data=plot_value,
                                   aes(x=xX, y=yX),
                                   shape=shape,
                                   color=color,
                                   fill=color, stroke=0.4,
                                   alpha=1)
                } else {
                    len_hist = start_hist
                }

                if (unit == 'm^{3}' | unit == 'm^{3}.s^{-1}') {
                    labelArrow = 'Plus sévère'
                } else if (unit == "jour de l'année") {
                    labelArrow = 'Plus tôt'
                } else if (unit == 'jour') {
                    labelArrow = 'Plus cours'
                }

                # Position of the arrow
                xArrow = len_hist + 0.2

                leg = leg +
                    # Arrow to show a worsening of the situation
                    geom_segment(aes(x=xArrow, y=valNorm*0.75+base,
                                     xend=xArrow, yend=valNorm*0.25+base),
                                 color='grey50', size=0.3,
                                 arrow=arrow(length=unit(2, "mm"))) +
                    # Text associated to the arrow
                    annotate('text',
                             x=xArrow+0.17, y=valNorm*0.5+base,
                             label=labelArrow,
                             angle=90,
                             hjust=0.5, vjust=1,
                             size=3, color='grey50')
                
                leg = leg +
                    # X axis of the colorbar
                    scale_x_continuous(limits=c(0, 10),
                                       expand=c(0, 0)) +
                    # Y axis of the colorbar
                    scale_y_continuous(limits=c(0, 100),
                                       expand=c(0, 0)) +
                    # Margin of the colorbar
                    theme(plot.margin=margin(t=0, r=0, b=0, l=0, unit="mm"))

                annText = "Les stations hydrométriques examinées
présentent des enregistrements sur plus
de 40 ans. Les mesures y sont réputées
bonnes par le gestionnaire et les débits
peu altérés par les activités humaines."

                if (mapType == 'trend') {
                    yAnn = 18
                } else if (mapType == 'mean') {
                    yAnn = 29
                }

                leg = leg +
                    annotate('text',
                             x=0, y=yAnn,
                             label=annText,
                             hjust=0, vjust=0,
                             size=3, color='grey70',
                             fontface='italic')

                
            # If there is a specified station code
            } else if (mapType %in% c('mini', 'minimal')) {
                if (!is.null(codeLight)) {
                    
                    # Extract data of all stations not to highlight
                    plot_map_codeNo = plot_map[plot_map$Code != codeLight,]
                    # Extract data of the station to highlight
                    plot_map_code = plot_map[plot_map$Code == codeLight,]
                    # Plots only the localisation
                    map = map +
                        # For all stations not to highlight
                        geom_point(data=plot_map_codeNo,
                                   aes(x=lon, y=lat),
                                   shape=21, size=0.5, stroke=0.5,
                                   color="grey50",
                                   fill="grey50") +
                        # For the station to highlight
                        geom_point(data=plot_map_code,
                                   aes(x=lon, y=lat),
                                   shape=21, size=2, stroke=0.5,
                                   color='grey97',
                                   fill='#00A3A8')
                } else {
                    if (mode == "dark") {
                        codeAll_color = '#FAFAFA'
                    } else {
                        codeAll_color = 'grey50'
                    }
                    # Plots only the localisation
                    map = map +
                        # For all stations not to highlight
                        geom_point(data=plot_map,
                                   aes(x=lon, y=lat),
                                   shape=21, size=0.5, stroke=0.5,
                                   color=codeAll_color,
                                   fill=codeAll_color)
                }
                
                leg = void()
                
            } else if (mapType == 'regime') {
                nudge_y = rnorm(nCode, mean=0, sd=1000)
                
                # Plots only the localisation
                map = map +
                    # For all stations not to highlight
                    geom_point(data=plot_map,
                               aes(x=lon, y=lat),
                               shape=21, size=3, stroke=0.5,
                               color='grey97', fill=regimeColor) +
                    
                    geom_text_repel(data=plot_map,
                                    aes(x=lon, y=lat, label=code),
                                    segment.colour="grey35",
                                    segment.size=0.25,
                                    min.segment.length=0.25,
                                    force=0.4,
                                    force_pull=1,
                                    size=2.5,
                                    color=regimeColor,
                                    bg.color="grey97",
                                    bg.r=.15)

                yRow1 = 87
                yRow2 = yRow1 - 7
                titleRow1 = "Régimes"
                titleRow2 = "hydrologiques"
                plot_title = tibble(x=c(0, 0),
                                    y=c(yRow1, yRow2),
                                    label=c(titleRow1, titleRow2))

                Xpal = rep(0.8, times=nRegime)
                Ypal = c(62, 67.5, 73)
                label = names(regimeColorSample)
                color = regimeColorSample
                plot_palette = tibble(Xpal=Xpal,
                                      Ypal=Ypal,
                                      color=color,
                                      label=label)
                
                # New plot with void theme
                leg = ggplot() + theme_void() +
                    
                    # Plots separation lines
                    geom_line(aes(x=c(0, 9.7), y=c(79.3, 79.3)),
                              size=0.6, color="#00A3A8") +
                        
                    # Writes title
                    geom_shadowtext(data=plot_title,
                                    aes(x=x, y=y, label=label),
                                    fontface="bold",
                                    color="#00A3A8",
                                    bg.colour="white",
                                    hjust=0, vjust=0, size=10) +
                    
                    # Plots the point of the colorbar
                    geom_point(data=plot_palette,
                               aes(x=Xpal, y=Ypal),
                               shape=21, size=5, stroke=1,
                               color='white', fill=color)
                
                # For all the ticks
                for (id in 1:nRegime) {
                    leg = leg +
                        # Adds the value
                        annotate('text', x=Xpal[id]+0.8,
                                 y=Ypal[id],
                                 label=bquote(bold(.(label[id]))),
                                 hjust=0, vjust=0.7, 
                                 size=4, color='grey40')
                }
                
                leg = leg +
                    # X axis of the colorbar
                    scale_x_continuous(limits=c(0, 10),
                                       expand=c(0, 0)) +
                    # Y axis of the colorbar
                    scale_y_continuous(limits=c(0, 100),
                                       expand=c(0, 0)) +
                    # Margin of the colorbar
                    theme(plot.margin=margin(t=0, r=0, b=0, l=0,
                                             unit="mm"))
            }

            if (!is.null(df_page)) {
                if (mapType == 'trend') {
                    section = 'Carte des tendances observées'
                    subsection = var
                } else if (mapType == 'mean') {
                    section = 'Carte des écarts observés'
                    subsection = var
                } else if (mapType == 'regime') {
                    section = 'Carte des régimes hydrologiques'
                    subsection = NA
                }
                
                n_page = df_page$n[nrow(df_page)] + 1
                # N_page = df_page$N[nrow(df_page)] + 1
                df_page = bind_rows(
                    df_page,
                    tibble(section=section,
                           subsection=subsection,
                           n=n_page))
            }
            
            # If there is a foot note
            if (foot_note) {
                if (mapType == 'trend') {
                    footName = 'carte des tendances observées'
                } else if (mapType == 'mean') {
                    footName = 'carte des écarts observés'
                } else if (mapType == 'regime') {
                    footName = 'carte des régimes hydrologiques'
                }
                
                if (is.null(df_page) | nrow(df_page) == 0) {
                    n_page = i
                }

                foot = panel_foot(footName, n_page,
                                  foot_height, logo_path)

                # Stores the map, the title and the colorbar in a list
                P = list(map, leg, foot)
                LM = matrix(c(1, 1, 1, 2,
                              3, 3, 3, 3),
                            nrow=2, byrow=TRUE)
            } else {
                foot_height = 0
                # Stores the map, the title and the colorbar in a list
                P = list(map, leg)
                LM = matrix(c(1, 1, 1, 2),
                            nrow=1, byrow=TRUE)
            }

            id_leg = 2
            id_foot = 3
            
            LMcol = ncol(LM)
            LMrow = nrow(LM)
            
            LM = rbind(rep(99, times=LMcol), LM, rep(99, times=LMcol))
            LMrow = nrow(LM)
            LM = cbind(rep(99, times=LMrow), LM, rep(99, times=LMrow))
            LMcol = ncol(LM)
            
            margin_size = 0.5
            height = 21
            width = 29.7

            leg_height = height - 2*margin_size - foot_height

            Hcut = LM[, LMcol-1]
            heightLM = rep(0, times=LMrow)
            heightLM[Hcut == id_leg] = leg_height
            heightLM[Hcut == id_foot] = foot_height
            heightLM[Hcut == 99] = margin_size

            col_width = (width - 2*margin_size) / (LMcol - 2)
            
            Wcut = LM[(nrow(LM)-1),]
            widthLM = rep(col_width, times=LMcol)
            widthLM[Wcut == 99] = margin_size

            # Arranges the graphical object
            plot = grid.arrange(grobs=P, layout_matrix=LM,
                                heights=heightLM, widths=widthLM)

            # If there is no specified station code to highlight
            # (mini map)
            if (!(mapType %in% c("mini", "minimal"))) {
                # Saving matrix plot
                ggsave(plot=plot,
                       path=outdirTmp_pdf,
                       filename=paste(outname, '.pdf', sep=''),
                       width=width, height=height, units='cm', dpi=100)

                ggsave(plot=plot,
                       path=outdirTmp_png,
                       filename=paste(outname, '.png', sep=''),
                       width=width, height=height, units='cm', dpi=400)
            }
        }
    }
    # If there is no specified station code to highlight
    # (mini map)
    if (!(mapType %in% c('mini', "minimal"))) {
        return (df_page)
        # Returns the map object
    } else {
        return (map)
    }
}
 
