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


## 1. DATASHEET PANEL MANAGER ________________________________________
# Manages datasheets creations for all stations. Makes the call to
# the different headers, trend analysis graphs and realises arranging
# every plots.
#' @title Datasheet panel
#' @export
sheet_station = function (list_df2plot, meta, trend_period,
                            mean_period, linetype_per, axis_xlim,
                            colorForce, exXprob, info_header, time_header,
                            foot_note, structure,
                            info_height, time_height,
                            var_ratio, foot_height,
                            paper_size, shapefile_list, logo_path,
                            zone_to_show, show_colorEvent,
                            outdirTmp_pdf, outdirTmp_png,
                            df_page=NULL, pdf_chunk="all") {
    
    if (!is.null(time_header)) {
        time_header = dplyr::rename(time_header,
                                    X=Q)
    }
    
    # The percentage of augmentation and diminution of the min
    # and max limits for y axis
    lim_pct = 10

    # Number of variable/plot
    nVar = length(list_df2plot)
    
    # Get all different stations code
    Code = rle(data$Code)$value
    nCode = length(Code)

    if (!is.null(trend_period)) {
        # Convert 'trend_period' to list
        trend_period = as.list(trend_period)
        # Number of trend period
        nPeriod_trend = length(trend_period)

        # Extracts the min and the max of the mean trend for all the station
        res = get_valueExtremes(list_df2plot, Code,
                                nPeriod_trend, nVar,
                                nCode,
                                valueType="trend",
                                colorForce=colorForce,
                                minXprob=exXprob, maxXprob=1-exXprob)
        minTrendX = res$min
        maxTrendX = res$max
    }

    # For all the station
    for (k in 1:nCode) {
        # Gets the code
        code = Code[k]
        # Print code of the station for the current plotting
        print(paste("Datasheet for station : ", code,
                    "   (", round(k/nCode*100, 1), " %)", 
                    sep=''))
        
        # Number of header (is info and time serie are needed)
        nbh = as.numeric(!is.null(info_header)) + as.numeric(!is.null(time_header))*3
        nbf = as.numeric(foot_note)
        # Actualises the number of plot
        nbg = nbh + nVar + nbf

        df_P = tibble() # name | first | last | plot
        
        # If the info header is needed
        if (!is.null(info_header)) {

            print("Info header panel")
            
            if ("data.frame" %in% class(info_header)) {
                # Extracts the data serie corresponding to the code
                info_header_code = info_header[info_header$Code == code,]
                to_do = 'all'
            } else {
                info_header_code = NULL
                to_do = info_header
            }

            if (!is.null(mean_period[[1]])) {
                period = mean_period[[1]]
            } else {
                period = trend_period[[1]]
            }
            
            # Gets the info plot
            Hinfo = panel_info_station(list_df2plot, 
                                       meta,
                                       trend_period=trend_period,
                                       mean_period=mean_period,
                                       period=period,
                                       shapefile_list=shapefile_list,
                                       codeLight=code,
                                       data_code=info_header_code,
                                       to_do=to_do,
                                       zone_to_show=zone_to_show)
            
            # Stores it
            df_P = add_plot(df_P,
                            plot=Hinfo,
                            name="info")
        }

        # If the time header is given
        if (!is.null(time_header)) {

            print("Time header panel")
            
            # Extracts the data serie corresponding to the code
            df_X_code = time_header[time_header$Code == code,]
            df_sqrtX_code = compute_sqrt(df_X_code)
            
            if (is.null(axis_xlim)) {
                # Gets the limits of the time serie
                axis_xlim_code = c(min(df_X_code$Date),
                                   max(df_X_code$Date))
            } else {
                axis_xlim_code = axis_xlim
            }

            # Gets the time serie plot
            HX = panel_trend(df_X_code, trend_code=NULL,
                            trend_period=trend_period,
                            axis_xlim=axis_xlim_code, missRect=TRUE,
                            unit2day=365.25, var='Q',
                            unit="m^{3}.s^{-1}",
                            grid=TRUE, ymin_lim=0,
                            first=TRUE, lim_pct=lim_pct)
            # Stores it
            df_P = add_plot(df_P,
                            plot=HX,
                            name="Q",
                            first=TRUE)

            if (any(c("Resume", "Étiage") %in% names(structure))) {
                # Gets the time serie plot
                HsqrtX = panel_trend(df_sqrtX_code, trend_code=NULL,
                                    trend_period=trend_period,
                                    axis_xlim=axis_xlim_code,
                                    missRect=TRUE,
                                    unit2day=365.25,
                                    var='\\sqrt{Q}',
                                    unit="m^{3/2}.s^{-1/2}",
                                    grid=TRUE, ymin_lim=0,
                                    first=TRUE, lim_pct=lim_pct)
                # Stores it
                df_P = add_plot(df_P,
                                plot=HsqrtX,
                                name="\\sqrt{Q}",
                                first=TRUE)

                # Gets the time serie plot
                HsqrtXmid = panel_trend(df_sqrtX_code, trend_code=NULL,
                                       trend_period=trend_period,
                                       axis_xlim=axis_xlim_code,
                                       missRect=TRUE,
                                       unit2day=365.25,
                                       var='\\sqrt{Q}',
                                       unit="m^{3/2}.s^{-1/2}",
                                       grid=TRUE, ymin_lim=0,
                                       first=FALSE, lim_pct=lim_pct)
                # Stores it
                df_P = add_plot(df_P,
                                plot=HsqrtXmid,
                                name="\\sqrt{Q}",
                                first=FALSE)
            }
            
        } else {
            axis_xlim_code = axis_xlim
        }

        # Computes the number of column of plot asked on the datasheet
        var_plotted = c()
        # For all variable
        for (i in 1:nVar) {
            # Extracts the data corresponding to the current variable
            data = list_df2plot[[i]]$data
            # Extracts the trend corresponding to the
            # current variable
            trend = list_df2plot[[i]]$trend
            
            unit2day = list_df2plot[[i]]$unit2day
            # Extract the variable of the plot
            var = list_df2plot[[i]]$var
            var_plotted = c(var_plotted, var)
            
            event = list_df2plot[[i]]$event
            unit = list_df2plot[[i]]$unit
            samplePeriod = list_df2plot[[i]]$samplePeriod

            if (is.tbl(samplePeriod)) {
                samplePeriod_code =
                    samplePeriod$sp[samplePeriod$Code == code]
            } else {
                samplePeriod_code = samplePeriod
            }
            
            # Extracts the data corresponding to the code
            data_code = data[data$Code == code,]
            # Extracts the trend corresponding to the code
            trend_code = trend[trend$Code == code,]

            # Blank vector to store color
            color = c()

            if (!is.null(trend_period)) {
                # For all the period
                for (j in 1:nPeriod_trend) {

                    # If the trend is significant
                    if (trend_code$p[j] <= level){
                        # Extract start and end of trend periods
                        Start = trend_code$start[j]
                        End = trend_code$end[j]

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
                        if (unit == 'hm^{3}' | unit == 'm^{3}.s^{-1}') {
                            # Computes the mean of the data on the period
                            dataMean = mean(data_code_per$X,
                                            na.rm=TRUE)
                            # Normalises the trend value by the mean
                            # of the data
                            value = trend_code_per$a / dataMean
                            # If it is a date variable
                        } else if (unit == "jour de l'année" | unit == 'jour' | unit == 'jour.an^{-1}') {
                            value = trend_code_per$a
                        }


                        reverse = get_reverse(var)
                        color_res = get_color(value,
                                              minTrendX[j, i],
                                              maxTrendX[j, i],
                                              Palette=Palette_ground(),
                                              colorStep=10,
                                              reverse=reverse)
                        # Stores it temporarily
                        colortmp = color_res

                        # Otherwise
                    } else {
                        # Stores the default grey color
                        colortmp = 'grey80'
                        
                    }
                    # Stores the color
                    color = append(color, colortmp)
                    grid = FALSE
                }
            }

            if (var != '\\sqrt{Q}' & var != 'Q') {
                grid = FALSE
                ymin_lim = NULL
            } else {
                grid = TRUE
                ymin_lim = 0
            }

            if (is.null(time_header) & i == 1) {
                first = TRUE
            } else {
                first = FALSE
            }

            print(paste0("Time panel for ", var))

            res = panel_trend(data_code, trend_code,
                             var=var, unit=unit,
                             samplePeriod_code=samplePeriod_code,
                             linetype_per=linetype_per,
                             level=level, colorForce=colorForce,
                             missRect=FALSE,
                             trend_period=trend_period,
                             mean_period=mean_period,
                             axis_xlim=axis_xlim_code, 
                             unit2day=unit2day, grid=grid,
                             ymin_lim=ymin_lim, color=color,
                             first=first,
                             last="all",
                             lim_pct=lim_pct)
            
            df_P = add_plot(df_P,
                            plot=res$lastTRUE,
                            name=var,
                            last=TRUE)
            df_P = add_plot(df_P,
                            plot=res$lastFALSE,
                            name=var,
                            last=FALSE)
        }

        print("Layout") 

        # If paper format is A4
        if (paper_size == 'A4') {
            width = 21
            height = 29.7
        } else if (is.vector(paper_size) & length(paper_size) > 1) {
            width = paper_size[1]
            height = paper_size[2]
        }

        nEvent = length(structure)
        page_code = 0
        for (i in 1:nEvent) {
            
            var_to_place = structure[[i]]
            event = names(structure)[i]

            if (event != 'Resume' & event != 'None' & show_colorEvent) {
                space = 0.7
                colorEvent = get_colorEvent()
                colorTextEvent = get_colorTextEvent()
                Hevent = panel_event(event, colorEvent, colorTextEvent)
                Heventinfo = merge_panel(add=Hevent, to=Hinfo,
                                         widths=c(space, width-space))
                df_P = add_plot(df_P,
                                plot=Heventinfo,
                                name="info",
                                overwrite_by_name=TRUE)
            }
            
            if (event == 'Resume') {
                nVar_max = 4
            } else {
                nVar_max = 5
            }

            nVar_to_place = length(var_to_place)
            nVar_page = ceiling(nVar_to_place/nVar_max)
            
            for (page in 1:nVar_page) {

                page_code = page_code + 1
                
                if (foot_note) {
                    if (event != 'Resume' & event != 'None') {
                        footName = paste0('fiche station : ', event)
                    } else {
                        footName = 'fiche station'
                    }
                    if (is.null(df_page)) {
                        n_page = k + page_code - 1
                    } else {
                        if (nrow(df_page) == 0 | pdf_chunk == 'by_code') {
                            n_page = page_code
                        } else {
                            n_page = df_page$n[nrow(df_page)] + page_code
                        }
                    }
                    foot = panel_foot(footName, n_page,
                                      foot_height, logo_path)
                    df_P = add_plot(df_P,
                                    plot=foot,
                                    name="foot",
                                    overwrite_by_name=TRUE)
                }

                var_to_place_page = var_to_place[(1+(nVar_max*(page-1))) : (nVar_max*page)]
                var_to_place_page = var_to_place_page[!is.na(var_to_place_page)]

                LM_id = c()
                LM_name = c()
                if (!is.null(info_header)) {
                    id_plot = which(df_P$name == "info")
                    LM_id = c(LM_id, id_plot)
                    LM_name = c(LM_name, df_P$name[id_plot])
                    nbi = 1
                } else {
                    nbi = 0
                }

                if (!is.null(time_header)) {
                    if (event == "Resume") {
                        id_plot = c(which(df_P$name == "Q"
                                          & df_P$first == TRUE),
                                    which(df_P$name == "\\sqrt{Q}"
                                          & df_P$first == FALSE))
                        nbt = 2
                    } else if (event == "Étiage")  {
                        id_plot = which(df_P$name == "\\sqrt{Q}"
                                        & df_P$first == TRUE)
                        nbt = 1
                    } else {
                        id_plot = which(df_P$name == "Q"
                                        & df_P$first == TRUE)
                        nbt = 1
                    }

                    LM_id = c(LM_id, id_plot)
                    LM_name = c(LM_name, df_P$name[id_plot])
                } else {
                    nbt = 0
                }

                P_var = c()
                for (var in var_to_place_page) {
                    if (var == var_to_place_page[length(var_to_place_page)]) {
                        last = TRUE
                    } else {
                        last = FALSE
                    }
                    id_plot = which(df_P$name == var & df_P$last == last)
                    LM_id = c(LM_id, id_plot)
                    LM_name = c(LM_name, df_P$name[id_plot])
                }
                
                nGraphMiss = nVar_max - length(var_to_place_page)

                if (nGraphMiss > 0) {
                    for (i in 1:nGraphMiss) {
                        LM_id = c(LM_id, NA)
                        LM_name = c(LM_name, "void")
                    }
                }

                if (foot_note) {
                    id_plot = which(df_P$name == "foot")
                    LM_id = c(LM_id, id_plot)
                    LM_name = c(LM_name, df_P$name[id_plot])
                    nbf = 1
                } else {
                    nbf = 0
                }

                Pevent = df_P$plot[LM_id[!is.na(LM_id)]]

                LM_name = matrix(LM_name)
                LM_id[!is.na(LM_id)] = 1:length(LM_id[!is.na(LM_id)])
                LM_id = matrix(LM_id)                

                LMcol = ncol(LM_id)
                LM_id = rbind(rep(NA, times=LMcol), LM_id,
                           rep(NA, times=LMcol))
                LM_name = rbind(rep("margin", times=LMcol), LM_name,
                                rep("margin", times=LMcol))
                
                LMrow = nrow(LM_id)
                LM_id = cbind(rep(NA, times=LMrow), LM_id,
                           rep(NA, times=LMrow))
                LM_name = cbind(rep("margin", times=LMrow), LM_name,
                                rep("margin", times=LMrow))
                LMcol = ncol(LM_id)

                margin_height = 0.5
                
                Norm_ratio = height * var_ratio * nVar_max / (height - 2*margin_height - time_height*nbt - foot_height*nbf - info_height*nbi)

                var_height = height * var_ratio / Norm_ratio
                
                Hcut = LM_name[, 2]
                heightLM = rep(0, times=LMrow)        
                
                heightLM[Hcut == "info"] = info_height
                heightLM[Hcut == "Q"] = time_height
                heightLM[Hcut == "\\sqrt{Q}"] = time_height
                heightLM[Hcut %in% var_to_place_page |
                         Hcut == "void"] = var_height
                heightLM[Hcut == "foot"] = foot_height
                heightLM[Hcut == "margin"] = margin_height

                col_width = (width - 2*margin_height) / (LMcol - 2)
                
                Wcut = LM_name[(LMrow-1),]
                widthLM = rep(col_width, times=LMcol)
                widthLM[Wcut == "margin"] = margin_height

                
                LM_inline = Pevent[as.vector(LM_id)]
                
                LM_name_inline = as.vector(LM_name)

                widths_var = list()
                nPlot = length(LM_inline)
                for (i in 1:nPlot) {
                    if (is.null(LM_inline[[i]])) {
                        LM_inline[[i]] = void()
                    }
                    if (LM_name_inline[i] %in% c(var_plotted, "Q", "\\sqrt{Q}")) {
                        LM_inline[[i]] = ggplot_gtable(ggplot_build(LM_inline[[i]]))
                        widths_var = append(widths_var, list(LM_inline[[i]]$widths))
                    }
                }        
                maxWidth = do.call(grid::unit.pmax, widths_var)
                for (i in 1:nPlot) {
                    if (LM_name_inline[i] %in% c(var_plotted, "Q", "\\sqrt{Q}")) {
                        LM_inline[[i]]$widths = as.list(maxWidth)
                    }
                }

                plot = grid.arrange(arrangeGrob(grobs=LM_inline,
                                                nrow=LMrow,
                                                ncol=LMcol,
                                                heights=heightLM,
                                                widths=widthLM,
                                                as.table=FALSE))
                

                eventName = chartr("áéèà", "aeea", tolower(event))
                filename = paste0(as.character(code), '_',
                                  gsub(' ', '_', eventName),
                                  '_', page)

                print("Saving")
                
                # Saving
                ggsave(plot=plot,
                       path=outdirTmp_pdf,
                       filename=paste0(filename, '.pdf'),
                       width=width, height=height, units='cm', dpi=100)
                
                # Saving
                ggsave(plot=plot, 
                       path=outdirTmp_png,
                       filename=paste0(filename, '.png'),
                       width=width, height=height, units='cm', dpi=400)

                
            }
        }

        print("")

        if (!is.null(df_page)) {
            section = 'Fiche station'
            subsection = code
            if (nrow(df_page) == 0 | pdf_chunk == 'by_code') {
                n_page = page_code
            } else {
                n_page = df_page$n[nrow(df_page)] + 1
            }
            df_page = bind_rows(
                df_page,
                tibble(section=section,
                       subsection=subsection,
                       n=n_page))
        }   
    }
    return (df_page)
}
