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


#' @title Time panel
#' @export
panel_trend = function (dataEX_code_var,
                        metaEX,
                        trendEX,
                        # period_change=NULL,
                        linetype_per='solid',
                        missRect=FALSE,
                        axis_xlim=NULL, grid=TRUE,
                        ymin_lim=NULL,
                        breaks="10 years",
                        minor_breaks="2 years",
                        date_labels="%Y",
                        first=FALSE, last=FALSE) {


    var = names(dataEX_code_var)[which(sapply(dataEX_code_var,
                                              is.numeric))[1]]
    
    unit = metaEX$unit[metaEX$var == var]
    is_date = metaEX$is_date[metaEX$var == var]
    normalize = metaEX$normalize[metaEX$var == var]
    reverse_palette = metaEX$reverse_palette[metaEX$var == var]
    samplePeriod = metaEX$samplePeriod[metaEX$var == var]
    Period = unique(trendEX$period)

    print(Period)
    
    nPeriod = length(Period)
    
    codeX = c(min(dataEX_code_var[[var]], na.rm=TRUE),
              max(dataEX_code_var[[var]], na.rm=TRUE))

    codeDate = c(
        min(dataEX_code_var$Date[!is.na(dataEX_code_var[[var]])],
            na.rm=TRUE),
        max(dataEX_code_var$Date[!is.na(dataEX_code_var[[var]])],
            na.rm=TRUE) + lubridate::years(1))

    if (!is.null(axis_xlim)) {
        codeDate = axis_xlim
    }
    
    p = ggplot() + theme_IPCC(isBack=FALSE,
                              isGrid=FALSE)

    ## Background ##
    Period = as.list(Period)
    Imin = 10^99
    for (per in Period) {
        I = lubridate::interval(per[1], per[2])
        if (I < Imin) {
            Imin = I
            Period_min = as.Date(per)
        }
    }

    minPer = Period_min[1]
    maxPer = Period_min[2]
    if (minPer < codeDate[1]) {
        minPer = codeDate[1]
    }
    if (maxPer > codeDate[2]) {
        maxPer = codeDate[2]
    }
    
    if (nPeriod > 1) {
        p = p + 
            geom_rect(aes(xmin=minPer,
                          ymin=-Inf, 
                          xmax=maxPer,
                          ymax=Inf),
                      color=NA,
                      linetype=0,
                      fill='grey97')
    }

    
    ## Mean step ##
    if ("period_change" %in% names(trendEX)) {
        
        period_change = trendEX$period_change[[1]]
        nPeriod_change = length(period_change)

        plot_mean = dplyr::tibble()
        plot_line = dplyr::tibble()

        for (j in 1:nPeriod_change) {
            xmin = as.Date(period_change[[j]][1])
            xmax = as.Date(period_change[[j]][2])

            dataEX_code_var_period =
                dataEX_code_var[dataEX_code_var$Date >= xmin
                                & dataEX_code_var$Date <= xmax,]
            
            # if (xmin > codeDate[1] & j != 1) {
            #     xmin = lubridate::add_with_rollback(xmin,
            #                                         -months(6)) 
            # }
            # if (j == 1) {
            #     if (!is.null(axis_xlim)) {
            #         if (xmin < axis_xlim[1]) {
            #             xmin = axis_xlim[1]
            #         }
            #     }
            # }
            
            # if (xmax < codeDate[2] &
            #     j != nPeriod_change) {
            #     xmax = lubridate::add_with_rollback(xmax,
            #                                         months(6)) 
            # }
            # if (j == nPeriod_change) {
            #     if (!is.null(axis_xlim)) {
            #         if (xmax + lubridate::years(1) < axis_xlim[2]) {
            #             xmax = xmax + lubridate::years(1)
            #         } else {
            #             xmax = axis_xlim[2]
            #         }
            #     }
            # }

            if (xmin < codeDate[1]) {
                xmin = codeDate[1]
            }
            if (xmax > codeDate[2]) {
                xmax = codeDate[2]
            }

            ymax = mean(dataEX_code_var_period[[var]], na.rm=TRUE)
            
            plot_meantmp = tibble(xmin=xmin, xmax=xmax, 
                                  ymin=-Inf, ymax=ymax, period=j)
            plot_mean = bind_rows(plot_mean, plot_meantmp)
            abs = c(xmin, xmax)
            ord = c(ymax, ymax)
            
            plot_linetmp = tibble(abs=abs, ord=ord, period=j)
            plot_line =  bind_rows(plot_line, plot_linetmp)
        }

        if (is_date) {
            plot_mean$ymin = plot_mean$ymin + as.Date("1970-01-01")
            plot_mean$ymax = plot_mean$ymax + as.Date("1970-01-01")
            plot_line$ord = plot_line$ord + as.Date("1970-01-01")
        }

        p = p + 
            geom_rect(aes(xmin=plot_mean$xmin,
                          ymin=plot_mean$ymin, 
                          xmax=plot_mean$xmax,
                          ymax=plot_mean$ymax),
                      color=NA,
                      fill='grey93')
        p = p +
            geom_line(data=plot_line,
                      aes(x=abs,
                          y=ord,
                          group=period),
                      color='grey80',
                      size=0.15)

        for (i in 1:(nPeriod_change - 1)) {
            dPeriod = abs(as.Date(period_change[[i+1]][1]) -
                          as.Date(period_change[[i]][2]))
            
            if (dPeriod < 10) {
                xLim = plot_mean$xmax[i]
                yLim = max(c(plot_mean$ymax[i],
                             plot_mean$ymax[i+1]))
                plot_lim = tibble(x=c(xLim, xLim),
                                  y=c(-Inf, yLim))

                if (is_date) {
                    plot_lim$y = plot_lim$y + as.Date("1970-01-01")
                }

                p = p + 
                    geom_line(aes(x=plot_lim$x,
                                  y=plot_lim$y),
                              linetype='dashed', size=0.15,
                              color='grey80')
                
            } else {
                xLim_i = plot_mean$xmax[i]
                yLim_i = plot_mean$ymax[i]
                xLim_i1 = plot_mean$xmin[i+1]
                yLim_i1 = plot_mean$ymax[i+1]
                
                plot_lim = tibble(x_i=c(xLim_i, xLim_i),
                                  y_i=c(-Inf, yLim_i),
                                  x_i1=c(xLim_i1, xLim_i1),
                                  y_i1=c(-Inf, yLim_i1))

                if (is_date) {
                    plot_lim$y_i = plot_lim$y_i + as.Date("1970-01-01")
                    plot_lim$y_i1 = plot_lim$y_i1 + as.Date("1970-01-01")
                }

                p = p +
                    geom_line(aes(x=plot_lim$x_i,
                                  y=plot_lim$y_i),
                              linetype='dashed', size=0.15,
                              color='grey80') +
                    geom_line(aes(x=plot_lim$x_i1,
                                  y=plot_lim$y_i1),
                              linetype='dashed', size=0.15,
                              color='grey80')
            }  
        }
    }


    ### Grid ###
    if (grid) {
        p = p +
            theme(panel.grid.major.y=element_line(color='grey80',
                                                  size=0.3))
    }

    
    ### Data ###
    if (is_date) {
        dataEX_code_var[[var]] = dataEX_code_var[[var]] +
            as.Date("1970-01-01")
    }
    p = p +
        geom_point(aes(x=dataEX_code_var$Date,
                       y=dataEX_code_var[[var]]),
                   shape=19, color='grey50', alpha=1,
                   stroke=0, size=1)

    
    ### Missing data ###
    if (missRect) {
        NAdate = dataEX_code_var$Date[is.na(dataEX_code_var[[var]])]
        dNAdate = diff(NAdate)
        NAdate_Down = NAdate[append(Inf, dNAdate) != 1]
        NAdate_Up = NAdate[append(dNAdate, Inf) != 1]

        p = p +
            geom_rect(aes(xmin=as.Date(NAdate_Down), 
                          ymin=-Inf, 
                          xmax=as.Date(NAdate_Up), 
                          ymax=Inf),
                      linetype=0, fill='#66c1bf', alpha=0.4)
    }
    
    
    ### Trend ###
    plot_trend = dplyr::tibble()
    leg_trend = dplyr::tibble()

    color_trend = c()
    
    for (j in 1:nPeriod) {
        period = Period[[j]]
        Start = period[1]
        End = period[2]
        
        dataEX_code_var_period =
            dataEX_code_var[dataEX_code_var$Date >= Start &
                            dataEX_code_var$Date <= End,]

        trendEX_code_var_period =
            trendEX[sapply(lapply(trendEX$period,
                                  '==', period), all),]

        Ntrend = nrow(trendEX_code_var_period)
        if (Ntrend > 1) {
            trendEX_code_var_period = trendEX_code_var_period[1,]
        }
        

        dataEX_code_var_period_NoNA =
            dataEX_code_var_period[!is.na(dataEX_code_var_period[[var]]),]
        iStart = which.min(abs(dataEX_code_var_period_NoNA$Date - Start[i]))
        iEnd = which.min(abs(dataEX_code_var_period_NoNA$Date - End[i]))
        xmin = dataEX_code_var_period_NoNA$Date[iStart]
        xmax = dataEX_code_var_period_NoNA$Date[iEnd]
        if (!is.null(axis_xlim)) {
            if (xmin < axis_xlim[1]) {
                xmin = axis_xlim[1]
            }
            if (xmax > axis_xlim[2]) {
                xmax = axis_xlim[2]
            } 
        }
        
        abs = c(xmin, xmax)
        abs_num = as.numeric(abs, origin="1970-01-01") / 365.25
        ord = abs_num * trendEX_code_var_period$a +
            trendEX_code_var_period$b
        
        plot_trendtmp = tibble(abs=abs, ord=ord, period=i)
        plot_trend = bind_rows(plot_trend, plot_trendtmp)

        x = gpct(1.5, codeDate, shift=TRUE)
        xend = x + gpct(3, codeDate)

        dy = gpct(9, codeX, min_lim=ymin_lim)
        y = gpct(100, codeX,
                 min_lim=ymin_lim, shift=TRUE) - (i-1)*dy
        yend = y

        xt = xend + gpct(1, codeDate)

        xminR = x - gpct(1, codeDate)
        yminR = y - gpct(5, codeX, min_lim=ymin_lim)
        if (normalize) {
            xmaxR = x + gpct(32.5, codeDate)
        } else {
            xmaxR = x + gpct(20.5, codeDate)
        }
        ymaxR = y + gpct(5, codeX, min_lim=ymin_lim)

        
        if (trendEX_code_var_period$H) {
            Palette = get_palette("BrBG",
                                  10,
                                  reverse_palette)
            res = compute_colorBin(trendEX_code_var_period$trend_min,
                                   trendEX_code_var_period$trend_max,
                                   colorStep=10,
                                   center=TRUE,
                                   include=FALSE)
            color = get_color(trendEX_code_var_period$trend,
                              upBin=res$upBin,
                              lowBin=res$lowBin,
                              Palette=Palette)

            colorLine = color
            colorLabel = switch_colorLabel(color)
        } else {
            colorLine = 'grey80'
            colorLabel = 'grey80'
        }

        color_trend = c(color_trend, colorLine)
        
        power = get_power(trendEX_code_var_period$a)
        powerC = as.character(power)
        if (powerC >= 0) {
            spaceC = ' '
        } else {
            spaceC = ''
        }

        brk = 10^power
        aC = as.character(format(round(
            trendEX_code_var_period$a / brk, 2),
            nsmall=2))
        if (aC >= 0) {
            aC = paste('  ', aC, sep='')
        }
        
        aMeanC = as.character(format(round(
            trendEX_code_var_period$trend*100, 2),
            nsmall=2))
        if (aMeanC >= 0) {
            aMeanC = paste('  ', aMeanC, sep='')
        }

        leg_trendtmp = tibble(x=x, xend=xend, 
                              y=y, yend=yend, 
                              xt=xt,
                              colorLine=colorLine,
                              colorLabel=colorLabel,
                              aC=aC,
                              powerC=powerC,
                              spaceC=spaceC,
                              aMeanC=aMeanC,
                              xminR=xminR, yminR=yminR,
                              xmaxR=xmaxR, ymaxR=ymaxR,
                              period=i)
        leg_trend = bind_rows(leg_trend, leg_trendtmp)  
    }

    if (length(linetype_per) < nPeriod) {
        linetype_per = rep(linetype_per, times=nPeriod)
    }

    linetypeLeg_per = linetype_per
    linetypeLeg_per[linetype_per == 'longdash'] = '33'
    linetypeLeg_per[linetype_per == 'dashed'] = '22'
    linetypeLeg_per[linetype_per == 'dotted'] = '11'

    for (i in 1:nPeriod) {
        leg_trend_per = leg_trend[leg_trend$period == i,]

        if (nPeriod > 1) {
            if (is_date) {
                leg_trend_per$yminR = leg_trend_per$yminR + as.Date("1970-01-01")
                leg_trend_per$ymaxR = leg_trend_per$ymaxR + as.Date("1970-01-01")
            }
            p = p +
                geom_rect(aes(xmin=leg_trend_per$xminR,
                              ymin=leg_trend_per$yminR,
                              xmax=leg_trend_per$xmaxR,
                              ymax=leg_trend_per$ymaxR),
                          linetype=0, fill='white', alpha=0.3)
        }
        
        colorLine = leg_trend_per$colorLine
        colorLabel = leg_trend_per$colorLabel
        aC = leg_trend_per$aC
        powerC = leg_trend_per$powerC
        spaceC = leg_trend_per$spaceC
        aMeanC = leg_trend_per$aMeanC

        unitF = gsub(" ", "\\\\,", unit)

        # if (unit == 'hm^{3}' | unit == 'm^{3}.s^{-1}') {
        #     label = paste0("\\textbf{", aC,
        #                    " x 10$^{$", powerC,"}}",
        #                    spaceC,
        #                    " ", "\\[$", unitF, ".an^{-1}$", "\\]",
        #                    "\\;", "\\textbf{", aMeanC, "}",
        #                    " ", "\\[%$.an^{-1}$\\]")
            
        # } else if (unit == "jour de l'année" | unit == "jour") {
        #     label = paste0("\\textbf{", aC,
        #                    " x 10$^{$", powerC,"}}",
        #                    spaceC,
        #                    " ", "\\[", "jour$.an^{-1}$", "\\]")
            
        # } else if (unit == 'jour.an^{-1}') {
        #     label = paste0("\\textbf{", aC,
        #                    " x 10$^{$", powerC,"}}",
        #                    spaceC,
        #                    " ", "\\[", "jour$.an^{-2}$", "\\]")
        # }

        if (grepl(".an^{-1}", unitF, fixed=TRUE)) {
            unitF = gsub(".an^{-1}", "", unitF, fixed=TRUE)
            unitT = ".an^{-2}$"
        } else {
            unitT = ".an^{-1}$"
        }
        
        if (normalize) {
            label = paste0("\\textbf{", aC,
                           " x 10$^{$", powerC,"}}",
                           spaceC,
                           " ", "\\[$", unitF, unitT, "\\]",
                           "\\;", "\\textbf{", aMeanC, "}",
                           " ", "\\[%$.an^{-1}$\\]")
        } else {
            label = paste0("\\textbf{", aC,
                           " x 10$^{$", powerC,"}}",
                           spaceC,
                           " ", "\\[$", unitF, unitT, "\\]")
        }

        
        if (nPeriod > 1) {
            if (is_date) {
                leg_trend_per$y = leg_trend_per$y + as.Date("1970-01-01")
                leg_trend_per$yend = leg_trend_per$yend + as.Date("1970-01-01")
            }
            p = p +
                annotate("segment",
                         x=leg_trend_per$x,
                         xend=leg_trend_per$xend,
                         y=leg_trend_per$y,
                         yend=leg_trend_per$yend,
                         color=colorLine,
                         linetype=linetypeLeg_per[i],
                         lwd=0.8,
                         lineend="round") +
                annotate("text",
                         label=TeX(label), size=2.8,
                         x=leg_trend_per$xt,
                         y=leg_trend_per$y, 
                         hjust=0, vjust=0.5,
                         color=colorLabel)
        } else {
            p = p +
                theme(plot.title=element_text(size=7,
                                              vjust=-1.5, 
                                              hjust=0,
                                              color=colorLabel)) + 
                ggtitle(TeX(label))
        }
    }

    for (i in 1:nPeriod) {
        plot_trend_per = plot_trend[plot_trend$period == i,]
        if (is_date) {
            plot_trend_per$ord = plot_trend_per$ord +
                as.Date("1970-01-01")
        }
        p = p + 
            geom_line(aes(x=plot_trend_per$abs,
                          y=plot_trend_per$ord),
                      color='white',
                      linetype='solid',
                      size=1.5,
                      lineend="round")
    }

    for (i in 1:nPeriod) {
        plot_trend_per = plot_trend[plot_trend$period == i,]
        p = p + 
            geom_line(aes(x=plot_trend_per$abs,
                          y=plot_trend_per$ord),
                      color=color_trend[i],
                      linetype=linetype_per[i],
                      size=0.75,
                      lineend="round")
    }

    if (!is.null(samplePeriod)) {
        hPx = gpct(0, codeDate, shift=TRUE)
        hPy = gpct(50, codeX, min_lim=ymin_lim, shift=TRUE)

        if (length(samplePeriod) > 1) {
            hPlabel = paste0(
                "$^{$",
                "\\small{",
                format(as.Date(paste0("1970-",
                                      samplePeriod[1])), "%d %B"),
                " / ",
                format(as.Date(paste0("1970-",
                                      samplePeriod[2])), "%d %B"),
                "}}")
        } else {
            hPlabel = paste0(
                "$^{$",
                "\\small{",
                format(as.Date(paste0("1970-",
                                      samplePeriod)), "%d %B"),
                " / ",
                format(as.Date(paste0("1970-",
                                      samplePeriod))-1, "%d %B"),
                "}}")
        }
    }
    
    varF = gsub("etiage", "étiage", var)  
    if (grepl("[_]", varF)) {
        varF = gsub("[_]", "$_{$", varF)
        varF = paste0(varF, "}")
    }
    unitF = gsub(" ", "\\\\,", unit)
    ylabel = paste0("\\textbf{", varF, "}", "\\;", "\\[$", unitF, "$\\]")

    if (!is.null(samplePeriod)) {
        yTeXlabel = bquote(atop(.(TeX(ylabel)[[1]]),.(TeX(hPlabel)[[1]])))
    } else {
        yTeXlabel = bquote(atop(.(TeX(ylabel)[[1]])," "))
    }
    
    p = p +
        ylab(yTeXlabel)
    
    if (first) {
        position = 'top'
    } else {
        position = 'bottom'
    }


    if (!is.null(axis_xlim)) {
        limits = axis_xlim
    } else {
        limits = NULL
    }

    
    get_breaks = function(X) {
        Xmin = round(lubridate::year(min(X)), -1)
        Xmax = round(lubridate::year(max(X)), -1)
        if (Xmax-Xmin <= 1) {
            Xmin = lubridate::year(X)[1]
            Xmax = lubridate::year(X)[1] + 1
        }
        res = seq.Date(from=as.Date(paste0(Xmin, "-01-01")),
                       to=as.Date(paste0(Xmax, "-01-01")),
                       by=breaks)

        return (res)
    }

    get_minor_breaks = function(X) {
        Xmin = round(lubridate::year(min(X)), -1)
        Xmax = round(lubridate::year(max(X)), -1)
        if (Xmax-Xmin <= 1) {
            Xmin = lubridate::year(X)[1]
            Xmax = lubridate::year(X)[1] + 1
        }
        res = seq.Date(from=as.Date(
                           as.Date(paste0(Xmin, "-01-01")) -
                           lubridate::duration(breaks)),
                       to=as.Date(
                           as.Date(paste0(Xmax, "-01-01")) +
                           lubridate::duration(breaks)),
                       by=minor_breaks)
        return (res)
    }
    

    p = p +
        scale_x_date(
            breaks=get_breaks,
            minor_breaks=get_minor_breaks,
            guide='axis_minor',
            date_labels=date_labels,
            limits=limits,
            position=position, 
            expand=c(0, 0)
        )

    if (!is_date) {
        if (get_power(codeX[1]) >= 4) {
            labels = function(X) {
                TeX(paste0(format(
                    round(X/10^get_power(X), 1), nsmall=1),
                    "x", 10, "$^{$",
                    get_power(X),
                    "}"))
            }
        } else {
            labels = waiver()
        }

        if (!is.null(ymin_lim)) {
            p = p +
                scale_y_continuous(limits=c(ymin_lim, NA),
                                   n.breaks=5,
                                   labels=labels,
                                   expand=expansion(mult=c(0, 0.1)))
        } else {
            p = p +
                scale_y_continuous(n.breaks=5,
                                   labels=labels,
                                   expand=expansion(mult=c(0.1, 0.1)))
        }

    } else {
        monthSpread = (codeX[2] - codeX[1]) %% 365.25 / 30.4375
        if (6 <= monthSpread) {
            p = p +
                scale_y_date(date_breaks="2 month",
                             date_labels="%b",
                             expand=expansion(mult=c(0.1, 0.1)))
        } else if (3 <= monthSpread & monthSpread < 6) {
            p = p +
                scale_y_date(date_breaks="month",
                             date_labels="%b",
                             expand=expansion(mult=c(0.1, 0.1)))
        } else if (monthSpread < 3) {
            p = p +
                scale_y_date(date_breaks="2 week",
                             date_labels="%d %b",
                             expand=expansion(mult=c(0.1, 0.1)))
        }
    }

    if (nPeriod > 1) {
        tt = 2.5
        t = 2
        tb = 3
        b = 2
    } else {
        tt = 0
        t = 0
        tb = 0
        b = 0
    }
    
    # if (last == "all") {
    #     pLastTRUE = p
    #     pLastFALSE = p
    #     if (first) {
    #         pLastFALSE = pLastFALSE +
    #             theme(plot.margin=margin(t=tt, r=0, b=tb, l=0,
    #                                      unit="mm"))
    #         pLastTRUE = pLastTRUE +
    #             theme(plot.margin=margin(t=tt, r=0, b=0, l=0,
    #                                      unit="mm"))
    #     } else {
    #         pLastFALSE = pLastFALSE + 
    #             theme(plot.margin=margin(t=t, r=0, b=b, l=0,
    #                                      unit="mm"),
    #                   axis.text.x=element_blank())
    #         pLastTRUE = pLastTRUE +
    #             theme(plot.margin=margin(t=t, r=0, b=0, l=0,
    #                                      unit="mm"))
    #     }

    #     res = list(lastTRUE=pLastTRUE, lastFALSE=pLastFALSE)
    #     return(res)
        
    # } else {
    if (first & !last) {
        p = p +
            theme(plot.margin=margin(t=tt, r=0, b=tb, l=0,
                                     unit="mm"))
    } else if (!first & last) {
        p = p + 
            theme(plot.margin=margin(t=t, r=0, b=0, l=0,
                                     unit="mm"))
    } else if (first & last) {
        p = p + 
            theme(plot.margin=margin(t=tt, r=0, b=0, l=0,
                                     unit="mm"))
    } else if (!first & !last) {
        p = p + 
            theme(plot.margin=margin(t=t, r=0, b=b, l=0,
                                     unit="mm"),
                  axis.text.x=element_blank())
    }
    return (p)
    # }
} 
