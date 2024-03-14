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
panel_trend = function (variable,
                        dataEX_code_variable,
                        trendEX_code_variable,
                        metaEX,
                        period_trend_show=NULL,
                        linetype='solid',
                        missRect=FALSE,
                        axis_xlim=NULL, grid=TRUE,
                        ymin_lim=NULL,
                        breaks="10 years",
                        minor_breaks="2 years",
                        date_labels="%Y",
                        color_to_switch=NULL,
                        margin_trend=
                            margin(t=0, r=0, b=0, l=0, "mm"),
                        first=FALSE, last=FALSE) {

    axis_xlim = as.Date(axis_xlim)

    variableEX = grep(variable, names(dataEX_code_variable), fixed=TRUE, value=TRUE)

    dataEX_code_variable$isNA =
        is.na(rowSums(
            dplyr::mutate_all(dataEX_code_variable[variableEX], as.numeric)))

    ok = metaEX$variable_en == variable
    if (any(names(metaEX) == "variable")) {
        variable_to_display = metaEX$variable[ok]
    } else {
        variable_to_display = variable
    }
    
    unit = metaEX$unit_fr[ok]
    is_date = metaEX$is_date[ok]
    to_normalise = metaEX$to_normalise[ok]
    Palette = metaEX$palette[ok]
    Palette = unlist(strsplit(Palette, " "))
    sampling_period = metaEX$sampling_period_en[ok]
    sampling_period = unlist(strsplit(sampling_period, ", "))
    

    Period = unique(trendEX_code_variable$period_trend)
    Period = strsplit(Period, " ")
    Period = lapply(Period, as.Date)
    nPeriod = length(Period)
    
    codeX = c(min(unlist(dataEX_code_variable[variableEX]), na.rm=TRUE),
              max(unlist(dataEX_code_variable[variableEX]), na.rm=TRUE))

    codeDate = c(
        min(dataEX_code_variable$date[!dataEX_code_variable$isNA], na.rm=TRUE),
        max(dataEX_code_variable$date[!dataEX_code_variable$isNA], na.rm=TRUE))

    codeDate[1] = lubridate::add_with_rollback(codeDate[1],
                                               -lubridate::years(1))
    codeDate[2] = lubridate::add_with_rollback(codeDate[2],
                                               lubridate::years(1))
    
    if (!is.null(axis_xlim)) {
        codeDate = axis_xlim
    }

    
    plan = matrix(c("title", "plot"),
                  nrow=2, 
                  byrow=TRUE)
    
    herd = bring_grass(verbose=verbose)
    herd = plan_of_herd(herd, plan,
                        verbose=verbose)

    
## TITLE _____________________________________________________________
    title = ggplot() + theme_void()

    color_trend = c()

    if (length(linetype) < nPeriod) {
        linetype = rep(linetype, times=nPeriod)
    }
    linetypeLeg_per = linetype
    linetypeLeg_per[linetype == 'longdash'] = '33'
    linetypeLeg_per[linetype == 'dashed'] = '22'
    linetypeLeg_per[linetype == 'dotted'] = '11'

    for (j in 1:nPeriod) {
        period = Period[[j]]
        trendEX_code_variable_period =
            trendEX_code_variable[trendEX_code_variable$period_trend ==
                                  paste0(period, collapse=" "),]
        Ntrend = nrow(trendEX_code_variable_period)
        if (Ntrend > 1) {
            trendEX_code_variable_period = trendEX_code_variable_period[1,]
        }

        if (trendEX_code_variable_period$H) {
            res = compute_colorBin(trendEX_code_variable_period$a_normalise_min,
                                   trendEX_code_variable_period$a_normalise_max,
                                   colorStep=length(Palette),
                                   center=0,
                                   include=FALSE)
            bin = res$bin
            upBin = res$upBin
            lowBin = res$lowBin
            color = get_colors(trendEX_code_variable_period$a_normalise,
                               upBin=upBin,
                               lowBin=lowBin,
                               Palette=Palette)

            colorLine = color
            colorLabel = switch_color(color, color_to_switch)
        } else {
            colorLine = 'grey80'
            colorLabel = 'grey80'
        }
        color_trend = c(color_trend, colorLine)
        
        aMeanC = as.character(format(round(
            trendEX_code_variable_period$a_normalise, 2),
            nsmall=2))
        if (aMeanC >= 0) {
            aMeanC = paste('  ', aMeanC, sep='')
        }

        unitF = gsub(" ", "\\\\,", unit)
        unitF = gsub("°", "\\textbf{^\\degree}", unitF, fixed=TRUE)
        
        if (grepl(".an^{-1}", unitF, fixed=TRUE)) {
            unitF = gsub(".an^{-1}", "", unitF, fixed=TRUE)
            unitT = ".an^{-2}"
        } else {
            unitT = ".an^{-1}"
        }

        power = get_power(trendEX_code_variable_period$a)

        if (-1 <= power & power <= 1) {
            aC = as.character(signif(
                trendEX_code_variable_period$a, 3))
            if (aC >= 0) {
                aC = paste0('  ', aC)
            }
            
            if (to_normalise) {
                label = paste0("\\textbf{", aC, "}",
                               " ", "\\[$", unitF, unitT, "$\\]",
                               "\\;", "\\textbf{", aMeanC, "}",
                               " ", "\\[%$.an^{-1}$\\]")
            } else {
                label = paste0("\\textbf{", aC, "}",
                               " ", "\\[$", unitF, unitT, "$\\]")
            }
            
        } else {
            powerC = as.character(power)
            if (powerC >= 0) {
                spaceC = ' '
            } else {
                spaceC = ''
            }
            brk = 10^power
            aC = as.character(format(round(
                trendEX_code_variable_period$a / brk, 2),
                nsmall=2))
            if (aC >= 0) {
                aC = paste0('  ', aC)
            }
            
            if (to_normalise) {
                label = paste0("\\textbf{", aC,
                               " x 10$^{$", powerC,"}}",
                               spaceC,
                               " ", "\\[$", unitF, unitT, "$\\]",
                               "\\;", "\\textbf{", aMeanC, "}",
                               " ", "\\[%$.an^{-1}$\\]")
            } else {
                label = paste0("\\textbf{", aC,
                               " x 10$^{$", powerC,"}}",
                               spaceC,
                               " ", "\\[$", unitF, unitT, "$\\]")
            }
        }

        dx0 = 0.05
        dx_space = 2
        dx_line = 0.2
        dx_text = 0.1

        title = title +
            annotate("segment",
                     x=dx0 + dx_space*(j-1),
                     xend=dx0 + dx_space*(j-1) + dx_line,
                     y=0,
                     yend=0,
                     color=colorLine,
                     linetype=linetypeLeg_per[j],
                     lwd=0.8,
                     lineend="round") +
            annotate("text",
                     label=TeX(label), size=2.8,
                     x=dx0 + dx_space*(j-1) + dx_line + dx_text,
                     y=0, 
                     hjust=0, vjust=0.5,
                     color=colorLabel)
        
        if (!trendEX_code_variable_period$H) {
            label = paste0("Non significatif à un risque de ",
                           trendEX_code_variable_period$level*100,
                           " %")
        } else {
            label = paste0("Significatif à un risque de ",
                           trendEX_code_variable_period$level*100,
                           " %")       
        }
        title = title +
                annotate("text",
                         label=label,
                         size=2.4,
                         x=10,
                         y=0, 
                         hjust=1, vjust=0.5,
                         color=colorLabel)
        
    }

    title = title +
        scale_x_continuous(limits=c(0, 10),
                           expand=c(0, 0)) +
        scale_y_continuous(limits=c(-0.5, 1),
                           expand=c(0, 0))

    herd = add_sheep(herd,
                     sheep=title,
                     id="title",
                     height=0.15,
                     verbose=verbose)



## PLOT ______________________________________________________________
    p = ggplot() + theme_IPCC(isBack=FALSE,
                              isGridX=FALSE, isGridY=FALSE,
                              isTitle=FALSE,
                              isLabelX=FALSE, isLabelY=TRUE)
    
    ## Background ##
    if (is.null(period_trend_show)) {
        period_trend_show = Period
    }

    if (!is.list(period_trend_show)) {
        period_trend_show = list(period_trend_show)
    } 
    period_trend_show = lapply(period_trend_show, as.Date)    
    Imin = 10^99

    for (per in period_trend_show) {
        I = per[2]-per[1]
        if (I < Imin) {
            Imin = I
            period_min = as.Date(per)
        }
    }


    minPer = period_min[1]
    maxPer = period_min[2]
    if (minPer < codeDate[1]) {
        minPer = codeDate[1]
    }
    if (maxPer > codeDate[2]) {
        maxPer = codeDate[2]
    }
    
    if (length(period_trend_show) > 1) {
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
    if ("period_change" %in% names(trendEX_code_variable)) {
        period_change = trendEX_code_variable$period_change[[1]]
        nPeriod_change = length(period_change)

        plot_mean = dplyr::tibble()
        plot_line = dplyr::tibble()

        for (j in 1:nPeriod_change) {
            xmin = as.Date(period_change[[j]][1])
            xmax = as.Date(period_change[[j]][2])

            dataEX_code_variable_period =
                dataEX_code_variable[dataEX_code_variable$date >= xmin
                                & dataEX_code_variable$date <= xmax,]

            if (xmin < codeDate[1]) {
                xmin = codeDate[1]
            }
            if (xmax > codeDate[2]) {
                xmax = codeDate[2]
            }

            ymax = mean(dataEX_code_variable_period[[variable]], na.rm=TRUE)
            
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
        dataEX_code_variable =
            dplyr::mutate(dataEX_code_variable,
                          dplyr::across(.cols=variableEX,
                                        .fns=\(x) x + as.Date("1970-01-01")))
    }
    p = p +
        geom_point(aes(x=dataEX_code_variable$date,
                       y=dataEX_code_variable[[variableEX[1]]]),
                   shape=19, color=IPCCgrey23, alpha=1,
                   stroke=0, size=1.2)

    
    ### Missing data ###
    if (missRect) {
        NAdate = dataEX_code_variable$date[dataEX_code_variable$isNA]
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
    for (j in 1:nPeriod) {
        period = Period[[j]]
        start = period[1]
        end = period[2]

        dataEX_code_variable_period =
            dataEX_code_variable[dataEX_code_variable$date >= start &
                                 dataEX_code_variable$date <= end,]

        trendEX_code_variable_period =
            trendEX_code_variable[trendEX_code_variable$period_trend ==
                                  paste0(period, collapse=" "),]
        Ntrend = nrow(trendEX_code_variable_period)
        if (Ntrend > 1) {
            trendEX_code_variable_period = trendEX_code_variable_period[1,]
        }

        dataEX_code_variable_period_NoNA =
            dplyr::filter(dataEX_code_variable_period, !isNA)

        iStart = which.min(abs(dataEX_code_variable_period_NoNA$date - start))
        iEnd = which.min(abs(dataEX_code_variable_period_NoNA$date - end))

        xmin = dataEX_code_variable_period_NoNA$date[iStart]
        xmax = dataEX_code_variable_period_NoNA$date[iEnd]
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
        ord = abs_num * trendEX_code_variable_period$a +
            trendEX_code_variable_period$b
        if (is_date) {
            ord = ord + as.Date("1970-01-01")
        }
        plot_trendtmp = tibble(abs=abs, ord=ord, period=j)
        plot_trend = bind_rows(plot_trend, plot_trendtmp)
    }

    for (j in 1:nPeriod) {
        plot_trend_per = plot_trend[plot_trend$period == j,]
        p = p + 
            annotate("line",
                     x=plot_trend_per$abs,
                     y=plot_trend_per$ord,
                     color='white',
                     linetype='solid',
                     size=1.2,
                     lineend="round")
    }

    for (j in 1:nPeriod) {
        plot_trend_per = plot_trend[plot_trend$period == j,]
        p = p + 
            annotate("line",
                     x=plot_trend_per$abs,
                     y=plot_trend_per$ord,
                     color=color_trend[j],
                     linetype=linetype[j],
                     size=0.75,
                     lineend="round")
    }
    

    if (!is.null(sampling_period)) {
        hPx = gpct(0, codeDate, shift=TRUE)
        hPy = gpct(50, codeX, min_lim=ymin_lim, shift=TRUE)
            
        if (length(sampling_period) > 1) {
            hPlabel = paste0(
                "$^{$",
                "\\small{",
                format(as.Date(paste0("1972-",
                                      sampling_period[1])), "%d %b"),
                " / ",
                format(as.Date(paste0("1972-",
                                      sampling_period[2])), "%d %b"),
                "}}")
        } else {
            if (nchar(sampling_period) > 5) {
                sampling_period = format(dataEX_code_variable$date[1], "%m-%d")
            }
            hPlabel = paste0(
                "$^{$",
                "\\small{",
                format(as.Date(paste0("1972-",
                                      sampling_period)), "%d %b"),
                " / ",
                format(as.Date(paste0("1972-",
                                      sampling_period))-1, "%d %b"),
                "}}")
        }
    }

    variableF = gsub("etiage", "étiage", variable_to_display)  
    if (grepl("[_]", variableF)) {
        variableF = gsub("[_]", "$_{$", variableF)
        variableF = paste0(variableF, "}")
    }
    unitF = gsub(" ", "\\\\,", unit)
    unitF = gsub("°", "\\textbf{^\\degree}", unitF, fixed=TRUE)
    if (!grepl("[[:digit:]]", unit)) {
        ylabel = paste0("\\textbf{", variableF, "}", "\\,", "\\small{\\[", unitF, "\\]}")
    } else {
        ylabel = paste0("\\textbf{", variableF, "}", "\\,", "\\small{\\[$", unitF, "$\\]}")
    }
    
    if (!is.null(sampling_period)) {
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

    if (first & !last) {
        p = p +
            theme(plot.margin=margin(t=tt, r=0, b=tb, l=0,
                                     unit="mm") + margin_trend)
    } else if (!first & last) {
        p = p + 
            theme(plot.margin=margin(t=t, r=0, b=0, l=0,
                                     unit="mm") + margin_trend)
    } else if (first & last) {
        p = p + 
            theme(plot.margin=margin(t=tt, r=0, b=0, l=0,
                                     unit="mm") + margin_trend)
    } else if (!first & !last) {
        p = p + 
            theme(plot.margin=margin(t=t, r=0, b=b, l=0,
                                     unit="mm") + margin_trend,
                  axis.text.x=element_blank())
    }


    herd = add_sheep(herd,
                     sheep=p,
                     id="plot",
                     height=0.85,
                     verbose=verbose)
    
    return (herd)
} 
