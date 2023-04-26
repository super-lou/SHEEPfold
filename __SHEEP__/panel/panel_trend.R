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
panel_trend = function (dataEx_code, trend_code, var, unit,
                       samplePeriod_code=NULL,
                       linetype_per='solid', level=0.1,
                       colorForce=FALSE, missRect=FALSE,
                       unit2day=365.25, trend_period=NULL,
                       mean_period=NULL, axis_xlim=NULL, grid=TRUE,
                       ymin_lim=NULL, color=NULL,
                       first=FALSE, last=FALSE, lim_pct=10) {

    isDate = as.Date("1970-01-01")
    
    # # Compute max and min of flow
    maxX = max(dataEx_code$X, na.rm=TRUE)
    minX = min(dataEx_code$X, na.rm=TRUE)

    maxX_win = maxX * 1.05
    minX_win = minX * 0.95#expansion
    
    # Open new plot
    p = ggplot() + theme_IPCC()

    ## Sub period background ##
    if (!is.null(trend_period)) {

        # Convert trend period to list if it is not
        trend_period = as.list(trend_period)
        # Fix a disproportionate minimum for period
        Imin = 10^99
        # For all the sub period of analysis in 'trend_period'
        for (per in trend_period) {
            # Compute time interval of period
            I = lubridate::interval(per[1], per[2])
            # If it is the smallest interval
            if (I < Imin) {
                # Store it
                Imin = I
                # Fix min period of analysis
                trend_period_min = as.Date(per)
            }
        }
        
        minPer = trend_period_min[1]
        maxPer = trend_period_min[2]
        
        # If there is an 'axis_lim'
        if (!is.null(axis_xlim)) {
            # If the temporary start of period is smaller 
            # than the fix start of x axis limit
            if (minPer < axis_xlim[1]) {
                # Set the start of the period to the start of
                # the x axis limit
                minPer = axis_xlim[1]
            }

            # If the temporary end of period plus one year 
            # is smaller than the fix end of x axis limit
            if (maxPer + lubridate::years(1) < axis_xlim[2]) {
                # Add one year the the temporary end of period
                maxPer = maxPer + lubridate::years(1)
            } else {
                # Set the start of the period to the start of
                # the x axis limit
                maxPer = axis_xlim[2]
            }
            
        # If there is no 'axis_lim'
        } else {
            if (minPer < min(dataEx_code$Date)) {
                minPer = min(dataEx_code$Date)
            }
            if (maxPer > max(dataEx_code$Date)) {
                maxPer = max(dataEx_code$Date)
            }
        }
    }

    ## Mean step ##
    # If there is a 'mean_period'
    if (!is.null(mean_period)) {
        # Convert 'mean_period' to list
        mean_period = as.list(mean_period)
        # Number of mean period
        nPeriod_mean = length(mean_period)

        # Blank tibble to store variable in order to plot
        # rectangle for mean period
        plot_mean = tibble()
        # Blank tibble to store variable in order to plot
        # upper limit of rectangle for mean period
        plot_line = tibble()
        # For all mean period
        for (j in 1:nPeriod_mean) {
            # Get the current start and end of the sub period
            xmin = as.Date(mean_period[[j]][1])
            xmax = as.Date(mean_period[[j]][2])

            # Extract the data corresponding to this sub period
            dataEx_code_per =
                dataEx_code[dataEx_code$Date >= xmin
                             & dataEx_code$Date <= xmax,]
            
            # If the min over the sub period is greater
            # than the min of the entier period and
            # it is not the first sub period
            if (xmin > min(dataEx_code$Date) & j != 1) {
                # Substract 6 months to be in the middle of
                # the previous year
                xmin = xmin - months(6)
            }
            # If it is not a flow or sqrt of flow time serie and
            # it is the first period
            if (var != '\\sqrt{Q}' & var != 'Q' & j == 1) {
                # If there is an x axis limit
                if (!is.null(axis_xlim)) {
                    # If the min of the period is before the x axis min
                    if (xmin < axis_xlim[1]) {
                        # The min for the sub period is the x axis
                        xmin = axis_xlim[1]
                    }
                }
            }

            # If the max over the sub period is smaller
            # than the max of the entier period and
            # it is not the last sub period
            if (xmax < max(dataEx_code$Date) & j != nPeriod_mean) {
                # Add 6 months to be in the middle of
                # the following year
                xmax = xmax + months(6)
            }
            # If it is not a flow or sqrt of flow time serie and
            # it is the last period
            if (var != '\\sqrt{Q}' & var != 'Q' & j == nPeriod_mean) {
                # If there is an x axis limit
                if (!is.null(axis_xlim)) {
                    # If the max of the period plus 1 year
                    # is smaller thant the max of the x axis limit
                    if (xmax + lubridate::years(1) < axis_xlim[2]) {
                        # Add one year to the max to include
                        # the entire last year graphically
                        xmax = xmax + lubridate::years(1)
                    } else {
                        # The max of this sub period is the max
                        # of the x axis limit
                        xmax = axis_xlim[2]
                    }
                   
                }
            }

            # Mean of the flow over the sub period
            ymax = mean(dataEx_code_per$X, na.rm=TRUE)

            # Create temporary tibble with variable
            # to create rectangle for mean step
            plot_meantmp = tibble(xmin=xmin, xmax=xmax, 
                                  ymin=-Inf, ymax=ymax, period=j)
            # Bind it to the main tibble to store it with other period
            plot_mean = bind_rows(plot_mean, plot_meantmp)

            # Create vector for the upper limit of the rectangle
            abs = c(xmin, xmax)
            ord = c(ymax, ymax)
            
            # Create temporary tibble with variable
            # to create upper limit for rectangle
            plot_linetmp = tibble(abs=abs, ord=ord, period=j)
            # Bind it to the main tibble to store it with other period
            plot_line =  bind_rows(plot_line, plot_linetmp)
        }
        if (unit == "jour de l'année") {
            # Plot rectangles
            p = p + 
            geom_rect(aes(xmin=plot_mean$xmin,
                          ymin=as.Date(plot_mean$ymin + isDate), 
                          xmax=plot_mean$xmax,
                          ymax=as.Date(plot_mean$ymax + isDate)),
                      linetype=0, fill='grey93')
            # Plot upper line for rectangle
            p = p +
                geom_line(aes(x=plot_line$abs,
                              y=as.Date(plot_line$ord + isDate),
                              group=period),
                          color='grey80',
                          size=0.15)
        } else {
            # Plot rectangles
            p = p + 
            geom_rect(aes(xmin=plot_mean$xmin,
                          ymin=plot_mean$ymin, 
                          xmax=plot_mean$xmax,
                          ymax=plot_mean$ymax),
                      linetype=0, fill='grey93')
            # Plot upper line for rectangle
            p = p +
                geom_line(aes(x=plot_line$abs,
                              y=plot_line$ord,
                              group=period),
                          color='grey80',
                          size=0.15)
        }
        
        # for all the sub periods except the last one
        for (i in 1:(nPeriod_mean - 1)) {
            # Computes the time difference in days between periods
            dPeriod = abs(as.Date(mean_period[[i+1]][1]) - as.Date(mean_period[[i]][2]))
                
            if (dPeriod < 10) {
                # The x limit is the x max of the ith rectangle
                xLim = plot_mean$xmax[i]
                # The y limit of rectangle is the max of
                # the two neighboring mean step rectangle
                yLim = max(c(plot_mean$ymax[i], plot_mean$ymax[i+1]))
                # Make a tibble to store dataEx
                plot_lim = tibble(x=c(xLim, xLim), y=c(minX_win, yLim))
                # Plot the limit of rectangles
                if (unit == "jour de l'année") {
                    p = p + 
                        geom_line(aes(x=plot_lim$x,
                                      y=as.Date(plot_lim$y + isDate)),
                                  linetype='dashed', size=0.15,
                                  color='grey80')
                } else {
                    p = p + 
                        geom_line(aes(x=plot_lim$x,
                                      y=plot_lim$y),
                                  linetype='dashed', size=0.15,
                                  color='grey80')
                }
                
            } else {
                # Takes the x and y limits for the ith rectangle
                xLim_i = plot_mean$xmax[i]
                yLim_i = plot_mean$ymax[i]
                # Takes the x and y limits for the i+1th rectangle
                xLim_i1 = plot_mean$xmin[i+1]
                yLim_i1 = plot_mean$ymax[i+1]
                
                # Make a tibble to store dataEx
                plot_lim = tibble(x_i=c(xLim_i, xLim_i),
                                  y_i=c(minX_win, yLim_i),
                                  x_i1=c(xLim_i1, xLim_i1),
                                  y_i1=c(minX_win, yLim_i1))
                # Plot the limit of rectangles
                if (unit == "jour de l'année") {
                    p = p +
                        geom_line(aes(x=plot_lim$x_i,
                                      y=as.Date(plot_lim$y_i + isDate)),
                                  linetype='dashed', size=0.15,
                                  color='grey80') +
                        geom_line(aes(x=plot_lim$x_i1,
                                      y=as.Date(plot_lim$y_i1 + isDate)),
                                  linetype='dashed', size=0.15,
                                  color='grey80')
                } else {
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
    }

    # ### Grid ###
    if (grid) {
        p = p +
            theme(panel.grid.major.y=element_line(color='grey80',
                                                  size=0.3))
    }

    
    ### Data ###
    # If it is a square root flow or flow
    if (var == '\\sqrt{Q}' | var == 'Q') {
        # Plot the data as line
        p = p +
            geom_line(aes(x=dataEx_code$Date,
                          y=dataEx_code$X),
                      color='grey20',
                      size=0.3,
                      lineend="round")
    } else {
        # Plot the data as point
        if (unit == "jour de l'année") {
            p = p +
                geom_point(aes(x=dataEx_code$Date,
                               y=as.Date(dataEx_code$X + isDate)),
                           shape=19, color='grey50', alpha=1,
                           stroke=0, size=1)
        } else {
            p = p +
                geom_point(aes(x=dataEx_code$Date,
                               y=dataEx_code$X),
                           shape=19, color='grey50', alpha=1,
                           stroke=0, size=1)
        }
    }

    ### Missing data ###
    # If the option is TRUE
    if (missRect) {
        # Remove NA data
        NAdate = dataEx_code$Date[is.na(dataEx_code$X)]
        # Get the difference between each point of date data without NA
        dNAdate = diff(NAdate)
        # If difference of day is not 1 then
        # it is TRUE for the beginning of each missing data period 
        NAdate_Down = NAdate[append(Inf, dNAdate) != 1]
        # If difference of day is not 1 then
        # it is TRUE for the ending of each missing data period 
        NAdate_Up = NAdate[append(dNAdate, Inf) != 1]

        # Plot the missing data period
        p = p +
            geom_rect(aes(xmin=as.Date(NAdate_Down), 
                          ymin=-Inf, 
                          xmax=as.Date(NAdate_Up), 
                          ymax=Inf),
                      linetype=0, fill='#66c1bf', alpha=0.4)
    }
        
    ### Trend ###
    # If there is trends
    if (!is.null(trend_code)) {

        # Extract start and end of trend periods
        Start = trend_code$start
        End = trend_code$end
        # Get the name of the different period
        UStart = levels(factor(Start))        
        UEnd = levels(factor(End))

        # Compute the max of different start and end
        # so the number of different period
        nPeriod_trend = max(length(UStart), length(UEnd))

        # Blank tibble to store trend data and legend data
        plot_trend = tibble()
        leg_trend = tibble()
        # For all the different period
        for (i in 1:nPeriod_trend) {

            # Extracts the corresponding data for the period
            dataEx_code_per =
                dataEx_code[dataEx_code$Date >= Start[i] 
                             & dataEx_code$Date <= End[i],]

            # Computes the mean of the data on the period
            dataExMean = mean(dataEx_code_per$X,
                            na.rm=TRUE)
            
            # Get the trend associated to the first period
            trend_code_per = 
                trend_code[trend_code$start == Start[i] 
                              & trend_code$end == End[i],]
            
            # Number of trend selected
            Ntrend = nrow(trend_code_per)
            # If the number of trend is greater than a unique one
            if (Ntrend > 1) {
                # Extract only the first hence it is the same period
                trend_code_per = trend_code_per[1,]
            }            

            dataEx_codeNoNA = dataEx_code[!is.na(dataEx_code$X),]
            
            # Search for the index of the closest existing date 
            # to the start of the trend period of analysis
            iStart = which.min(abs(dataEx_codeNoNA$Date - Start[i]))
            # Same for the end
            iEnd = which.min(abs(dataEx_codeNoNA$Date - End[i]))
            # Get the start and end date associated
            xmin = dataEx_codeNoNA$Date[iStart]
            xmax = dataEx_codeNoNA$Date[iEnd]

            # If there is a x axis limit
            if (!is.null(axis_xlim)) {
                # If the min of the current period
                # is smaller than the min of the x axis limit
                if (xmin < axis_xlim[1]) {
                    # The min of the period is the min
                    # of the x axis limit
                    xmin = axis_xlim[1]
                }
                # Same for end
                if (xmax > axis_xlim[2]) {
                    xmax = axis_xlim[2]
                } 
            }

            # Create vector to store x dataEx
            abs = c(xmin, xmax)

            # Convert the number of day to the unit of the period
            abs_num = as.numeric(abs, origin="1970-01-01") / unit2day
            # Compute the y of the trend
            ord = abs_num * trend_code_per$a +
                trend_code_per$b

            # Create temporary tibble with variable to plot trend
            # for each period
            plot_trendtmp = tibble(abs=abs, ord=ord, period=i)
            # Bind it to the main tibble to store it with other period
            plot_trend = bind_rows(plot_trend, plot_trendtmp)

            # If there is a x axis limit
            if (!is.null(axis_xlim)) {
                # The x axis limit is selected
                codeDate = axis_xlim
            } else {
                # The entire date data is selected
                codeDate = dataEx_code$Date
            }
            # The y limit is stored in a vector
            codeX = c(minX, maxX)

            # Position of the x beginning and end of the legend symbol
            x = gpct(1.5, codeDate, shift=TRUE)
            xend = x + gpct(3, codeDate)

            # Spacing between legend symbols
            dy = gpct(9, codeX, min_lim=ymin_lim)
            # Position of the y beginning and end of the legend symbol
            y = gpct(100, codeX,
                     min_lim=ymin_lim, shift=TRUE) - (i-1)*dy
            yend = y

            # Position of x for the beginning of the associated text
            xt = xend + gpct(1, codeDate)

            # Position of the background rectangle of the legend
            xminR = x - gpct(1, codeDate)
            yminR = y - gpct(5, codeX, min_lim=ymin_lim)
            # If it is a flow variable
            if (unit == 'hm^{3}' | unit == 'm^{3}.s^{-1}') {
                xmaxR = x + gpct(32.5, codeDate)
            # If it is a date variable
            } else if (unit == "jour de l'année" | unit == 'jour' | unit == 'jour.an^{-1}') {
                xmaxR = x + gpct(20.5, codeDate)
            }
            ymaxR = y + gpct(5, codeX, min_lim=ymin_lim)

            # Gets the trend
            trend = trend_code_per$a
            # Gets the p value
            pVal = trend_code_per$p

            if (pVal <= level) {
                colorLine = color[i]
                colorLabel = color[i]
                colorLabel = switch_colorLabel(colorLabel)
            } else {
                colorLine = 'grey80'
                colorLabel = 'grey80'
            }

            # Computes the mean trend
            trendMean = trend/dataExMean
            # Computes the magnitude of the trend
            power = get_power(trend)
            # Converts it to character
            powerC = as.character(power)
            # If the power is positive
            if (powerC >= 0) {
                # Adds a space in order to compensate for the minus
                # sign that sometimes is present for the other periods
                spaceC = ' '
            # Otherwise
            } else {
                # No space is added
                spaceC = ''
            }

            # Gets the power of ten of magnitude
            brk = 10^power
            # Converts trend to character for sientific expression
            aC = as.character(format(round(trend / brk, 2),
                                         nsmall=2))
            # If the trend is positive
            if (aC >= 0) {
                # Adds two spaces in order to compensate for the minus
                # sign that sometimes is present for the other periods
                aC = paste('  ', aC, sep='')
            }
            # Converts mean trend to character
            aMeanC = as.character(format(round(trendMean*100, 2),
                                             nsmall=2))
            if (aMeanC >= 0) {
                # Adds two spaces in order to compensate for the minus
                # sign that sometimes is present for the other periods
                aMeanC = paste('  ', aMeanC, sep='')
            }

            # Create temporary tibble with variable to plot legend
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
            # Bind it to the main tibble to store it with other period
            leg_trend = bind_rows(leg_trend, leg_trendtmp)  
        }

        if (length(linetype_per) < nPeriod_trend) {
            linetype_per = rep(linetype_per, times=nPeriod_trend)
        }

        linetypeLeg_per = linetype_per
        linetypeLeg_per[linetype_per == 'longdash'] = '33'
        linetypeLeg_per[linetype_per == 'dashed'] = '22'
        linetypeLeg_per[linetype_per == 'dotted'] = '11'
        
        # For all periods
        for (i in 1:nPeriod_trend) {
            # Extract the trend of the current sub period
            leg_trend_per = leg_trend[leg_trend$period == i,]

            if (nPeriod_trend > 1) {
                # Plot the background for legend
                if (unit == "jour de l'année") {
                    p = p +
                        geom_rect(aes(xmin=leg_trend_per$xminR,
                                      ymin=as.Date(leg_trend_per$yminR
                                                   + isDate),
                                      xmax=leg_trend_per$xmaxR,
                                      ymax=as.Date(leg_trend_per$ymaxR
                                                   + isDate)),
                                  linetype=0, fill='white', alpha=0.3)
                } else {
                    p = p +
                        geom_rect(aes(xmin=leg_trend_per$xminR,
                                      ymin=leg_trend_per$yminR,
                                      xmax=leg_trend_per$xmaxR,
                                      ymax=leg_trend_per$ymaxR),
                                  linetype=0, fill='white', alpha=0.3)
                }
            }
                
            # Get the character variable for naming the trend
            colorLine = leg_trend_per$colorLine
            colorLabel = leg_trend_per$colorLabel
            aC = leg_trend_per$aC
            powerC = leg_trend_per$powerC
            spaceC = leg_trend_per$spaceC
            aMeanC = leg_trend_per$aMeanC

            unitF = gsub(" ", "\\\\,", unit)
            if (unit == 'hm^{3}' | unit == 'm^{3}.s^{-1}') {
                label = paste0("\\textbf{", aC,
                               " x 10$^{$", powerC,"}}",
                               spaceC,
                               " ", "\\[$", unitF, ".an^{-1}$", "\\]",
                               "\\;", "\\textbf{", aMeanC, "}",
                               " ", "\\[%$.an^{-1}$\\]")
                
            } else if (unit == "jour de l'année" | unit == "jour") {
                label = paste0("\\textbf{", aC,
                               " x 10$^{$", powerC,"}}",
                               spaceC,
                               " ", "\\[", "jour$.an^{-1}$", "\\]")
            } else if (unit == 'jour.an^{-1}') {
                label = paste0("\\textbf{", aC,
                               " x 10$^{$", powerC,"}}",
                               spaceC,
                               " ", "\\[", "jour$.an^{-2}$", "\\]")
            }

            if (nPeriod_trend > 1) {
                if (unit == "jour de l'année") {
                    # Plot the trend symbole and value of the legend
                    p = p +
                        annotate("segment",
                                 x=leg_trend_per$x,
                                 xend=leg_trend_per$xend,
                                 y=as.Date(leg_trend_per$y + isDate),
                                 yend=as.Date(leg_trend_per$yend + isDate),
                                 color=colorLine,
                                 linetype=linetypeLeg_per[i],
                                 lwd=0.8,
                                 lineend="round") +
                        annotate("text",
                                 label=TeX(label), size=2.8,
                                 x=leg_trend_per$xt,
                                 y=as.Date(leg_trend_per$y + isDate), 
                                 hjust=0, vjust=0.5,
                                 color=colorLabel)
                } else {
                    # Plot the trend symbole and value of the legend
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
                }
            } else {
                p = p +
                    theme(plot.title=element_text(size=7,
                                                  vjust=-1.5, 
                                                  hjust=0,
                                                  color=colorLabel)) + 
                    ggtitle(TeX(label))
            }
        }

        # For all periods
        for (i in 1:nPeriod_trend) {
            # Extract the trend of the current sub period
            plot_trend_per = plot_trend[plot_trend$period == i,]
            
            # Plot the line of white background of each trend
            if (unit == "jour de l'année") {
                p = p + 
                    geom_line(aes(x=plot_trend_per$abs,
                                  y=as.Date(plot_trend_per$ord
                                            + isDate)),
                              color='white',
                              linetype='solid',
                              size=1.5,
                              lineend="round")
            } else {
                p = p + 
                    geom_line(aes(x=plot_trend_per$abs,
                                  y=plot_trend_per$ord),
                              color='white',
                              linetype='solid',
                              size=1.5,
                              lineend="round")
            }
        }
        
        # For all periods
        for (i in 1:nPeriod_trend) {
            # Extract the trend of the current sub period
            plot_trend_per = plot_trend[plot_trend$period == i,]

            # Plot the line of trend
            if (unit == "jour de l'année") {
                p = p + 
                    geom_line(aes(x=plot_trend_per$abs,
                                  y=as.Date(plot_trend_per$ord
                                            + isDate)),
                              color=color[i],
                              linetype=linetype_per[i],
                              size=0.75,
                              lineend="round")
            } else {
                p = p + 
                    geom_line(aes(x=plot_trend_per$abs,
                                  y=plot_trend_per$ord),
                              color=color[i],
                              linetype=linetype_per[i],
                              size=0.75,
                              lineend="round")
            }
        }
    }

    if (!is.null(samplePeriod_code)) {
        # If there is a x axis limit
        if (!is.null(axis_xlim)) {
            # The x axis limit is selected
            codeDate = axis_xlim
        } else {
            # The entire date data is selected
            codeDate = dataEx_code$Date
        }
        # The y limit is stored in a vector
        codeX = c(minX_win, maxX_win)

        # Position of the x beginning and end of the legend symbol
        hPx = gpct(0, codeDate, shift=TRUE)
        # Position of the y beginning and end of the legend symbol
        hPy = gpct(50, codeX, min_lim=ymin_lim, shift=TRUE)

        if (length(samplePeriod_code) > 1) {
            hPlabel = paste0(
                "$^{$",
                "\\small{",
                format(as.Date(paste0("1970-",
                                      samplePeriod_code[1])), "%d %B"),
                " / ",
                format(as.Date(paste0("1970-",
                                      samplePeriod_code[2])), "%d %B"),
                "}}")
        } else {
            hPlabel = paste0(
                "$^{$",
                "\\small{",
                format(as.Date(paste0("1970-",
                                      samplePeriod_code)), "%d %B"),
                " / ",
                format(as.Date(paste0("1970-",
                                      samplePeriod_code))-1, "%d %B"),
                "}}")
        }
    }
    
    # Y axis title
    # If it is a flow variable
    varF = gsub("etiage", "étiage", var)  
    if (grepl("[_]", varF)) {
        varF = gsub("[_]", "$_{$", varF)
        varF = paste0(varF, "}")
    }
    unitF = gsub(" ", "\\\\,", unit)
    ylabel = paste0("\\textbf{", varF, "}", "\\;", "\\[$", unitF, "$\\]")

    if (!is.null(samplePeriod_code)) {
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

    if (is.null(axis_xlim)) {
        limits = c(min(dataEx_code$Date), max(dataEx_code$Date))
    } else {
        limits = axis_xlim
    }

    breaks = function(X) {
        Xmin = min(X)
        Xmax = max(X)
        seq.Date(from=as.Date(paste0(
                     round(lubridate::year(Xmin), -1),
                     "-01-01")),
                 to=as.Date(paste0(
                     round(lubridate::year(Xmax), -1),
                     "-01-01")),
                 by="10 years")
    }

    minor_breaks = function(X) {
        Xmin = min(X)
        Xmax = max(X)
        seq.Date(from=as.Date(paste0(
                     round(lubridate::year(Xmin), -1),
                     "-01-01")),
                 to=as.Date(paste0(
                     round(lubridate::year(Xmax), -1),
                     "-01-01")),
                 by="2 years")
    }
    
    date_labels = "%Y"

    # Parameters of the x axis contain the limit of the date dataEx
    p = p +
        scale_x_date(
            breaks=breaks,
            minor_breaks=minor_breaks,
            guide='axis_minor',
            date_labels=date_labels,
            limits=limits,
            position=position, 
            expand=c(0, 0)
        )

    # Parameters of the y axis
    # If it is a flow variable
    if (unit == 'jour' | unit == 'hm^{3}' | unit == 'm^{3}.s^{-1}' | unit == 'm^{3/2}.s^{-1/2}' | unit == 'jour.an^{-1}') {
        
        if (get_power(minX) >= 4) {
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

    # If it is a date variable
    } else if (unit == "jour de l'année") {
        monthSpread = (maxX - minX) %% 365.25 / 30.4375
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

    # Margins
    if (!is.null(trend_code)) {
        if (nPeriod_trend > 1) {
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
    } else {
        tt = 2.5
        t = 2
        tb = 3
        b = 2
    }
    
    if (last == "all") {
        pLastTRUE = p
        pLastFALSE = p
        if (first) {
            pLastFALSE = pLastFALSE +
                theme(plot.margin=margin(t=tt, r=0, b=tb, l=0,
                                         unit="mm"))
            pLastTRUE = pLastTRUE +
                theme(plot.margin=margin(t=tt, r=0, b=0, l=0,
                                         unit="mm"))
        } else {
            pLastFALSE = pLastFALSE + 
                theme(plot.margin=margin(t=t, r=0, b=b, l=0,
                                         unit="mm"),
                      axis.text.x=element_blank())
            pLastTRUE = pLastTRUE +
                theme(plot.margin=margin(t=t, r=0, b=0, l=0,
                                         unit="mm"))
        }

        res = list(lastTRUE=pLastTRUE, lastFALSE=pLastFALSE)
        return(res)
        
    } else {
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
        } else if (!first & !last){
            p = p + 
                theme(plot.margin=margin(t=t, r=0, b=b, l=0,
                                         unit="mm"),
                      axis.text.x=element_blank())
        }
        return(p)
    }
} 
