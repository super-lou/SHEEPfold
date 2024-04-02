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


#' @title panel spaghetti
#' @export
panel_spaghetti = function (data_code, Colors=NULL,
                            title="débit journalier",
                            subtitle=NULL,
                            unit="m^{3}.s^{-1}",
                            alpha=0.7,
                            alpha_non_color=0.3,
                            isSqrt=FALSE, missRect=FALSE,
                            isBack=TRUE,
                            isTitleAbove=TRUE,
                            isLegend=FALSE,
                            obsLegend="Observations",
                            simLegend="Simulations",
                            addHMLegend=FALSE,
                            sizeYticks=9,
                            date_labels="%Y",
                            breaks="10 years",
                            minor_breaks="2 years",
                            d_breaks=0,
                            break_round=-1,
                            add_x_breaks=NULL,
                            isNormLaw=FALSE,
                            Xlabel=NULL,
                            isZeroLine=TRUE,
                            limits_ymin=NA,
                            limits_ymax=NA,
                            # ymax_expand=0.1,
                            isBackObsAbove=TRUE,
                            lwObs=0.55,
                            lwObs_back=1.7,
                            lwSim=0.4,
                            lwSim_back=0.7,
                            lwSim_non_color=0.2,
                            axis_xlim=NULL, grid=TRUE,
                            convertTeX=TRUE,
                            dx0_title=0.05,
                            dx0_subtitle=0.1,
                            ratio_title=1/5,
                            margin_title=margin(t=0, r=0, b=0, l=0, "mm"),
                            margin_spag=margin(t=0, r=0, b=0, l=0, "mm"),
                            first=FALSE, last=FALSE,
                            hide_y_axis=FALSE,
                            verbose=FALSE) {

    if (!is.null(axis_xlim))  {
        axis_xlim = as.Date(axis_xlim)
    }
    
    # unitTeX = convert2TeX(unit, bold=FALSE)    
    unitTeX = gsub(" ", "\\\\,", unit)

    if (convertTeX) {
        titleTeX = convert2TeX(title, bold=FALSE) 
    } else {
        titleTeX = title
    }
    
    if (isTitleAbove) {
        if (grepl("[*]unit[*]", titleTeX)) {
            titleTeX = gsub("[*]unit[*]",
                            paste0("($", unitTeX, "$)"),
                            titleTeX)
        } else {
            titleTeX = paste0(titleTeX, "\\,", "($", unitTeX, "$)")
        }
        
    } else {
        if (grepl("[*]unit[*]", titleTeX)) {
            titleTeX = gsub("[*]unit[*]",
                            paste0("[$", unitTeX, "$]"),
                            titleTeX)
        } else {
            titleTeX = paste0("\\textbf{", titleTeX, "}", "\\,", "\\small{\\[$", unitTeX, "$\\]}")
        }
    }

    
    if (!is.null(subtitle) & convertTeX) {
        subtitleTeX = convert2TeX(subtitle, bold=FALSE)
    } else {
        subtitleTeX = subtitle
    }

    if (isTitleAbove | isLegend) {
        title = ggplot() + theme_void() +
            theme(plot.margin=margin_title)

        # dx0 = 0.05
        
        if (isTitleAbove) {
            dx_title = 0.25
            title = title +
                annotate("text",
                         x=dx0_title,
                         y=1,
                         label=TeX(titleTeX),
                         size=3, hjust=0, vjust=1,
                         color=IPCCgrey25)

            if (!is.null(subtitle)) {
                title = title +
                    annotate("text",
                             x=dx0_subtitle,
                             y=0,
                             label=TeX(subtitleTeX),
                             size=2.5, hjust=0, vjust=0,
                             color=IPCCgrey25)
            }
        } else {
            dx_title = 0
        }

        if (isLegend) {

            if (!is.null(obsLegend)) {
                dx_obs = 0.12
                title = title +
                    annotate("line",
                             x=dx0 + c(dx_title,
                                       dx_title+0.02),
                             y=rep(0.25, 2),
                             color=IPCCgrey23,
                             linewidth=0.7,
                             lineend="round") +
                    annotate("text",
                             x=dx_title+0.027,
                             y=0.5,
                             label=obsLegend,
                             size=2.5, hjust=0, vjust=1,
                             color=IPCCgrey50)
            } else {
                dx_obs = 0 
            }

            if (addHMLegend & length(Colors) == 1) {
                dx_hm = 0.14
                title = title +
                    annotate("line",
                             x=dx0 + c(dx_title+dx_obs,
                                       dx_title+dx_obs+0.02),
                             y=rep(0.25, 2),
                             color=Colors,
                             linewidth=0.7,
                             lineend="round") +
                    annotate("text",
                             x=dx0 + dx_title+dx_obs+0.027,
                             y=0.5,
                             label=names(Colors),
                             size=2.5, hjust=0, vjust=1,
                             color=IPCCgrey50)
            } else if (!is.null(simLegend)) {
                dx_hm = 0.11
                title = title +
                    annotate("line",
                             x=dx0 + c(dx_title+dx_obs,
                                       dx_title+dx_obs+0.02),
                             y=rep(0.25, 2),
                             color=IPCCgrey67,
                             linewidth=0.7,
                             alpha=alpha,
                             lineend="round") +
                    annotate("text",
                             x=dx0 + dx_title+dx_obs+0.027,
                             y=0.5,
                             label=simLegend,
                             size=2.5, hjust=0, vjust=1,
                             color=IPCCgrey50)
            } else {
                dx_hm = 0
            }
            
            if (missRect) {
                title = title +
                    annotate("rect",
                             xmin=dx0+ dx_title+dx_obs+dx_hm, 
                             ymin=0, 
                             xmax=dx0 + dx_title+dx_obs+dx_hm+0.005, 
                             ymax=0.55,
                             linetype=0,
                             fill=INRAElightcyan,
                             alpha=0.4) +
                    annotate("text",
                             x=dx0 + dx_title+dx_obs+dx_hm+0.01,
                             y=0.5,
                             label="Lacunes",
                             size=2.5, hjust=0, vjust=1,
                             color=IPCCgrey50)
            }
        }

        title = title +
            scale_x_continuous(limits=c(0, 1),
                               expand=c(0, 0)) +
            scale_y_continuous(limits=c(0, 1),
                               expand=c(0, 0))
    }
    
    isDate = lubridate::is.Date(data_code$date[1])
    
    if (isNormLaw) {
        data_code = data_code[data_code$date != 0 &
                              data_code$date != 1,]
    }
    
    if ("HM" %in% names(data_code)) {
        HM = sort(unique(names(Colors)))
        nHM = length(HM)
        
        select_good = function (X) {
            Xrle = rle(X)
            value = Xrle$values[Xrle$lengths == max(Xrle$lengths)]
            if (length(value) > 1) {
                value = mean(value, na.rm=TRUE)
            }
            return (value)
        }
        
        data_code_obs =
            dplyr::summarise(dplyr::group_by(data_code, date),
                             Q=median(Q_obs, na.rm=TRUE),
                             .groups="drop")

        maxQ_obs = max(data_code_obs$Q, na.rm=TRUE)
        minQ_obs = min(data_code_obs$Q, na.rm=TRUE)
        maxQ_sim = max(data_code$Q_sim, na.rm=TRUE)
        minQ_sim = min(data_code$Q_sim, na.rm=TRUE)
        maxQ = max(c(maxQ_obs, maxQ_sim), na.rm=TRUE)
        minQ = min(c(minQ_obs, minQ_sim), na.rm=TRUE)
        
    } else {
        data_code_obs = data_code
        maxQ = max(data_code_obs$Q, na.rm=TRUE)
        minQ = min(data_code_obs$Q, na.rm=TRUE)
    }

    if (is.null(axis_xlim)) {
        limits = c(min(data_code_obs$date), max(data_code_obs$date))
    } else {
        limits = axis_xlim
    }

    # Open new plot
    spag = ggplot() + coord_cartesian(clip="off") + 
        theme_IPCC(isBack,
                   isLabelX=!is.null(Xlabel),
                   isLabelY=!isTitleAbove) +
        theme(panel.border=element_blank(),
              axis.text.y=element_text(size=sizeYticks))

    if (!isTitleAbove) {
        spag = spag + ylab(TeX(titleTeX))
    }
    

    ### Grid ###
    if (!grid) {
        spag = spag +
            theme(panel.grid.major.y=element_blank())
    }

    ### Missing data ###
    # If the option is TRUE
    if (missRect) {
        # Remove NA data
        NAdate = data_code_obs$date[is.na(data_code_obs$Q)]
        # Get the difference between each point of date data without NA
        dNAdate = diff(NAdate)
        # If difference of day is not 1 then
        # it is TRUE for the beginning of each missing data period 
        NAdate_Down = NAdate[append(Inf, dNAdate) != 1]
        # If difference of day is not 1 then
        # it is TRUE for the ending of each missing data period 
        NAdate_Up = NAdate[append(dNAdate, Inf) != 1]

        if (isDate) {
            xmin = as.Date(NAdate_Down)
            xmax = as.Date(NAdate_Up)
        } else {
            xmin = NAdate_Down
            xmax = NAdate_Up
        }
        
        # Plot the missing data period
        spag = spag +
            annotate("rect",
                     xmin=xmin, 
                     ymin=limits_ymin, 
                     xmax=xmax, 
                     ymax=Inf,
                     linetype=0,
                     fill=INRAElightcyan,
                     alpha=0.4)
    }

    # zeroline
    if (isZeroLine) {
        spag = spag + 
            theme(axis.line.x=element_line(color=IPCCgrey60,
                                           size=0.5,
                                           lineend="square"))
    }
    
    ### Data ###
    if (!isBackObsAbove) {
        spag = spag +
            ggplot2::annotate("line",
                              x=data_code_obs$date,
                              y=data_code_obs$Q,
                              color="white",
                              linewidth=lwObs_back,#0.4,
                              lineend="round")
    }


    HM_ALL = unique(data_code$HM)
    
    if (!is.null(Colors) & length(HM_ALL) > 0) {
        data_nHM_code = data_code[!(data_code$HM %in% HM),]
        spag = spag +
            ggplot2::geom_line(data=data_nHM_code,
                               aes(x=date,
                                   y=Q_sim,
                                   group=HM),
                               color=IPCCgrey67,
                               linewidth=lwSim_non_color,
                               alpha=alpha_non_color,
                               lineend="round")
    }
    

    if ("HM" %in% names(data_code)) {
        for (i in 1:nHM) {
            hm = HM[i]
            data_hm_code = data_code[data_code$HM == hm,]

            # Plot the data as line
            spag = spag +
                ggplot2::annotate("line",
                                  x=data_hm_code$date,
                                  y=data_hm_code$Q_sim,
                                  color="white",
                                  linewidth=lwSim_back,
                                  lineend="round")
        }
        if (is.null(Colors)) {
            Colors = rep(IPCCgrey67, nHM)
            names(Colors) = HM
        }
        for (i in 1:nHM) {
            hm = HM[i]
            data_hm_code = data_code[data_code$HM == hm,]
            spag = spag +
                ggplot2::annotate("line",
                                  x=data_hm_code$date,
                                  y=data_hm_code$Q_sim,
                                  color=Colors[names(Colors) == hm],
                                  linewidth=lwSim,
                                  alpha=alpha,
                                  lineend="round")
        }
    }

    if (isBackObsAbove) {
        spag = spag +
            ggplot2::annotate("line",
                              x=data_code_obs$date,
                              y=data_code_obs$Q,
                              color="white",
                              linewidth=lwObs_back,
                              lineend="round") +
            ggplot2::annotate("line",
                              x=data_code_obs$date,
                              y=data_code_obs$Q,
                              color=IPCCgrey23,
                              linewidth=lwObs,
                              lineend="round")
    } else {
        spag = spag +
            ggplot2::annotate("line",
                              x=data_code_obs$date,
                              y=data_code_obs$Q,
                              color=IPCCgrey23,
                              linewidth=lwObs,
                              lineend="round") 
    }

    if (!is.null(Xlabel)) {
        spag = spag +
            xlab(Xlabel)
    }

    if (first) {
        position = 'top'
    } else {
        position = 'bottom'
    }

    get_breaks = function(X, add_breaks=add_x_breaks) {
        if (isDate) {
            Xmin = round(lubridate::year(min(X)), break_round)
            Xmax = round(lubridate::year(max(X)), break_round)
            if (Xmax-Xmin <= 1) {
                Xmin = lubridate::year(X)[1]
                Xmax = lubridate::year(X)[1] + 1
            }
            res = seq.Date(from=as.Date(paste0(Xmin, "-01-01")) + d_breaks,
                           to=as.Date(paste0(Xmax, "-01-01")) + d_breaks,
                           by=breaks)
        } else {
            Xmin = round(min(X), break_round)
            Xmax = round(max(X), break_round)
            res = seq(from=Xmin + d_breaks,
                      to=Xmax + d_breaks,
                      by=breaks)
        }

        if (!is.null(add_breaks)) {
            res = sort(c(res, add_breaks))
        }

        return (res)
    }

    get_minor_breaks = function(X) {
        if (isDate) {
            Xmin = round(lubridate::year(min(X)), break_round)
            Xmax = round(lubridate::year(max(X)), break_round)
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
        } else {
            Xmin = round(min(X), break_round)
            Xmax = round(max(X), break_round)
            res = seq(from=Xmin + d_breaks - breaks,
                      to=Xmax + d_breaks + breaks,
                      by=minor_breaks)
        }
        return (res)
    }

    if (isNormLaw) {
        low_major = c(1e-3, 1e-2, 1e-1, 0.5)
        up_major = rev(1-low_major)
        major_breaks = c(low_major, up_major)
        major_breaks = major_breaks[!duplicated(major_breaks)]
        minor_breaks = RcppRoll::roll_mean(major_breaks, n=2)
            
        spag = spag +
            scale_x_continuous(
                trans=scales::probability_trans("norm"),
                breaks=major_breaks,
                minor_breaks=minor_breaks,
                guide='axis_minor',
                limits=limits,
                position=position, 
                expand=c(0, 0))
        
    } else {
        if (isDate) {
            spag = spag +
                scale_x_date(
                    breaks=get_breaks,
                    minor_breaks=get_minor_breaks,
                    guide='axis_minor',
                    date_labels=date_labels,
                    limits=limits,
                    position=position, 
                    expand=c(0, 0))
        } else {
            spag = spag +
                scale_x_continuous(
                    breaks=get_breaks,
                    minor_breaks=get_minor_breaks,
                    guide='axis_minor',
                    limits=limits,
                    position=position, 
                    expand=c(0, 0))
        }
    }
    
    # Parameters of the y axis
    if (get_power(minQ) >= 4) {
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

    if (is.na(limits_ymin) & is.na(limits_ymax)) {
        limits = NULL
        expand = expansion(mult=c(0.2, 0.1))
    } else {
        limits = c(limits_ymin, limits_ymax)
        expand = expansion(mult=c(0, NA))
    }

    if (isSqrt) {
        spag = spag + scale_y_sqrt(limits=limits,
                                   n.breaks=5,
                                   labels=labels,
                                   expand=expand)
        
    } else {        
        spag = spag +
            scale_y_continuous(limits=limits,
                               n.breaks=5,
                               labels=labels,
                               expand=expand)
    }    

    # Margins
    # tt = 2.5
    # t = 2
    # tb = 3
    # b = 2

    if (first & !last) {
        spag = spag +
            theme(plot.margin=
                      # margin(t=tt, r=0, b=tb, l=0, unit="mm")+
                      margin_spag)
    } else if (!first & last) {
        spag = spag + 
            theme(plot.margin=
                      # margin(t=t, r=0, b=0, l=0, unit="mm")+
                      margin_spag)
    } else if (first & last) {
        spag = spag + 
            theme(plot.margin=
                      # margin(t=tt, r=0, b=0, l=0, unit="mm")+
                      margin_spag)
    } else if (!first & !last){
        spag = spag + 
            theme(plot.margin=
                      # margin(t=t, r=0, b=b, l=0, unit="mm")+
                      margin_spag,
                  axis.text.x=element_blank())
    }

    if (hide_y_axis) {
        spag = spag + 
            theme(axis.ticks.y=element_blank(),
                  axis.text.y=element_blank())
    }    


    if (isTitleAbove | isLegend) {
        plan = matrix(c("title",
                        "spag"),
                      nrow=2, 
                      byrow=TRUE)

        herd = bring_grass(verbose=verbose)
        herd = plan_of_herd(herd, plan,
                            verbose=verbose)
        
        herd = add_sheep(herd,
                         sheep=title,
                         id="title",
                         height=ratio_title,
                         verbose=verbose)
        herd = add_sheep(herd,
                         sheep=spag,
                         id="spag",
                         height=1,
                         verbose=verbose)
        
    } else {
        plan = matrix("spag",
                      nrow=1, 
                      byrow=TRUE)

        herd = bring_grass(verbose=verbose)
        herd = plan_of_herd(herd, plan,
                            verbose=verbose)
        herd = add_sheep(herd,
                         sheep=spag,
                         id="spag",
                         height=1,
                         verbose=verbose)
    }


    

    
    return (herd)
} 
