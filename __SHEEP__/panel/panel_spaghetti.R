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
                            unit="m^{3}.s^{-1}",
                            alpha=0.7,
                            isSqrt=FALSE, missRect=FALSE,
                            isBack=TRUE,
                            isTitle=TRUE,
                            isLegend=FALSE,
                            sizeYticks=9,
                            date_labels="%Y",
                            breaks="10 years",
                            minor_breaks="2 years",
                            d_breaks=0,
                            break_round=-1,
                            isNormLaw=FALSE,
                            Xlabel=NULL,
                            isZeroLine=TRUE,
                            limits_ymin=NA,
                            isBackObsAbove=TRUE,
                            lwObs=0.55,
                            lwObs_back=1.7,
                            lwSim=0.4,
                            lwSim_back=0.7,
                            axis_xlim=NULL, grid=TRUE,
                            ratio_title=1/5,
                            margin_title=margin(t=0, r=0, b=0, l=0, "mm"),
                            margin_spag=margin(t=0, r=0, b=0, l=0, "mm"),
                            first=FALSE, last=FALSE) {


    if (grepl("racine", title) & !grepl("racine[{]", title)) {
        title = gsub("racine", "\u221A", title)
    } else if (grepl("racine", title) & grepl("racine[{]", title)) {
        title = gsub("[}]", "", title)
        title = gsub("racine[{]", "\u221A", title)
    }
    
    unit = gsub(" ", "\\\\,", unit)
    if (grepl("[_]", title)) {
        title = gsub("[_]", "$_{$", title)
        title = paste0(title, "}")
    }
    if (grepl("\\unit", title)) {
        title = gsub("\\\\unit",
                     paste0("($", unit, "$)"),
                     title)
    } else {
        title = paste0(title, "\\,", "($", unit, "$)")
    }
    
    label = TeX(title)
    
    title = ggplot() + theme_void() +
        theme(plot.margin=margin_title)

    title = title +
        annotate("text",
                 x=0,
                 y=1,
                 label=label,
                 size=3, hjust=0, vjust=1,
                 color=IPCCgrey25)

    if (isLegend) {
        title = title +
            annotate("line",
                     x=c(0.25, 0.27),
                     y=rep(0.25, 2),
                     color=IPCCgrey25,
                     linewidth=0.7,
                     lineend="round") +
            annotate("text",
                     x=0.277,
                     y=0.5,
                     label="Observations",
                     size=2.5, hjust=0, vjust=1,
                     color=IPCCgrey50)
        
        if (missRect) {
            title = title +
                annotate("rect",
                         xmin=0.37, 
                         ymin=0, 
                         xmax=0.375, 
                         ymax=0.55,
                         linetype=0,
                         fill=INRAElightcyan,
                         alpha=0.4) +
                annotate("text",
                         x=0.38,
                         y=0.5,
                         label="Lacune",
                         size=2.5, hjust=0, vjust=1,
                         color=IPCCgrey50)
        }
    }

    title = title +
        scale_x_continuous(limits=c(0, 1),
                           expand=c(0, 0)) +
        scale_y_continuous(limits=c(0, 1),
                           expand=c(0, 0))

    
   
    isDate = inherits(data_code$Date, 'Date')

    if (isNormLaw) {
        data_code = data_code[data_code$Date != 0 &
                              data_code$Date != 1,]
    }
    
    if ("Model" %in% names(data_code)) {
        Model = levels(factor(data_code$Model))
        nModel = length(Model)
        
        select_good = function (X) {
            Xrle = rle(X)
            value = Xrle$values[Xrle$lengths == max(Xrle$lengths)]
            if (length(value) > 1) {
                value = mean(value, na.rm=TRUE)
            }
            return (value)
        }
        
        data_code_obs =
            dplyr::summarise(dplyr::group_by(data_code, Date),
                             Q=select_good(Q_obs),
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

    maxQ_win = maxQ * 1.05
    minQ_win = minQ * 0.95#expansion

    if (is.null(axis_xlim)) {
        limits = c(min(data_code_obs$Date), max(data_code_obs$Date))
    } else {
        limits = axis_xlim
    }

    # Open new plot
    spag = ggplot() + coord_cartesian(clip="off") + 
        theme_IPCC(isBack, isLabelX=!is.null(Xlabel)) +
        # theme_WIP() + 
        theme(panel.border=element_blank(),
              axis.text.y=element_text(size=sizeYticks))
    

    ### Grid ###
    if (!grid) {
        spag = spag +
            theme(panel.grid.major.y=element_blank())
    }

    ### Missing data ###
    # If the option is TRUE
    if (missRect) {
        # Remove NA data
        NAdate = data_code_obs$Date[is.na(data_code_obs$Q)]
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
            ggplot2::annotate("line",
                              x=limits,
                              y=c(0, 0),
                              color=IPCCgrey60,
                              size=0.5,
                              lineend="round")
    }
    
    ### Data ###
    if (!isBackObsAbove) {
        spag = spag +
            ggplot2::annotate("line",
                              x=data_code_obs$Date,
                              y=data_code_obs$Q,
                              color="white",
                              linewidth=lwObs_back,#0.4,
                              lineend="round")
    }
    
    if ("Model" %in% names(data_code)) {
        for (i in 1:nModel) {
            model = Model[i]
            data_model_code = data_code[data_code$Model == model,] 
            # Plot the data as line
            spag = spag +
                ggplot2::annotate("line",
                                  x=data_model_code$Date,
                                  y=data_model_code$Q_sim,
                                  color="white",
                                  linewidth=lwSim_back,#0.7,
                                  lineend="round")
        }
        if (is.null(Colors)) {
            Colors = rep(IPCCgrey67, nModel)
            names(Colors) = Model
        }
        for (i in 1:nModel) {
            model = Model[i]
            data_model_code = data_code[data_code$Model == model,]
            spag = spag +
                ggplot2::annotate("line",
                                  x=data_model_code$Date,
                                  y=data_model_code$Q_sim,
                                  color=Colors[names(Colors) == model],
                                  linewidth=lwSim,
                                  alpha=alpha,
                                  lineend="round")
        }
    }

    if (isBackObsAbove) {
        spag = spag +
            ggplot2::annotate("line",
                              x=data_code_obs$Date,
                              y=data_code_obs$Q,
                              color="white",
                              linewidth=lwObs_back,#1.7,
                              lineend="round") +
            ggplot2::annotate("line",
                              x=data_code_obs$Date,
                              y=data_code_obs$Q,
                              color=IPCCgrey25,
                              linewidth=lwObs,#0.55,
                              lineend="round")
    } else {
        spag = spag +
            ggplot2::annotate("line",
                              x=data_code_obs$Date,
                              y=data_code_obs$Q,
                              color=IPCCgrey25,
                              linewidth=lwObs,#0.2,
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

    get_breaks = function(X) {
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
            res = seq.Date(from=as.Date(paste0(Xmin, "-01-01")),
                           to=as.Date(paste0(Xmax, "-01-01")),
                           by=minor_breaks)
        } else {
            Xmin = round(min(X), break_round)
            Xmax = round(max(X), break_round)
            res = seq(from=Xmin + d_breaks,
                      to=Xmax + d_breaks,
                      by=minor_breaks)
        }
        return (res)
    }

    if (isNormLaw) {
        
        # spag = spag +
        #     annotation_custom(
        #         ggplotGrob(ggplot() + theme_void() +
        #                    annotate("segment",
        #                             x=0.72, xend=0.77,
        #                             y=-1.45, yend=-1.45,
        #                             color=IPCCgrey50,
        #                             linewidth=0.3,
        #                             arrow=arrow(length=unit(1, "mm")),
        #                             lineend="round") +
        #                    annotate("text",
        #                             x=0.78, y=-1.45,
        #                             label="  basses eaux",
        #                             color=IPCCgrey50,
        #                             size=2.7,
        #                             vjust=0.51, hjust=0) +
        #                    annotate("segment",
        #                             x=0.28, xend=0.23,
        #                             y=-1.45, yend=-1.45,
        #                             color=IPCCgrey50,
        #                             linewidth=0.3,
        #                             arrow=arrow(length=unit(1, "mm")),
        #                             lineend="round") +
        #                    annotate("text",
        #                             x=0.22, y=-1.45,
        #                             label="hautes eaux  ",
        #                             color=IPCCgrey50,
        #                             size=2.7,
        #                             vjust=0.51, hjust=1) +
        #                    scale_x_continuous(limits=c(0, 1),
        #                                       expand=c(0, 0)) +
        #                    scale_y_continuous(limits=c(-2, 2),
        #                                       expand=c(0, 0))),
        #         xmin=-Inf, xmax=Inf,
        #         ymin=-3, ymax=3)

        low_major = c(1e-3, 1e-2, 1e-1, 0.5)
        up_major = rev(1-low_major)
        major_breaks = c(low_major, up_major)
        major_breaks = major_breaks[!duplicated(major_breaks)]
        minor_breaks = RcppRoll::roll_mean(major_breaks, n=2)
            
        spag = spag +
            scale_x_continuous(
                trans=scales::probability_trans("norm"),
                # breaks=get_breaks,
                breaks=major_breaks,
                # minor_breaks=get_minor_breaks,
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

    if (isSqrt) {
        spag = spag + scale_y_sqrt(limits=c(limits_ymin, NA),
                                   n.breaks=5,
                                   labels=labels,
                                   expand=expansion(mult=c(0, 0.1)))
        
    } else {
        spag = spag +
            scale_y_continuous(limits=c(limits_ymin, NA),
                               n.breaks=5,
                               labels=labels,
                               expand=expansion(mult=c(0, 0.1)))
    }    

    # Margins
    tt = 2.5
    t = 2
    tb = 3
    b = 2

    if (first & !last) {
        spag = spag +
            theme(plot.margin=
                      margin(t=tt, r=0, b=tb, l=0, unit="mm")+
                      margin_spag)
    } else if (!first & last) {
        spag = spag + 
            theme(plot.margin=
                      margin(t=t, r=0, b=0, l=0, unit="mm")+
                      margin_spag)
    } else if (first & last) {
        spag = spag + 
            theme(plot.margin=
                      margin(t=tt, r=0, b=0, l=0, unit="mm")+
                      margin_spag)
    } else if (!first & !last){
        spag = spag + 
            theme(plot.margin=
                      margin(t=t, r=0, b=b, l=0, unit="mm")+
                      margin_spag,
                  axis.text.x=element_blank())
    }


    if (isTitle) {
        height_title = ratio_title
    } else {
        height_title = 0
    }

    plan = matrix(c("title",
                    "spag"),
                  nrow=2, 
                  byrow=TRUE)
    
    flock = bring_grass()
    flock = plan_of_flock(flock, plan)
    
    flock = add_sheep(flock,
                      sheep=title,
                      id="title",
                      height=height_title)
    flock = add_sheep(flock,
                      sheep=spag,
                      id="spag",
                      height=1)
    
    flock = shear_sheeps(flock)
    
    return (flock)
} 
