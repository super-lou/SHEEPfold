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


#' @title Info panel
#' @export
panel_axis = function(X,
                      axis.text.x_size=9,
                      date_labels="%Y",
                      breaks="10 years",
                      minor_breaks="2 years",
                      d_breaks=0,
                      break_round=-1,
                      add_breaks=NULL,
                      axis_margin=margin(t=0, r=0, b=0, l=0, "cm")) {

    # Labels = c("1976", "2005",
    #            paste0("<b style='color:", IPCCgrey35, "'>2020</b>"),
    #            "2040",
    #            paste0("<b style='color:", IPCCgrey35, "'>2050</b>"),
    #            "2070",
    #            paste0("<b style='color:", IPCCgrey35, "'>2099</b>"))
    # majorDate = as.Date(c("1976-01-01", "2005-01-01", "2020-01-01",
    #                       "2040-01-01", "2050-01-01",
    #                       "2070-01-01", "2099-01-01"))
    # minorDate = seq.Date(as.Date("1980-01-01"),
    #                      as.Date("2090-01-01"),
    #                      "10 years")
    # minorDate = minorDate[!(minorDate %in% majorDate)]

    get_breaks = function(X) {
        if (lubridate::is.Date(X)) {
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
        if (lubridate::is.Date(X)) {
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
    
    axis = ggplot2::ggplot() +
        ggplot2::theme(plot.margin=axis_margin) +
        theme_IPCC(isGridY=FALSE,
                   is_axis.ticks.y=FALSE,
                   is_axis.text.y=FALSE,
                   axis.text.x_size=axis.text.x_size,
                   is_axis.line.x=TRUE) +
        
        ggplot2::annotate("point",
                          x=X, y=0,
                          color=NA, fill=NA) +
        # ggplot2::scale_x_date(
        #              breaks=majorDate,
        #              minor_breaks=minorDate, 
        #              labels=Labels,
        #              guide="axis_minor",
        #              expand=c(0, 0)) +
        # ggplot2::scale_x_date(
        #              date_breaks="10 years",
        #              date_minor_breaks="5 years", 
        #              date_labels="%Y",
        #              guide="axis_minor",
        #              expand=c(0, 0)) +
        ggplot2::scale_y_continuous(limits=c(0, 1),
                                    expand=c(0, 0))


    if (lubridate::is.Date(X)) {
        axis = axis +
            ggplot2::scale_x_date(
                         breaks=get_breaks,
                         minor_breaks=get_minor_breaks,
                         guide='axis_minor',
                         date_labels=date_labels,
                         expand=c(0, 0))
    } else {
        axis = axis +
            ggplot2::scale_x_continuous(
                         breaks=get_breaks,
                         minor_breaks=get_minor_breaks,
                         guide='axis_minor',
                         expand=c(0, 0))
    }
    
    return (axis)
}  
