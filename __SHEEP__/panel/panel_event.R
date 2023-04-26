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

#' @title Event panel
#' @export
panel_event = function(event, colorEvent, colorTextEvent) {

    if (event != 'Resume' & event != 'None') {
        plot = ggplot() + theme_void() +
            
            theme(plot.margin=margin(t=0, r=3, b=0, l=0, unit="mm")) + 
            
            annotate("rect",
                     xmin=0, xmax=1,
                     ymin=0, ymax=5,
                     fill=colorEvent[event]) +
            
            annotate("text",
                     x=0.5, y=0.1,
                     vjust=0.5, hjust=0,
                     label=toupper(event),
                     color=colorTextEvent[event],
                     fontface="bold",
                     size=2.9,
                     angle=90) +
            
            scale_x_continuous(limits=c(0, 1),
                               expand=c(0, 0)) +
            
            scale_y_continuous(limits=c(0, 5),
                               expand=c(0, 0))
    } else {
        plot = void()
    }
    return (plot)
} 
