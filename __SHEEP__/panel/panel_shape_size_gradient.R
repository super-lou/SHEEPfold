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


panel_shape_size_gradient = function (shape="rect",
                                    Size=c(0.4, 0.6, 0.8, 1),
                                    color=IPCCgrey50,
                                    labelArrow=NULL,
                                    dx_shape=0.4,
                                    dy_shape=1,
                                    size_arrow=0.25,
                                    dy_arrow=1,
                                    dz_arrow=2,
                                    dl_arrow=0,
                                    dr_arrow=0,
                                    dx_text=0.2,
                                    height=10,
                                    width=10,
                                    shift=c(x=0, y=0),
                                    WIP=FALSE) {        

    nSize = length(Size)
    dX = seq(0, dx_shape*(nSize-1), dx_shape)
    dS = cumsum(Size) - Size/2
    X = dX + dS

    limit = min(c(height, width))
    options(repr.plot.width=width, repr.plot.height=height)
    
    plot = ggplot() + theme_void() +
        coord_fixed(clip="off") + 
        theme(text=element_text(family="Helvetica"),
              plot.margin=margin(0, 0, 0, 0))

    if (WIP) {
        plot = plot + theme_WIP()
    }

    if (!is.null(labelArrow)) {
        plot = plot +
            
            annotate(
                "segment",
                x=(shift[1]+min(X, na.rm=TRUE)-dl_arrow),
                xend=(shift[1]+max(X, na.rm=TRUE)+dr_arrow),
                y=(shift[2]+dy_arrow),
                yend=(shift[2]+dy_arrow),
                color=IPCCgrey50, size=size_arrow,
                arrow=arrow(length=unit(dz_arrow, "mm"))) +
            
            annotate('text',
                     x=(shift[1]+mean(X, na.rm=TRUE)+dx_text),
                     y=shift[2]+0,
                     label=labelArrow,
                     angle=0,
                     hjust=0.5, vjust=0,
                     size=3, color=IPCCgrey50)
    }
    
    if (shape == "rect") {
        plot = plot +
            annotate(
                "rect",
                xmin=(shift[1]+X),
                xmax=(shift[1]+X+Size),
                ymin=(shift[2]+rep(dy_arrow+dy_shape, nSize)),
                ymax=(shift[2]+dy_arrow+dy_shape+Size),
                fill=color)
    }

    plot = plot +
        scale_x_continuous(limits=c(0, width),
                           expand=c(0, 0)) + 
        scale_y_continuous(limits=c(0, height),
                           expand=c(0, 0))
    

    return (plot)
}
