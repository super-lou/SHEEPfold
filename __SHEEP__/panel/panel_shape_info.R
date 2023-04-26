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


panel_shape_info = function (Shape="rect",
                           Size=1,
                           Color=IPCCgrey50,
                           Label="A",
                           ColorLabel=IPCCgrey50,
                           Cross=FALSE,
                           dy_label=1,
                           dx_label=1,
                           height=10,
                           width=10,
                           shift=c(x=0, y=0),
                           fontface="plain",
                           margin_add=margin(0, 0, 0, 0),
                           WIP=FALSE) {

    N = max(c(length(Shape), length(Size),
                 length(Color), length(Label), length(Cross)))
    
    if (length(Shape) != N) {
        Shape = rep(Shape[1], N)
    }
    if (length(Size) != N) {
        Size = rep(Size[1], N)
    }
    if (length(Color) != N) {
        Color = rep(Color[1], N)
    }
    if (length(ColorLabel) != N) {
        ColorLabel = rep(ColorLabel[1], N)
    }
    if (length(Label) != N) {
        Label = rep(Label[1], N)
    }
    if (length(Cross) != N) {
        Cross = rep(Cross[1], N)
    }

    Shape = rev(Shape)
    Size = rev(Size)
    Color = rev(Color)
    ColorLabel = rev(ColorLabel)
    Label = rev(Label)
    Cross = rev(Cross)

    limit = min(c(height, width))
    options(repr.plot.width=width, repr.plot.height=height)
    
    plot = ggplot() + theme_void() +
        coord_fixed(clip="off") + 
        theme(text=element_text(family="Helvetica"),
              plot.margin=margin_add)

    if (WIP) {
        plot = plot + theme_WIP()
    }

    for (i in 1:N) {
        shape = Shape[i]
        size = Size[i]
        color = Color[i]
        colorLabel = ColorLabel[i]
        label = Label[i]
        cross = Cross[i]
        
        if (shape == "rect") {
            plot = plot +
                annotate("rect",
                         xmin=shift[1]+0,
                         xmax=(shift[1]+size),
                         ymin=(shift[2]+dy_label*(i-1)),
                         ymax=(shift[2]+dy_label*(i-1)+size),
                         fill=color)
        }
        
        if (file.exists(shape)) {
            plot = plot +
                annotation_custom(
                    svgparser::read_svg(shape),
                    xmin=shift[1]+0,
                    xmax=(shift[1]+size),
                    ymin=(shift[2]+dy_label*(i-1)),
                    ymax=(shift[2]+dy_label*(i-1)+size))
        }

        if (cross) {
            plot = plot +
                annotate("point",
                         x=shift[1]+size/2,
                         y=(shift[2]+dy_label*(i-1)+size/2),
                         shape=4, size=size*2, color="white")
        }

        if (!is.null(label)) {
            plot = plot +
                annotate('text',
                         x=(shift[1]+size+dx_label),
                         y=(shift[2]+dy_label*(i-1)+size/2),
                         label=label,
                         angle=0,
                         hjust=0, vjust=0.5,
                         size=3, color=colorLabel,
                         fontface=fontface)
        }
    }

    plot = plot +
        scale_x_continuous(limits=c(0, width),
                           expand=c(0, 0)) + 
        scale_y_continuous(limits=c(0, height),
                           expand=c(0, 0))
    

    return (plot)
}
