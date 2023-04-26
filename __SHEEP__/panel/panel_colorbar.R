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


panel_colorbar = function (min, max, Palette,
                         colorStep=256, include=FALSE,
                         label=NULL, asFrac=FALSE,
                         reverse=FALSE,
                         size_color=1,
                         dx_color=0.5,
                         dy_color=0.5,
                         height=10,
                         width=10,
                         shift=c(x=0, y=0),
                         WIP=FALSE) {
    
    colorBin = compute_colorBin(min, max,
                                Palette=Palette,
                                colorStep=colorStep,
                                include=include,
                                reverse=reverse)

    Palette = colorBin$Palette
    bin = colorBin$bin
    if (is.null(label)) {
        if (asFrac) {
            label = float2frac(bin, round(colorStep/2))
        } else {
            label = round_label(bin, direction="H", ncharLim=4)
        }
    }
    
    nBin = length(bin)-1
    maxBin = max(bin, na.rm=TRUE)
    minBin = min(bin, na.rm=TRUE)
    bin = (bin - minBin) / (maxBin - minBin)
    bin = bin + seq(0, nBin*(dx_color+size_color), dx_color+size_color)
    midBin = zoo::rollmean(bin, 2)
    
    options(repr.plot.width=width, repr.plot.height=height)
    
    plot = ggplot() + theme_void() +
        coord_fixed(clip="off") + 
        theme(text=element_text(family="Helvetica"),
              plot.margin=margin(0, 0, 0, 0))

    if (WIP) {
        plot = plot + theme_WIP()
    }

    plot = plot +
        annotate("rect",
                 xmin=shift[1]+midBin-size_color/2,
                 xmax=shift[1]+midBin+size_color/2,
                 ymin=shift[2]+dy_color+size_color*2/3-size_color/2,
                 ymax=shift[2]+dy_color+size_color*2/3+size_color/2,
                 fill=Palette)
    
    for (i in 1:(nBin+1)) {
        b = bin[i]
        plot = plot +
            annotate("line",
                     x=shift[1]+c(b, b),
                     y=shift[2]+dy_color+size_color*2/3+
                         c(-size_color*2/3, size_color*2/3),
                     linewidth=0.6, color=IPCCgrey85)
    }
    
    plot = plot +
        annotate("text",
                 x=shift[1]+bin,
                 y=shift[2]+rep(0, nBin+1),
                 label=label, size=3.5,
                 hjust=0.5, vjust=0,
                 fontface="bold", color=IPCCgrey50)

    plot = plot +
        scale_x_continuous(limits=c(0, width),
                           expand=c(0, 0)) + 
        scale_y_continuous(limits=c(0, height),
                           expand=c(0, 0))
    

    return (plot)
}
