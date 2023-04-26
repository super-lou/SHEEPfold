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

IPCCgrey99 = "#f8f9f9" # lighter lighter plot background
IPCCgrey97b = "#f9f8f7" # lighter hot plot background
IPCCgrey97 = "#f6f7f7" # lighter blue plot background
IPCCgrey95 = "#f4f2f1" # plot background
IPCCgrey92 = "#e9eceb" # 
IPCCgrey90 = "#e3e2e0" # minor tick
IPCCgrey85 = "#dcdad9" # grid on grey background low important axis 
IPCCgrey80 = "#cfd1d0" # grid on white background
IPCCgrey75 = "#bebdbb" # major tick
IPCCgrey67 = "#adabaa" # minor line
IPCCgrey60 = "#9c9c9b" # important axis
IPCCgrey50 = "#81848b" # low important annotation
IPCCgrey48 = "#847b73" # major line
IPCCgrey40 = "#656769" # low important label
IPCCgrey25 = "#454547"
IPCCgrey20 = "#060403" # important title, label or annotation
IPCCgrey18 = "#2f2f32" # low important title
IPCCgrey13 = "#231f20" # font
IPCCgrey05 = "#100f0d" # realy important title
IPCCcyan = "#449c93"
IPCCligthcyan = "#90d6c6"
IPCCwhitecyan = "#a8ded3"
IPCCbrique = "#794822"
IPCCgold = "#e6d495"
IPCCblue = "#1e2f59"

INRAEcyan = "#00a3a6"
INRAElightcyan = "#66c1bf"
INRAEmediumcyan = "#008c8e"
INRAEdarkcyan = "#275662"
INRAElightblue = "#9ed6e3"



## 1. PERSONALISATION ________________________________________________
### 1.1. Personal theme ______________________________________________
#' @title Ggplot2 theme ash
#' @export
theme_IPCC = function (isBack=TRUE, isTitle=FALSE, dTitle=0,
                       isLabelX=FALSE, isLabelY=FALSE) {

    if (isBack) {
        panel.background=element_rect(fill=IPCCgrey97)
    } else {
        panel.background=element_blank()
    }
    
    if (isTitle) {
        plot.title=element_text(size=9,
                                vjust=0, hjust=dTitle,
                                color=IPCCgrey25)
    } else {
        plot.title=element_blank()
    }

    if (isLabelX) {
        axis.title.x = element_text(size=7.5,
                                    vjust=1, hjust=0.5,
                                    color=IPCCgrey40)
    } else {
        axis.title.x = element_blank()
    }
    
    if (isLabelY) {
        axis.title.y=element_text(size=9,
                                  vjust=1.2, hjust=0.5,
                                  color=IPCCgrey25)
    } else {
        axis.title.y=element_blank()
    }

    library(ggh4x)
    theme =
        theme(
            # White background
            panel.background=panel.background,
            # Font
            # text=element_text(family='sans'),
            text=element_text(family="Helvetica"),
            # Border of plot
            panel.border=element_rect(color=IPCCgrey85,
                                      fill=NA,
                                      size=0.7),
            # Grid
            panel.grid.major.x=element_blank(),
            # panel.grid.major.y=element_blank(),
            panel.grid.major.y=element_line(color=IPCCgrey85,
                                            size=0.25),
            panel.grid.minor.x=element_blank(),
            panel.grid.minor.y=element_blank(),
            # Ticks marker
            axis.ticks.x=element_line(color=IPCCgrey75, size=0.3),
            axis.ticks.y=element_line(color=IPCCgrey75, size=0.3),
            # Ticks label
            axis.text.x=element_text(color=IPCCgrey40),
            axis.text.y=element_text(color=IPCCgrey40),
            # Ticks length
            axis.ticks.length=unit(1.5, 'mm'),
            # Ticks minor
            ggh4x.axis.ticks.length.minor=rel(0.5),
            # Title
            plot.title=plot.title,
            # Axis title
            axis.title.x=axis.title.x,
            axis.title.y=axis.title.y,
            # Axis line
            axis.line.x=element_blank(),
            axis.line.y=element_blank()
            )
    return (theme)
}

theme_WIP = function () {
    theme(panel.background=element_rect(fill=IPCCgrey97),
          axis.ticks.x=element_line(color=IPCCgrey75, size=0.3),
          axis.ticks.y=element_line(color=IPCCgrey75, size=0.3),
          # Ticks label
          axis.text.x=element_text(color=IPCCgrey75),
          axis.text.y=element_text(color=IPCCgrey75),
          # Ticks length
          axis.ticks.length=unit(1.5, 'mm'),
          # Ticks minor
          ggh4x.axis.ticks.length.minor=rel(0.5),
          # Title
          plot.title=element_blank(),
          # Axis title
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          # Axis line
          axis.line.x=element_line(color=IPCCgrey75, size=0.3),
          axis.line.y=element_line(color=IPCCgrey75, size=0.3))
}
    
### 1.2. Color palette _______________________________________________
#' @title Palette ground
#' @export
Palette_ground = function () {
    palette = c("#543005",
                "#8c510a",
                "#bf812d",
                "#dfc27d",
                "#f6e8c3",
                "#c7eae5",
                "#80cdc1",
                "#35978f",
                "#01665e",
                "#003c30")
    return (palette)
}

#' @title Palette rainbow
#' @export
Palette_rainbow = function () {
    palette = c('#d73027',
                '#fc8d59',
                '#fee090',
                '#e0f3f8',
                '#91bfdb',
                '#4575b4')
    return (palette)
}


## 2. COLOR MANAGEMENT _______________________________________________
### 2.1. Compute colors ______________________________________________
#' @title Compute color bin
#' @export
compute_colorBin = function (min, max, Palette, colorStep=256,
                             include=FALSE, reverse=FALSE) {

    # Gets the number of discrete colors in the palette
    nSample = length(Palette)

    if (reverse) {
        Palette = rev(Palette)
    }
    # Recreates a continuous color palette
    PaletteColors = colorRampPalette(Palette)(colorStep)

    # Computes the absolute max
    maxAbs = max(abs(max), abs(min))

    if (include) {
        nBin = colorStep + 1
    } else {
        nBin = colorStep - 1
    } 
    
    bin = seq(-maxAbs, maxAbs, length.out=nBin)
    if (!include) {
        upBin = c(bin, Inf)
        lowBin = c(-Inf, bin)
        bin = c(-Inf, bin, Inf)
        
    } else {
        upBin = bin[2:length(bin)]
        lowBin = bin[1:(length(bin)-1)]
    }

    midBin = zoo::rollmean(bin, 2)

    res = list(Palette=PaletteColors, bin=bin,
               upBin=upBin, midBin=midBin,
               lowBin=lowBin)
    return (res)
}

#' @title Compute color
#' @export
compute_color = function (value, min, max, Palette,
                          colorStep=256, include=FALSE,
                          reverse=FALSE) {

    if (is.na(value)) {
        return (NA)
    }
    
    res = compute_colorBin(min=min, max=max, Palette=Palette,
                           colorStep=colorStep, include=include,
                           reverse=reverse)
    upBin = res$upBin
    lowBin = res$lowBin
    PaletteColors = res$Palette

    id = which(value <= upBin & value > lowBin)
    color = PaletteColors[id]
    return(color)
}
# compute_color(-51, -50, 40, Palette, colorStep=10)

### 2.2. Get colors __________________________________________________
#' @title Get color
#' @export
get_color = function (value, min, max, Palette, colorStep=256,
                      reverse=FALSE, include=FALSE,
                      noneColor='black') {
    
    color = sapply(value, compute_color,
                   min=min,
                   max=max,
                   Palette=Palette,
                   colorStep=colorStep,
                   include=include,
                   reverse=reverse)
    
    color[is.na(color)] = noneColor    
    return(color)
}

#' @title Color event
#' @export
get_colorEvent = function () {
    colorEvent = c("#423089", "#9ed6e3", "#9dc544", "#ed6e6c")
    names(colorEvent) = c("Crue", "Crue Nivale", "Moyennes Eaux", "Étiage")
    return(colorEvent)
}

#' @title Text color event
#' @export
get_colorTextEvent = function () {
    colorTextEvent = c("#9687d5", "#d8eff4", "#cee2a2", "#f6b6b5")
    names(colorTextEvent) = c("Crue", "Crue Nivale", "Moyennes Eaux", "Étiage")
    return(colorTextEvent)
}

#' @title Switch color label
#' @export
switch_colorLabel = function (color) {
    #switch 12% https://mdigi.tools/darken-color/#f6e8c3
    if (color == "#F6E8C3") {
        newColor = "#efd695"
        
    } else if (color == "#C7EAE5") {
        newColor = "#a1dcd3"
        
    } else {
        newColor = color
    }
    return (newColor)
}

#' @title Get reverse
#' @export
get_reverse = function (var) {
    # gets the color corresponding to the mean trend
    reverse = FALSE
    if (grepl('^tFIN', var) | grepl('^dt[_]', var) | grepl('^t[_]', var) | grepl('^v', var)) {
        reverse = TRUE
    }
    return (reverse)
}

### 2.3. Palette tester ______________________________________________
# Allows to display the current personal palette
#' @title Palette tester
#' @export
palette_tester = function (Palette, colorStep=256) {

    outdir = 'palette'
    if (!(file.exists(outdir))) {
        dir.create(outdir)
    }

    # An arbitrary x vector
    X = 1:colorStep
    # All the same arbitrary y position to create a colorbar
    Y = rep(0, times=colorStep)

    # Recreates a continuous color palette
    Palette = colorRampPalette(Palette)(colorStep)

    # Open a void plot
    p = ggplot() + theme_void()

    for (x in X) {
        # Plot the palette
        p = p +
            annotate("segment",
                     x=x, xend=x,
                     y=0, yend=1,
                     color=Palette[x], size=1)
    }

    p = p +
        scale_x_continuous(limits=c(0, colorStep),
                           expand=c(0, 0)) +
        
        scale_y_continuous(limits=c(0, 1),
                           expand=c(0, 0))

    # Saves the plot
    outname = deparse(substitute(Palette))
    
    ggsave(plot=p,
           path=outdir,
           filename=paste(outname, '.pdf', sep=''),
           width=10, height=10, units='cm', dpi=100)

    ggsave(plot=p,
           path=outdir,
           filename=paste(outname, '.png', sep=''),
           width=10, height=10, units='cm', dpi=300)
}


#' @title Get palette
#' @export
get_palette = function (Palette, colorStep=256) {
    
    # Gets the number of discrete colors in the palette
    nSample = length(Palette)
    # Recreates a continuous color palette
    Palette = colorRampPalette(Palette)(colorStep)

    return (Palette)
}


