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
IPCCgrey23 = "#3b3b3c"
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
IPCCfreshblue = "#BBD6ED"

INRAEcyan = "#00a3a6"
INRAElightercyan = "#b2e0df"
INRAElightcyan = "#66c1bf"
INRAEmediumcyan = "#008c8e"
INRAEdarkcyan = "#275662"
INRAElightblue = "#9ed6e3"

EXPLORE2blue = "#007A92"
EXPLORE2orange = "#EE7402"

# EXPLORE2pmc = "#008837"
# EXPLORE2pc = "#a6dba0"
# EXPLORE2pn = "#f7f7f7"
# EXPLORE2np = "#c2a5cf"
# EXPLORE2nng = "#7b3294"

EXPLORE2pmc = '#075e9b'
EXPLORE2pc = '#43a2ca'
EXPLORE2pn = '#7bccc4'
EXPLORE2np = '#bae4bc'
EXPLORE2nng = '#f0f9e8'



refCOL =
    # INRAEcyan
    EXPLORE2blue


get_IPCC_Palette = function (palette_name, colorStep=NA, reverse=FALSE) {
    if (palette_name == "SSP") {
        Palette =
            c("#971A1F",
              "#E11D25",
              "#F68815",
              "#203563",
              "#45AED4")
    }

    if (palette_name == "ground_8") {
        Palette =
            c("#7F4A23",
              "#B3762A",
              "#D4B86A",
              "#EFE0B0",
              "#BCE6DB",
              "#7ACEB9",
              "#449C93",
              "#2A6863")
    }

    if (palette_name == "MAKAHO_ground") {
        Palette =
            c("#543005",
              "#8C510A",
              "#BF812D",
              "#DFC27D",
              "#F6E8C3",
              "#C7EAE5",
              "#80CDC1",
              "#35978F",
              "#01665E",
              "#003C30")
    }

    if (palette_name == "ground_10") {
        Palette =
            c("#452C1A",
              "#7F4A23",
              "#B3762A",
              "#D4B86A",
              "#EFE0B0",
              "#BCE6DB",
              "#7ACEB9",
              "#449C93",
              "#2A6863",
              "#193830")
    }

    if (palette_name == "ground_14") {
        Palette =
            c("#452C1A",
              "#704220",
              "#9A6327",
              "#BC8A3C",
              "#D5BA6D",
              "#EADBA4",
              "#F0ECD6",
              "#DCF1E9",
              "#B1E2D5",
              "#7CCFBB",
              "#51AFA0",       
              "#36847F",
              "#255A54",
              "#193830")
    }

    if (palette_name == "OrangePurple") {
        Palette =
            c("#CD5629",
              "#E8B962",
              "#F8E6DF",
              "#EDE1ED",
              "#E58DBE",
              "#893687")
    }

    if (palette_name == "rainbow_6") {
        Palette =  
            c("#D73027",
              "#FC8D59",
              "#FEE090",
              "#E0F3F8",
              "#91BFDB",
              "#4575B4")
    }
        
    if (palette_name == "rainbow_8") {
        Palette =  
            c("#96221b",
              "#D73027",
              "#FC8D59",
              "#FEE090",
              "#E0F3F8",
              "#91BFDB",
              "#4575B4",
              "#30527e")
    }

    if (palette_name == "red_ramp") {
        Palette =  
            c("#59141C",
              "#F9DDD5")
    }
    
    if (!is.na(colorStep)) {
        Palette = colorRampPalette(Palette)(colorStep)
    }
    
    if (reverse) {
        Palette = rev(Palette)
    }
    
    return (Palette)
}



## 1. PERSONALISATION ________________________________________________
### 1.1. Personal theme ______________________________________________
#' @title Ggplot2 theme ash
#' @export
theme_IPCC = function (isBack=TRUE, isGrid=TRUE, isTitle=FALSE,
                       dTitle=0, isLabelX=FALSE, isLabelY=FALSE) {

    if (isBack) {
        panel.background=element_rect(fill=IPCCgrey97)
    } else {
        panel.background=element_blank()
    }

    if (isGrid) {
        panel.grid.major.y=element_line(color=IPCCgrey85,
                                        size=0.25)
    } else {
        panel.grid.major.y=element_blank()
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
            panel.grid.major.y=panel.grid.major.y,
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


## 2. COLOR MANAGEMENT _______________________________________________
### 2.1. Compute colors ______________________________________________
#' @title Compute color bin
#' @export
compute_colorBin = function (min, max, colorStep, center=NULL,
                             include=FALSE) {

    if (!is.null(center)) {
        maxAbs = max(abs(max), abs(min))
        minValue = -maxAbs + center
        maxValue = maxAbs + center
    } else {
        minValue = min
        maxValue = max
    }
    
    if (all(include)) {
        nBin = colorStep + 1
    } else if (all(!include)) {
        nBin = colorStep - 1
    } else {
        nBin = colorStep
    }
    
    bin = seq(minValue, maxValue, length.out=nBin)

    if (length(include) == 1) {
        if (!include) {
            upBin = c(bin, Inf)
            lowBin = c(-Inf, bin)
            bin = c(-Inf, bin, Inf)
        } else {
            upBin = bin[2:length(bin)]
            lowBin = bin[1:(length(bin)-1)]
        }

    } else if (length(include) == 2) {
        if (!include[1] & !include[2]) {
            upBin = c(bin, Inf)
            lowBin = c(-Inf, bin)
            bin = c(-Inf, bin, Inf)
            
        } else if (include[1] & !include[2]) {
            upBin = c(bin[2:length(bin)], Inf)
            lowBin = bin
            bin = c(bin, Inf)

        } else if (!include[1] & include[2]) {
            upBin = bin
            lowBin = c(-Inf, bin[1:(length(bin)-1)])
            bin = c(-Inf, bin)
            
        } else if (include[1] & include[2]) {
            upBin = bin[2:length(bin)]
            lowBin = bin[1:(length(bin)-1)]
        }
    }

    midBin = zoo::rollmean(bin, 2)

    res = list(bin=bin, upBin=upBin,
               midBin=midBin, lowBin=lowBin)
    return (res)
}


# get_id = function (value, upBin, lowBin, include_min, include_max) {
#     if ()
# }

get_color = function (value, upBin, lowBin, Palette,
                      include_min=FALSE,
                      include_max=TRUE,
                      return_id=FALSE) {


    if (length(include_min) != 1 | length(include_max) != 1) {
        if (length(include_min) != length(Palette)) {
            include_min = rep(include_min, length.out=length(Palette))
        }
        if (length(include_max) != length(Palette)) {
            include_max = rep(include_max, length.out=length(Palette))
        }

        id = mapply(get_color, include_min, include_max,
                    MoreArgs=list(value=value,
                                  upBin=upBin, lowBin=lowBin,
                                  Palette=Palette,
                                  return_id=TRUE))
        id = id[!is.na(id)]
        id = id[1]
        
    } else {
        if (!include_min & include_max) {
            id = which(lowBin < value & value <= upBin)
        } else if (include_min & !include_max) {
            id = which(lowBin <= value & value < upBin)
        } else if (!include_min & !include_max) {
            id = which(lowBin < value & value < upBin)
        } else if (include_min & include_max) {
            id = which(lowBin < value & value <= upBin)
        }
    }

    if (return_id) {
        if (length(id) == 0) {
            id = NA
        }
        return (id)
        
    } else {
        if (length(id) == 0) {
            color = NA
        } else {
            color = Palette[id]
        }
        return (color)
    }
}

get_colors = function (Value, upBin, lowBin, Palette,
                       include_min=FALSE,
                       include_max=TRUE) {
    colors = unlist(sapply(Value, get_color,
                           upBin=upBin,
                           lowBin=lowBin,
                           Palette=Palette,
                           include_min=include_min,
                           include_max=include_max))
    return (colors)
} 


#RdBu

#' @title Get palette
#' @export
get_palette = function (palette_name="BrBG", colorStep=10,
                        reverse=FALSE) {

    if (length(palette_name) > 1) {
        Palette = palette_name
    } else {
        Palette = RColorBrewer::brewer.pal(n=colorStep,
                                           name=palette_name)
    }
    Palette = colorRampPalette(Palette)(colorStep)
    if (reverse) {
        Palette = rev(Palette)
    }
    return (Palette)
}



# colorStep = palette_n_shade*2+1
# Palette = get_palette(palette_name, colorStep, palette_reverse)
# res = compute_colorBin(minValue, maxValue,
#                        colorStep,
#                        center=palette_is_center,
#                        include=include)
# stripe_color = unlist(sapply(Value, get_color,
#                              upBin=res$upBin,
#                              lowBin=res$lowBin,
#                              Palette=Palette))





##########

# #' @title Compute color
# #' @export
# compute_color = function (value, min, max, Palette,
#                           colorStep=256, include=FALSE,
#                           reverse=FALSE) {

#     if (is.na(value)) {
#         return (NA)
#     }
    
#     res = compute_colorBin(min=min, max=max, Palette=Palette,
#                            colorStep=colorStep, include=include,
#                            reverse=reverse)
#     upBin = res$upBin
#     lowBin = res$lowBin
#     PaletteColors = res$Palette

#     id = which(value <= upBin & value > lowBin)
#     color = PaletteColors[id]
#     return(color)
# }
# compute_color(-51, -50, 40, Palette, colorStep=10)

### 2.2. Get colors __________________________________________________
# #' @title Get color
# #' @export
# get_color = function (value, min, max, Palette, colorStep=256,
#                       reverse=FALSE, include=FALSE,
#                       noneColor='black') {
    
#     color = sapply(value, compute_color,
#                    min=min,
#                    max=max,
#                    Palette=Palette,
#                    colorStep=colorStep,
#                    include=include,
#                    reverse=reverse)
    
#     color[is.na(color)] = noneColor    
#     return(color)
# }




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
    reverse = FALSE
    if (grepl('(^fin)|([{]fin)', var) |
        grepl('(^dt)|([{]dt)', var) |
        grepl('(^t)|([{]t)', var) |
        grepl('(^v)|([{]v)', var)) {
        reverse = TRUE
    }
    return (reverse)
}

# ### 2.3. Palette tester ______________________________________________
# # Allows to display the current personal palette
# #' @title Palette tester
# #' @export
# palette_tester = function (Palette, colorStep=256) {

#     outdir = 'palette'
#     if (!(file.exists(outdir))) {
#         dir.create(outdir)
#     }

#     # An arbitrary x vector
#     X = 1:colorStep
#     # All the same arbitrary y position to create a colorbar
#     Y = rep(0, times=colorStep)

#     # Recreates a continuous color palette
#     Palette = colorRampPalette(Palette)(colorStep)

#     # Open a void plot
#     p = ggplot() + theme_void()

#     for (x in X) {
#         # Plot the palette
#         p = p +
#             annotate("segment",
#                      x=x, xend=x,
#                      y=0, yend=1,
#                      color=Palette[x], size=1)
#     }

#     p = p +
#         scale_x_continuous(limits=c(0, colorStep),
#                            expand=c(0, 0)) +
        
#         scale_y_continuous(limits=c(0, 1),
#                            expand=c(0, 0))

#     # Saves the plot
#     outname = deparse(substitute(Palette))
    
#     ggsave(plot=p,
#            path=outdir,
#            filename=paste(outname, '.pdf', sep=''),
#            width=10, height=10, units='cm', dpi=100)

#     ggsave(plot=p,
#            path=outdir,
#            filename=paste(outname, '.png', sep=''),
#            width=10, height=10, units='cm', dpi=300)
# }


# #' @title Get palette
# #' @export
# get_palette = function (Palette, colorStep=256) {
    
#     # Gets the number of discrete colors in the palette
#     nSample = length(Palette)
#     # Recreates a continuous color palette
#     Palette = colorRampPalette(Palette)(colorStep)

#     return (Palette)
# }


