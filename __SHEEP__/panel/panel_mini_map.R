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


## 1. MAP PANEL ______________________________________________________
# Generates a map plot of the tendancy of a hydrological variable
#' @title Mini map panel
#' @export
panel_mini_map = function (meta, Shapefiles,
                           codeLight=NULL,
                           regionLight=NULL,
                           regimeCodeLight=NULL,
                           verbose=FALSE) {
    
    # Extract shapefiles
    france = Shapefiles$france
    basinHydro = Shapefiles$basinHydro
    regionHydro = Shapefiles$regionHydro
    entiteHydro = Shapefiles$entiteHydro
    river = Shapefiles$river

    # Stores the coordonate system 
    cf = coord_fixed()
    # Makes it the default one to remove useless warning
    cf$default = TRUE

    # Open a new plot with the personalise theme
    map = ggplot() + theme_void() +
        # Fixed coordinate system (remove useless warning)
        cf +
        
        theme(plot.margin=margin(t=0, r=0, b=0, l=0,
                                 unit="mm")) +
        
        # Plot the background of France
        geom_sf(data=france,
                color=NA,
                fill=IPCCgrey97)

    map = map +
        # Plot the hydrological basin
        geom_sf(data=basinHydro,
                color=IPCCgrey85,
                fill=NA,
                size=0.25)
    
    # If the river shapefile exists
    if (!is.null(river)) {
        # Plot the river
        map = map +
            geom_sf(data=river,
                    color="white",
                    alpha=1,
                    fill=NA,
                    linewidth=0.7,
                    na.rm=TRUE)
        map = map +
            geom_sf(data=river,
                    color=INRAElightcyan,
                    alpha=1,
                    fill=NA,
                    linewidth=0.35,
                    na.rm=TRUE)
    }

    # Plot the white back of boundaries
    map = map +
        geom_sf(data=france,
                color="white",
                fill=NA,
                linewidth=0.7)
    map = map +
        geom_sf(data=france,
                color=IPCCgrey40,
                fill=NA,
                linewidth=0.35)
    
    # Leaves space around the France
    xlim = c(90000, 1250000)
    ylim = c(6040000, 7120000)
    
    # Same but with less graduation and smaller size
    xmin = gpct(2, xlim, shift=TRUE)
    xint = c(0, 250*1E3)
    ymin = gpct(1, ylim, shift=TRUE)
    ymax = ymin + gpct(3, ylim)
    size = 2
    sizekm = 1.5

    map = map +
        # Adds the base line of the scale
        geom_line(aes(x=c(xmin, max(xint)+xmin),
                      y=c(ymin, ymin)),
                  color=IPCCgrey40, size=0.2) +
        # Adds the 'km' unit
        annotate("text",
                 x=max(xint)+xmin+gpct(1, xlim), y=ymin,
                 vjust=0, hjust=0, label="km",
                 color=IPCCgrey40, size=sizekm)
    # For all graduations
    for (x in xint) {
        map = map +
            # Draws the tick
            annotate("segment",
                     x=x+xmin, xend=x+xmin, y=ymin, yend=ymax,
                     color=IPCCgrey40, size=0.2) +
            # Adds the value
            annotate("text",
                     x=x+xmin, y=ymax+gpct(0.5, ylim),
                     vjust=0, hjust=0.5, label=x/1E3,
                     color=IPCCgrey40, size=size)
    }

    if (!is.null(codeLight)) {
        Code = levels(factor(meta$Code))
        L93X = meta$XL93_m[match(meta$Code, Code)]           
        L93Y = meta$YL93_m[match(meta$Code, Code)]
        
        # Creates a tibble to stores all the data to plot
        plot_map = tibble(L93X=L93X, L93Y=L93Y, Code=Code)
        
        # Extract data of all stations not to highlight
        plot_map_codeNo = plot_map[plot_map$Code != codeLight,]
        # Extract data of the station to highlight
        plot_map_code = plot_map[plot_map$Code == codeLight,]
        # Plots only the localisation
        
        map = map +
            geom_sf(data=entiteHydro[entiteHydro$Code == codeLight,],
                    color="white",
                    fill=NA,
                    linewidth=1) +
            geom_sf(data=entiteHydro[entiteHydro$Code == codeLight,],
                    color=INRAEdarkcyan,
                    fill=NA,
                    linewidth=0.3)

        map = map +
            geom_point(data=plot_map_code,
                       aes(x=L93X, y=L93Y),
                       shape=21, size=1.4, stroke=0.3,
                       color="white",
                       fill=INRAEcyan)
    }

    if (!is.null(regionLight)) {
        map = map +
            geom_sf(data=regionHydro[regionHydro$CdRegionHy == regionLight,],
                    color="white",
                    fill=NA,
                    linewidth=1) +
            geom_sf(data=regionHydro[regionHydro$CdRegionHy == regionLight,],
                    color=INRAEdarkcyan,
                    fill=NA,
                    linewidth=0.3)
    }

    if (!is.null(regimeCodeLight)) {
        matchCode = match(meta$Code, regimeCodeLight)
        matchCode = matchCode[!is.na(matchCode)]
        L93X = meta$XL93_m[matchCode]           
        L93Y = meta$YL93_m[matchCode]
        
        # Creates a tibble to stores all the data to plot
        plot_map = tibble(L93X=L93X, L93Y=L93Y, Code=regimeCodeLight)

        map = map +
            geom_point(data=plot_map,
                       aes(x=L93X, y=L93Y),
                       shape=21, size=1, stroke=0.3,
                       color="white",
                       fill=INRAEdarkcyan)
    }

    map = map +
        # Allows to crop shapefile without graphical problem
        coord_sf(xlim=xlim, ylim=ylim,
                 expand=FALSE)
    
    return (map)
}

