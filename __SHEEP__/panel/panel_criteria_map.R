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
panel_criteria_map = function (dataEXind_model_var,
                               metaEXind,
                               meta,
                               Shapefiles=Shapefiles,
                               verbose=verbose) {

    is_numeric_logical = function (x) {
        return (is.logical(x) | is.numeric(x))
    }

    Model = levels(factor(dataEXind_model_var$Model))
    
    var = names(dataEXind_model_var)[which(sapply(dataEXind_model_var,
                                                  is_numeric_logical))[1]]
    
    unit = metaEXind$unit[metaEXind$var == var]
    is_date = metaEXind$is_date[metaEXind$var == var]
    normalize = metaEXind$normalize[metaEXind$var == var]
    reverse_palette = metaEXind$reverse_palette[metaEXind$var == var]

    
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
    map = ggplot() + theme_void() + cf +
        
        theme(plot.margin=margin(t=0, r=0, b=0, l=0,unit="mm")) +
        
        # Plot the background of France
        geom_sf(data=france,
                color=NA,
                fill=IPCCgrey99)

    map = map +
        # Plot the hydrological basin
        geom_sf(data=basinHydro,
                color=IPCCgrey85,
                fill=NA,
                size=0.2)
    
    # If the river shapefile exists
    if (!is.null(river)) {
        # Plot the river
        # map = map +
        #     geom_sf(data=river,
        #             color="white",
        #             alpha=1,
        #             fill=NA,
        #             linewidth=0.4,
        #             na.rm=TRUE)
        map = map +
            geom_sf(data=river,
                    color=INRAElightcyan,
                    alpha=1,
                    fill=NA,
                    linewidth=0.3,
                    na.rm=TRUE)
    }

    map = map +
        geom_sf(data=france,
                color=IPCCgrey50,
                fill=NA,
                linewidth=0.35)
    
    xlim = c(90000, 1250000)
    ylim = c(6040000, 7120000)
    
    xmin = gpct(2, xlim, shift=TRUE)
    xint = c(0, 50*1E3, 100*1E3, 250*1E3)
    ymin = gpct(1, ylim, shift=TRUE)
    ymax = ymin + gpct(1, ylim)
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

    print(var)

    if (!grepl("RAT", var)) {
        dataEXind_var =
            dplyr::summarise(dplyr::group_by(dataEXind_model_var, Code),
                             !!var:=median(get(var), na.rm=TRUE))
    } else {
        if (length(Model) == 1) {
            dataEXind_var = dataEXind_model_var
        } else {
            return (void())
        }
    }

    dataEXind_var =
        dplyr::left_join(dataEXind_var,
                         dplyr::select(meta,
                                       c("Code",
                                         "XL93_m",
                                         "YL93_m")),
                         by="Code")
    
    
    
    if (grepl("KGE", var)) {
        upBin = c(0, 0.25, 0.5, 0.75, 1)
        lowBin = c(-Inf, 0, 0.25, 0.5, 0.75)
        Palette = get_IPCC_Palette("rainbow")[4:8]
        nColor = 5
    }
    if (grepl("Biais", var)) {
        upBin = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, Inf)
        lowBin = c(-Inf, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3)
        Palette = rev(get_IPCC_Palette("rainbow"))
    }
    if (grepl("(^Q)|(^med[{]t)", var)) {
        upBin = c(-0.8, -0.4, -0.2, 0, 0.2, 0.4, 0.8, Inf)
        lowBin = c(-Inf, -0.8, -0.4, -0.2, 0, 0.2, 0.4, 0.8)
        Palette = rev(get_IPCC_Palette("rainbow"))
    }
    if (grepl("(^epsilon)|(^alpha)", var)) {
        upBin = c(0.5, 0.66, 0.83, 1, 1.2, 1.5, 2, Inf)
        lowBin = c(-Inf, 0.5, 0.66, 0.83, 1, 1.2, 1.5, 2, Inf)
        Palette = rev(get_IPCC_Palette("rainbow"))
    }

    
    if (grepl("RAT", var)) {
        Palette = get_IPCC_Palette("OrangePurple")
        RAT = dataEXind_var[[var]]
        RAT[is.na(RAT)] = FALSE
        fills = rep(Palette[(length(Palette)-1)], length(RAT))
        fills[RAT] = Palette[2]
        dataEXind_var$fill = fills
    } else {
        dataEXind_var$fill = get_colors(dataEXind_var[[var]],
                                        upBin=upBin,
                                        lowBin=lowBin,
                                        Palette=Palette)
    }
    
    dataEXind_var$color = "grey75"

    map = map +
        geom_point(data=dataEXind_var,
                   aes(x=XL93_m, y=YL93_m),
                   color=dataEXind_var$color,
                   fill=dataEXind_var$fill,
                   shape=21, size=3, stroke=0.4)


    map = map +
        # Allows to crop shapefile without graphical problem
        coord_sf(xlim=xlim, ylim=ylim,
                 expand=FALSE)
    
    return (map)
}

