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


### 4.2. Foot note panel______________________________________________
#' @title Foot panel
#' @export
panel_foot = function (name, n_page, foot_height, logo_path) {
    
    nLogo = length(logo_path)
    nbg = nLogo + 3
    P = vector(mode='list', length=nbg)

    # gtext_name = richtext_grob(name,
    #                            x=0, y=0.5,
    #                            margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
    #                            hjust=0, vjust=0.5,
    #                            gp=gpar(col="#00A3A8", fontsize=8))
    
    # P[[1]] = gtext_name
    P[[1]] = void()
    LM_row = c(1)
    widths = c(1)

    for (i in 1:nLogo) {
        path = logo_path[i]
        logo = names(logo_path)[i]
        img = readPNG(path)
        
        if (logo == 'PR') {
            grob = grid::rasterGrob(img,
                              x=0,
                              hjust=0,
                              width=unit(0.8*foot_height, "cm"))
            width = 0.2
        } else if (logo == 'FR') { 
            grob = grid::rasterGrob(img,
                              x=0,
                              hjust=0,
                              width=unit(1*foot_height, "cm"))
            width = 0.2
        } else if (logo == 'INRAE') {
            grob = grid::rasterGrob(img,
                              y=0.565,
                              vjust=0.5,
                              width=unit(1.08*foot_height, "cm"))
            width = 0.25
        } else if (logo == 'AEAG') {
            grob = grid::rasterGrob(img,
                              y=0.49,
                              vjust=0.5,
                              width=unit(0.7*foot_height, "cm"))
            width = 0.2
        } else if (logo == 'Explore2') {
            grob = grid::rasterGrob(img,
                              x=0,
                              vjust=0.7,
                              hjust=0.5,
                              width=unit(1.15*foot_height, "cm"))
            width = 0
        }
        P[[i+1]] = grob
        LM_row = c(LM_row, i+1)
        widths = c(widths, width)
    }

    # text_page = paste0("<b>p. ", n_page, "</b>")
    text_page = paste0(name,
                       "<span style='color:white'>&#95;</span>",
                       "<b>p. ",
                       n_page, "</b>")

    
    text_date = format(Sys.Date(),
                       "%B<span style='color:white'>&#95;</span>%Y")

    # Converts all texts to graphical object in the right position
    gtext_page = richtext_grob(text_page,
                               x=1, y=0,
                               margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                               hjust=1, vjust=0.5,
                               gp=gpar(col="#00A3A8", fontsize=8))

    gtext_date = richtext_grob(text_date,
                               x=1, y=0.55,
                               margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                               hjust=1, vjust=0.5,
                               gp=gpar(col="#00A3A8", fontsize=6))

    P[[nLogo+2]] = gtext_page
    LM_row1 = c(LM_row, nLogo+2)
    P[[nLogo+3]] = gtext_date
    LM_row2 = c(LM_row, nLogo+3)
    widths = c(widths, 1)
    
    # Creates the matrix layout
    LM = matrix(c(LM_row1,
                  LM_row2),
                nrow=2,
                byrow=TRUE)
    
    # Arranges all the graphical objetcs
    plot = grid.arrange(grobs=P,
                        layout_matrix=LM,
                        widths=widths)
    
    # Return the plot object
    return (plot)
} 
