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
panel_foot = function (name, n_page, foot_height, logo_info) {
    
    plan = matrix(names(logo_info),
                  nrow=1,
                  byrow=TRUE)
    
    herd_logo = bring_grass(verbose=verbose)
    herd_logo = plan_of_herd(herd_logo, plan,
                             verbose=verbose)
    widths = c()

    for (i in 1:length(logo_info)) {
        logo_name = names(logo_info)[i]
        logo = logo_info[[i]]
        widths = c(widths, as.numeric(logo["width"]))
        
        grob = grid::rasterGrob(readPNG(logo["path"]),
                                vjust=1-as.numeric(logo["y"]),
                                height=unit(as.numeric(logo["height"])*foot_height, "cm"))
        
        herd_logo = add_sheep(herd_logo,
                              sheep=grob,
                              id=logo_name,
                              width=as.numeric(logo["width"]),
                              verbose=verbose)
    }

    text_page = paste0(name,
                       "<span style='color:white'>&#95;</span>",
                       "<b>p. ",
                       n_page, "</b>")
    page = richtext_grob(text_page,
                         x=1, y=0,
                         margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                         hjust=1, vjust=0.5,
                         gp=gpar(col=refCOL, fontsize=8))
    
    text_date = format(Sys.Date(),
                       "%B<span style='color:white'>&#95;</span>%Y")
    date = richtext_grob(text_date,
                         x=1, y=0.55,
                         margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                         hjust=1, vjust=0.5,
                         gp=gpar(col=refCOL, fontsize=6))
    
    plan = matrix(c("void", "logo", "page",
                    "void", "logo", "date"),
                  nrow=2, 
                  byrow=TRUE)
    
    herd = bring_grass(verbose=verbose)
    herd = plan_of_herd(herd, plan,
                        verbose=verbose)

    herd = add_sheep(herd,
                     sheep=void(),
                     id="void",
                     width=1,
                     verbose=verbose)
    herd = add_sheep(herd,
                     sheep=herd_logo,
                     id="logo",
                     width=sum(widths),
                     verbose=verbose)
    herd = add_sheep(herd,
                     sheep=page,
                     id="page",
                     width=1,
                     verbose=verbose)
    herd = add_sheep(herd,
                     sheep=date,
                     id="date",
                     width=1,
                     verbose=verbose)

    return (herd)
} 
