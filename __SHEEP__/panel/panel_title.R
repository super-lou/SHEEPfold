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


#' @title Info panel
#' @export
panel_title = function(title=NULL,
                       margin=margin(t=0, r=0, b=0, l=0, "cm")) {
    text = paste0("<b>", title, '</b>')
    plot = richtext_grob(text,
                         x=0, y=1,
                         margin=margin,
                         hjust=0, vjust=1,
                         gp=gpar(col=refCOL, fontsize=14))
    
    return (plot)
}  
