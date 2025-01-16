# Copyright 2022-2024 Louis Héraut (louis.heraut@inrae.fr)*1,
#                     Éric Sauquet (eric.sauquet@inrae.fr)*1,
#                     Jean-Philippe Vidal (jean-philippe.vidal@inrae.fr)*1
#
# *1   INRAE, UR RiverLy, Villeurbanne, France
#
# This file is part of SHEEPfold R toolbox.
#
# SHEEPfold R toolbox is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# SHEEPfold R toolbox is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with SHEEPfold R toolbox.
# If not, see <https://www.gnu.org/licenses/>.


sheet_regime_map = function (meta,
                             subtitle=NULL,
                             icon_path="",
                             logo_path="",
                             is_foot=TRUE,
                             # is_secteur=FALSE,
                             figdir="",
                             Pages=NULL,
                             Shapefiles=NULL,
                             verbose=FALSE) {

    paper_size = c(15, 15)
    page_margin = c(t=0.5, r=0.5, b=0.25, l=0.5)
    if (is_foot) {
        foot_height = 1.25
    } else {
        foot_height = 0
    }
    map_height = 15 - 0.5 - 0.25 - foot_height
    
    if (is_foot) {
        plan = matrix(c("title", "map", "foot",
                        "map", "map", "foot"),
                      ncol=2)
    } else {
        plan = matrix(c("title", "map",
                        "map", "map"),
                      ncol=2)
    }

    Code = levels(factor(meta$code))
    nCode = length(Code)
        
    if (verbose) {
        print("regime map")
    }

    herd = bring_grass(verbose=verbose)
    herd = plan_of_herd(herd, plan, verbose=verbose)
            
    title = ggplot() + theme_void_Lato() +
        theme(plot.margin=margin(t=0, r=0, b=0, l=0, "cm"))
    
    if (is_foot) {
        y1 = 0.98
        y2 = 0.79
        
    } else {
        y1 = 0.98
        y2 = 0.8
    }

    title = title +
        annotate("text",
                 x=0,
                 y=y1,
                 label=TeX(paste0("\\textbf{Régime}")),
                 size=6, hjust=0, vjust=1,
                 color=refCOL)

    title = title +
        annotate("text",
                 x=0,
                 y=y2,
                 label=TeX(paste0("\\textbf{hydrologique}")),
                 size=6, hjust=0, vjust=0,
                 color=refCOL)

    title = title +
        scale_x_continuous(limits=c(0, 1),
                           expand=c(0, 0)) +
        scale_y_continuous(limits=c(0, 1),
                           expand=c(0, 0))
    
    herd = add_sheep(herd,
                     sheep=title,
                     id="title",
                     verbose=verbose)

    map = panel_regime_map(meta,
                           Shapefiles=Shapefiles,
                           margin(t=0, r=0, b=0, l=0, "cm"),
                           verbose=verbose)
    
    herd = add_sheep(herd,
                     sheep=map,
                     id="map",
                     height=map_height,
                     verbose=verbose)
    
    footName = "Carte des régimes hydrologiques observés"
    if (is.null(Pages)) {
        n_page = i
    } else {
        if (nrow(Pages) == 0) {
            n_page = 1
        } else {
            n_page = Pages$n[nrow(Pages)] + 1
        }
        Pages = bind_rows(
            Pages,
            tibble(section=footName,
                   subsection=NULL,
                   n=n_page))
    }

    if (is_foot) {
        foot = panel_foot(footName, n_page,
                          foot_height, logo_path)
        herd = add_sheep(herd,
                         sheep=foot,
                         id="foot",
                         height=foot_height,
                         verbose=verbose)
    }

    res = return_to_sheepfold(herd,
                              page_margin=page_margin,
                              paper_size=paper_size,
                              hjust=0, vjust=1,
                              verbose=verbose)
            
    plot = res$plot
    paper_size = res$paper_size

    # if (is_secteur) {
        # filename = paste0("regime_secteur.pdf")
    # } else {
        filename = paste0("regime.pdf")
    # }
    
    if (!(file.exists(figdir))) {
        dir.create(figdir, recursive=TRUE)
    }
    ggplot2::ggsave(plot=plot,
                    path=figdir,
                    filename=filename,
                    width=paper_size[1],
                    height=paper_size[2], units='cm',
                    dpi=300,
                    device=cairo_pdf)
    return (Pages)
}
