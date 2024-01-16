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


sheet_stationnarity_map = function (trendEX,
                                    metaEX_serie,
                                    meta,
                                    prob=0.1,
                                    suffix_names=NULL,
                                    icon_path="",
                                    logo_path="",
                                    is_foot=TRUE,
                                    is_secteur=FALSE,
                                    zoom=NULL,
                                    figdir="",
                                    Pages=NULL,
                                    Shapefiles=NULL,
                                    verbose=FALSE) {

    paper_size = c(15, 15)

    page_margin = c(t=0.5, r=0.5, b=0.5, l=0.5)
    
    if (is_foot) {
        foot_height = 1.25
    } else {
        foot_height = 0
    }

    title_height = 2
    map_height =
        paper_size[1] - page_margin["t"] - page_margin["b"] - title_height - foot_height
    fill_width = 2
    map_width =
        paper_size[2] - page_margin["l"] - page_margin["r"] - fill_width
    
    if (is_foot) {
        plan = matrix(c("title", "map",
                        "map", "map",
                        "foot", "foot"),
                      nrow=3, byrow=TRUE)
    } else {
        plan = matrix(c("title", "title",
                        "map", "fill",
                        "map", "shape"),
                      nrow=3, byrow=TRUE)
    }

    Code = levels(factor(data$Code))
    nCode = length(Code)

    Variable = metaEX_serie$variable
    VariableTeX = convert2TeX(Variable)
    nVariable = length(Variable)

    Unit = metaEX_serie$unit
    UnitTeX = convert2TeX(Unit, size="small", bold=FALSE)
    PX = get_alphabet_in_px()
    
    for (i in 1:nVariable) {
        variable = Variable[i]

        trendEX_variable = trendEX[grepl(variable, trendEX$variable),]
        metaEX_variable = metaEX_serie[metaEX_serie$variable == variable,]

        prob = 0.1
        Palette = unlist(strsplit(metaEX_variable$palette[metaEX_variable$variable == variable], " "))
        min_variable = quantile(trendEX_variable$trend,
                           prob, na.rm=TRUE)
        max_variable = quantile(trendEX_variable$trend,
                           1-prob, na.rm=TRUE)
        
        if (is.null(suffix_names)) {
            nSuffix = 1
        } else {
            nSuffix = length(suffix_names)
        }
        
        for (j in 1:nSuffix) {
            suffix = names(suffix_names)[j]
            suffix_name = suffix_names[j]

            if (is.null(suffix_names)) {
                variable_suffix = variable
            } else {
                variable_suffix = paste0(variable, "_", suffix) 
            }
            
            herd = bring_grass(verbose=verbose)
            herd = plan_of_herd(herd, plan, verbose=verbose)
            
            title = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0, b=0, l=0, "cm"))

            if (is_foot) {
                # y1 = 0.98
                # y3 = 0.885
            } else {
                y1 = 0.98
                y3 = 0.66
            }
            y4 = y3-0.25
            newline = 0.12

            x_off = 0
            
            title = title +
                annotate("text",
                         x=x_off,
                         y=y1,
                         label=TeX(paste0("\\textbf{", VariableTeX[i], "}")),
                         size=7, hjust=0, vjust=1,
                         color=INRAEcyan)

            if (is.null(suffix_names)) {
                label = TeX(paste0("Tendances en %\\.an$^{-1}$"))
            } else {
                label = TeX(paste0("Tendances sur les \\textbf{", suffix_name, "} en %\\.an$^{-1}$"))
            }
            title = title +
                annotate("text",
                         x=x_off,
                         y=y3,
                         label=label,
                         size=4, hjust=0, vjust=1,
                         color=INRAEcyan)

            
            glose = metaEX_variable$glose
            glose = guess_newline(glose, px=50, PX=PX)
            glose = unlist(strsplit(glose, "\n"))
            
            for (k in 1:length(glose)) {
                title = title +
                    annotate("text",
                             x=x_off+0.005,
                             y=y4-(k-1)*newline,
                             label=glose[k],
                             size=2.5, hjust=0, vjust=1,
                             color=INRAEcyan)
            }
            
            title = title +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            herd = add_sheep(herd,
                             sheep=title,
                             id="title",
                             height=title_height,
                             verbose=verbose)

            trendEX_variable_suffix = trendEX[trendEX$variable == variable_suffix,]
            
            map = panel_stationnarity_map(trendEX_variable_suffix,
                                          metaEX_variable,
                                          meta,
                                          min_variable=min_variable,
                                          max_variable=max_variable,
                                          prob=prob,
                                          is_secteur=is_secteur,
                                          zoom=zoom,
                                          x_echelle_pct=10,
                                          y_echelle_pct=1,
                                          echelle=c(0, 20, 50, 100), 
                                          Shapefiles=Shapefiles,
                                          margin_map=margin(t=-9, r=0,
                                                            b=0, l=0, "mm"),
                                          # margin_shape=margin(t=0, r=1.5,
                                          # b=0, l=0.5, "cm"),
                                          # margin_fill=margin(t=1.4, r=0.3,
                                          # b=4.6, l=9.2, "cm"),
                                          verbose=verbose)
            
            herd = add_sheep(herd,
                             sheep=map,
                             id="map",
                             height=map_height,
                             width=map_width,
                             verbose=verbose)


            # border = panel_colorbar_circle(c(0, 1),
            #                                c("transparent",
            #                                  "transparent"),
            #                                size_circle=2.2,
            #                                d_line=0.1,
            #                                linewidth=0.35,
            #                                d_space=0,
            #                                d_text=0.5,
            #                                text_size=2.8,
            #                                stroke=c(1, 1),
            #                                color=c(IPCCgrey40,
            #                                        IPCCgold),
            #                                label=c("débits observés",
            #                                        "débits naturalisés"),
            #                                shape=c(21, 21),
            #                                on_circle=TRUE,
            #                                margin=margin(t=0.2, r=0,
            #                                              b=0.5, l=-5, "cm"))
            # herd = add_sheep(herd,
            #                  sheep=border,
            #                  id="border",
            #                  verbose=verbose)


            res = compute_colorBin(min_variable,
                                   max_variable,
                                   colorStep=length(Palette),
                                   center=0,
                                   include=FALSE)
            bin = res$bin
            
            fill = panel_colorbar_circle(bin*100,
                                         Palette,
                                         size_circle=3.3,
                                         d_line=0.2,
                                         linewidth=0.35,
                                         d_space=0.15,
                                         d_text=0.5,
                                         text_size=3,
                                         label=NULL,
                                         ncharLim=4,
                                         colorText=IPCCgrey50,
                                         colorLine=IPCCgrey50,
                                         on_circle=FALSE,
                                         margin=margin(t=-0.5, r=0,
                                                       b=-0.7, l=0.5, "cm"))
            herd = add_sheep(herd,
                             sheep=fill,
                             id="fill",
                             width=fill_width,
                             verbose=verbose)


            shape = panel_colorbar_circle(c(0, 0.5, 1),
                                          c("transparent",
                                            "transparent",
                                            "transparent"),
                                          size_circle=2.2,
                                          d_line=0.1,
                                          linewidth=0.35,
                                          d_space=0,
                                          d_text=0.5,
                                          text_size=2.6,
                                          stroke=c(0.5, 0.5, 0.5),
                                          color=c(IPCCgrey40,
                                                  IPCCgrey40,
                                                  IPCCgrey40),
                                          label=c("Baisse significative à 10 %",
                                                  "Non significatif à 10 %",
                                                  "Hausse significative à 10 %"),
                                          shape=c(25, 21, 24),
                                          on_circle=TRUE,
                                          margin=margin(t=2.5, r=0,
                                                        b=2, l=-2.2, "cm"))
            herd = add_sheep(herd,
                             sheep=shape,
                             id="shape",
                             verbose=verbose)

            

            footName = "Carte de stationnarité"
            if (is.null(Pages)) {
                n_page = i
            } else {
                if (nrow(Pages) == 0) {
                    n_page = 1
                } else {
                    n_page = Pages$n[nrow(Pages)] + 1
                }
                if (is.null(HMSelection)) {
                    subsection = hm
                } else {
                    subsection = variable
                }
                Pages = bind_rows(
                    Pages,
                    tibble(section=footName,
                           subsection=subsection,
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

            if (is_secteur) {
                filename = paste0("map_stationnarity_", variable_suffix, "_secteur.pdf")
            } else {
                filename = paste0("map_stationnarity_", variable_suffix, ".pdf")
            }
            
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
        }
    }
    return (Pages)
}
