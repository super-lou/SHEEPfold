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
                                    code_selection=NULL,
                                    show_MK=TRUE,
                                    icon_path="",
                                    logo_path="",
                                    is_foot=TRUE,
                                    foot_resume=TRUE,
                                    is_secteur=FALSE,
                                    zoom=NULL,
                                    map_limits=NULL,
                                    x_echelle_pct=62,
                                    y_echelle_pct=5,
                                    echelle=c(0, 50, 100, 250),
                                    figdir="",
                                    suffix=NULL,
                                    Pages=NULL,
                                    Shapefiles=NULL,
                                    verbose=FALSE) {

    if (!is.null(code_selection)) {
        Code = code_selection
    } else {
        Code = levels(factor(meta$code))
    }
    nCode = length(Code)
    trendEX = trendEX[trendEX$code %in% Code,]
    
    paper_size = c(15, 15)
    page_margin = c(t=0.5, r=0.5, b=0.5, l=0.5)
    if (is_foot) {
        foot_height = 1
        title_height = 2.2
    } else {
        foot_height = 0
        title_height = 2
    }

    
    map_height =
        paper_size[1] - page_margin["t"] - page_margin["b"] - title_height - foot_height

    width = paper_size[2] - page_margin["l"] - page_margin["r"]
    fill_width = 2.5
    map_width = width - fill_width
    
    if (is_foot) {
        plan = matrix(c("title", "title",
                        "map", "fill",
                        "map", "shape",
                        "foot", "foot"),
                      nrow=4, byrow=TRUE)
    } else {
        plan = matrix(c("title", "title",
                        "map", "fill",
                        "map", "shape"),
                      nrow=3, byrow=TRUE)
    }

    Variable = metaEX_serie$variable_en
    if (any(names(metaEX_serie) == "variable")) {
        Variable_to_display = metaEX_serie$variable
    } else {
        Variable_to_display = Variable
    }
    VariableTeX = convert2TeX(Variable_to_display)
    nVariable = length(Variable)

    Unit = metaEX_serie$unit_fr
    Unit = gsub("^jour$", "jour", Unit)
    Unit = gsub("^jour de l'année$", "jour", Unit)
    
    UnitTeX = convert2TeX(Unit, bold=FALSE)
    PX = get_alphabet_in_px()
    
    for (i in 1:nVariable) {
        variable = Variable[i]
        variable_to_display = Variable_to_display[i]

        trendEX_variable = trendEX[grepl(variable, trendEX$variable_en),]
        metaEX_variable = metaEX_serie[metaEX_serie$variable_en == variable,]

        to_normalise = metaEX_variable$to_normalise
        
        herd = bring_grass(verbose=verbose)
        herd = plan_of_herd(herd, plan, verbose=verbose)
        
        title = ggplot() + theme_void() +
            theme(plot.margin=margin(t=0, r=0, b=0, l=0, "cm"))

        if (is_foot) {
            y1 = 0.98
            y3 = 0.63
            y4 = y3 - 0.24
        } else {
            y1 = 0.98
            y3 = 0.63
            y4 = y3 - 0.26
        }
       
        newline = 0.12

        x_off = 0
        
        title = title +
            annotate("text",
                     x=x_off,
                     y=y1,
                     label=TeX(paste0("\\textbf{", VariableTeX[i], "}")),
                     size=7, hjust=0, vjust=1,
                     color=INRAEcyan)

        if (to_normalise) {
            label = TeX(paste0("Tendances en %\\.an$^{-1}$"))
        } else {
            label = TeX(paste0("Tendances en ", UnitTeX[i], ".an$^{-1}$"))
        }
        
        title = title +
            annotate("text",
                     x=x_off,
                     y=y3,
                     label=label,
                     size=4, hjust=0, vjust=1,
                     color=INRAEcyan)

        
        name = metaEX_variable$name_fr
        name = guess_newline(name, px=50, PX=PX)
        name = unlist(strsplit(name, "\n"))
        
        for (k in 1:length(name)) {
            title = title +
                annotate("text",
                         x=x_off+0.005,
                         y=y4-(k-1)*newline,
                         label=name[k],
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

        
        map = panel_stationnarity_map(trendEX_variable,
                                      metaEX_variable,
                                      meta,
                                      show_MK=show_MK,
                                      is_secteur=is_secteur,
                                      zoom=zoom,
                                      map_limits=map_limits,
                                      x_echelle_pct=x_echelle_pct,
                                      y_echelle_pct=y_echelle_pct,
                                      echelle=echelle, 
                                      Shapefiles=Shapefiles,
                                      margin_map=margin(t=-2, r=0,
                                                        b=0, l=0, "mm"),
                                      verbose=verbose)
        
        herd = add_sheep(herd,
                         sheep=map,
                         id="map",
                         height=map_height,
                         width=map_width,
                         verbose=verbose)

        Palette = unlist(strsplit(metaEX_variable$palette[metaEX_variable$variable_en == variable], " "))
        res = compute_colorBin(trendEX_variable$a_normalise_min,
                               trendEX_variable$a_normalise_max,
                               colorStep=length(Palette),
                               center=0,
                               include=FALSE)
        bin = res$bin
        upBin = res$upBin
        lowBin = res$lowBin
        color = get_colors(trendEX_variable$a_normalise,
                           upBin=upBin,
                           lowBin=lowBin,
                           Palette=Palette)

        # if (to_normalise) {
            # bin_tmp = bin*100
        # }
        
        fill = panel_colorbar_circle(bin,
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
                                     margin=margin(t=-0.9, r=0,
                                                   b=-1, l=0.8, "cm"))
        herd = add_sheep(herd,
                         sheep=fill,
                         id="fill",
                         width=fill_width,
                         verbose=verbose)

        if (show_MK) {
            if (is_foot) {
                margin_shape = margin(t=2.5, r=0, b=1.6, l=-1.8, "cm")
            } else {
                margin_shape = margin(t=2.2, r=0, b=2.4, l=-1.8, "cm")
            }
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
                                          margin=margin_shape)
        } else {
            shape = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0, b=0, l=-1.6, "cm"))

            shape = shape +
                annotate("text",
                         x=0,
                         y=0.32,
                         label=TeX(paste0("\\small{Les valeurs des pentes de Sen\nne préjugent pas de la stationnarité ou\nnon des séries temporelles examinées.}")),
                         size=3.2, hjust=0, vjust=0.5,
                         color=IPCCgrey50)
            
            shape = shape +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
        }
        herd = add_sheep(herd,
                         sheep=shape,
                         id="shape",
                         verbose=verbose)
        

        if (!is.null(suffix)) {
            footName = paste0('Carte de stationnarité - ', suffix)
        } else {
            footName = 'Carte de stationnarité'
        }
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
                       subsection=variable_to_display,
                       n=n_page))
        }

        if (is_foot) {
            if (foot_resume) {
                foot = panel_foot(footName, n_page, foot_height, logo_info)
            } else {
                foot = panel_foot("", n_page, foot_height, logo_info)
            }
            herd = add_sheep(herd,
                             sheep=foot,
                             id="foot",
                             height=foot_height,
                             width=width,
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
            # filename = paste0("map_stationnarity_", variable_to_display, "_secteur.pdf")
        # } else {
            # filename = paste0("map_stationnarity_", variable_to_display, ".pdf")
        # }
        
        if (!is.null(suffix)) {
            filename = paste0("map_stationnarity_", variable_to_display, "_", to_link(suffix), ".pdf")
        } else {
            filename = paste0("map_stationnarity_", variable_to_display, ".pdf")
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
        # }
    }
    return (Pages)
}
