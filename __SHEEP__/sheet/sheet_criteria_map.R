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


sheet_criteria_map = function (dataEX_criteria,
                               metaEX_criteria,
                               meta,
                               prob=0.1,
                               HMSelection=NULL,
                               Colors=refCOL,
                               subtitle=NULL,
                               one_colorbar=FALSE,
                               icon_path="",
                               logo_path="",
                               is_foot=TRUE,
                               is_secteur=FALSE,
                               is_warning=FALSE,
                               for_paper=FALSE,
                               HM_by_shape=FALSE,
                               remove_warning_lim=FALSE,
                               figdir="",
                               Pages=NULL,
                               Shapefiles=NULL,
                               verbose=FALSE) {

    paper_size = c(15, 15)

    page_margin = c(t=0.25, r=0.5, b=0.25, l=0.5)
    
    if (is_foot) {
        foot_height = 1.25
    } else {
        foot_height = 0
    }
    
    map_height =
        paper_size[1] - page_margin["t"] - page_margin["b"] - foot_height

    title_height = map_height/2
    
    if (is_foot) {
        plan = matrix(c("title", "map", "foot",
                        "map", "map", "foot"),
                      ncol=2)
    } else {
        plan = matrix(c("title", "map",
                        "map", "map"),
                      ncol=2)
    }


    if (is.null(HMSelection)) {
        HM = levels(factor(dataEX_criteria$HM))
        HM = as.list(HM)
        names(HM) = HM
    } else {
        HM = HMSelection
    }
    nHM = length(HM)

    if (length(Colors) == 1) {
        Colors = rep(Colors, nHM)
    }
    
    Code = levels(factor(data$code))
    CodeALL = levels(factor(dataEX_criteria$code))
    nCode = length(Code)

    Variable = metaEX_criteria$variable_en
    VariableTeX = convert2TeX(Variable)
    nVariable = length(Variable)

    # if (!is_warning) {
    #     Unit = metaEX_criteria$unit_fr
    #     Unit[!grepl("jour de l", Unit) &
    #          !grepl("bool", Unit)] = "sans unité"
    #     Unit[grepl("jour de l", Unit)] = "en mois"
        
    #     UnitTeX = convert2TeX(Unit, size="small", bold=FALSE)
    # } else {
    #     UnitTeX = rep("\\small{proportion en %}", nVariable)
    # }
    Unit = metaEX_criteria$unit_fr
    if (for_paper) {
        size = "tiny"
    } else {
        size = "small"
    }
    UnitTeX = convert2TeX(Unit, size=size, bold=FALSE)
    PX = get_alphabet_in_px()
    
    for (i in 1:nHM) {
        hm = HM[[i]]
        hm_names = names(HM)[i]

        if (is.null(hm_names)) {
            hm_names = paste0(hm, collapse=" ")
        }
        if (nchar(hm_names) == 0) {
            hm2Disp = paste0(hm, collapse=" ")
            hm4Save = paste0(hm, collapse="_")
        } else {
            hm2Disp = hm_names
            hm4Save = gsub(" ", "_", hm_names)
        }
        
        if (verbose) {
            print(paste0("diagnostic map for ",
                         hm2Disp,
                         "   ", round(i/nHM*100, 1), "% done"))
        }

        if (is.null(Colors) | !(hm2Disp %in% names(Colors))) {
            hm_color = refCOL
        } else {
            hm_color = Colors[names(Colors) == hm2Disp]
        }

        for (j in 1:nVariable) {

            variable = Variable[j]
            
            herd = bring_grass(verbose=verbose)
            herd = plan_of_herd(herd, plan, verbose=verbose)
            
            title = ggplot() + theme_void_Lato() +
                theme(plot.margin=margin(t=0, r=0, b=0, l=0, "cm"))

            if (!for_paper) {
                if (is_foot) {
                    if (is.null(subtitle)) {
                        y1 = 0.98
                        y3 = 0.875
                    } else {
                        y1 = 0.98
                        y2 = 0.89
                        y3 = 0.825
                    }

                } else {
                    if (is.null(subtitle)) {
                        y1 = 0.98
                        y3 = 0.89
                    } else {
                        y1 = 0.98
                        y2 = 0.9
                        y3 = 0.84
                    }
                }
                y4 = y3-0.07
                newline = 0.04
                
                title = title +
                    annotate("text",
                             x=0,
                             y=y1,
                             label=TeX(paste0("\\textbf{", hm2Disp, "}")),
                             size=7, hjust=0, vjust=1,
                             color=hm_color)

                if (!is.null(subtitle)) {
                    title = title +
                        annotate("shadowText",
                                 x=0,
                                 y=y2,
                                 label=subtitle,
                                 size=3, hjust=0, vjust=1,
                                 bg.color="white",
                                 bg.r=0.15,
                                 color=hm_color)
                }

                title = title +
                    annotate("text",
                             x=0,
                             y=y3,
                             label=TeX(paste0(VariableTeX[j],
                                              " ", UnitTeX[j])),
                             size=4, hjust=0, vjust=1,
                             color=IPCCgrey40)
                
                glose = metaEX_criteria$glose[metaEX_criteria$variable_en ==
                                              variable]
                glose = guess_newline(glose, px=20, PX=PX)
                glose = unlist(strsplit(glose, "\n"))
                
                for (k in 1:length(glose)) {
                    title = title +
                        annotate("text",
                                 x=0,
                                 y=y4-(k-1)*newline,
                                 label=glose[k],
                                 size=2.5, hjust=0, vjust=1,
                                 color=IPCCgrey40)
                }
            } else {
                title = title +
                    annotate("text",
                             x=0.02,
                             y=0.93,
                             label=TeX(paste0(VariableTeX[j],
                                              " ", UnitTeX[j])),
                             size=8, hjust=0, vjust=1,
                             color=IPCCgrey40)
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

            dataEX_criteria_variable =
                dplyr::select(dataEX_criteria, c("HM", "code", variable))

            dataEX_criteria_hm_variable =
                dataEX_criteria_variable[dataEX_criteria_variable$HM %in% hm,]

            if (one_colorbar) {
                min_variable = quantile(dataEX_criteria_variable[[variable]],
                                   prob, na.rm=TRUE)
                max_variable = quantile(dataEX_criteria_variable[[variable]],
                                   1-prob, na.rm=TRUE)
            } else {
                min_variable = NULL
                max_variable = NULL
            }

            if (is.null(subtitle)) {
                margin_map = margin(t=5, r=0, b=0, l=0, "mm")
            } else {
                margin_map = margin(t=10, r=0, b=0, l=0, "mm")
            }
            
            map = panel_criteria_map(dataEX_criteria_hm_variable,
                                     metaEX_criteria,
                                     meta,
                                     min_variable,
                                     max_variable,
                                     prob=prob,
                                     is_secteur=is_secteur,
                                     is_warning=is_warning,
                                     for_paper=for_paper,
                                     HM_by_shape=HM_by_shape,
                                     remove_warning_lim=remove_warning_lim,
                                     Shapefiles=Shapefiles,
                                     margin_map,
                                     verbose=verbose)
            
            herd = add_sheep(herd,
                             sheep=map,
                             id="map",
                             height=map_height,
                             verbose=verbose)

            footName = "Carte des critères d'évaluation"
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
                filename = paste0(hm4Save, "_", variable, "_secteur.pdf")
            } else {
                filename = paste0(hm4Save, "_", variable, ".pdf")
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
