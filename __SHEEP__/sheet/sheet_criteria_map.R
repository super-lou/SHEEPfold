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


sheet_criteria_map = function (dataEXind,
                               metaEXind,
                               meta,
                               ModelSelection=NULL,
                               Colors=refCOL,
                               subtitle=NULL,
                               one_colorbar=FALSE,
                               icon_path="",
                               logo_path="",
                               is_foot=TRUE,
                               is_secteur=FALSE,
                               is_warning=FALSE,
                               model_by_shape=FALSE,
                               remove_warning_lim=FALSE,
                               figdir="",
                               df_page=NULL,
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


    if (is.null(ModelSelection)) {
        Model = levels(factor(dataEXind$Model))
        Model = as.list(Model)
        names(Model) = Model
    } else {
        Model = ModelSelection
    }
    nModel = length(Model)

    if (length(Colors) == 1) {
        Colors = rep(Colors, nModel)
    }
    
    Code = levels(factor(data$Code))
    CodeALL = levels(factor(dataEXind$Code))
    nCode = length(Code)

    Var = metaEXind$var
    VarTeX = convert2TeX(Var)
    nVar = length(Var)

    if (!is_warning) {
        Unit = metaEXind$unit
        Unit[!grepl("jour de l", Unit) &
             !grepl("bool", Unit)] = "sans unité"
        Unit[grepl("jour de l", Unit)] = "en mois"
        
        UnitTeX = convert2TeX(Unit, size="small", bold=FALSE)
    } else {
        UnitTeX = rep("\\small{proportion en %}", nVar)
    }

    for (i in 1:nModel) {
        model = Model[[i]]
        model_names = names(Model)[i]

        if (is.null(model_names)) {
            model_names = paste0(model, collapse=" ")
        }
        if (nchar(model_names) == 0) {
            model2Disp = paste0(model, collapse=" ")
            model4Save = paste0(model, collapse="_")
        } else {
            model2Disp = model_names
            model4Save = gsub(" ", "_", model_names)
        }
        
        if (verbose) {
            print(paste0("diagnostic map for ",
                         model2Disp,
                         "   ", round(i/nModel*100, 1), "% done"))
        }

        if (is.null(Colors) | !(model2Disp %in% names(Colors))) {
            model_color = refCOL
        } else {
            model_color = Colors[names(Colors) == model2Disp]
        }

        for (j in 1:nVar) {

            var = Var[j]
            
            herd = bring_grass(verbose=verbose)
            herd = plan_of_herd(herd, plan, verbose=verbose)

            
            title = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0, b=0, l=0, "cm"))

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

            title = title +
                annotate("text",
                         x=0,
                         y=y1,
                         label=TeX(paste0("\\textbf{", model2Disp, "}")),
                         size=7, hjust=0, vjust=1,
                         color=model_color)

            if (!is.null(subtitle)) {
                title = title +
                    annotate("shadowText",
                             x=0,
                             y=y2,
                             label=subtitle,
                             size=3, hjust=0, vjust=1,
                             bg.color="white",
                             bg.r=0.15,
                             color=model_color)
            }

            title = title +
                annotate("text",
                         x=0,
                         y=y3,
                         label=TeX(paste0(VarTeX[j],
                                          " ", UnitTeX[j])),
                         size=4, hjust=0, vjust=1,
                         color=IPCCgrey40)
            title = title +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            herd = add_sheep(herd,
                             sheep=title,
                             id="title",
                             verbose=verbose)

            dataEXind_var =
                dplyr::select(dataEXind, c("Model", "Code", var))

            dataEXind_model_var =
                dataEXind_var[dataEXind_var$Model %in% model,]

            if (one_colorbar) {
                min_var = quantile(dataEXind_var[[var]],
                                   0.1, na.rm=TRUE)
                max_var = quantile(dataEXind_var[[var]],
                                   0.9, na.rm=TRUE)
            } else {
                min_var = NULL
                max_var = NULL
            }
            
            map = panel_criteria_map(dataEXind_model_var,
                                     metaEXind,
                                     meta,
                                     min_var,
                                     max_var,
                                     is_secteur=is_secteur,
                                     is_warning=is_warning,
                                     model_by_shape=model_by_shape,
                                     remove_warning_lim=remove_warning_lim,
                                     Shapefiles=Shapefiles,
                                     margin(t=0, r=0, b=0, l=0, "cm"),
                                     verbose=verbose)
            
            herd = add_sheep(herd,
                             sheep=map,
                             id="map",
                             height=map_height,
                             verbose=verbose)

            footName = "Carte des critères d'évaluation"
            if (is.null(df_page)) {
                n_page = i
            } else {
                if (nrow(df_page) == 0) {
                    n_page = 1
                } else {
                    n_page = df_page$n[nrow(df_page)] + 1
                }
                if (is.null(ModelSelection)) {
                    subsection = model
                } else {
                    subsection = var
                }
                df_page = bind_rows(
                    df_page,
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
                filename = paste0(model4Save, "_", var, "_secteur.pdf")
            } else {
                filename = paste0(model4Save, "_", var, ".pdf")
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
    return (df_page)
}
