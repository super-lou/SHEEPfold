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
                               ModelGroup=NULL,
                               Colors=INRAEcyan,
                               icon_path="",
                               logo_path="",
                               figdir="",
                               df_page=NULL,
                               Shapefiles=NULL,
                               verbose=FALSE) {

    paper_size = c(15, 15)
    page_margin = c(t=0.5, r=0.5, b=0.25, l=0.5)

    # title_height = 1.25
    # title_width = 7
    foot_height = 1.25
    foot_width = 14
    map_height = 15 - 1 - foot_height
    map_width = 14
    
    
    plan = matrix(c("title", "map", "foot",
                    "map", "map", "foot"),
                  ncol=2)

        # plan = matrix(c("title", "map", "foot"),
        # ncol=1)


    if (is.null(ModelGroup)) {
        Model = levels(factor(dataEX$Model))
        ModelGroup = as.list(Model)
        names(ModelGroup) = Model
    }
    nModelGroup = length(ModelGroup)

    if (length(Colors) == 1) {
        Colors = rep(Colors, nModelGroup)
    }
    
    Model = levels(factor(dataEXind$Model))
    nModel = length(Model)
    
    Code = levels(factor(data$Code))
    CodeALL = levels(factor(dataEXind$Code))
    nCode = length(Code)

    Var = metaEXind$var
    VarTeX = convert2TeX(Var)
    nVar = length(Var)

    Unit = metaEXind$unit
    Unit[!grepl("jour de l", Unit) &
         !grepl("bool", Unit)] = "sans unité"
    Unit[grepl("jour de l", Unit)] = "en mois"
    
    UnitTeX =  convert2TeX(Unit, bold=FALSE, font="small")

    for (i in 1:nModelGroup) {
        Model = ModelGroup[[i]]
        Model_names = names(ModelGroup)[i]
        nModel = length(Model)

        if (is.null(Model_names)) {
            Model_names = paste0(Model, collapse=" ")
        }
        if (nchar(Model_names) == 0) {
            Model2Disp = paste0(Model, collapse=" ")
            Model4Save = paste0(Model, collapse="_")
        } else {
            Model2Disp = Model_names
            Model4Save = gsub(" ", "_", Model_names)
        }
        
        if (verbose) {
            print(paste0("diagnostic map for ",
                         Model2Disp,
                         "   ", round(i/nModelGroup*100, 1), "% done"))
        }

        for (j in 1:nVar) {

            var = Var[j]
            
            herd = bring_grass(verbose=verbose)
            herd = plan_of_herd(herd, plan, verbose=verbose)

            
            title = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0, b=0, l=0, "cm"))
            title = title +
                annotate("text",
                         x=0,
                         y=0.98,
                         label=TeX(paste0("\\textbf{",
                                          Model2Disp, "}")),
                         size=7, hjust=0, vjust=1,
                         color=Colors[Model_names]) +
                annotate("text",
                         x=0,
                         y=0.87,
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


            dataEXind_model_var =
                dplyr::select(dataEXind[dataEXind$Model %in% Model,],
                              c("Model", "Code", var))
            
            map = panel_criteria_map(dataEXind_model_var,
                                     metaEXind,
                                     meta,
                                     Shapefiles=Shapefiles,
                                     margin(t=0, r=0, b=0, l=0, "cm"),
                                     verbose=verbose)
            
            herd = add_sheep(herd,
                             sheep=map,
                             id="map",
                             height=map_height,
                             width=map_width,
                             verbose=verbose)


            footName = 'Carte de diagnostic'
            if (is.null(df_page)) {
                n_page = i
            } else {
                if (nrow(df_page) == 0) {
                    n_page = 1
                } else {
                    n_page = df_page$n[nrow(df_page)] + 1
                }
                df_page = bind_rows(
                    df_page,
                    tibble(section=footName,
                           subsection=code,
                           n=n_page))
            }
            
            foot = panel_foot(footName, n_page,
                              foot_height, logo_path)

            
            herd = add_sheep(herd,
                             sheep=foot,
                             id="foot",
                             height=foot_height,
                             width=foot_width,
                             verbose=verbose)
            

            res = return_to_sheepfold(herd,
                                      page_margin=page_margin,
                                      paper_size=paper_size,
                                      hjust=0, vjust=1,
                                      verbose=verbose)
            
            plot = res$plot
            paper_size = res$paper_size

            figdir_model = file.path(figdir, Model4Save)
            filename = paste0(var, ".pdf")

            if (!(file.exists(figdir_model))) {
                dir.create(figdir_model, recursive=TRUE)
            }
            ggplot2::ggsave(plot=plot,
                            path=figdir_model,
                            filename=filename,
                            width=paper_size[1],
                            height=paper_size[2], units='cm',
                            dpi=300,
                            device=cairo_pdf)
        }
    }
    return (df_page)
}
