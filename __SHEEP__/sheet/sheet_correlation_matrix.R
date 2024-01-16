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


sheet_correlation_matrix = function (dataEX, metaEX,
                                     HMGroup=NULL,
                                     Colors=NULL,
                                     subtitle=NULL,
                                     criteria_selection=NULL,
                                     icon_path="", logo_path="",
                                     Pages=NULL,
                                     figdir='',
                                     verbose=FALSE) {

    if (is.null(HMGroup)) {
        HM = levels(factor(dataEX$HM))
        HMGroup = append(as.list(HM), list(HM))
        names(HMGroup) = c(HM, "Multi-modèle")
    }
    nHMGroup = length(HMGroup)

    page_margin = c(t=0.5, r=0.5, b=0.5, l=0.5)
    
    title_height = 1
    cb_height = 1.25
    ssg_height = 1.25
    si_height = 1.25
    foot_height = 1.25
    
    cm_height = 29.7 - page_margin["t"] - page_margin["b"] -
        title_height - cb_height - si_height - foot_height
    cm_width = 21 - page_margin["l"] - page_margin["r"]
    

    cm_margin = margin(t=1, r=0, b=2, l=1.75, "cm")
    cb_margin = margin(t=0, r=2, b=0.4, l=1.2, "cm")
    ssg_margin = margin(t=0.3, r=1, b=0.1, l=2, "cm")
    si_margin = margin(t=0.3, r=3.5, b=0.4, l=0, "cm")
    

    plan = matrix(c("title", "cm", "cb", "ssg", "foot",
                    "title", "cm", "cb", "si", "foot",
                    "title", "cm", "void", "void", "foot",
                    "title", "cm", "void", "void", "foot"),
                  ncol=4)
    WIP = FALSE

    for (i in 1:nHMGroup) {
        HM = HMGroup[[i]]
        HM_names = names(HMGroup)[i]
        nHM = length(HM)
        
        if (is.null(HM_names)) {
            HM_names = ""
        }
        if (nchar(HM_names) == 0) {
            HM2Disp = paste0(HM, collapse=" ")
            HM4Save = paste0(HM, collapse="_")
        } else {
            HM2Disp = HM_names
            HM4Save = gsub(" ", "_", HM_names)
        }
        
        if (verbose) {
            print(paste0("diagnostic correlation matrix for ",
                         HM2Disp,
                         "   ", round(i/nHMGroup*100, 1), "% done"))
        }

        herd = bring_grass(verbose=verbose)
        herd = plan_of_herd(herd, plan, verbose=verbose)
        
        variable_plotted = c()
        
        dataEX_hm = dataEX[dataEX$HM %in% HM,]

        if (is.null(Colors) | !(HM2Disp %in% names(Colors))) {
            hm_color = refCOL
        } else {
            hm_color = Colors[names(Colors) == HM2Disp]
        }

        title = paste0("<b style='font-size:16pt; color:", refCOL, "'>",
                       "Matrice de corrélation des critères d'évaluation", "</b>",
                       "<br>",
                       "<b style='font-size:14pt; color:", hm_color, "'>", HM2Disp, "</b>")
        if (!is.null(subtitle)) {
            title = paste0(title, nbsp(1),
                           "<span style='font-size:10pt; color:", hm_color, "'>", subtitle, "</span>")
        }
        
        title = richtext_grob(title,
                              x=0, y=1,
                              margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                              hjust=0, vjust=1)

        herd = add_sheep(herd,
                         sheep=title,
                         id="title",
                         height=title_height,
                         verbose=verbose)
        
        cm = panel_correlation_matrix(dataEX_hm,
                                      metaEX,
                                      icon_path=icon_path,
                                      criteria_selection=
                                          criteria_selection,
                                      margin=cm_margin)
        herd = add_sheep(herd,
                         sheep=cm,
                         id="cm",
                         height=cm_height,
                         width=cm_width,
                         verbose=verbose)

        herd = add_sheep(herd,
                         sheep=void(),
                         id="void",
                         verbose=verbose)
        
        cb = panel_colorbar(-1, 1,
                            palette_name="rainbow_6",
                            colorStep=6, include=TRUE,
                            asFrac=TRUE,
                            reverse=TRUE,
                            round=FALSE,
                            size_color=0.3,
                            dx_color=0.4,
                            dy_color=0.45,
                            margin=cb_margin,
                            WIP=WIP)
        herd = add_sheep(herd,
                         sheep=cb,
                         id="cb",
                         height=cb_height,
                         verbose=verbose)

        ssg = panel_shape_size_gradient(shape="rect",
                                        Size=c(0.1, 0.15, 0.2, 0.25),
                                        color=IPCCgrey50,
                                        labelArrow="Plus corrélé",
                                        dx_shape=0.2,
                                        dy_shape=0.1,
                                        dy_arrow=0.3,
                                        size_arrow=0.25,
                                        dz_arrow=1,
                                        dl_arrow=0,
                                        dr_arrow=0,
                                        dx_text=0, 
                                        margin=ssg_margin,
                                        WIP=WIP)
        herd = add_sheep(herd,
                         sheep=ssg,
                         id="ssg",
                         height=ssg_height,
                         verbose=verbose)

        if (nHM == 1) {
            si = panel_shape_info(Shape="rect",
                                Size=0.2,
                                Color=IPCCgrey50,
                                Label=c(
                                    "Significatif à un risque de 10 %",
                                    "Non significatif à un risque de 10 %"),
                                Cross=c(FALSE, TRUE),
                                dy_label=0.35,
                                dx_label=0.2,
                                margin=si_margin,
                                WIP=WIP)
        } else {
            si = void()
        }
        herd = add_sheep(herd,
                         sheep=si,
                         id="si",
                         height=si_height,
                         verbose=verbose)

        footName = paste0('Matrice de corrélation : ', HM2Disp)
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
                dplyr::tibble(section="Matrice de corrélation",
                              subsection=HM2Disp,
                              n=n_page))
        }
        foot = panel_foot(footName, n_page,
                          foot_height, logo_path)
        herd = add_sheep(herd,
                         sheep=foot,
                         id="foot",
                         height=foot_height,
                         verbose=verbose)

        res = return_to_sheepfold(herd,
                                  page_margin=page_margin,
                                  paper_size="A4",
                                  hjust=0, vjust=1,
                                  verbose=verbose)
        
        plot = res$plot
        paper_size = res$paper_size

        if (!is.null(subtitle)) {
            filename = paste0("matrice_correlation_", HM4Save, "_",
                              gsub(" ", "_", subtitle), ".pdf")
        } else {
            filename = paste0("matrice_correlation_", HM4Save, ".pdf")
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
    return (Pages)
}
