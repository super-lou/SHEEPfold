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



sheet_diagnostic_couche = function (data,
                                    meta,
                                    dataEX_criteria,
                                    metaEX_criteria,
                                    dataEX_serie,
                                    Colors,
                                    icon_path="",
                                    logo_path="",
                                    Pages=NULL,
                                    Shapefiles=NULL,
                                    figdir="",
                                    verbose=FALSE) {

    page_margin = c(t=0.5, r=0.5, b=0.5, l=0.5)
    
    info_height = 3
    void_height = 0.2
    medQJ_height = 7
    foot_height = 1.25
    criteria_height = 11.45
    # criteria_height = 29.7 - 0.5*2 - void_height - info_height - medQJ_height*2 - foot_height
    
    medQJ_width = 10

    plan = matrix(c(
        "info", "void", "medQJ_1", "medQJ_025", "criteria", "foot",
        "info", "void", "medQJ_075", "medQJ_0", "criteria", "foot"),
        ncol=2)

    HM = levels(factor(dataEX_criteria$HM))
    nHM = length(HM)
    
    Code = levels(factor(dataEX_criteria$Code))
    nCode = length(Code)

    Couche = levels(factor(unlist(meta$Couche)))
    Couche = Couche[nchar(Couche) > 0]
    # Couche = "139"
    nCouche = length(Couche)

    for (i in 1:nCouche) {
        couche = Couche[i]
        
        meta_couche = meta[is_in_couche(meta$Couche, couche),]
        Code_couche = meta_couche$Code
        couche_disp = paste0(couche)

        if (verbose) {
            print(paste0("diagnostic couche datasheet for ",
                         couche_disp, "   ",
                         round(i/nCouche*100, 1), "% done"))
        }
        
        dataEX_criteria_couche = dataEX_criteria[dataEX_criteria$Code %in% Code_couche,]
        
        dataEX_serie_couche = list()
        for (j in 1:length(dataEX_serie)) {
            dataEX_serie_couche = append(
                dataEX_serie_couche,
                list(dataEX_serie[[j]][dataEX_serie[[j]]$Code %in%
                                      Code_couche,]))
        }
        names(dataEX_serie_couche) = names(dataEX_serie)
        
        medREF =
            dplyr::summarise(dplyr::group_by(dataEX_criteria_couche,
                                             Code),
                             value=median(get("NSEbiais"),
                                          na.rm=TRUE),
                             .groups="drop")
        REFprobs = c(1, 0.75, 0.25, 0)
        REFnames = c("maximum du NSE\\textit{biais} dans l'entité",
                     "quantile 75 % du \\textit{biais} dans l'entité",
                     "quantile 25 % du \\textit{biais} dans l'entité",
                     "minimum du \\textit{biais} dans l'entité")
        REFq = quantile(medREF$value,
                        probs=REFprobs, na.rm=TRUE)

        if (length(Code_couche) > 3)  {
            id_nearest = function (target, In) {
                id = which.min(abs(In - target))
                return (id)
            }
            Code_REFprobs =
                medREF$Code[sapply(REFq,
                                       id_nearest,
                                       In=medREF$value)]
            Code_REFprobs[duplicated(Code_REFprobs)] = NA
            names(Code_REFprobs) = REFprobs

        } else {
            Code_REFprobs =
                medREF$Code[order(medREF$value,
                                      decreasing=TRUE)]
            Code_REFprobs[duplicated(Code_REFprobs)] = NA
            Code_REFprobs = c(Code_REFprobs,
                              rep(NA, 4-length(Code_couche)))
            names(Code_REFprobs) = REFprobs
        }

        herd = bring_grass(verbose=verbose)
        herd = plan_of_herd(herd, plan,
                            verbose=verbose)

        info = panel_info_couche(meta_couche,
                                 Shapefiles=Shapefiles,
                                 coucheLight=couche,
                                 to_do='all')
        # info = void()
        herd = add_sheep(herd,
                         sheep=info,
                         id="info",
                         height=info_height,
                         verbose=verbose)
        

        for (j in 1:length(REFprobs)) {
            code = Code_REFprobs[j]
            prob = names(Code_REFprobs)[j]
            prob_name = REFnames[j]

            if (is.na(code)) {
                medQJ = void()
                
            } else {
                dataEX_serie_code = list()
                for (k in 1:length(dataEX_serie)) {
                    dataEX_serie_code = append(
                        dataEX_serie_code,
                        list(dataEX_serie[[k]][dataEX_serie[[k]]$Code == code,]))
                }
                names(dataEX_serie_code) = names(dataEX_serie)

                title = paste0("(", letters[j],
                               ") Hauteur journalière médiane interannuelle ",
                               "*unit*")
                subtitle = paste0("     \\textbf{", code, "} ")
                if (j %% 2 == 0) {
                    margin_add = margin(t=0, r=0, b=0, l=3.5, "mm")
                } else {
                    margin_add = margin(t=0, r=3.5, b=0, l=0, "mm")
                }

                dataMOD = dataEX_serie_code[["medQJC5"]]
                dataMOD =
                    dplyr::mutate(dplyr::group_by(dataMOD,
                                                  HM, Code),
                                  n=1:dplyr::n())
                dataMOD = filter(dataMOD, n <= 365)
                dataMOD = dplyr::rename(dataMOD,
                                        date="date",
                                        Q_obs="medQJC5_obs",
                                        Q_sim="medQJC5_sim")
                

                if (all(is.na(dataMOD$Q_obs))) {
                    dataMOD =  data[data$HM == dataMOD$HM[1] & data$Code == code,]
                    
                    dataMOD = dplyr::rename(dataMOD,
                                            date="date",
                                            Q_obs="H_obs",
                                            Q_sim="H_sim")

                    title = paste0("(", letters[j],
                                   ") Hauteur annuelle ",
                                   "*unit*")
                    subtitle = paste0("     \\textbf{", code, "}")
                    
                    medQJ = panel_spaghetti(
                        dataMOD,
                        Colors,
                        title=title,
                        subtitle=subtitle,
                        unit="m",
                        alpha=0.85,
                        isSqrt=FALSE,
                        missRect=FALSE,
                        isBack=FALSE,
                        isTitle=TRUE,
                        date_labels="%Y",
                        breaks="5 years",
                        minor_breaks="1 years",
                        Xlabel="",
                        limits_ymin=NA,
                        isBackObsAbove=TRUE,
                        lwObs=0.6,
                        lwObs_back=1,
                        lwSim=0.4,
                        lwSim_back=0.7,
                        grid=TRUE,
                        ratio_title=1.5/15,
                        margin_title=
                            margin(t=0, r=7, b=0, l=0, "mm"),
                        margin_spag=
                            margin(t=0, r=6, b=0, l=0, "mm"),
                        first=FALSE,
                        last=TRUE)
                    
                } else {
                    medQJ = panel_spaghetti(
                        dataMOD,
                        Colors,
                        title=title,
                        subtitle=subtitle,
                        unit="m",
                        alpha=0.85,
                        isSqrt=FALSE,
                        missRect=FALSE,
                        isBack=FALSE,
                        isTitle=TRUE,
                        date_labels="%d %b",
                        breaks="3 months",
                        minor_breaks="1 months",
                        add_x_breaks=
                            as.Date("1970-12-31"),
                        Xlabel="",
                        limits_ymin=NA,
                        isBackObsAbove=TRUE,
                        lwObs=0.6,
                        lwObs_back=1,
                        lwSim=0.4,
                        lwSim_back=0.7,
                        grid=TRUE,
                        ratio_title=1.5/15,
                        margin_title=
                            margin(t=0, r=7, b=0, l=0, "mm"),
                        margin_spag=
                            margin(t=0, r=6, b=0, l=0, "mm"),
                        first=FALSE,
                        last=TRUE)
                }
            }
            
            herd = add_sheep(herd,
                             sheep=medQJ,
                             id=paste0("medQJ", "_",
                                       gsub("[.]", "", prob)),
                             height=medQJ_height,
                             width=medQJ_width,
                             verbose=verbose)
        }

        herd$sheep$label[herd$sheep$id %in% c("medQJ_1.spag", "medQJ_025.spag")] = "align1"
        herd$sheep$label[herd$sheep$id %in% c("medQJ_075.spag", "medQJ_0.spag")] = "align2"

        Warnings = "Les piézomètres choisis pour illustrer les résultats à l'échelle de l'entité illustrent la variabilité des performances obtenues (piézomètres associés aux maximum, quantile 75 % et 25 %, et minimum de la médiane multi-modèle des NSE<i>biais</i>)."
        
        criteria = panel_diagnostic_criteria(
            dataEX_criteria,
            metaEX_criteria,
            meta,
            Colors,
            groupCode=Code_couche,
            icon_path=icon_path,
            Warnings=Warnings,
            title="(e) Critères de diagnostic",
            alpha_marker=0.85,
            Alpha=0.5,
            Probs=0.1,
            dTitle=0,
            width=11.9,
            add_name=TRUE,
            group_name="dans l'entité",
            text2px_lim=50,
            margin_add=
                margin(t=0, r=0, b=0, l=0, "cm"))

        herd = add_sheep(herd,
                         sheep=criteria,
                         id="criteria",
                         height=criteria_height,
                         verbose=verbose)

        herd = add_sheep(herd,
                         sheep=void(),
                         id="void",
                         height=void_height,
                         verbose=verbose)


        footName = "Fiche de diagnostic piézomètres"
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
                       subsection=couche_disp,
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

        filename = paste0(couche_disp, "_diagnostic_datasheet.pdf")

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
