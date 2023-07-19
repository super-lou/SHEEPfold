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



sheet_diagnostic_couche = function (meta,
                                    dataEXind,
                                    metaEXind,
                                    dataEXserie,
                                    Colors,
                                    icon_path="",
                                    logo_path="",
                                    df_page=NULL,
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

    Model = levels(factor(dataEXind$Model))
    nModel = length(Model)
    
    Code = levels(factor(dataEXind$Code))
    nCode = length(Code)

    Couche = levels(factor(unlist(meta$Couche)))
    Couche = Couche[nchar(Couche) > 0]

    nCouche = length(Couche)

    for (i in 1:nCouche) {
        couche = Couche[i]
        Code_couche = Code[is_in_couche(meta$Couche, couche)]
        meta_couche = meta[is_in_couche(meta$Couche, couche),]
        couche_disp = paste0(couche)

        if (verbose) {
            print(paste0("diagnostic couche datasheet for ",
                         couche_disp, "   ",
                         round(i/nCouche*100, 1), "% done"))
        }
        
        dataEXind_couche = dataEXind[dataEXind$Code %in% Code_couche,]
        
        dataEXserie_couche = list()
        for (j in 1:length(dataEXserie)) {
            dataEXserie_couche = append(
                dataEXserie_couche,
                list(dataEXserie[[j]][dataEXserie[[j]]$Code %in%
                                      Code_couche,]))
        }
        names(dataEXserie_couche) = names(dataEXserie)
        
        medREF =
            dplyr::summarise(dplyr::group_by(dataEXind_couche,
                                             Code),
                             value=median(get("NSEbiais"),
                                          na.rm=TRUE),
                             .groups="drop")
        REFprobs = c(1, 0.75, 0.25, 0)
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

            if (is.na(code)) {
                medQJ = void()
                
            } else {
                dataEXserie_code = list()
                for (k in 1:length(dataEXserie)) {
                    dataEXserie_code = append(
                        dataEXserie_code,
                        list(dataEXserie[[k]][dataEXserie[[k]]$Code == code,]))
                }
                names(dataEXserie_code) = names(dataEXserie)

                title = paste0("(", letters[j],
                               ") Hauteur journalière médiane inter-annuel ",
                               "\\unit : ",
                               "\\textbf{", code, "}")
                if (j %% 2 == 0) {
                    margin_add = margin(t=0, r=0, b=0, l=3.5, "mm")
                } else {
                    margin_add = margin(t=0, r=3.5, b=0, l=0, "mm")
                }
                
                dataMOD = dataEXserie_code[["medQJC5"]]
                dataMOD = dplyr::rename(dataMOD,
                                        Date="Date",
                                        Q_obs="medQJC5_obs",
                                        Q_sim="medQJC5_sim")
                
                medQJ = panel_spaghetti(dataMOD,
                                        Colors,
                                        title=title,
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
                                        ratio_title=1/15,
                                        margin_title=
                                            margin(t=0, r=7, b=0, l=0, "mm"),
                                        margin_spag=
                                            margin(t=0, r=6, b=0, l=0, "mm"),
                                        first=FALSE,
                                        last=TRUE)
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

        Warnings = "Les stations choisies pour illustrer les résultats à l'échelle régionale illustrent la variabilité des performances obtenues sur les hydrogrammes des débits journaliers médians (piézomètres associées aux maximum, quantile 75 % et    25 %, et minimum du KGE<i>biais</i>)."
        
        criteria = panel_diagnostic_criteria(
            dataEXind,
            metaEXind,
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
            text2px_lim=51,
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
    return (df_page)
}
