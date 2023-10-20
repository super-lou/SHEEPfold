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


sheet_diagnostic_region = function (meta,
                                    dataEX_criteria,
                                    metaEX_criteria,
                                    dataEXserie,
                                    Colors,
                                    icon_path="",
                                    Warnings=NULL,
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
    # criteria_height = 29.7 - 0.5*2 - info_height - void_height - medQJ_height*2 - foot_height
    
    medQJ_width = 10
    
    plan = matrix(c(
        "info", "void", "medQJ_1", "medQJ_025", "criteria", "foot",
        "info", "void", "medQJ_075", "medQJ_0", "criteria", "foot"),
        ncol=2)
    

    Model = levels(factor(dataEX_criteria$Model))
    nModel = length(Model)
    
    Code = levels(factor(dataEX_criteria$Code))
    nCode = length(Code)

    Region = levels(factor(substr(Code, 1, 1)))
    nRegion = length(Region)

    # Region = "K"
    # nRegion = 1
    
    for (i in 1:nRegion) {
        region = Region[i]
        Code_region = Code[substr(Code, 1, 1) == region]
        meta_region = meta[substr(meta$Code, 1, 1) == region,]
        region_disp = paste0(meta_region$Region_Hydro[1],
                             " - ", region)

        if (verbose) {
            print(paste0("diagnostic region datasheet for ", region_disp,
                         "   ", round(i/nRegion*100, 1), "% done"))
        }
        
        dataEX_criteria_region = dataEX_criteria[dataEX_criteria$Code %in% Code_region,]
        
        dataEXserie_region = list()
        for (j in 1:length(dataEXserie)) {
            dataEXserie_region = append(
                dataEXserie_region,
                list(dataEXserie[[j]][dataEXserie[[j]]$Code %in% Code_region,]))
        }
        names(dataEXserie_region) = names(dataEXserie)
        
        medKGEracine =
            dplyr::summarise(dplyr::group_by(dataEX_criteria_region,
                                             Code),
                             value=median(KGEracine,
                                          na.rm=TRUE),
                             .groups="drop")
        KGEprobs = c(1, 0.75, 0.25, 0)
        KGEnames = c("maximum du KGE\u221A dans la région",
                     "quantile 75 % du KGE\u221A dans la région",
                     "quantile 25 % du KGE\u221A dans la région",
                     "minimum du KGE\u221A dans la région")
        KGEq = quantile(medKGEracine$value,
                        probs=KGEprobs, na.rm=TRUE)
        id_nearest = function (target, In) {
            id = which.min(abs(In - target))
            return (id)
        }
        Code_KGEprobs =
            medKGEracine$Code[sapply(KGEq,
                                     id_nearest,
                                     In=medKGEracine$value)]
        Code_KGEprobs[duplicated(Code_KGEprobs)] = NA
        names(Code_KGEprobs) = KGEprobs

        herd = bring_grass(verbose=verbose)
        herd = plan_of_herd(herd, plan,
                            verbose=verbose)

        info = panel_info_region(meta,
                                 Shapefiles=Shapefiles,
                                 regionLight=region,
                                 to_do='all')
        herd = add_sheep(herd,
                         sheep=info,
                         id="info",
                         height=info_height,
                         verbose=verbose)
        

        for (j in 1:length(KGEprobs)) {
            code = Code_KGEprobs[j]
            prob = names(Code_KGEprobs)[j]
            prob_name = KGEnames[j]

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
                               ") Débit journalier médian interannuel ",
                               "*unit*")
                subtitle = paste0("     \\textbf{", code, "} ")
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
                                        subtitle=subtitle,
                                        unit="m^{3}.s^{-1}",
                                        alpha=0.85,
                                        isSqrt=TRUE,
                                        missRect=FALSE,
                                        isBack=FALSE,
                                        isTitle=TRUE,
                                        date_labels="%d %b",
                                        breaks="3 months",
                                        minor_breaks="1 months",
                                        add_x_breaks=
                                            as.Date("1970-12-31"),
                                        Xlabel="",
                                        limits_ymin=0,
                                        isBackObsAbove=TRUE,
                                        lwObs=0.6,
                                        lwObs_back=1,
                                        lwSim=0.4,
                                        lwSim_back=0.7,
                                        grid=TRUE,
                                        ratio_title=1.9/15,
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


        Warnings = "Les stations choisies pour illustrer les résultats à l'échelle régionale illustrent la variabilité des performances obtenues sur les hydrogrammes des débits journaliers médians (stations associées aux maximum, quantile 75 % et 25 %, et minimum de la médiane multi-modèle des KGE\u221A)."
        
        criteria = panel_diagnostic_criteria(
            dataEX_criteria,
            metaEX_criteria,
            meta,
            Colors,
            groupCode=Code_region,
            icon_path=icon_path,
            Warnings=Warnings,
            title="(e) Critères de diagnostic",
            alpha_marker=0.85,
            Alpha=0.5,
            Probs=0.1,
            dTitle=0,
            add_name=TRUE,
            group_name="dans la région",
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


        footName = 'Fiche de diagnostic région'
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
                       subsection=region_disp,
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

        filename = paste0(gsub(" ", "_",
                               gsub(" [-] ", "_",
                                    region_disp)),
                          "_diagnostic_datasheet.pdf")

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
