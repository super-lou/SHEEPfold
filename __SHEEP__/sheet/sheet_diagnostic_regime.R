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


sheet_diagnostic_regime = function (meta,
                                    dataEXind,
                                    metaEXind,
                                    dataEXserie,
                                    Colors,
                                    icon_path="",
                                    Warnings=NULL,
                                    logo_path="",
                                    df_page=NULL,
                                    Shapefiles=NULL,
                                    figdir="",
                                    verbose=FALSE) {

    page_margin = c(t=0.5, r=0.5, b=0.5, l=0.5)
    
    info_height = 3
    void_height = 0.2
    medQJ_height = 7
    criteria_height = 15

    foot_height = 1.25
    
    medQJ_width = 10

    
    NAME = matrix(c(
        "info", "void", "medQJ_1", "medQJ_0.25", "criteria", "foot",
        "info", "void", "medQJ_0.75", "medQJ_0", "criteria", "foot"),
    ncol=2)
    WIP = FALSE


    Model = levels(factor(dataEXind$Model))
    nModel = length(Model)
    
    Code = levels(factor(dataEXind$Code))
    nCode = length(Code)

    dataEXserieQM_obs =
        dplyr::summarise(dplyr::group_by(dataEXserie$QM, Code, Month),
                         QM=select_good(QM_obs),
                         .groups="drop")

    dataEXseriePA_med = dplyr::summarise(dplyr::group_by(dataEXserie$PA,
                                                         Code, Date),
                                         PAs=median(PAs, na.rm=TRUE),
                                         PAl=median(PAl, na.rm=TRUE),
                                         PA=median(PA, na.rm=TRUE),
                                         .groups="drop")

    regimeHydro = find_regimeHydro(dataEXserieQM_obs, lim_number=2,
                                   dataEXseriePA_med)

    Regime = split(regimeHydro$detail, factor(regimeHydro$typology_2))
    rm_duplicated = function (X) {
        return (X[!duplicated(X)])
    }
    Regime = lapply(Regime, rm_duplicated)
    orderRegime = lapply(Regime, stringr::str_extract,
                         pattern="[[:digit:]]+")
    orderRegime = lapply(orderRegime, '[', 1)
    orderRegime = order(as.numeric(unlist(orderRegime)))
    Regime = Regime[orderRegime]
    nRegime = length(Regime)

    for (i in 1:nRegime) {
        regime = names(Regime)[i]

        if (verbose) {
            print(paste0("diagnostic regime datasheet for ", regime,
                         "   ", round(i/nRegime*100, 1), "% done"))
        }

        Code_regime = regimeHydro$Code[regimeHydro$typology_2 == regime]
        dataEXind_regime = dataEXind[dataEXind$Code %in% Code_regime,]
        
        dataEXserie_regime = list()
        for (j in 1:length(dataEXserie)) {
            dataEXserie_regime = append(
                dataEXserie_regime,
                list(dataEXserie[[j]][dataEXserie[[j]]$Code %in% Code_regime,]))
        }
        names(dataEXserie_regime) = names(dataEXserie)

        medKGEracine =
            dplyr::summarise(dplyr::group_by(dataEXind_regime,
                                             Code),
                             value=median(KGEracine,
                                          na.rm=TRUE),
                             .groups="drop")
        KGEprobs = c(1, 0.75, 0.25, 0)
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

        Detail = Regime[[i]]
        QM_code = list()
        for (detail in Detail) {
            Code_detail =
                regimeHydro$Code[regimeHydro$typology_2 == regime &
                                 regimeHydro$detail == detail]
            dataEXserieQM_obs_detail =
                dataEXserieQM_obs[dataEXserieQM_obs$Code %in%
                                  Code_detail,]
            dataEXserieQM_obs_detail_med =
                dplyr::summarise(dplyr::group_by(
                                            dataEXserieQM_obs_detail,
                                            Month),
                                 QM=median(QM, na.rm=TRUE),
                                 .groups="drop")
            QM_code = append(QM_code,
                             list(dataEXserieQM_obs_detail_med$QM))
            names(QM_code)[length(QM_code)] = detail
        }

        orderQM = lapply(names(QM_code), stringr::str_extract,
                             pattern="[[:digit:]]+")
        orderQM = order(as.numeric(unlist(orderQM)))
        QM_code = QM_code[orderQM]
        
        info = panel_info_regime(QM_code,
                                 regimeLight=regime,
                                 meta=meta,
                                 Code_regime=Code_regime,
                                 Shapefiles=Shapefiles,
                                 to_do='all')
        STOCK = tibble()
        STOCK = add_plot(STOCK,
                         plot=info,
                         name="info",
                         height=info_height)


        for (j in 1:length(KGEprobs)) {
            code = Code_KGEprobs[j]
            prob = names(Code_KGEprobs)[j]

            if (is.na(code)) {
                medQJ = void()
                
            } else {
                dataEXserie_code = list()
                for (k in 1:length(dataEXserie)) {
                    dataEXserie_code = append(
                        dataEXserie_code,
                        list(dataEXserie[[k]][dataEXserie[[k]]$Code ==
                                              code,]))
                }
                names(dataEXserie_code) = names(dataEXserie)

                title = paste0("(", letters[j],
                               ") Débit journalier médian inter-annuel ",
                               "\\unit : ",
                               "\\textbf{", code, "}")
                if (j %% 2 == 0) {
                    margin_add = margin(t=0, r=0, b=0, l=3.5, "mm")
                } else {
                    margin_add = margin(t=0, r=3.5, b=0, l=0, "mm")
                }
                
                dataMOD = dataEXserie_code[["median{QJ}C5"]]
                dataMOD$Date = as.Date(dataMOD$Yearday-1,
                                       origin=as.Date("1972-01-01"))
                dataMOD = dplyr::rename(dataMOD,
                                        Q_obs="median{QJ}C5_obs",
                                        Q_sim="median{QJ}C5_sim")
                medQJ = panel_spaghetti(
                    dataMOD,
                    Colors,
                    title=title,
                    unit="m^{3}.s^{-1}",
                    alpha=0.85,
                    isSqrt=TRUE,
                    missRect=FALSE,
                    isBack=FALSE,
                    isTitle=TRUE,
                    date_labels="%d %b",
                    breaks="3 months",
                    minor_breaks="1 months",
                    Xlabel="",
                    limits_ymin=0,
                    isBackObsAbove=TRUE,
                    grid=TRUE,
                    ratio_title=1/15,
                    margin_title=
                        margin(t=0, r=7, b=0, l=0, "mm"),
                    margin_spag=
                        margin(t=0, r=3.5, b=0, l=0, "mm"),
                    first=FALSE,
                    last=TRUE)
            }
            STOCK = add_plot(STOCK,
                             plot=medQJ,
                             name=paste0("medQJ", "_", prob),
                             height=medQJ_height,
                             width=medQJ_width)
        }
        

        criteria = panel_diagnostic_criteria(
            dataEXind,
            metaEXind,
            meta,
            Colors,
            groupCode=Code_regime,
            icon_path=icon_path,
            Warnings=Warnings,
            title="(e) Critères de diagnostic",
            alpha_marker=0.85,
            Alpha=0.5,
            Probs=0.1,
            dTitle=0,
            add_name=TRUE,
            margin_add=
                margin(t=-3.8, r=0, b=0, l=0, "cm"))
        STOCK = add_plot(STOCK,
                         plot=criteria,
                         name="criteria",
                         height=criteria_height)

        STOCK = add_plot(STOCK,
                         plot=void(),
                         name="void",
                         height=void_height)


        footName = 'Fiche de diagnostic par régime'
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
                       subsection=regime,
                       n=n_page))
        }
        foot = panel_foot(footName, n_page,
                          foot_height, logo_path)
        STOCK = add_plot(STOCK,
                         plot=foot,
                         name="foot",
                         height=foot_height)

        res = merge_panel(STOCK, NAME=NAME,
                          page_margin=page_margin,
                          paper_size="A4",
                          hjust=0, vjust=1)

        plot = res$plot
        paper_size = res$paper_size


        regime = gsub("[ ][-][ ]", "_", regime)
        filename = paste0(regime, "_diagnostic_datasheet.pdf")

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
