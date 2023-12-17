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


sheet_diagnostic_station = function (data,
                                     meta,
                                     dataEX_criteria,
                                     metaEX_criteria,
                                     dataEX_serie,
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
    chronicle_height = 3
    QA_height = 3
    medQJ_height = 7
    CDC_height = 7
    foot_height = 1.25
    criteria_height = 29.7 - 0.5*2 - info_height - chronicle_height - QA_height - medQJ_height - foot_height
    
    medQJ_width = 10
    CDC_width = 10

    plan = matrix(c(
        "info", "chronicle", "QA", "medQJ", "criteria", "foot",
        "info", "chronicle", "QA", "CDC", "criteria", "foot"),
        ncol=2)

    

    # dataEX_serieQM_obs =
    #     dplyr::summarise(dplyr::group_by(dataEX_serie$QM, Code, Date),
    #                      QM=select_good(QM_obs),
    #                      .groups="drop")

    # dataEX_seriePA_med = dplyr::summarise(dplyr::group_by(dataEX_serie$PA,
    #                                                      Code, Date),
    #                                      PAs=median(PAs_obs, na.rm=TRUE),
    #                                      PAl=median(PAl_obs, na.rm=TRUE),
    #                                      PA=median(PA_obs, na.rm=TRUE),
    #                                      .groups="drop")

    # regimeHydro = find_regimeHydro(dataEX_serieQM_obs, lim_number=2, dataEX_seriePA_med)

    # dataEX_serieQM_obs =
    #     dplyr::summarise(dplyr::group_by(dataEX_serie$QM,
    #                                      Code, Date),
    #                      QM=select_good(QM_obs),
    #                      .groups="drop")
    # dataEX_seriePA_ratio =
    #     dplyr::summarise(dplyr::group_by(dataEX_serie$PA_ratio,
    #                                      Code),
    #                      Ps_ratio=median(Ps_ratio_obs, na.rm=TRUE),
    #                      Pl_ratio=median(Pl_ratio_obs, na.rm=TRUE),
    #                      .groups="drop")
    # regimeHydro = find_regimeHydro(dataEX_serieQM_obs,
    #                                lim_number=NULL,
    #                                dataEX_seriePA_ratio)

    data_obs =
        dplyr::summarise(dplyr::group_by(data, Code, Date),
                         Q=median(Q_obs, na.rm=TRUE),
                         .groups="drop")

    dataEX_serieQM_obs =
        dplyr::summarise(dplyr::group_by(dataEX_serie$QM,
                                         Code, Date),
                         QM=median(QM_obs, na.rm=TRUE),
                         .groups="drop")
    
    dataEX_serieR_ratio =
        dplyr::full_join(dataEX_serie$Rl_ratio,
                         dataEX_serie$Rs_ratio,
                         by=c("Code", "Model"))
    dataEX_serieR_ratio =
        dplyr::summarise(
                   dplyr::group_by(dataEX_serieR_ratio,
                                   Code),
                   Rs_ratio=median(Rs_ratio_obs,
                                   na.rm=TRUE),
                   Rl_ratio=median(Rl_ratio_obs,
                                   na.rm=TRUE),
                   .groups="drop")
    
    regimeHydro = find_regimeHydro(dataEX_serieQM_obs,
                                   lim_number=NULL,
                                   dataEX_serieR_ratio)

    

    Model = levels(factor(dataEX_criteria$Model))
    nModel = length(Model)
                   
    Code = levels(factor(data$Code))
    CodeALL = levels(factor(dataEX_criteria$Code))
    nCode = length(Code)

    for (i in 1:nCode) {
        code = Code[i]
        if (verbose) {
            print(paste0("diagnostic station datasheet for ", code,
                         "   ", round(i/nCode*100, 1), "% done"))
        }
        
        data_code = data[data$Code == code,]

        data_code =
            dplyr::filter(data_code,
                          Model %in%
                          dataEX_criteria$Model[dataEX_criteria$Code == code])
        
        
        data_obs_code = data_obs[data_obs$Code == code,]
        dataEX_serieQM_obs_code =
            dataEX_serieQM_obs[dataEX_serieQM_obs$Code == code,]

        dataEX_serie_code = list()
        for (j in 1:length(dataEX_serie)) {
            dataEX_serie_code = append(
                dataEX_serie_code,
                list(dataEX_serie[[j]][dataEX_serie[[j]]$Code == code,]))
        }
        names(dataEX_serie_code) = names(dataEX_serie)
        
        herd = bring_grass(verbose=verbose)
        herd = plan_of_herd(herd, plan,
                            verbose=verbose)
        
        info = panel_info_station(
            data_obs_code,
            dataEX_serieQM_obs_code$QM,
            regimeLight=regimeHydro$detail[regimeHydro$Code == code],
            meta=meta,
            Shapefiles=Shapefiles,
            codeLight=code,
            to_do='all',
            zone_to_show='France',
            verbose=verbose)
        # info = contour()
        herd = add_sheep(herd,
                         sheep=info,
                         id="info",
                         height=info_height,
                         verbose=verbose)

        # print("info")
        # print(herd)
        
        chronicle = panel_spaghetti(data_code,
                                    title="(a) Débit journalier",
                                    unit="m^{3}.s^{-1}",
                                    alpha=0.35,
                                    isSqrt=TRUE,
                                    missRect=TRUE,
                                    isBack=FALSE,
                                    isTitle=TRUE,
                                    isLegend=TRUE,
                                    sizeYticks=6,
                                    date_labels="%Y",
                                    breaks="5 years",
                                    minor_breaks="1 years",
                                    limits_ymin=0,
                                    isBackObsAbove=FALSE,
                                    lwObs=0.2,
                                    lwObs_back=0.4,
                                    lwSim=0.4,
                                    lwSim_back=0.7,
                                    grid=TRUE,
                                    ratio_title=1/4,
                                    margin_title=
                                        margin(t=2, r=0, b=0, l=0, "mm"),
                                    margin_spag=
                                        margin(t=0, r=0, b=0, l=0, "mm"),
                                    first=FALSE,
                                    last=FALSE,
                                    verbose=verbose)
        # chronicle = contour()
        herd = add_sheep(herd,
                         sheep=chronicle,
                         id="chronicle",
                         label="align",
                         height=chronicle_height,
                         verbose=verbose)

        # print("chronicle")
        # print(herd)

        # print(dataEX_serie_code)
        
        dataMOD = dataEX_serie_code[["QA"]]
        dataMOD = dplyr::rename(dataMOD,
                                Q_obs="QA_obs",
                                Q_sim="QA_sim")
        QA = panel_spaghetti(
            dataMOD,
            Colors,
            title="(b) Débit annuel",
            unit="m^{3}.s^{-1}",
            alpha=0.85,
            isSqrt=FALSE,
            missRect=FALSE,
            isBack=FALSE,
            isTitle=TRUE,
            sizeYticks=6,
            date_labels="%Y",
            breaks="5 years",
            minor_breaks="1 years",
            isBackObsAbove=TRUE,
            limits_ymin=0,
            lwObs=0.6,
            lwObs_back=1,
            lwSim=0.4,
            lwSim_back=0.7,
            axis_xlim=c(min(data_obs_code$Date),
                        max(data_obs_code$Date)),
            grid=TRUE,
            ratio_title=1/7,
            margin_title=
                margin(t=0, r=0, b=0, l=0, "mm"),
            margin_spag=
                margin(t=0, r=0, b=2, l=0, "mm"),
            first=FALSE,
            last=TRUE,
            verbose=verbose)
        # QA = contour()
        herd = add_sheep(herd,
                         sheep=QA,
                         id="QA",
                         label="align",
                         height=QA_height,
                         verbose=verbose)

        herd$sheep$label[herd$sheep$id == "chronicle.spag"] = "align"
        herd$sheep$label[herd$sheep$id == "QA.spag"] = "align"

        dataMOD = dataEX_serie_code[["medQJC5"]]
        dataMOD =
            dplyr::mutate(dplyr::group_by(dataMOD,
                                          Model, Code),
                          n=1:dplyr::n())
        dataMOD = filter(dataMOD, n <= 365)
        dataMOD = dplyr::rename(dataMOD,
                                Date="Date",
                                Q_obs="medQJC5_obs",
                                Q_sim="medQJC5_sim")
        
        medQJ = panel_spaghetti(dataMOD,
                                Colors,
                                title="(c) Débit journalier médian interannuel",
                                unit="m^{3}.s^{-1}",
                                alpha=0.85,
                                isSqrt=TRUE,
                                missRect=FALSE,
                                isBack=FALSE,
                                isTitle=TRUE,
                                date_labels="%d %b",
                                breaks="3 months",
                                minor_breaks="1 months",
                                add_x_breaks=as.Date("1970-12-31"),
                                Xlabel="",
                                limits_ymin=0,
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
                                last=TRUE,
                                verbose=verbose)
        # medQJ = contour()

        print(herd)
        print(medQJ)
        herd = add_sheep(herd,
                         sheep=medQJ,
                         id="medQJ",
                         height=medQJ_height,
                         width=medQJ_width,
                         verbose=verbose)

        # print("medQJ")
        # print(herd)

        dataMOD = dataEX_serie_code[["CDC"]]
        dataMOD = dplyr::rename(dataMOD,
                                Date="CDC_p",
                                Q_obs="CDC_Q_obs",
                                Q_sim="CDC_Q_sim")
        CDC = panel_spaghetti(dataMOD,
                              Colors,
                              title="(d) Courbe des débits classés",
                              unit="m^{3}.s^{-1}",
                              alpha=0.85,
                              isSqrt=TRUE,
                              missRect=FALSE,
                              isTitle=TRUE,
                              isBack=FALSE,
                              breaks=0.2,
                              minor_breaks=0.1,
                              break_round=1,
                              isNormLaw=TRUE,
                              Xlabel="hautes eaux    \u27F5    Probabilité de dépassement    \u27F6    basses eaux",
                              limits_ymin=0,
                              isBackObsAbove=TRUE,
                              lwObs=0.6,
                              lwObs_back=1,
                              lwSim=0.4,
                              lwSim_back=0.7,
                              grid=TRUE,
                              ratio_title=1/15,
                              margin_title=
                                  margin(t=0, r=0, b=0, l=3.5, "mm"),
                              margin_spag=
                                  margin(t=0, r=0, b=0, l=3.5, "mm"),
                              first=FALSE,
                              last=TRUE,
                              verbose=verbose)
        # CDC = contour()
        herd = add_sheep(herd,
                         sheep=CDC,
                         id="CDC",
                         height=CDC_height,
                         width=CDC_width,
                         verbose=verbose)

        # print("CDC")
        # print(herd)

        Code_region = CodeALL[substr(CodeALL, 1, 1) == substr(code, 1, 1)]

        criteria = panel_diagnostic_criteria(
            dataEX_criteria,
            metaEX_criteria,
            meta,
            Colors,
            codeLight=code,
            groupCode=Code_region,
            icon_path=icon_path,
            Warnings=Warnings,
            title="(e) Critères de diagnostic",
            alpha_marker=0.85,
            Alpha=0.25,
            Probs=0.1,
            dTitle=0,
            add_name=TRUE,
            group_name="dans la région",
            text2px_lim=70,
            margin_add=
                margin(t=0, r=0, b=0, l=0, "cm"),
            verbose=verbose)
        # criteria = contour()
        herd = add_sheep(herd,
                         sheep=criteria,
                         id="criteria",
                         height=criteria_height,
                         verbose=verbose)

        # print("criteria")
        # print(herd)

        footName = 'Fiche de diagnostic station'
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
                       subsection=code,
                       n=n_page))
        }
        
        foot = panel_foot(footName, n_page,
                          foot_height, logo_path)
        herd = add_sheep(herd,
                         sheep=foot,
                         id="foot",
                         height=foot_height,
                         verbose=verbose)

        # print("foot")
        # print(herd)

        res = return_to_sheepfold(herd,
                                  page_margin=page_margin,
                                  paper_size="A4",
                                  hjust=0, vjust=1,
                                  verbose=verbose)
        
        plot = res$plot
        paper_size = res$paper_size

        filename = paste0(code, "_diagnostic_datasheet.pdf")

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
