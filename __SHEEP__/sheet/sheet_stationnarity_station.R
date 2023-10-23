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


sheet_stationnarity_station = function (data,
                                        meta,
                                        trendEX_serie,
                                        dataEX_serie,
                                        metaEX_serie,
                                        period_trend_show=NULL,
                                        logo_path="",
                                        Pages=NULL,
                                        Shapefiles=NULL,
                                        figdir="",
                                        verbose=FALSE) {

    Var = metaEX_serie$var
    Var = Var[Var != "QM"]
    nVar = length(Var)
    
    page_margin = c(t=0.5, r=0.5, b=0.5, l=0.5)

    info_height = 3
    chronicle_height = 3
    foot_height = 1.25
    var_height = (29.7 - 0.5*2 - info_height - chronicle_height - foot_height) / nVar
    
    var_width = 21 - 0.5*2 
    
    plan = matrix(c(
        "info", "chronicle", Var, "foot"
    ), ncol=1)

    regimeHydro =
        find_regimeHydro(
            dplyr::rename(dplyr::select(dataEX_serie$QM,
                                        c("Code", "Date", "QM_obs")),
                          QM=QM_obs))

    Code = levels(factor(data$Code))
    CodeALL = levels(factor(dataEX_serie$Code))
    nCode = length(Code)
    
    for (i in 1:nCode) {
        code = Code[i]
        if (verbose) {
            print(paste0("Datasheet station datasheet for ", code,
                         "   ", round(i/nCode*100, 1), "% done"))
        }
        
        data_code = data[data$Code == code,]
        
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
            data_code,
            dataEX_serie_code$QM$QM_obs,
            regimeLight=regimeHydro$detail[regimeHydro$Code == code],
            meta=meta,
            Shapefiles=Shapefiles,
            codeLight=code,
            to_do='all',
            zone_to_show='France')

        herd = add_sheep(herd,
                         sheep=info,
                         id="info",
                         height=info_height,
                         verbose=verbose)

        data_code_chronicle = dplyr::rename(data_code, Q_sim=Q_nat)
        data_code_chronicle$Model = "Naturalisé"
        Colors = IPCCgold
        names(Colors) = "Naturalisé"
        
        limits = c(min(data_code$Date), max(data_code$Date))
        
        chronicle = panel_spaghetti(data_code_chronicle,
                                    Colors=Colors,
                                    title="Q",
                                    unit="m^{3}.s^{-1}",
                                    alpha=1,
                                    isSqrt=TRUE,
                                    missRect=TRUE,
                                    isBack=FALSE,
                                    isTitleAbove=FALSE,
                                    isLegend=TRUE,
                                    obsLegend="Observé",
                                    addModelLegend=TRUE,
                                    sizeYticks=6,
                                    date_labels="%Y",
                                    breaks="5 years",
                                    minor_breaks="1 years",
                                    limits_ymin=0,
                                    isZeroLine=TRUE,
                                    isBackObsAbove=FALSE,
                                    lwObs=0.3,
                                    lwObs_back=0.7,
                                    lwSim=0.5,
                                    lwSim_back=0.9,
                                    grid=TRUE,
                                    ratio_title=1/4,
                                    margin_title=
                                        margin(t=2, r=0, b=0, l=20, "mm"),
                                    margin_spag=
                                        margin(t=0, r=0, b=0, l=0, "mm"),
                                    first=FALSE,
                                    last=TRUE)
        herd = add_sheep(herd,
                         sheep=chronicle,
                         id="chronicle",
                         height=chronicle_height,
                         verbose=verbose)
        herd$sheep$label[herd$sheep$id == "chronicle.spag"] = "align"

        for (j in 1:nVar) {
            var = Var[j]
            print(paste0("Time panel for ", var))

            if (j == nVar) {
                first = FALSE
                last = TRUE
            } else {
                first = FALSE
                last = FALSE
            }

            dataEX_serie_code_var = dataEX_serie_code[[var]]

            trendEX_serie_code_var =
                trendEX_serie[trendEX_serie$Code == code &
                              grepl(var, trendEX_serie$var, fixed=TRUE),]
            
            trend = panel_trend(var,
                                dataEX_serie_code_var,
                                trendEX_serie_code_var,
                                metaEX_serie,
                                period_trend_show=period_trend_show,
                                linetype='solid',
                                missRect=FALSE,
                                axis_xlim=limits,
                                grid=FALSE,
                                ymin_lim=NULL,
                                breaks="5 years",
                                minor_breaks="1 years",
                                date_labels="%Y",
                                margin_trend=
                                    margin(t=1, r=0, b=0, l=0, "mm"),
                                first=first, last=last)
            
            herd = add_sheep(herd,
                             sheep=trend,
                             id=var,
                             label="align",
                             height=var_height,
                             width=var_width,
                             verbose=verbose)
        }

        
        footName = 'Fiche station'
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

        res = return_to_sheepfold(herd,
                                  page_margin=page_margin,
                                  paper_size="A4",
                                  hjust=0, vjust=1,
                                  verbose=verbose)
        
        plot = res$plot
        paper_size = res$paper_size

        filename = paste0(code, "_stationnarity_datasheet.pdf")

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
