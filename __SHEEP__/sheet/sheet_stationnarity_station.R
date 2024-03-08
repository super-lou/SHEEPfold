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
                                        trendEX,
                                        dataEX_serie,
                                        metaEX_serie,
                                        axis_xlim=NULL,
                                        code_selection=NULL,
                                        subtitle=NULL,
                                        logo_info=NULL,
                                        Shapefiles=NULL,
                                        zoom=NULL,
                                        map_limits=NULL,
                                        x_echelle_pct=62,
                                        y_echelle_pct=5,
                                        echelle=c(0, 50, 100, 250),
                                        foot_resume=TRUE,
                                        color_to_switch=NULL,
                                        figdir="",
                                        suffix=NULL,
                                        Pages=NULL,
                                        verbose=FALSE) {
    
    if (!is.null(code_selection)) {
        data = data[data$code %in% code_selection,]
        meta = meta[meta$code %in% code_selection,]
        trendEX = trendEX[trendEX$code %in% code_selection,]
        for (i in 1:length(dataEX_serie)) {
            dataEX_serie[[i]] =
                dplyr::filter(dataEX_serie[[i]],
                              code %in% code_selection)
        }
        Code = code_selection
    } else {
        Code = levels(factor(meta$code))
    }
    
    Variable = metaEX_serie$variable_en
    Variable = Variable[Variable != "QM"]
    nVariable = length(Variable)
    
    page_margin = c(t=0.5, r=0.5, b=0.5, l=0.5)

    info_height = 3
    chronicle_height = 3
    foot_height = 1.25
    variable_height = (29.7 - 0.5*2 - info_height - chronicle_height - foot_height) / nVariable
    
    width = 21 - 0.5*2 
    
    plan = matrix(c(
        "info", "chronicle", Variable, "foot"
    ), ncol=1)

    regimeHydro =
        find_regimeHydro(dplyr::select(dataEX_serie$QM,
                                       dplyr::all_of(c("code", "date", "QM"))))
    nCode = length(Code)

    for (i in 1:nCode) {
        code = Code[i]
        if (verbose) {
            print(paste0("Datasheet station datasheet for ", code,
                         "   ", round(i/nCode*100, 1), "% done"))
        }
        
        data_code = data[data$code == code,]
        
        dataEX_serie_code = list()
        for (j in 1:length(dataEX_serie)) {
            dataEX_serie_code = append(
                dataEX_serie_code,
                list(dataEX_serie[[j]][dataEX_serie[[j]]$code == code,]))
        }
        names(dataEX_serie_code) = names(dataEX_serie)

        
        herd = bring_grass(verbose=verbose)
        herd = plan_of_herd(herd, plan,
                            verbose=verbose)

        info = panel_info_station(
            data_code,
            dataEX_serie_code$QM$QM,
            regimeLight=regimeHydro$detail[regimeHydro$code == code],
            meta=meta,
            Shapefiles=Shapefiles,
            codeLight=code,
            to_do="all",
            subtitle=subtitle,
            if_NA_unkowned=FALSE,
            show_regime_info=FALSE,
            zoom=zoom,
            map_limits=map_limits,
            x_echelle_pct=x_echelle_pct,
            y_echelle_pct=y_echelle_pct,
            echelle=echelle,
            size_codeLight=2,
            stroke_codeLight=0.6)

        herd = add_sheep(herd,
                         sheep=info,
                         id="info",
                         height=info_height,
                         verbose=verbose)

        # data_code_chronicle = dplyr::rename(data_code, Q_sim=Q)
        # data_code_chronicle$HM = "Naturalisé"
        # Colors = IPCCgold
        # names(Colors) = "Naturalisé"
        
        # limits = c(min(data_code$date), max(data_code$date))
        # limits = period

        chronicle = panel_spaghetti(data_code,
                                    # Colors=Colors,
                                    title="Q",
                                    unit="m^{3}.s^{-1}",
                                    alpha=1,
                                    isSqrt=TRUE,
                                    missRect=TRUE,
                                    isBack=FALSE,
                                    isTitleAbove=FALSE,
                                    isLegend=TRUE,
                                    obsLegend=NULL,
                                    simLegend=NULL,
                                    addHMLegend=FALSE,
                                    sizeYticks=6,
                                    date_labels="%Y",
                                    breaks="5 years",
                                    minor_breaks="1 years",
                                    limits_ymin=0,
                                    isZeroLine=TRUE,
                                    isBackObsAbove=FALSE,
                                    lwObs=0.3,
                                    lwObs_back=0.7,
                                    axis_xlim=axis_xlim,
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
                         width=width,
                         verbose=verbose)
        # herd$sheep$label[herd$sheep$id == "chronicle.title"] = "align"
        herd$sheep$label[herd$sheep$id == "chronicle.spag"] = "align"

        for (j in 1:nVariable) {
            variable = Variable[j]
            
            print(paste0("Time panel for ", variable))

            if (j == nVariable) {
                first = FALSE
                last = TRUE
            } else {
                first = FALSE
                last = FALSE
            }

            dataEX_serie_code_variable = dataEX_serie_code[[variable]]

            trendEX_code_variable =
                trendEX[trendEX$code == code &
                              grepl(variable, trendEX$variable_en, fixed=TRUE),]

            trend = panel_trend(variable,
                                dataEX_serie_code_variable,
                                trendEX_code_variable,
                                metaEX_serie,
                                period_trend_show=NULL,
                                linetype='solid',
                                missRect=FALSE,
                                axis_xlim=axis_xlim,
                                grid=FALSE,
                                ymin_lim=NULL,
                                breaks="5 years",
                                minor_breaks="1 years",
                                date_labels="%Y",
                                color_to_switch=color_to_switch,
                                margin_trend=
                                    margin(t=1, r=0, b=0, l=0, "mm"),
                                first=first, last=last)
            
            herd = add_sheep(herd,
                             sheep=trend,
                             id=variable,
                             height=variable_height,
                             verbose=verbose)

            herd$sheep$label[herd$sheep$id == paste0(variable, ".title")] = "align"
            herd$sheep$label[herd$sheep$id == paste0(variable, ".plot")] = "align"
            
        }


        if (!is.null(suffix)) {
            footName = paste0('Fiche station - ', suffix)
        } else {
            footName = 'Fiche station'
        }
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

        if (foot_resume) {
            foot = panel_foot(footName, n_page, foot_height, logo_info)
        } else {
            foot = panel_foot("", n_page, foot_height, logo_info)
        }
        herd = add_sheep(herd,
                         sheep=foot,
                         id="foot",
                         height=foot_height,
                         width=width,
                         verbose=verbose)

        res = return_to_sheepfold(herd,
                                  page_margin=page_margin,
                                  paper_size="A4",
                                  hjust=0, vjust=1,
                                  verbose=verbose)
        
        plot = res$plot
        paper_size = res$paper_size

        if (!is.null(suffix)) {
            filename = paste0(code, "_", to_link(suffix), "_stationnarity_datasheet.pdf")
        } else {
            filename = paste0(code, "_stationnarity_datasheet.pdf")
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
