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


sheet_projection_station = function (meta,
                                     dataEX_serie,
                                     metaEX_serie,
                                     Colors,
                                     historical=c("1976-01-01", "2005-08-31"),
                                     prob=0.01, 
                                     icon_path="",
                                     Warnings=NULL,
                                     logo_info="",
                                     Pages=NULL,
                                     Shapefiles=NULL,
                                     figdir="",
                                     verbose=FALSE) {
    
    page_margin = c(t=0.5, r=0.5, b=0.5, l=0.5)

    info_height = 3
    medQJ_height = 7
    axis_height = 0.63
    foot_height = 1.25

    free_height = 29.7 - page_margin["t"] - page_margin["b"] -
        info_height - medQJ_height - axis_height*3 - foot_height
    
    
    spread_height = free_height/3 * 6/10
    signe_height = free_height/3 * 1/10
    stripes_height = free_height/3 * 3/10
    
    width = 21 - page_margin["l"] - page_margin["r"]
    medQJ_width = width/3

    plan = matrix(c(
        "info", "info", "info",
        
        "medQJ_H0", "medQJ_H2", "medQJ_H3",
        
        "spread_QJXA", "spread_QJXA", "spread_QJXA",
        "signe_QJXA", "signe_QJXA", "signe_QJXA",
        "stripes_QJXA", "stripes_QJXA", "stripes_QJXA",
        "axis_QJXA", "axis_QJXA", "axis_QJXA",
        
        "spread_QA", "spread_QA", "spread_QA",
        "signe_QA", "signe_QA", "signe_QA",
        "stripes_QA", "stripes_QA", "stripes_QA",
        "axis_QA", "axis_QA", "axis_QA",
        
        "spread_VCN10_summer", "spread_VCN10_summer", "spread_VCN10_summer",
        "signe_VCN10_summer", "signe_VCN10_summer", "signe_VCN10_summer",
        "stripes_VCN10_summer", "stripes_VCN10_summer", "stripes_VCN10_summer", 

        "axis_VCN10_summer", "axis_VCN10_summer", "axis_VCN10_summer",
        
        "foot", "foot", "foot"
    ), ncol=3, byrow=TRUE)

    
    # data_obs =
    #     dplyr::summarise(dplyr::group_by(data, code, date),
    #                      Q=median(Q_obs, na.rm=TRUE),
    #                      .groups="drop")


    for (k in 1:length(dataEX_serie)) {
        dataEX_serie[[k]]$climateChain = paste(dataEX_serie[[k]]$GCM,
                                               dataEX_serie[[k]]$EXP,
                                               dataEX_serie[[k]]$RCM,
                                               dataEX_serie[[k]]$BC, sep="|")
        dataEX_serie[[k]]$Chain = paste(dataEX_serie[[k]]$climateChain,
                                        dataEX_serie[[k]]$HM, sep="|")
    }
    Chain = unique(dataEX_serie[[1]]$Chain)
    nChain = length(Chain)
    
    Code = levels(factor(dataEX_serie[[1]]$code))
    nCode = length(Code)
    
    Horizons = c("Période de référence 1976-2005",
                 "Horizon moyen 2041-2070",
                 "Horizon lointain 2070-2099")
    Variables_medQJ = c("medQJ_H0", "medQJ_H2", "medQJ_H3")
    Variables = unique(metaEX_serie$variable_en)
    Variables = Variables[!grepl("medQJ", Variables)]

    
    Storylines = names(Colors)
    nStorylines = length(Storylines)
    names(Colors) = paste0(names(Colors), "|median")
    
    for (i in 1:nCode) {
        code = Code[i]
        if (verbose) {
            print(paste0("diagnostic station datasheet for ", code,
                         "   ", round(i/nCode*100, 1), "% done"))
        }
        
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

        
        nProjections = length(unique(dataEX_serie[[1]]$Chain))
        info = panel_info_station(
            meta=meta,
            Shapefiles=Shapefiles,
            codeLight=code,
            nProjections=nProjections,
            to_do=c("title", "subtitle", "map", "spatial", "projection"),
            if_NA_unkowned=FALSE,
            subtitle_height=0.28,
            echelle=c(0, 250),
            x_echelle_pct=2,
            y_echelle_pct=3,
            echelle_size=2,
            echelle_km_size=1.9,
            echelle_tick_height=3,
            margin_map=margin(t=0, r=0, b=0, l=0, unit="mm"),
            verbose=verbose)
        # info = contour()
        herd = add_sheep(herd,
                         sheep=info,
                         id="info",
                         height=info_height,
                         width=width,
                         verbose=verbose)



        limits_ymax = quantile(c(dataEX_serie_code$medQJ_H0$medQJ_H0,
                                 dataEX_serie_code$medQJ_H2$medQJ_H2,
                                 dataEX_serie_code$medQJ_H3$medQJ_H3),
                               0.995,
                               na.rm=TRUE)

        hide_y_axis = FALSE
        
        for (j in 1:length(Variables_medQJ)) {

            variable = Variables_medQJ[j]
            print(variable)
            
            dataMOD = dataEX_serie_code[[variable]]
            dataMOD$date = as.Date("1970-01-01") + lubridate::yday(dataMOD$date)-1
            dataMOD_storylines_med =
                dplyr::summarise(
                           dplyr::group_by(
                                      dplyr::filter(dataMOD,
                                                    climateChain %in%
                                                    Storylines),
                                      date,
                                      climateChain),
                           Chain=paste0(climateChain[1], "|median"),
                           !!variable:=median(get(variable), na.rm=TRUE),
                           .groups="drop")

            dataMOD = dplyr::bind_rows(dataMOD, dataMOD_storylines_med)
            start_year = min(dataMOD$date)
                
            variable_SAFRAN = "medQJ_H0"
            dataMOD_SAFRAN = dataEX_serie_code[[variable_SAFRAN]]
            dataMOD_SAFRAN$date = as.Date("1970-01-01") + lubridate::yday(dataMOD_SAFRAN$date)-1
            dataMOD_SAFRAN_med =
                dplyr::summarise(
                           dplyr::group_by(
                                      dplyr::filter(dataMOD_SAFRAN,
                                                    EXP == "SAFRAN"),
                                      date,
                                      climateChain),
                           !!paste0(variable, "_SAFRAN"):=median(get(variable_SAFRAN),
                                                                 na.rm=TRUE),
                           .groups="drop")

            # dataMOD_SAFRAN_med =
                # dplyr::mutate(dataMOD_SAFRAN_med,
                              # date=start_year + lubridate::yday(date)-1)
            
            dataMOD = dplyr::left_join(dataMOD,
                                       dplyr::select(dataMOD_SAFRAN_med,
                                                     -climateChain))

            dataMOD = dplyr::select(dataMOD,
                                    date,
                                    HM=Chain,
                                    Q_sim=variable,
                                    Q_obs=paste0(variable, "_SAFRAN"))
            if (j == 1) {
                margin_l = 0
                margin_r = 1
            } else if (j == 2) {
                margin_l = 4
                margin_r = 0
            } else {
                margin_l = 5
                margin_r = 0
            }
            
            medQJ = panel_spaghetti(dataMOD,
                                    Colors,
                                    title=paste0("(", letters[j], ") Régime hydrologique"),
                                    unit="m^{3}.s^{-1}",
                                    subtitle=Horizons[j],
                                    alpha=0.85,
                                    alpha_non_color=0.1,
                                    isSqrt=TRUE,
                                    missRect=FALSE,
                                    isBack=FALSE,
                                    isTitle=TRUE,
                                    date_labels="%d %b",
                                    breaks="3 months",
                                    minor_breaks="1 months",
                                    # add_x_breaks=as.Date("1970-12-31"),
                                    Xlabel="",
                                    limits_ymin=0,
                                    limits_ymax=limits_ymax,
                                    isBackObsAbove=TRUE,
                                    lwObs=0.6,
                                    lwObs_back=1,
                                    lwSim=0.4,
                                    lwSim_back=0.7,
                                    lwSim_non_color=0.2,
                                    grid=TRUE,
                                    useTeX=FALSE,
                                    dx0_title=0.03,
                                    dx0_subtitle=0.105,
                                    ratio_title=1/6.6,
                                    margin_title=
                                        margin(t=2, r=margin_r,
                                               b=0, l=margin_l, "mm"),
                                    margin_spag=
                                        margin(t=0, r=margin_r,
                                               b=0, l=margin_l, "mm"),
                                    first=FALSE,
                                    last=TRUE,
                                    hide_y_axis=hide_y_axis,
                                    verbose=verbose)
            # medQJ = contour()
            herd = add_sheep(herd,
                             sheep=medQJ,
                             id=variable,
                             height=medQJ_height,
                             width=medQJ_width,
                             verbose=verbose)
            hide_y_axis = TRUE
        }

        
        for (variable in Variables) {

            print(variable)
            dataMOD = dataEX_serie_code[[variable]]
            dataMOD_SAFRAN = dplyr::filter(dataMOD, EXP == "SAFRAN")
            dataMOD = dplyr::filter(dataMOD, climateChain %in% Storylines)     
            dataMOD$date = as.Date(paste0(lubridate::year(dataMOD$date), "-01-01"))

            dataMOD_historical =
                dplyr::summarise(
                           dplyr::group_by(
                                      dplyr::filter(dataMOD,
                                                    historical[1] <= date &
                                                    date <= historical[2]),
                                      Chain),
                           !!paste0("mean", variable):=mean(get(variable), na.rm=TRUE))

            dataMOD = dplyr::left_join(dataMOD, dataMOD_historical,
                                       by="Chain")

            dataMOD$delta =
                dataMOD[[variable]] - dataMOD[[paste0("mean", variable)]]

            dataMOD = dplyr::mutate(dplyr::group_by(dataMOD, Chain),
                                    spline_to_date(.data, "date", "delta",
                                                   spar=0.15))
            
            # if (!(file.exists(file.path(figdir, "tmp")))) {
            #     dir.create(file.path(figdir, "tmp"), recursive=TRUE)
            # }
            # for (chain in unique(dataMOD$Chain)) {
            #     dataMOD_chain = filter(dataMOD, Chain == chain)
            #     dataMOD_ss_chain = filter(dataMOD_ss, Chain == chain)
                
            #     plot = ggplot2::ggplot() + theme_minimal() +
            #         annotate("line",
            #                  dataMOD_chain$date,
            #                  dataMOD_chain$delta,
            #                  color="blue") +
            #         annotate("line",
            #                  dataMOD_ss_chain$date,
            #                  dataMOD_ss_chain$delta,
            #                  color="black")
                
            #     ggplot2::ggsave(plot=plot,
            #                     path=file.path(figdir, "tmp"),
            #                     filename=paste0(variable, "_", chain),
            #                     width=21,
            #                     height=15, units='cm',
            #                     dpi=300,
            #                     device=cairo_pdf)
            # }
            # dataMOD = dataMOD_ss
            
            Palette = metaEX_serie$palette[metaEX_serie$variable_en == variable]
            Palette = unlist(strsplit(Palette, " "))

            min_value = quantile(dataMOD$delta, prob, na.rm=TRUE)
            max_value = quantile(dataMOD$delta, 1-prob, na.rm=TRUE)
            
            res = compute_colorBin(min_value, max_value,
                                   length(Palette),
                                   center=0,
                                   include=FALSE)
            dataMOD$fill = get_colors(dataMOD$delta,
                                      res$upBin, res$lowBin, Palette)

            stripes = bring_grass(verbose=verbose)
            stripes = plan_of_herd(stripes,
                                   as.matrix(c("margin_top",
                                               paste0("stripes_", variable,
                                                      "_", 1:nStorylines),
                                               "margin_bottom")),
                                   verbose=verbose)
            
            stripes = add_sheep(stripes,
                                sheep=void(),
                                id="margin_top",
                                height=0,
                                verbose=verbose)
            stripes = add_sheep(stripes,
                                sheep=void(),
                                id="margin_bottom",
                                height=0,
                                verbose=verbose)
                
            for (k in 1:nStorylines) {
                dataMOD_storyline = dplyr::filter(dataMOD, climateChain == Storylines[k])
                dataMOD_storyline$y = factor(dataMOD_storyline$Chain)
                dataMOD_storyline = dplyr::arrange(dataMOD_storyline, HM)
                
                stripes_k =
                    ggplot2::ggplot() + theme_void() + 
                    ggplot2::theme(plot.margin=margin(t=0.3, r=0,
                                                      b=0.3, l=0, "mm")) +
                    ggplot2::annotate("tile",
                                      x=dataMOD_storyline$date,
                                      y=dataMOD_storyline$y,
                                      fill=dataMOD_storyline$fill) +
                    ggplot2::scale_x_date(expand=c(0, 0)) +
                    ggplot2::scale_y_discrete(expand=c(0, 0))

                stripes = add_sheep(stripes,
                                    sheep=stripes_k,
                                    id=paste0("stripes_", variable, "_", k),
                                    # label="align",
                                    height=1,
                                    verbose=verbose)
            }

            herd = add_sheep(herd,
                             sheep=stripes,
                             id=paste0("stripes_", variable),
                             # label="align",
                             height=stripes_height,
                             verbose=verbose)

            # spread = panel_spread(verbose=verbose)
            spread = contour()
            herd = add_sheep(herd,
                             sheep=spread,
                             id=paste0("spread_", variable),
                             # label="align",
                             height=spread_height,
                             verbose=verbose)

            # signe = panel_signe(verbose=verbose)
            signe = contour()
            herd = add_sheep(herd,
                             sheep=signe,
                             id=paste0("signe_", variable),
                             # label="align",
                             height=signe_height,
                             verbose=verbose)
        }


        axis = ggplot2::ggplot() +
            ggplot2::theme(plot.margin=margin(t=0.0, r=0,
                                              b=0.0, l=0, "mm")) +
            theme_IPCC(isGridY=FALSE,
                       tick_y=FALSE,
                       label_y=FALSE,
                       zeroLine=TRUE) + 
            ggplot2::annotate("point",
                              x=unique(dataEX_serie[["QA"]]$date),
                              y=0, color=NA, fill=NA) +
            ggplot2::scale_x_date(expand=c(0, 0)) +
            ggplot2::scale_y_continuous(limits=c(0, 1),
                                        expand=c(0, 0))
        herd = add_sheep(herd,
                         sheep=axis,
                         id="axis_QJXA",
                         height=axis_height,
                         verbose=verbose)
        herd = add_sheep(herd,
                         sheep=axis,
                         id="axis_QA",
                         height=axis_height,
                         verbose=verbose)
        herd = add_sheep(herd,
                         sheep=axis,
                         id="axis_VCN10_summer",
                         height=axis_height,
                         verbose=verbose)
        

        footName = 'Fiche résultats projection'
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
                          foot_height, logo_info,
                          verbose=verbose)
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

        filename = paste0(code, "_projection_datasheet.pdf")

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
