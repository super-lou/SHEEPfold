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
                                     dataEX_criteria,
                                     metaEX_criteria,
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

    height = 29.7 - page_margin["t"] - page_margin["b"]
    width = 21 - page_margin["l"] - page_margin["r"]
    
    info_height = 3
    medQJ_height = 7
    foot_height = 1.25

    block_height = (height - info_height - medQJ_height - foot_height) / 3

    variable_info_width = 0.2
    variable_graph_width = 0.8

    variable_title_height = 0.05
    variable_spread_height = 0.46
    variable_signe_height = 0.1
    variable_stripes_height = 0.25
    variable_axis_height = 0.1
    variable_void_height = 0.04
    
    medQJ_width = width/3

    plan_1 = matrix(c(
        "info", "info", "info",
        "medQJ_H0", "medQJ_H2", "medQJ_H3",
        "QJXA", "QJXA", "QJXA",
        "QA", "QA", "QA",
        "VCN10_summer", "VCN10_summer", "VCN10_summer",
        "foot", "foot", "foot"
    ), ncol=3, byrow=TRUE)

    plan_2 = matrix(c(
        "HF", "HF", 
        "MF", "MF",
        "LF", "LF",
        "QJXA-10", "VCN10-5", 
        "foot", "foot"
    ), ncol=2, byrow=TRUE)

    extreme_height = 12
    extreme_title_height = 0.048
    extreme_graph_height = 0.952

    delta_height = (height - extreme_height- foot_height) / 3
    delta_title_height = 0.07
    delta_graph_height = 0.93
    
    extreme_width = width/2
    

    for (k in 1:length(dataEX_serie)) {
        dataEX_serie[[k]]$climateChain = paste(dataEX_serie[[k]]$GCM,
                                               dataEX_serie[[k]]$EXP,
                                               dataEX_serie[[k]]$RCM,
                                               dataEX_serie[[k]]$BC, sep="|")
        dataEX_serie[[k]]$Chain = paste(dataEX_serie[[k]]$climateChain,
                                        dataEX_serie[[k]]$HM, sep="|")
    }

    dataEX_criteria$climateChain = paste(dataEX_criteria$GCM,
                                         dataEX_criteria$EXP,
                                         dataEX_criteria$RCM,
                                         dataEX_criteria$BC, sep="|")
    dataEX_criteria$Chain = paste(dataEX_criteria$climateChain,
                                  dataEX_criteria$HM, sep="|")
    
    Code = levels(factor(dataEX_serie[[1]]$code))
    nCode = length(Code)
    
    Horizons = c("\\textbf{Période de référence} 1976-2005",
                 "\\textbf{Horizon moyen} 2041-2070",
                 "\\textbf{Horizon lointain} 2070-2099")
    
    Variables_medQJ = c("medQJ_H0", "medQJ_H2", "medQJ_H3")
    nVariables_medQJ = length(Variables_medQJ)
    
    Variables_serie = unique(metaEX_serie$variable_en)
    Variables_serie = Variables_serie[!grepl("medQJ", Variables_serie)]
    nVariables_serie = length(Variables_serie)
    
    Storylines = names(Colors)
    nStorylines = length(Storylines)
    names(Colors) = paste0(names(Colors), "|median")


    Titles_extreme = c("Crues extrèmes",
                       "Étiages extrèmes")
    Subtitles_extreme = c("Période de retour 10 ans", "Période de retour 5 ans")
    Variables_extreme = c("QJXA-10", "VCN10-5")
    nVariables_extreme = length(Variables_extreme)

    Variables_delta = metaEX_criteria$variable_en
    Topics = metaEX_criteria$topic_fr
    Ok = !grepl(get_regexp(Variables_extreme), Variables_delta)
    Variables_delta = Variables_delta[Ok]
    Topics = Topics[Ok]
    Variables_delta = gsub("[_]H[[:digit:]]", "", Variables_delta)
    Ok = !duplicated(Variables_delta)
    Variables_delta = Variables_delta[Ok]
    Topics = Topics[Ok]
    nVariables_delta = length(Variables_delta)

    TypesALL = sapply(strsplit(Topics, ", "), "[", 2)
    Types = unique(TypesALL)
    nTypes = length(Types)
    Types_plan = c("HF", "MF", "LF")
    
    dataEX_criteria = dataEX_criteria[, !grepl("[_]H1", names(dataEX_criteria))]
    deltaHorizon = c("H2", "H3")
    nDeltaHorizon = length(deltaHorizon)


    delta_prob = 0.05

    dataEX_criteria_prob =
        dplyr::summarise(dplyr::group_by(dataEX_criteria,
                                         code),
                         dplyr::across(dplyr::where(is.numeric),
                                       ~quantile(.x, delta_prob,
                                                 na.rm=TRUE),
                                       .names="min_{.col}"),
                         dplyr::across(dplyr::where(is.numeric),
                                       ~quantile(.x, 1-delta_prob,
                                                 na.rm=TRUE),
                                       .names="max_{.col}"))

    
    for (i in 1:nCode) {
        code = Code[i]
        if (verbose) {
            print(paste0("diagnostic station datasheet for ", code,
                         "   ", round(i/nCode*100, 1), "% done"))
        }

        id_letter = 0
        
        dataEX_serie_code = list()
        for (j in 1:length(dataEX_serie)) {
            dataEX_serie_code = append(
                dataEX_serie_code,
                list(dataEX_serie[[j]][dataEX_serie[[j]]$code == code,]))
        }
        names(dataEX_serie_code) = names(dataEX_serie)

        dataEX_criteria_code = dataEX_criteria[dataEX_criteria$code == code,]
        dataEX_criteria_prob_code = dataEX_criteria_prob[dataEX_criteria_prob$code == code,]
            
        Chain = unique(dplyr::filter(dataEX_criteria_code, EXP != "SAFRAN")$Chain)
        nChain = length(Chain)
        
        
## 1. PAGE 1 _________________________________________________________
        herd = bring_grass(verbose=verbose)
        herd = plan_of_herd(herd, plan_1,
                            verbose=verbose)
        
        # nProjections = length(unique(dataEX_serie_code[[1]]$Chain))
        info = panel_info_station(
            meta=meta,
            Shapefiles=Shapefiles,
            codeLight=code,
            nProjections=nChain,
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
        
        for (j in 1:nVariables_medQJ) {

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
                                    title=paste0("(", letters[id_letter+j], ") Régime hydrologique"),
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
                                    convertTeX=FALSE,
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
        id_letter = id_letter + nVariables_medQJ

        Date =
            as.Date(paste0(
                lubridate::year(unique(dataEX_serie[["QA"]]$date)),
                "-01-01"))

        axis = panel_axis(Date,
                          axis.text.x_size=9,
                          date_labels="%Y",
                          breaks="10 years",
                          minor_breaks="5 years")
        
        
        for (j in 1:nVariables_serie) {
            variable = Variables_serie[j]
            print(variable)
            
            variable_to_display =
                metaEX_serie$variable_fr[metaEX_serie$variable_en == variable]
            
            name_to_display =
                metaEX_serie$name_fr[metaEX_serie$variable_en == variable]
            
            block_plan = matrix(c("info", "title",
                                  "info", "spread",
                                  "info", "signe", 
                                  "info", "stripes",
                                  "info", "axis",
                                  "info", "void"),
                                ncol=2, byrow=TRUE)
            block = bring_grass(verbose=verbose)
            block = plan_of_herd(block, block_plan,
                                 verbose=verbose)

            
            titleTeX = TeX(paste0("(", letters[id_letter+j], ") ",
                              convert2TeX(variable_to_display, bold=TRUE),
                              " $-$ ", name_to_display))
            
            title = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) + 
                annotate("text",
                         x=0,
                         y=1,
                         label=titleTeX,
                         size=3, hjust=0, vjust=1,
                         color=IPCCgrey25) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
    
            block = add_sheep(block,
                              sheep=title,
                              id="title",
                              height=variable_title_height,
                              width=variable_graph_width,
                              verbose=verbose)


            block = add_sheep(block,
                              sheep=void(),
                              id="info",
                              height=1,
                              width=variable_info_width,
                              verbose=verbose)
            
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
                (dataMOD[[variable]] - dataMOD[[paste0("mean", variable)]]) /
                dataMOD[[paste0("mean", variable)]]

            colorStep = 256
            Palette = metaEX_serie$palette[metaEX_serie$variable_en == variable]
            Palette = unlist(strsplit(Palette, " "))
            Palette = colorRampPalette(Palette)(colorStep)

            min_value = quantile(dataMOD$delta, prob, na.rm=TRUE)
            max_value = quantile(dataMOD$delta, 1-prob, na.rm=TRUE)
            
            res = compute_colorBin(min_value, max_value,
                                   length(Palette),
                                   center=0,
                                   include=FALSE)
            dataMOD$fill = get_colors(dataMOD$delta,
                                      res$upBin, res$lowBin, Palette)

            stripes_plan = matrix(c(
                # "margin_top",
                            1:nStorylines
                # "margin_bottom"
            ))
            stripes = bring_grass(verbose=verbose)
            stripes = plan_of_herd(stripes, stripes_plan,
                                   verbose=verbose)
            
            # stripes = add_sheep(stripes,
            #                     sheep=void(),
            #                     id="margin_top",
            #                     height=0,
            #                     verbose=verbose)
            # stripes = add_sheep(stripes,
            #                     sheep=void(),
            #                     id="margin_bottom",
            #                     height=0,
            #                     verbose=verbose)
                
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
                                    id=k,
                                    # label="align",
                                    height=1,
                                    verbose=verbose)
            }

            block = add_sheep(block,
                              sheep=stripes,
                              id="stripes",
                              # label="align",
                              height=variable_stripes_height,
                              width=variable_graph_width,
                              verbose=verbose)

            # spread = panel_spread(verbose=verbose)
            spread = contour()
            block = add_sheep(block,
                              sheep=spread,
                              id="spread",
                              # label="align",
                              height=variable_spread_height,
                              width=variable_graph_width,
                              verbose=verbose)

            # signe = panel_signe(verbose=verbose)
            signe = contour()
            block = add_sheep(block,
                              sheep=signe,
                              id="signe",
                              # label="align",
                              height=variable_signe_height,
                              width=variable_graph_width,
                              verbose=verbose)

            block = add_sheep(block,
                              sheep=axis,
                              id="axis",
                              height=variable_axis_height,
                              width=variable_graph_width,
                              verbose=verbose)

            block = add_sheep(block,
                              sheep=void(),
                              id="void",
                              height=variable_void_height,
                              width=1,
                              verbose=verbose)

            herd = add_sheep(herd,
                             sheep=block,
                             id=variable,
                             height=block_height,
                             width=width,
                             verbose=verbose)
        }
        id_letter = id_letter + nVariables_serie
        

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

        # res = return_to_sheepfold(herd,
        #                           page_margin=page_margin,
        #                           paper_size="A4",
        #                           hjust=0, vjust=1,
        #                           verbose=verbose)
        
        # plot = res$plot
        # paper_size = res$paper_size

        # filename = paste0(code, "_projection_datasheet_1.pdf")

        # if (!(file.exists(figdir))) {
        #     dir.create(figdir, recursive=TRUE)
        # }
        # ggplot2::ggsave(plot=plot,
        #                 path=figdir,
        #                 filename=filename,
        #                 width=paper_size[1],
        #                 height=paper_size[2], units='cm',
        #                 dpi=300,
        #                 device=cairo_pdf)


## 2. PAGE 2 _________________________________________________________
        herd = bring_grass(verbose=verbose)
        herd = plan_of_herd(herd, plan_2,
                            verbose=verbose)

        for (j in 1:nTypes) {
            type = Types[j]
            type_plan = Types_plan[j]

            print(type)
            delta_plan = matrix(c("title",
                                  "graph"),
                                ncol=1, byrow=TRUE)
            delta = bring_grass(verbose=verbose)
            delta = plan_of_herd(delta, delta_plan,
                                 verbose=verbose)
            
            titleTeX = TeX(paste0("(", letters[id_letter+j], ") ",
                                  type))

            title = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) + 
                annotate("text",
                         x=0,
                         y=1,
                         label=titleTeX,
                         size=3, hjust=0, vjust=1,
                         color=IPCCgrey25) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            delta = add_sheep(delta,
                            sheep=title,
                            id="title",
                            height=delta_title_height,
                            verbose=verbose)


            Variables_delta_type = Variables_delta[TypesALL == type]
            nVariables_delta_type = length(Variables_delta_type)

            graph_plan = matrix(c(Variables_delta_type, "void"),
                                ncol=nVariables_delta_type+1, byrow=TRUE)
            graph = bring_grass(verbose=verbose)
            graph = plan_of_herd(graph, graph_plan,
                                 verbose=verbose)

            for (k in 1:nVariables_delta_type) {
                variable = Variables_delta_type[k]

                dx_bar = 0.6
                dColor = 1
                
                limits_x = c(-dx_bar*1.3, 1+(nDeltaHorizon-1)*(1+dx_bar)+dx_bar*1.3)
                
                bar = ggplot() + theme_IPCC(is_panel.background=TRUE,
                                            is_axis.ticks.y=FALSE,
                                            axis.text.y_size=8,
                                            axis.text.y_margin=
                                                margin(t=0.5, r=-0.6,
                                                       b=0, l=0,
                                                unit="mm"),
                                            is_axis.line.x=FALSE,
                                            is_axis.ticks.x=FALSE,
                                            is_axis.text.x=FALSE) +
                    theme(plot.margin=margin(t=2, r=3,
                                             b=4, l=0, "mm"))
                
                for (h in 1:nDeltaHorizon) {
                    H = deltaHorizon[h]
                    variable_H = paste0(variable, "_", H)
                    limits_bar = c((h-1)*(1+dx_bar),
                                   1+(h-1)*(1+dx_bar))

                    Ok = metaEX_criteria$variable_en == variable_H
                    Palette = metaEX_criteria$palette[Ok]
                    Palette = unlist(strsplit(Palette, " "))
                    nColor = length(Palette)

                    unit = metaEX_criteria$unit_fr[Ok]
                    
                    Delta_variable_H = dataEX_criteria_code[[variable_H]]
                    Chain_variable_H = dataEX_criteria_code$Chain
                    

                    minDelta_variable_H =
                        dataEX_criteria_prob_code[[paste0("min_", variable_H)]]
                    maxDelta_variable_H =
                        dataEX_criteria_prob_code[[paste0("max_", variable_H)]]
                    nDelta_variable_H = length(Delta_variable_H)
                    
                    tmp = dplyr::tibble(Chain=rep(Chain_variable_H, each=2),
                                        x=rep(limits_bar, nDelta_variable_H),
                                        y=rep(Delta_variable_H, each=2))
                    
                    bar = bar +
                        annotate("rect",
                                 xmin=limits_bar[1], xmax=limits_bar[2],
                                 ymin=minDelta_variable_H,
                                 ymax=maxDelta_variable_H,
                                 fill="white",
                                 color=NA)

                    if (minDelta_variable_H <= 0) {
                        bar = bar +
                            annotate("rect",
                                     xmin=limits_bar[1], xmax=limits_bar[2],
                                     ymin=minDelta_variable_H,
                                     ymax=min(c(0, maxDelta_variable_H)),
                                     fill=Palette[1+dColor],
                                     alpha=0.2,
                                     color=NA) +
                            geom_line(data=dplyr::filter(tmp, y <= 0),
                                      aes(x=x, y=y, group=Chain),
                                      color=Palette[1+dColor],
                                      linewidth=0.3,
                                      alpha=0.07,
                                      lineend="round")
                    }
                    if (0 <= maxDelta_variable_H) {
                        bar = bar +
                            annotate("rect",
                                     xmin=limits_bar[1], xmax=limits_bar[2],
                                     ymin=max(c(0, minDelta_variable_H)),
                                     ymax=maxDelta_variable_H,
                                     fill=Palette[nColor-dColor],
                                     alpha=0.2,
                                     color=NA) +
                            geom_line(data=dplyr::filter(tmp, 0 <= y),
                                      aes(x=x, y=y, group=Chain),
                                      color=Palette[nColor-dColor],
                                      linewidth=0.3,
                                      alpha=0.07,
                                      lineend="round")
                    }
                }
                
                bar = bar +
                    annotate("line",
                             x=limits_x, y=c(0, 0),
                             color=IPCCgrey60, size=0.35,
                             lineend="square")

                for (h in 1:nDeltaHorizon) {
                    H = deltaHorizon[h]
                    variable_H = paste0(variable, "_", H)
                    limits_bar = c((h-1)*(1+dx_bar),
                                   1+(h-1)*(1+dx_bar))

                    Delta_variable_H = dataEX_criteria_code[[variable_H]]
                    medDelta_variable_H = median(Delta_variable_H, na.rm=TRUE)

                    if (medDelta_variable_H <= 0) {
                        bar = bar +
                            annotate("line",
                                     x=limits_bar, y=rep(medDelta_variable_H, 2),
                                     color=Palette[1+dColor],
                                     linewidth=0.4,
                                     alpha=0.7,
                                     lineend="round")
                    } else {
                        bar = bar +
                            annotate("line",
                                     x=limits_bar, y=rep(medDelta_variable_H, 2),
                                     color=Palette[nColor-dColor],
                                     linewidth=0.4,
                                     alpha=0.7,
                                     lineend="round")
                    }
                }

                
                Delta_min =
                    unlist(dplyr::select(dataEX_criteria_prob,
                                         dplyr::all_of(paste0("min_", variable,
                                                              "_", deltaHorizon))),
                           use.names=FALSE)

                Delta_max =
                    unlist(dplyr::select(dataEX_criteria_prob,
                                         dplyr::all_of(paste0("max_", variable,
                                                              "_", deltaHorizon))),
                           use.names=FALSE)
                
                limits_y = c(min(Delta_min, na.rm=TRUE),
                             max(Delta_max, na.rm=TRUE))

                Labels = pretty(limits_y)
                Breaks = Labels
                
                # Labels = seq(round(limits_y[1]/20)-1,
                             # round(limits_y[2]/20)+1)*20
                # Labels =  Labels[limits_y[1] <= Labels &
                # Labels <= limits_y[2]]
                                
                get_label = function (x) {
                    if (nchar(unit) == 1) {
                        unitHTML = paste0("<span style='font-size:6pt'>", unit, "</span>")
                    } else {
                        if (x != 0) {
                            unit = paste0(unit, "s")
                        }
                        unitHTML = paste0("<br><sup style='font-size:6pt'>", unit, "</sup>")
                    }
                    
                    if (x < 0) {
                        paste0("<span style='color:", Palette[1+dColor], "'>",
                               "<b>", x, "</b>",
                               unitHTML,
                               "</span>")
                    } else if (x > 0) {
                        paste0("<span style='color:", Palette[nColor-dColor], "'>",
                               "<b>", x, "</b>",
                               unitHTML,
                               "</span>")
                    } else {
                        paste0("<span style=''>",
                               "<b>", x, "</b>",
                               unitHTML,
                               "</span>")
                    }
                }

                Labels = sapply(Labels, get_label)
                
                
                bar = bar +
                    scale_x_continuous(limits=limits_x,
                                       expand=c(0, 0)) +
                    scale_y_continuous(limits=limits_y,
                                       labels=Labels,
                                       breaks=Breaks,
                                       expand=c(0, 0))

                
                graph = add_sheep(graph,
                                  sheep=bar,
                                  id=variable,
                                  width=0.16,
                                  verbose=verbose)
            }
            
            graph = add_sheep(graph,
                              sheep=void(),
                              id="void",
                              width=0.1,
                              verbose=verbose)
            
            delta = add_sheep(delta,
                              sheep=graph,
                              id="graph",
                              height=delta_graph_height,
                              width=width,
                              verbose=verbose)
            
            herd = add_sheep(herd,
                             sheep=delta,
                             id=type_plan,
                             height=delta_height,
                             width=width,
                             verbose=verbose)
        }
        id_letter = id_letter + nTypes

        
        for (j in 1:nVariables_extreme) {
            variable = Variables_extreme[j]
            print(variable)
            
            title_text = Titles_extreme[j]
            subtitle_text = Subtitles_extreme[j]

            extreme_plan = matrix(c("title",
                                    "graph"),
                                  ncol=1, byrow=TRUE)
            extreme = bring_grass(verbose=verbose)
            extreme = plan_of_herd(extreme, extreme_plan,
                                   verbose=verbose)
            
            titleTeX = TeX(paste0("(", letters[id_letter+j], ") ",
                                  convert2TeX(variable, bold=TRUE),
                                  " $-$ ", title_text))

            title = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) + 
                annotate("text",
                         x=0,
                         y=1,
                         label=titleTeX,
                         size=3, hjust=0, vjust=1,
                         color=IPCCgrey25) +
                annotate("text",
                         x=0.04,
                         y=0,
                         label=subtitle_text,
                         size=2.5, hjust=0, vjust=0,
                         color=IPCCgrey25) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            extreme = add_sheep(extreme,
                                sheep=title,
                                id="title",
                                height=extreme_title_height,
                                verbose=verbose)

            extreme = add_sheep(extreme,
                                sheep=contour(),
                                id="graph",
                                height=extreme_graph_height,
                                verbose=verbose)

            
            herd = add_sheep(herd,
                             sheep=extreme,
                             id=variable,
                             height=extreme_height,
                             width=extreme_width,
                             verbose=verbose)
        }
        id_letter = id_letter + nVariables_extreme


        
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

        filename = paste0(code, "_projection_datasheet_2.pdf")

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
