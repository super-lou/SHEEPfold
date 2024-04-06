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
                                     Colors_light,
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
        "delta", 
        "QJXA-10",
        "VCN10-5", 
        "foot"
    ), ncol=1, byrow=TRUE)

    extreme_height = 6
    extreme_title_height = 0.12
    extreme_H_height = 0.88

    extreme_leg_width = 0.5
    extreme_void_width = 0.1
    extreme_voidex_width = 0.15
    
    extreme_H_title_height = 0.1
    extreme_H_sL_height = 0.9
    extreme_H_sL_n_height = 2
    extreme_H_sL_n_text_height = 1
    extreme_H_sL_delta_height = 4.5
    extreme_H_sL_delta_text_height = 1

    delta_height = height - extreme_height*2 - foot_height
    delta_title_height = 0.1
    delta_type_height = 0.9
    delta_type_title_width = 0.065
    delta_type_bar_width = 0.20
    delta_type_bar_title_height = 0.08
    delta_type_bar_plot_height = 0.94


    
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
    
    Horizons_medQJ = c("\\textbf{Période de référence} 1976-2005",
                       "\\textbf{Horizon moyen} 2041-2070",
                       "\\textbf{Horizon lointain} 2070-2099")
    Horizons_delta = c("période de référence 1976-2005",
                       "horizon moyen 2041-2070",
                       "horizon lointain 2070-2099")
    Horizons_extreme = c("\\textbf{1976-2005}",
                         "\\textbf{Horizon moyen 2041-2070}",
                         "\\textbf{Horizon lointain 2070-2099}")
    
    Variables_medQJ = c("medQJ_H0", "medQJ_H2", "medQJ_H3")
    nVariables_medQJ = length(Variables_medQJ)
    
    Variables_serie = unique(metaEX_serie$variable_en)
    Variables_serie = Variables_serie[!grepl("medQJ", Variables_serie)]
    nVariables_serie = length(Variables_serie)
    
    Storylines = names(Colors)
    nStorylines = length(Storylines)


    Titles_extreme = c("Crues extrèmes de période de retour 10 ans",
                       "Étiages extrèmes de période de retour 5 ans")
    Variables_extreme = c("QJXA-10", "VCN10-5")
    nVariables_extreme = length(Variables_extreme)
    rp_Variables_extreme = c(10, 5)
    div_Variables_extreme = 30/rp_Variables_extreme
    limits_bar_y = list(c(0, 100), c(-100, 0))

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
    Types_short = c("HF", "MF", "LF")
    
    dataEX_criteria = dataEX_criteria[, !grepl("[_]H1", names(dataEX_criteria))]
    deltaHorizon = c("H2", "H3")
    nDeltaHorizon = length(deltaHorizon)


    x_10dot = c(0.48, 0.59, 0.27, 0.41, 0.66, 0.33, 0.55, 0.52, 0.77, 0.24)
    y_10dot = c(0.44, 0.60, 0.52, 0.65, 0.36, 0.29, 0.78, 0.22, 0.53, 0.72)

    x_5dot = c(0.48, 0.59, 0.41, 0.27, 0.69)
    y_5dot = c(0.44, 0.60, 0.65, 0.52, 0.41)
    
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
        dataEX_criteria_prob_code =
            dataEX_criteria_prob[dataEX_criteria_prob$code == code,]
            
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
            
            Colors_tmp = Colors 
            names(Colors_tmp) = paste0(names(Colors_tmp), "|median")
            
            medQJ = panel_spaghetti(dataMOD,
                                    Colors_tmp,
                                    title=paste0("(", letters[id_letter+j], ") Régime hydrologique"),
                                    unit="m^{3}.s^{-1}",
                                    subtitle=Horizons_medQJ[j],
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
                         color=IPCCgrey23) +
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
                         width=width,
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

        delta_plan = matrix(c("title", Types_short),
                            ncol=1, byrow=TRUE)
        delta = bring_grass(verbose=verbose)
        delta = plan_of_herd(delta, delta_plan,
                             verbose=verbose)

        text = paste0("(", letters[id_letter+1], ") ",
                      "Changement sur l'", Horizons_delta[2], " (à gauche) et ",
                      "l'", Horizons_delta[3], " (à droite) par rapport à la ", Horizons_delta[1])
        
        title = ggplot() + theme_void() +
            theme(plot.margin=margin(t=0, r=0,
                                     b=0, l=0, "mm")) + 
            annotate("text",
                     x=0,
                     y=1,
                     label=text,
                     size=3, hjust=0, vjust=1,
                     color=IPCCgrey23) +
            scale_x_continuous(limits=c(0, 1),
                               expand=c(0, 0)) +
            scale_y_continuous(limits=c(0, 1),
                               expand=c(0, 0))
        
        delta = add_sheep(delta,
                          sheep=title,
                          id="title",
                          height=delta_title_height,
                          verbose=verbose)
        
        for (j in 1:nTypes) {
            type = Types[j]
            type_short = Types_short[j]

            print(type)

            Variables_delta_type = Variables_delta[TypesALL == type]
            nVariables_delta_type = length(Variables_delta_type)

            # print(Variables_delta_type)

            grepl_first = function (pattern, table) {
                which(grepl(get_regexp(pattern), table))[1]
            }
            Ok = sapply(Variables_delta_type, grepl_first,
                        table=metaEX_criteria$variable_en)
            
            Units = metaEX_criteria$unit_fr[Ok]
            if (all(nchar(Units) == 1) | all(nchar(Units) != 1)) {
                delta_type_void_width = 0.09
            } else {
                delta_type_void_width = 0
            }
            Variables_delta_type =
                c(Variables_delta_type[nchar(Units) == 1],
                  Variables_delta_type[nchar(Units) > 1])
            
            Variables_delta_type_display =
                gsub("(delta)|([{])|([}])", "", Variables_delta_type)
            
            delta_type_plan = matrix(c("title", Variables_delta_type_display, "void"),
                                     ncol=nVariables_delta_type+2, byrow=TRUE)
            delta_type = bring_grass(verbose=verbose)
            delta_type = plan_of_herd(delta_type, delta_type_plan,
                                      verbose=verbose)

            
            titleTeX = TeX(paste0("\\textbf{", type, "}"))

            title = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0,
                                         b=0, l=0, "mm")) + 
                annotate("text",
                         x=0,
                         y=0.5,
                         label=titleTeX,
                         size=3, hjust=0.5, vjust=1,
                         color=IPCCgrey23, angle=90) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            delta_type = add_sheep(delta_type,
                                   sheep=title,
                                   id="title",
                                   width=delta_type_title_width,
                                   verbose=verbose)

            delta_type = add_sheep(delta_type,
                                   sheep=void(),
                                   id="void",
                                   width=delta_type_void_width,
                                   verbose=verbose)

            unit_save = ""
            
            for (k in 1:nVariables_delta_type) {
                variable = Variables_delta_type[k]
                variable_display = Variables_delta_type_display[k]
                
                # print(variable)
                # print(variable_clean)
                
                dx_bar = 1.1
                dx_sL = 1.1
                dColor = 1
                
                limits_x = c(-dx_bar*0.7 - dx_sL, (nDeltaHorizon-1)*dx_bar +
                                                  nDeltaHorizon*(1+dx_sL) +
                                                  dx_bar*0.7)


                bar_plan = matrix(c("title",
                                    "plot"),
                                  ncol=1, byrow=TRUE)
                bar = bring_grass(verbose=verbose)
                bar = plan_of_herd(bar, bar_plan,
                                   verbose=verbose)

                titleTeX = TeX(convert2TeX(variable_display, bold=TRUE))
                
                title = ggplot() + theme_void() +
                    theme(plot.margin=margin(t=0, r=0,
                                             b=0, l=0, "mm")) + 
                    annotate("text",
                             x=0.5,
                             y=0.96,
                             label=titleTeX,
                             size=2.5, hjust=0.5, vjust=1,
                             color=IPCCgrey40) +
                    annotate("line",
                             # x=c(0.07, 0.86),
                             x=c(0, 1),
                             y=c(0.08, 0.08),
                             linewidth=0.4, color=IPCCgrey40,
                             lineend="round") +
                    scale_x_continuous(limits=c(0, 1),
                                       expand=c(0, 0)) +
                    scale_y_continuous(limits=c(0, 1),
                                       expand=c(0, 0))
                
                bar = add_sheep(bar,
                                sheep=title,
                                id="title",
                                label=paste0("align_", variable_display),
                                height=delta_type_bar_title_height, 
                                verbose=verbose)
                
                plot = ggplot() + 
                    theme(plot.margin=margin(t=1, r=3,
                                             b=3, l=0, "mm"))
                
                
                for (h in 1:nDeltaHorizon) {
                    H = deltaHorizon[h]
                    variable_H = paste0(variable, "_", H)
                    limits_bar = c((h-1)*(1+dx_bar+dx_sL),
                                   1+(h-1)*(1+dx_bar+dx_sL))

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
                    
                    plot = plot +
                        annotate("rect",
                                 xmin=limits_bar[1], xmax=limits_bar[2],
                                 ymin=minDelta_variable_H,
                                 ymax=maxDelta_variable_H,
                                 fill="white",
                                 color=NA)

                    if (minDelta_variable_H <= 0) {
                        plot = plot +
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
                    if (0 < maxDelta_variable_H) {
                        plot = plot +
                            annotate("rect",
                                     xmin=limits_bar[1], xmax=limits_bar[2],
                                     ymin=max(c(0, minDelta_variable_H)),
                                     ymax=maxDelta_variable_H,
                                     fill=Palette[nColor-dColor],
                                     alpha=0.2,
                                     color=NA) +
                            geom_line(data=dplyr::filter(tmp, 0 < y),
                                      aes(x=x, y=y, group=Chain),
                                      color=Palette[nColor-dColor],
                                      linewidth=0.3,
                                      alpha=0.07,
                                      lineend="round")
                    }
                }
                
                if (k == 1 | unit != unit_save) {
                    show_y_axis = TRUE
                    if (nchar(unit) == 1) {
                        times = 1.3
                    } else {
                        times = 1.45
                    }
                } else {
                    show_y_axis = FALSE
                    times = 1
                }
                unit_save = unit
                plot = plot +
                    theme_IPCC(is_panel.background=TRUE,
                               is_plot.title=TRUE,
                               plot.title_size=8,
                               is_axis.ticks.y=FALSE,
                               is_axis.text.y=show_y_axis,
                               axis.text.y_size=8,
                               axis.text.y_margin=
                                   margin(t=0.5, r=-0.6,
                                          b=0, l=0,
                                          unit="mm"),
                               is_axis.line.x=FALSE,
                               is_axis.ticks.x=FALSE,
                               is_axis.text.x=FALSE)
                
                
                plot = plot +
                    annotate("line",
                             x=limits_x, y=c(0, 0),
                             color=IPCCgrey60, size=0.35,
                             lineend="square")

                for (h in 1:nDeltaHorizon) {
                    H = deltaHorizon[h]
                    variable_H = paste0(variable, "_", H)
                    limits_bar = c((h-1)*(1+dx_bar+dx_sL),
                                   1+(h-1)*(1+dx_bar+dx_sL))

                    plot_y = c()
                    plot_color = c()
                    
                    for (storyline in Storylines) {
                        color = Colors[names(Colors) == storyline]
                        Delta_variable_H =
                            dplyr::filter(dataEX_criteria_code,
                                          climateChain == storyline)[[variable_H]]
                        medDelta_variable_H = median(Delta_variable_H, na.rm=TRUE)
    
                        plot_y = c(plot_y, medDelta_variable_H)
                        plot_color = c(plot_color, color)
                    }
                    
                    n = length(plot_y)
                    tmp = dplyr::tibble(id=rep(Storylines, each=2),
                                        x=rep(c(limits_bar[1],
                                                limits_bar[2]+dx_sL/2), n),
                                        y=rep(plot_y, each=2))
                    
                    plot = plot +
                        geom_line(data=dplyr::filter(tmp, y <= 0),
                                  aes(x=x, y=y, group=id), 
                                  color=Palette[1+dColor],
                                  linewidth=0.4,
                                  alpha=0.5,
                                  lineend="round") +
                        geom_line(data=dplyr::filter(tmp, 0 < y),
                                  aes(x=x, y=y, group=id), 
                                  color=Palette[nColor-dColor],
                                  linewidth=0.4,
                                  alpha=0.5,
                                  lineend="round") +
    
                        annotate("point",
                                 x=rep(limits_bar[2]+dx_sL/2, n),
                                 y=plot_y,
                                 color=IPCCgrey97,
                                 size=2.2,
                                 shape=20) +
                        annotate("point",
                                 x=rep(limits_bar[2]+dx_sL/2, n),
                                 y=plot_y,
                                 color=plot_color,
                                 size=1.2,
                                 shape=20)
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
                
                limits_y = c(-100, 100) 
                if (nchar(unit) == 1) {
                    Labels = c(-75, -50, -25, 0, 25, 50, 75)
                } else {
                    Labels = c(-90, -60, -30, 0, 30, 60, 90)
                }
                Breaks = Labels
                                
                get_label = function (x) {
                    if (nchar(unit) == 1) {
                        unitHTML = paste0("<span style='font-size:6pt'>",
                                          unit, "</span>")
                    } else {
                        if (x != 0) {
                            unit = paste0(unit, "s")
                        }
                        unitHTML = paste0("<span style='font-size:6pt'> ",
                                          unit, "</span>")
                    }
                    
                    if (x < 0) {
                        paste0("<span style='color:", Palette[1+dColor], "'>",
                               "<b>", x, "</b>",
                               unitHTML,
                               "</span>")
                    } else if (x > 0) {
                        paste0("<span style='color:", Palette[nColor-dColor], "'>",
                               "<b>+", x, "</b>",
                               unitHTML,
                               "</span>")
                    } else {
                        paste0("<span style=''>",
                               "<b>", x, "</b>",
                               "</span>")
                    }
                }

                Labels = sapply(Labels, get_label)
                
                
                plot = plot +
                    scale_x_continuous(limits=limits_x,
                                       expand=c(0, 0)) +
                    scale_y_continuous(limits=limits_y,
                                       labels=Labels,
                                       breaks=Breaks,
                                       expand=c(0, 0))

                bar = add_sheep(bar,
                                sheep=plot,
                                id="plot",
                                label=paste0("align_", variable_display),
                                height=delta_type_bar_plot_height, 
                                verbose=verbose)

                delta_type = add_sheep(delta_type,
                                  sheep=bar,
                                  id=variable_display,
                                  height=1,
                                  width=delta_type_bar_width*times,
                                  verbose=verbose)
            }
            delta = add_sheep(delta,
                              sheep=delta_type,
                              id=type_short,
                              height=delta_type_height,
                              width=width,
                              verbose=verbose)
        }
        herd = add_sheep(herd,
                         sheep=delta,
                         id="delta",
                         height=delta_height,
                         width=width,
                         verbose=verbose)
        ###
        # herd = add_sheep(herd,
        #                  sheep=contour(),
        #                  id="delta",
        #                  height=delta_height,
        #                  width=width,
        #                  verbose=verbose)
        ###
        
        
        id_letter = id_letter + 1

        
        for (j in 1:nVariables_extreme) {
            variable = Variables_extreme[j]
            div = div_Variables_extreme[j]
            rp = rp_Variables_extreme[j]

            x_dot = get(paste0("x_", rp, "dot"))
            y_dot = get(paste0("y_", rp, "dot"))

            dp_x = 0.7
            dp_y = 0.15
            
            print(variable)
            
            title_text = Titles_extreme[j]

            extreme_plan = matrix(c(
                "title", "title", "title", "title", "title", "title", "title",
                "Hleg", "H0", "void1", "H2", "void2", "H3", "void3"),
                ncol=7, byrow=TRUE)
            extreme = bring_grass(verbose=verbose)
            extreme = plan_of_herd(extreme, extreme_plan,
                                   verbose=verbose)
            
            titleTeX = TeX(paste0("(", letters[id_letter+j], ") ",
                                  convert2TeX(variable, bold=TRUE),
                                  " $-$ ", title_text))

            title = ggplot() + theme_void() +
                theme(plot.margin=margin(t=3, r=0, b=0, l=0, "mm")) + 
                annotate("text",
                         x=0,
                         y=1,
                         label=titleTeX,
                         size=3, hjust=0, vjust=1,
                         color=IPCCgrey23) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            extreme = add_sheep(extreme,
                                sheep=title,
                                id="title",
                                height=extreme_title_height,
                                verbose=verbose)

            void_line = void(panel.background_fill=IPCCgrey97,
                             plot.margin=margin(t=0, r=0,
                                                b=0, l=0, "mm")) +
                annotate("line",
                         x=0.5,
                         y=c(0.1, 0.884),
                         linewidth=0.25, color=IPCCgrey85) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            extreme = add_sheep(extreme,
                                sheep=void_line,
                                id="void1",
                                height=extreme_H_height,
                                width=extreme_void_width,
                                verbose=verbose)
            extreme = add_sheep(extreme,
                                sheep=void_line,
                                id="void2",
                                height=extreme_H_height,
                                width=extreme_void_width,
                                verbose=verbose)
            
            extreme = add_sheep(extreme,
                                sheep=
                                    void(panel.background_fill=IPCCgrey97,
                                         plot.margin=margin(t=0, r=0,
                                                            b=0, l=0, "mm")),
                                id="void3",
                                height=extreme_H_height,
                                width=extreme_voidex_width,
                                verbose=verbose)
            
### Hleg _____________________________________________________________
            extreme_H_plan = matrix(c("title",
                                       "0"),
                                     ncol=1, byrow=TRUE)
            extreme_H = bring_grass(verbose=verbose)
            extreme_H = plan_of_herd(extreme_H, extreme_H_plan,
                                      verbose=verbose)
            
            extreme_H = add_sheep(extreme_H,
                                  sheep=void(panel.background_fill=IPCCgrey97,
                                             plot.margin=margin(t=0, r=0,
                                                                b=0, l=0, "mm")),
                                  id="title",
                                  height=extreme_H_title_height,
                                  verbose=verbose)

            extreme_sL_plan = matrix(c("n",
                                       "delta"),
                                     ncol=1, byrow=TRUE)
            extreme_sL = bring_grass(verbose=verbose)
            extreme_sL = plan_of_herd(extreme_sL, extreme_sL_plan,
                                      verbose=verbose)

            text = ggplot() + theme_void() +
                theme(panel.background=element_rect(fill=IPCCgrey97, color=NA),
                      plot.margin=margin(t=0, r=0, b=0, l=0, "mm")) + 
                annotate("text",
                         x=0.48,
                         y=0.52,
                         label=TeX(paste0("\\textbf{FRÉQUENCE}")),
                         size=2.2, hjust=0.5, vjust=0.5, angle=90,
                         color=IPCCgrey35) +
                annotate("text",
                         x=0.75,
                         y=0.52,
                         label=TeX(paste0("\\textbf{par ", rp, " ans}")),
                         size=2.2, hjust=0.5, vjust=0.5, angle=90,
                         color=IPCCgrey35) +
                annotate("line",
                         x=0.98,
                         y=c(0.08, 0.95),
                         linewidth=0.3, color=IPCCgrey85) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            extreme_sL = add_sheep(extreme_sL,
                                   sheep=text,
                                   id="n",
                                   height=extreme_H_sL_n_height+
                                       extreme_H_sL_n_text_height,
                                   verbose=verbose)

            text = ggplot() + theme_void() +
                theme(panel.background=element_rect(fill=IPCCgrey97, color=NA),
                      plot.margin=margin(t=0, r=0, b=0, l=0, "mm")) + 
                annotate("text",
                         x=0.48,
                         y=0.57,
                         label=TeX(paste0("\\textbf{Changement}")),
                         size=2.2, hjust=0.5, vjust=0.5, angle=90,
                         color=IPCCgrey35) +
                annotate("text",
                         x=0.75,
                         y=0.57,
                         label=TeX(paste0("\\textbf{d'INTENSITÉ}")),
                         size=2.2, hjust=0.5, vjust=0.5, angle=90,
                         color=IPCCgrey35) +
                annotate("line",
                         x=0.98,
                         y=c(0.18, 0.95),
                         linewidth=0.3, color=IPCCgrey85) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            extreme_sL = add_sheep(extreme_sL,
                                   sheep=text,
                                   id="delta",
                                   height=extreme_H_sL_delta_height+
                                       extreme_H_sL_delta_text_height,
                                   verbose=verbose)
            
            extreme_H = add_sheep(extreme_H,
                                   sheep=extreme_sL,
                                   id=0,
                                   height=extreme_H_sL_height,
                                   verbose=verbose)
            extreme = add_sheep(extreme,
                                sheep=extreme_H,
                                id="Hleg",
                                height=extreme_H_height,
                                width=extreme_leg_width,
                                verbose=verbose)

            
### H0 _______________________________________________________________
            extreme_H_plan = matrix(c("title",
                                       "0"),
                                     ncol=1, byrow=TRUE)
            extreme_H = bring_grass(verbose=verbose)
            extreme_H = plan_of_herd(extreme_H, extreme_H_plan,
                                      verbose=verbose)


            title = ggplot() + theme_void() +
                theme(panel.background=element_rect(fill=IPCCgrey97, color=NA),
                      plot.margin=margin(t=0, r=0, b=0, l=0, "mm")) + 
                annotate("text",
                         x=0.5,
                         y=0.35,
                         label=TeX(Horizons_extreme[1]),
                         size=3, hjust=0.5, vjust=0.5,
                         color=IPCCgrey35) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            extreme_H = add_sheep(extreme_H,
                                  sheep=title,
                                  id="title",
                                  height=extreme_H_title_height,
                                  verbose=verbose)


            extreme_sL_plan = matrix(c("n",
                                       "n_text",
                                       "axis",
                                       "void"),
                                     ncol=1, byrow=TRUE)
            extreme_sL = bring_grass(verbose=verbose)
            extreme_sL = plan_of_herd(extreme_sL, extreme_sL_plan,
                                      verbose=verbose)


            plot = ggplot() + theme_void() +
                theme(panel.background=element_rect(fill=IPCCgrey97,
                                                    color=NA),
                      plot.margin=margin(t=0, r=0, b=0, l=0, "mm"))
            plot = plot +
                annotate("point",
                         x=x_dot[2:rp],
                         y=y_dot[2:rp],
                         size=1.35,
                         fill=IPCCgrey85,
                         color="transparent",
                         shape=21)
            plot = plot +
                annotate("point",
                         x=x_dot[1],
                         y=y_dot[1],
                         size=1.35,
                         fill=IPCCgrey50,
                         color="transparent",
                         shape=21) +
                scale_x_continuous(limits=c(-dp_x, 1+dp_x),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(-dp_y, 1+dp_y),
                                   expand=c(0, 0))
            extreme_sL = add_sheep(extreme_sL,
                                   sheep=plot,
                                   id="n",
                                   height=extreme_H_sL_n_height,
                                   verbose=verbose)

            text = ggplot() + theme_void() + 
                theme(panel.background=element_rect(fill=IPCCgrey97,
                                                    color=NA),
                      plot.margin=margin(t=0, r=0, b=0, l=0, "mm")) + 
                annotate("text",
                         x=0.5,
                         y=0.75,
                         label=TeX(paste0("\\textbf{1 fois}")),
                         size=3, hjust=0.5, vjust=0.5,
                         color=IPCCgrey50) +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            extreme_sL = add_sheep(extreme_sL,
                                   sheep=text,
                                   id="n_text",
                                   height=extreme_H_sL_n_text_height,
                                   verbose=verbose)
            get_labels = function (X) {
                up = X > 0
                X[X != 0] = paste0(X[X != 0], " %")
                X[up] = paste0("+", X[up])
                return (X)
            }

            axis = ggplot() + theme_void() + 
                theme(plot.background=element_rect(fill=IPCCgrey97,
                                                   color=NA),
                      panel.grid.major.y=element_line(color=IPCCgrey85,
                                                      size=0.25),
                      axis.text.y=element_text(color=IPCCgrey35,
                                               hjust=1, size=7,
                                               margin=margin(t=0, r=1,
                                                             b=0, l=0, "mm")),
                      plot.margin=margin(t=3, r=0, b=1, l=6, "mm")) + 
                
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=limits_bar_y[[j]],
                                   labels=get_labels,
                                   expand=c(0, 0))
            
            extreme_sL = add_sheep(extreme_sL,
                                   sheep=axis,
                                   id="axis",
                                   height=extreme_H_sL_delta_height,
                                   verbose=verbose)
            
            extreme_sL = add_sheep(extreme_sL,
                                   sheep=void(panel.background_fill=IPCCgrey97),
                                   id="void",
                                   height=extreme_H_sL_delta_text_height,
                                   verbose=verbose)
            
            extreme_H = add_sheep(extreme_H,
                                   sheep=extreme_sL,
                                   id=0,
                                   height=extreme_H_sL_height,
                                   verbose=verbose)
            
            extreme = add_sheep(extreme,
                                sheep=extreme_H,
                                id="H0",
                                height=extreme_H_height,
                                width=1,
                                verbose=verbose)


### HX _______________________________________________________________
            for (h in 1:nDeltaHorizon) {
                H = deltaHorizon[h]
                
                variable_H = paste0(variable, "_", H)

                variable_delta_H = paste0("delta", variable_H)
                Ok = metaEX_criteria$variable_en == variable_delta_H
                unit_delta = metaEX_criteria$unit_fr[Ok]

                variable_n_H = paste0("n", variable_H)
                Ok = metaEX_criteria$variable_en == variable_n_H
                unit_n = metaEX_criteria$unit_fr[Ok]
                

                extreme_H_plan = matrix(c(rep("title", nStorylines),
                                          1:nStorylines),
                                        ncol=nStorylines, byrow=TRUE)
                extreme_H = bring_grass(verbose=verbose)
                extreme_H = plan_of_herd(extreme_H, extreme_H_plan,
                                         verbose=verbose)

                title = ggplot() + theme_void() + 
                    theme(panel.background=element_rect(fill=IPCCgrey97,
                                                        color=NA),
                          plot.margin=margin(t=0, r=0, b=0, l=0, "mm")) + 
                    annotate("text",
                             x=0.5,
                             y=0.3,
                             label=TeX(Horizons_extreme[h+1]),
                             size=3, hjust=0.5, vjust=0.5,
                             color=IPCCgrey35) +
                    scale_x_continuous(limits=c(0, 1),
                                       expand=c(0, 0)) +
                    scale_y_continuous(limits=c(0, 1),
                                       expand=c(0, 0))
                
                extreme_H = add_sheep(extreme_H,
                                      sheep=title,
                                       id="title",
                                       height=extreme_H_title_height,
                                       verbose=verbose)
                
                
                for (k in 1:nStorylines) {
                    storyline = Storylines[k]
                    color = Colors[names(Colors) == storyline]
                    color_light = Colors_light[names(Colors_light) == storyline]

                    Delta_variable_n_H =
                        dplyr::filter(dataEX_criteria_code,
                                      climateChain == storyline)[[variable_n_H]]
                    medDelta_variable_n_H = median(Delta_variable_n_H, na.rm=TRUE)
                    minDelta_variable_n_H = min(Delta_variable_n_H, na.rm=TRUE)
                    maxDelta_variable_n_H = max(Delta_variable_n_H, na.rm=TRUE)
                    
                    nH_r0 = round(medDelta_variable_n_H / div, 0)
                    nH_r1 =
                        sprintf("%.1f", round(medDelta_variable_n_H / div, 1))
                    nH_min_r1 =
                        sprintf("%.1f", round(minDelta_variable_n_H / div, 1))
                    nH_max_r1 =
                        sprintf("%.1f", round(maxDelta_variable_n_H / div, 1))

                    Delta_variable_delta_H =
                        dplyr::filter(dataEX_criteria_code,
                                      climateChain ==
                                      storyline)[[variable_delta_H]]
                    
                    medDelta_variable_delta_H =
                        median(Delta_variable_delta_H, na.rm=TRUE)
                    minDelta_variable_delta_H =
                        min(Delta_variable_delta_H, na.rm=TRUE)
                    maxDelta_variable_delta_H =
                        max(Delta_variable_delta_H, na.rm=TRUE)


                    if (medDelta_variable_delta_H > 0) {
                        deltaH_r1 =
                            paste0("+",
                                   sprintf("%.1f",
                                           round(medDelta_variable_delta_H, 1)))
                    } else {
                        deltaH_r1 =
                            sprintf("%.1f", round(medDelta_variable_delta_H, 1))
                    }
                    
                    extreme_sL_plan = matrix(c("n",
                                               "n_text",
                                               "delta",
                                               "delta_text"),
                                             ncol=1, byrow=TRUE)
                    extreme_sL = bring_grass(verbose=verbose)
                    extreme_sL = plan_of_herd(extreme_sL, extreme_sL_plan,
                                              verbose=verbose)

                    plot = ggplot() + theme_void() +
                        theme(panel.background=element_rect(fill=IPCCgrey97,
                                                            color=NA),
                              plot.margin=margin(t=0, r=0, b=0, l=0, "mm"))
                    if (nH_r0 < rp) {
                        plot = plot +
                            annotate("point",
                                     x=x_dot[(nH_r0+1):rp],
                                     y=y_dot[(nH_r0+1):rp],
                                     size=1.35,
                                     fill=color_light,
                                     color="transparent",
                                     shape=21)
                    }
                    plot = plot +
                        annotate("point",
                                 x=x_dot[1:nH_r0],
                                 y=y_dot[1:nH_r0],
                                 size=1.35,
                                 fill=color,
                                 color="transparent",
                                 shape=21) +
                        scale_x_continuous(limits=c(-dp_x, 1+dp_x),
                                           expand=c(0, 0)) +
                        scale_y_continuous(limits=c(-dp_y, 1+dp_y),
                                           expand=c(0, 0))
                    extreme_sL = add_sheep(extreme_sL,
                                           sheep=plot,
                                           id="n",
                                           height=extreme_H_sL_n_height,
                                           verbose=verbose)

                    text = ggplot() + theme_void() + 
                        theme(panel.background=element_rect(fill=IPCCgrey97,
                                                            color=NA),
                              plot.margin=margin(t=0, r=0, b=0, l=0, "mm")) + 
                        annotate("text",
                                 x=0.5,
                                 y=0.75,
                                 label=TeX(paste0("\\textbf{", nH_r1, " fois}")),
                                 size=3, hjust=0.5, vjust=0.5,
                                 color=color) +
                        annotate("text",
                                 x=0.5,
                                 y=0.3,
                                 label=paste0("(", nH_min_r1, " - ",
                                              nH_max_r1, ")"),
                                 size=2, hjust=0.5, vjust=0.5,
                                 color=color) +
                        scale_x_continuous(limits=c(0, 1),
                                           expand=c(0, 0)) +
                        scale_y_continuous(limits=c(0, 1),
                                           expand=c(0, 0))
                    extreme_sL = add_sheep(extreme_sL,
                                           sheep=text,
                                           id="n_text",
                                           height=extreme_H_sL_n_text_height,
                                           verbose=verbose)

                    plot = ggplot() + theme_void() + 
                        theme(plot.background=element_rect(fill=IPCCgrey97,
                                                            color=NA),
                              panel.grid.major.y=element_line(color=IPCCgrey85,
                                                              size=0.25),
                              plot.margin=margin(t=3, r=0, b=1, l=0, "mm")) + 
                        annotate("rect",
                                 xmin=0.45, xmax=0.55, 
                                 ymin=0, ymax=medDelta_variable_delta_H,
                                 color=NA, fill=color) +
                        
                        annotate("line",
                                 x=c(0.5, 0.5), 
                                 y=c(max(c(minDelta_variable_delta_H,
                                           limits_bar_y[[j]][1])),
                                     min(c(maxDelta_variable_delta_H,
                                           limits_bar_y[[j]][2]))),
                                 color=color_light,
                                 linewidth=0.4)
                    
                    if (minDelta_variable_delta_H >= limits_bar_y[[j]][1]) {
                        plot = plot +
                            annotate("line",
                                     x=c(0.475, 0.525), 
                                     y=c(minDelta_variable_delta_H,
                                         minDelta_variable_delta_H),
                                     color=color_light,
                                     linewidth=0.4)
                    }
                    if (maxDelta_variable_delta_H <= limits_bar_y[[j]][2]) {
                        plot = plot +
                            annotate("line",
                                     x=c(0.475, 0.525), 
                                     y=c(maxDelta_variable_delta_H,
                                         maxDelta_variable_delta_H),
                                     color=color_light,
                                     linewidth=0.4)
                    }
                    plot = plot +
                        scale_x_continuous(limits=c(0, 1),
                                           expand=c(0, 0)) +
                        scale_y_continuous(limits=limits_bar_y[[j]],
                                           expand=c(0, 0))
                    
                    extreme_sL = add_sheep(extreme_sL,
                                           sheep=plot,
                                           id="delta",
                                           height=extreme_H_sL_delta_height,
                                           verbose=verbose)


                    text = ggplot() + theme_void() + 
                        theme(panel.background=element_rect(fill=IPCCgrey97,
                                                            color=NA),
                              plot.margin=margin(t=0, r=0, b=0, l=0, "mm")) + 
                        annotate("text",
                                 x=0.5,
                                 y=0.6,
                                 label=TeX(paste0("\\textbf{", deltaH_r1, " %}")),
                                 size=3, hjust=0.5, vjust=0.5,
                                 color=color) +
                        scale_x_continuous(limits=c(0, 1),
                                           expand=c(0, 0)) +
                        scale_y_continuous(limits=c(0, 1),
                                           expand=c(0, 0))
                    extreme_sL = add_sheep(extreme_sL,
                                           sheep=text,
                                           id="delta_text",
                                           height=extreme_H_sL_delta_text_height,
                                           verbose=verbose)

                    extreme_H = add_sheep(extreme_H,
                                          sheep=extreme_sL,
                                          id=k,
                                          height=extreme_H_sL_height,
                                          verbose=verbose)
                }
                extreme = add_sheep(extreme,
                                    sheep=extreme_H,
                                    id=H,
                                    height=extreme_H_height,
                                    width=nStorylines,
                                    verbose=verbose)
            }

            herd = add_sheep(herd,
                             sheep=extreme,
                             id=variable,
                             height=extreme_height,
                             width=width,
                             verbose=verbose)

            ###
            # herd = add_sheep(herd,
            #                  sheep=void(),
            #                  id=variable,
            #                  height=extreme_height,
            #                  width=width,
            #                  verbose=verbose)
            ###
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
                         width=width,
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
