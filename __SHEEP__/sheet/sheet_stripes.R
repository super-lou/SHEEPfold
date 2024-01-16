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


sheet_stripes = function (dataEX_serie,
                          metaEX_serie,
                          meta,
                          Projections,
                          prob=0.01,
                          palette_name="ground_8",
                          palette_reverse=FALSE,
                          period_reference=c(as.Date("1976-01-01"),
                                             as.Date("2005-12-31")),
                          icon_path=NULL,
                          figdir="",
                          Pages=NULL,
                          verbose=FALSE) {

    period_reference = as.Date(period_reference)
    
    paper_size = c(25, 16)
    page_margin = c(t=0.25, r=0.25, b=0.25, l=0.25)

    title_height = 1
    legend_height = 6

    variable_width = 7
    palette_width = 3.5
    legend_width = 14
    
    climateChain = unique(Projections$climateChain)
    climateChain = climateChain[climateChain != "SAFRAN"]
    nClimateChain = length(climateChain)

    stripes_height =
        paper_size[1] - page_margin["t"] - page_margin["b"] - title_height - legend_height
    stripes_height = stripes_height / nClimateChain

    plan = matrix(c("title",
                    climateChain,
                    "variable",
                    
                    "title",
                    climateChain,
                    "palette",

                    "title",
                    climateChain,
                    "legend"),
                  ncol=3)

    
    Code = levels(factor(dataEX_serie[[1]]$Code))
    nCode = length(Code)

    Variable = names(dataEX_serie)
    VariableTeX = convert2TeX(Variable)
    nVariable = length(Variable)
    
    Unit = metaEX_serie$unit
    UnitTeX = convert2TeX(Unit, size="small", bold=FALSE)
    PX = get_alphabet_in_px()


    icon = lapply(
        file.path(icon_path, paste0(climateChain, ".svg")),
        svgparser::read_svg)
    
    
    for (i in 1:nVariable) {
        variable = Variable[i]
        dataEX_variable = dataEX_serie[[i]]

        names(dataEX_variable)[grepl(variable, names(dataEX_variable))] = variable

        dataEX_variable$date =
            as.Date(paste0(lubridate::year(dataEX_variable$date),
                           "-01-01"))

        dataEX_variable_SAFRAN = dplyr::filter(dataEX_variable,
                                          climateChain == "SAFRAN")

        dataEX_variable_SAFRAN =
            dplyr::reframe(dplyr::group_by(dataEX_variable_SAFRAN,
                                           HM, Code,
                                           date,
                                           !!!rlang::data_syms(variable)),
                           climateChain=!!climateChain)
        dataEX_variable_SAFRAN$Chain =
            paste0(dataEX_variable_SAFRAN$climateChain,
                   "|", dataEX_variable_SAFRAN$HM)
        
        dataEX_variable = dplyr::filter(dataEX_variable,
                                   climateChain != "SAFRAN" &
                                   max(dataEX_variable_SAFRAN$date,
                                       na.rm=TRUE) < date)

        dataEX_variable = dplyr::bind_rows(dataEX_variable, dataEX_variable_SAFRAN)

        DateEX_variable = dplyr::summarise(dplyr::group_by(dataEX_variable,
                                                      climateChain),
                                      minDate=min(date),
                                      maxDate=max(date))
        minDate = min(DateEX_variable$minDate)
        maxDate = max(DateEX_variable$maxDate)
        
        dataEX_variable_mean =
            dplyr::filter(dplyr::group_by(dataEX_variable, Code, Chain),
                          period_reference[1] <= date &
                          date <= period_reference[2])
        dataEX_variable_mean =
            dplyr::summarise(dplyr::group_by(dataEX_variable_mean,
                                             Code, Chain),
                             !!paste0("mean", variable):=mean(get(variable),
                                                         na.rm=TRUE),
                             .groups="drop")
        dataEX_variable = dplyr::left_join(dataEX_variable, dataEX_variable_mean,
                                      by=c("Code", "Chain"))
        dataEX_variable[[variable]] =
            dataEX_variable[[variable]] -
            dataEX_variable[[paste0("mean", variable)]]
        
        dataEX_variable_med =
            dplyr::summarise(
                       dplyr::group_by(dataEX_variable,
                                       date, Code, climateChain),
                       !!variable := median(get(variable),
                                       na.rm=FALSE))
        
        min_value = quantile(dataEX_variable_med[[variable]],
                            prob, na.rm=TRUE)
        max_value = quantile(dataEX_variable_med[[variable]],
                            1-prob, na.rm=TRUE)
        
        
        for (j in 1:nCode) {
            code = Code[j]
            Name = meta$name[meta$Code == code]
            
            print(code)
            print(Name)
            
            dataEX_variable_med_code =
                dataEX_variable_med[dataEX_variable_med$Code == code,]
            
            herd = bring_grass(verbose=verbose)
            herd = plan_of_herd(herd, plan, verbose=verbose)
            
            title = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0, b=0, l=1, "mm"))
            
            label = paste0("<b style='font-size:12pt;
                                 color:", IPCCgrey20, "'>",
                           Name, "</b> ", nbsp(1),
                           "<span style='font-size:8pt;
                                 color:", IPCCgrey40, "'>",
                           code)
            
            title = title +
                ggtext::geom_richtext(
                            aes(x=0,
                                y=0.1,
                                label=label),
                            hjust=0, vjust=0,
                            fill=NA, label.color=NA,
                            label.padding=grid::unit(rep(0, 4),
                                                     "pt"))
            
            title = title +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            herd = add_sheep(herd,
                             sheep=title,
                             id="title",
                             height=title_height,
                             verbose=verbose)
            
                
            for (k in 1:nClimateChain) {
                cchain = climateChain[k]

                dataEX_variable_med_code_cchain =
                    dataEX_variable_med_code[
                        dataEX_variable_med_code$climateChain ==
                        cchain,]

                if (k == nClimateChain) {
                    is_x_axis = TRUE
                } else {
                    is_x_axis = FALSE
                }

                stripes = panel_stripes(
                    dataEX_variable_med_code_cchain[[variable]],
                    dataEX_variable_med_code_cchain$date,
                    min_value=min_value,
                    max_value=max_value,
                    palette_name=palette_name,
                    palette_reverse=palette_reverse,
                    palette_center=0,
                    palette_q_extrem=prob,
                    is_x_axis=is_x_axis,
                    x_limits=c(minDate, maxDate),
                    x_size=10,
                    x_color=IPCCgrey25,
                    x_hjust=NULL,
                    x_vjust=0,
                    x_breaks=get_breaks_function(
                        "10 years",
                        rm_breaks=as.Date("2100-01-01")),
                    x_label_format="%Y",
                    x_date_breaks=NULL,
                    margin_stripes=ggplot2::margin(0, 0, 0, 0,
                                                   unit="mm"))
                # axis_margin=ggplot2::margin(0.5, 1, 1, 1,
                                            # unit="mm"))
                
                herd = add_sheep(herd,
                                 sheep=stripes,
                                 id=cchain,
                                 height=stripes_height,
                                 verbose=verbose)
            }


            legend = ggplot() + theme_void() +
                theme(plot.margin=margin(t=5, r=0, b=5, l=0, "mm"))

            size_icon = 1
            dx_text = 0.2
            nIcon = length(icon)

            for (ii in 1:nIcon) {
                id = nIcon-ii+1
                
                legend = legend +
                    annotation_custom(icon[[id]],
                                      xmin=1 - size_icon/2,
                                      xmax=1 + size_icon/2,
                                      ymin=ii - size_icon/2,
                                      ymax=ii + size_icon/2) +
                    annotate("text",
                             x=1+dx_text,
                             y=ii,
                             label=Projections$storyLines[Projections$climateChain ==
                                                          climateChain[id]],
                             vjust=0.5,
                             hjust=0,
                             color=IPCCgrey40,
                             size=3.3)
            }
                
            legend = legend +
                scale_x_continuous(limits=c(0, 10),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, nIcon+1),
                                   expand=c(0, 0))
            
            herd = add_sheep(herd,
                             sheep=legend,
                             id="legend",
                             height=legend_height,
                             width=legend_width,
                             verbose=verbose)


            Palette = get_IPCC_Palette(palette_name,
                                       reverse=palette_reverse)
            colorStep = length(Palette)
            res = compute_colorBin(min_value, max_value,
                                   colorStep,
                                   center=0,
                                   include=FALSE,
                                   round=TRUE)

            palette = panel_colorbar_circle(
                res$bin,
                Palette,
                size_circle=3.3,
                d_line=0.2,
                linewidth=0.35,
                d_space=0.15,
                d_text=0.6,
                text_size=3,
                label=NULL,
                ncharLim=4,
                colorText=IPCCgrey50,
                colorLine=IPCCgrey50,
                on_circle=FALSE,
                margin=margin(t=-0.4, r=1, b=0.4, l=1, "cm"))

            herd = add_sheep(herd,
                             sheep=palette,
                             id="palette",
                             height=legend_height,
                             width=palette_width,
                             verbose=verbose)


            newline = 0.1
            
            variable = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0, b=0, l=5, "mm"))

            variable = variable +
                annotate("text",
                         x=0,
                         y=0.85,
                         label=TeX(paste0(VariableTeX[i],
                                          " ", UnitTeX[i])),
                         size=5, hjust=0, vjust=1,
                         color=IPCCgrey40)
            
            glose = metaEX_serie$glose[metaEX_serie$variable == variable]
            glose = guess_newline(glose, px=20, PX=PX)
            glose = unlist(strsplit(glose, "\n"))
            
            for (ii in 1:length(glose)) {
                variable = variable +
                    annotate("text",
                             x=0,
                             y=0.7-(ii-1)*newline,
                             label=glose[ii],
                             size=3, hjust=0, vjust=1,
                             color=IPCCgrey40)
            }
                
            variable = variable +
                scale_x_continuous(limits=c(0, 1),
                                   expand=c(0, 0)) +
                scale_y_continuous(limits=c(0, 1),
                                   expand=c(0, 0))
            
            herd = add_sheep(herd,
                             sheep=variable,
                             id="variable",
                             height=legend_height,
                             width=variable_width,
                             verbose=verbose)




            
                
            res = return_to_sheepfold(herd,
                                      page_margin=page_margin,
                                      paper_size=paper_size,
                                      hjust=0, vjust=1,
                                      verbose=verbose)
            
            plot = res$plot
            paper_size = res$paper_size

            filename = paste0("stripes_", Name, "_", variable, ".pdf")
            
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
    }
    return (Pages)
}
