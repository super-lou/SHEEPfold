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
                          Projections,
                          prob=0.01,
                          period_reference=c(as.Date("1976-01-01"),
                                             as.Date("2005-12-31")),
                          figdir="",
                          Pages=NULL,
                          verbose=FALSE) {

    period_reference = as.Date(period_reference)
    
    paper_size = c(25, 10)
    page_margin = c(t=0.25, r=0.25, b=0.25, l=0.25)

    title_height = 1

    climateChain = unique(Projections$climateChain)
    nClimateChain = length(climateChain) 

    stripes_height =
        paper_size[1] - page_margin["t"] - page_margin["b"] - title_height
    stripes_height = stripes_height / nClimateChain

    plan = matrix(c("title",
                    climateChain),
                  ncol=1)

    
    Code = levels(factor(dataEX_serie[[1]]$Code))
    nCode = length(Code)

    Var = names(dataEX_serie)
    VarTeX = convert2TeX(Var)
    nVar = length(Var)
    
    Unit = metaEX_serie$unit
    UnitTeX = convert2TeX(Unit, size="small", bold=FALSE)
    # PX = get_alphabet_in_px()

    
    for (i in 1:nVar) {
        var = Var[i]
        dataEX_var = dataEX_serie[[i]]

        names(dataEX_var)[grepl(var, names(dataEX_var))] = var

        dataEX_var$Date =
            as.Date(paste0(lubridate::year(dataEX_var$Date),
                           "-01-01"))

        dataEX_var_mean =
            dplyr::filter(dplyr::group_by(dataEX_var, Code, Chain),
                          period_reference[1] <= Date &
                          Date <= period_reference[2])
        dataEX_var_mean =
            dplyr::summarise(dplyr::group_by(dataEX_var_mean, Code, Chain),
                             !!paste0("mean", var):=mean(get(var),
                                                         na.rm=TRUE),
                             .groups="drop")
        dataEX_var = dplyr::left_join(dataEX_var, dataEX_var_mean,
                                      by=c("Code", "Chain"))
        dataEX_var[[var]] =
            dataEX_var[[var]] -
            dataEX_var[[paste0("mean", var)]]
        
        dataEX_var_med =
            dplyr::summarise(
                       dplyr::group_by(dataEX_var,
                                       Date, Code, climateChain),
                       !!var := median(get(var),
                                       na.rm=FALSE))
        
        min_value = quantile(dataEX_var_med[[var]],
                            prob, na.rm=TRUE)
        max_value = quantile(dataEX_var_med[[var]],
                            1-prob, na.rm=TRUE)
        
        
        for (j in 1:nCode) {
            code = Code[j]
            dataEX_var_med_code =
                dataEX_var_med[dataEX_var_med$Code == code,]

            
            herd = bring_grass(verbose=verbose)
            herd = plan_of_herd(herd, plan, verbose=verbose)
            
            title = ggplot() + theme_void() +
                theme(plot.margin=margin(t=0, r=0, b=0, l=0, "cm"))

            river = code
            
            label = paste0("<b style='font-size:10pt;
                                 color:", IPCCgrey25, "'>",
                           river, "</b> ", nbsp(1),
                           "<span style='font-size:6pt;
                                 color:", IPCCgrey60, "'>",
                           code, nbsp(2),
                           # gsub("[|]", "_", chain), "_",
                           # "MULTI</span>", nbsp(2),
                           "<span style='font-size:6pt;
                                 color:", IPCCgrey60, "'>",
                           var,
                           "</span>")
            
            title = title +
                ggtext::geom_richtext(
                            aes(x=0,
                                y=1,
                                label=label),
                            hjust=0, vjust=1,
                            # color=IPCCgrey25,
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
                dataEX_var_med_code_cchain =
                    dataEX_var_med_code[
                        dataEX_var_med_code$climateChain ==
                        cchain,]

                if (k == nClimateChain) {
                    is_x_axis = TRUE
                } else {
                    is_x_axis = FALSE
                }

                stripes = panel_stripes(
                    dataEX_var_med_code_cchain[[var]],
                    dataEX_var_med_code_cchain$Date,
                    min_value=min_value,
                    max_value=max_value,
                    palette_name="ground_8",
                    palette_reverse=FALSE,
                    palette_center=0,
                    palette_q_extrem=prob,
                    is_x_axis=is_x_axis,
                    x_size=8,
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

                
            res = return_to_sheepfold(herd,
                                      page_margin=page_margin,
                                      paper_size=paper_size,
                                      hjust=0, vjust=1,
                                      verbose=verbose)
            
            plot = res$plot
            paper_size = res$paper_size

            filename = paste0("stripes_", river, "_", var, ".pdf")
            
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
