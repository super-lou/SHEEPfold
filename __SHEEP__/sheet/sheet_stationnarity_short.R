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


#' @title Datasheet panel
#' @export
sheet_stationnarity_short = function (meta, data,
                                      dataEX, metaEX, trendEX,
                                      period_trend_show=NULL,
                                      exProb=0.01,
                                      linetype_per=NULL,
                                      logo_path=NULL,
                                      Shapefiles=NULL,
                                      figdir="",
                                      paper_size=c(21, 18),
                                      df_page=NULL,
                                      verbose=FALSE) {

    page_margin = c(t=0.5, r=0.5, b=0.5, l=0.5)

    Code = levels(factor(dataEX$Code))
    nCode = length(Code)
    Period = unique(trendEX$period)
    nPeriod = length(Period)

    Var = names(dataEX)[!(names(dataEX) %in% c("Code", "Date"))]
    # Var =  levels(factor(trendEX$var))
    nVar = length(Var)

    title_height = 0.7
    var_height = (paper_size[1] - 0.5*2 - title_height) / nVar
    
    plan = matrix(c("title", Var),
                  ncol=1)
    

    # For all the station
    for (k in 1:nCode) {
        # Gets the code
        code = Code[k]
        # Print code of the station for the current plotting
        print(paste0("Datasheet for station : ", code,
                    "   (", round(k/nCode*100, 1), " %)"))

        herd = bring_grass(verbose=verbose)
        herd = plan_of_herd(herd, plan,
                            verbose=verbose)
        
        title = panel_title(code,
                            margin=margin(t=0, r=0, b=0, l=5, "mm"))
        
        herd = add_sheep(herd,
                         sheep=title,
                         id="title",
                         height=title_height,
                         verbose=verbose)

        for (i in 1:nVar) {
            var = Var[i]
            print(paste0("Time panel for ", var))

            if (i == 1) {
                first = TRUE
                last = FALSE
            } else if (i == nVar) {
                first = FALSE
                last = TRUE
            } else {
                first = FALSE
                last = FALSE
            }

            dataEX_code_var =
                dplyr::select(dataEX[dataEX$Code == code,],
                              c("Code", "Date", var))

            trendEX_code_var = trendEX[trendEX$Code == code &
                                      trendEX$var == var,]
            
            trend = panel_trend(dataEX_code_var,
                                trendEX_code_var,
                                metaEX,
                                period_trend_show=period_trend_show,
                                linetype_per=linetype_per,
                                missRect=FALSE,
                                axis_xlim=NULL, grid=FALSE,
                                ymin_lim=NULL,
                                breaks="10 years",
                                minor_breaks="2 years",
                                date_labels="%Y",
                                first=first, last=last)
            
            herd = add_sheep(herd,
                             sheep=trend,
                             id=var,
                             label="align",
                             height=var_height,
                             verbose=verbose)
        }

        res = return_to_sheepfold(herd,
                                  page_margin=page_margin,
                                  paper_size=paper_size,
                                  hjust=0, vjust=1,
                                  verbose=verbose,
                                  verbose=verbose)
        
        plot = res$plot
        # paper_size = res$paper_size

        filename = paste0(code, "_stationnarity_short.pdf")

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









# nEvent = length(structure)
#         page_code = 0
#         for (i in 1:nEvent) {
            
#             var_to_place = structure[[i]]
#             event = names(structure)[i]

#             if (event != 'Resume' & event != 'None' & show_colorEvent) {
#                 space = 0.7
#                 colorEvent = get_colorEvent()
#                 colorTextEvent = get_colorTextEvent()
#                 Hevent = panel_event(event, colorEvent, colorTextEvent)
#                 Heventinfo = merge_panel(add=Hevent, to=Hinfo,
#                                          widths=c(space, width-space))
#                 df_P = add_plot(df_P,
#                                 plot=Heventinfo,
#                                 name="info",
#                                 overwrite_by_name=TRUE)
#             }
            
#             if (event == 'Resume') {
#                 nVar_max = 4
#             } else {
#                 nVar_max = 5
#             }

#             nVar_to_place = length(var_to_place)
#             nVar_page = ceiling(nVar_to_place/nVar_max)
            
#             for (page in 1:nVar_page) {

#                 page_code = page_code + 1
                
#                 if (foot_note) {
#                     if (event != 'Resume' & event != 'None') {
#                         footName = paste0('fiche station : ', event)
#                     } else {
#                         footName = 'fiche station'
#                     }
#                     if (is.null(df_page)) {
#                         n_page = k + page_code - 1
#                     } else {
#                         if (nrow(df_page) == 0 | pdf_chunk == 'by_code') {
#                             n_page = page_code
#                         } else {
#                             n_page = df_page$n[nrow(df_page)] + page_code
#                         }
#                     }
#                     foot = panel_foot(footName, n_page,
#                                       foot_height, logo_path)
#                     df_P = add_plot(df_P,
#                                     plot=foot,
#                                     name="foot",
#                                     overwrite_by_name=TRUE)
#                 }

#                 var_to_place_page = var_to_place[(1+(nVar_max*(page-1))) : (nVar_max*page)]
#                 var_to_place_page = var_to_place_page[!is.na(var_to_place_page)]

#                 LM_id = c()
#                 LM_name = c()
#                 if (!is.null(info_header)) {
#                     id_plot = which(df_P$name == "info")
#                     LM_id = c(LM_id, id_plot)
#                     LM_name = c(LM_name, df_P$name[id_plot])
#                     nbi = 1
#                 } else {
#                     nbi = 0
#                 }

#                 if (!is.null(time_header)) {
#                     if (event == "Resume") {
#                         id_plot = c(which(df_P$name == "Q"
#                                           & df_P$first == TRUE),
#                                     which(df_P$name == "\\sqrt{Q}"
#                                           & df_P$first == FALSE))
#                         nbt = 2
#                     } else if (event == "Étiage")  {
#                         id_plot = which(df_P$name == "\\sqrt{Q}"
#                                         & df_P$first == TRUE)
#                         nbt = 1
#                     } else {
#                         id_plot = which(df_P$name == "Q"
#                                         & df_P$first == TRUE)
#                         nbt = 1
#                     }

#                     LM_id = c(LM_id, id_plot)
#                     LM_name = c(LM_name, df_P$name[id_plot])
#                 } else {
#                     nbt = 0
#                 }

#                 P_var = c()
#                 for (var in var_to_place_page) {
#                     if (var == var_to_place_page[length(var_to_place_page)]) {
#                         last = TRUE
#                     } else {
#                         last = FALSE
#                     }
#                     id_plot = which(df_P$name == var & df_P$last == last)
#                     LM_id = c(LM_id, id_plot)
#                     LM_name = c(LM_name, df_P$name[id_plot])
#                 }
                
#                 nGraphMiss = nVar_max - length(var_to_place_page)

#                 if (nGraphMiss > 0) {
#                     for (i in 1:nGraphMiss) {
#                         LM_id = c(LM_id, NA)
#                         LM_name = c(LM_name, "void")
#                     }
#                 }

#                 if (foot_note) {
#                     id_plot = which(df_P$name == "foot")
#                     LM_id = c(LM_id, id_plot)
#                     LM_name = c(LM_name, df_P$name[id_plot])
#                     nbf = 1
#                 } else {
#                     nbf = 0
#                 }

#                 Pevent = df_P$plot[LM_id[!is.na(LM_id)]]

#                 LM_name = matrix(LM_name)
#                 LM_id[!is.na(LM_id)] = 1:length(LM_id[!is.na(LM_id)])
#                 LM_id = matrix(LM_id)                

#                 LMcol = ncol(LM_id)
#                 LM_id = rbind(rep(NA, times=LMcol), LM_id,
#                            rep(NA, times=LMcol))
#                 LM_name = rbind(rep("margin", times=LMcol), LM_name,
#                                 rep("margin", times=LMcol))
                
#                 LMrow = nrow(LM_id)
#                 LM_id = cbind(rep(NA, times=LMrow), LM_id,
#                            rep(NA, times=LMrow))
#                 LM_name = cbind(rep("margin", times=LMrow), LM_name,
#                                 rep("margin", times=LMrow))
#                 LMcol = ncol(LM_id)

#                 margin_height = 0.5
                
#                 Norm_ratio = height * var_ratio * nVar_max / (height - 2*margin_height - time_height*nbt - foot_height*nbf - info_height*nbi)

#                 var_height = height * var_ratio / Norm_ratio
                
#                 Hcut = LM_name[, 2]
#                 heightLM = rep(0, times=LMrow)        
                
#                 heightLM[Hcut == "info"] = info_height
#                 heightLM[Hcut == "Q"] = time_height
#                 heightLM[Hcut == "\\sqrt{Q}"] = time_height
#                 heightLM[Hcut %in% var_to_place_page |
#                          Hcut == "void"] = var_height
#                 heightLM[Hcut == "foot"] = foot_height
#                 heightLM[Hcut == "margin"] = margin_height

#                 col_width = (width - 2*margin_height) / (LMcol - 2)
                
#                 Wcut = LM_name[(LMrow-1),]
#                 widthLM = rep(col_width, times=LMcol)
#                 widthLM[Wcut == "margin"] = margin_height

                
#                 LM_inline = Pevent[as.vector(LM_id)]
                
#                 LM_name_inline = as.vector(LM_name)

#                 widths_var = list()
#                 nPlot = length(LM_inline)
#                 for (i in 1:nPlot) {
#                     if (is.null(LM_inline[[i]])) {
#                         LM_inline[[i]] = void()
#                     }
#                     if (LM_name_inline[i] %in% c(var_plotted, "Q", "\\sqrt{Q}")) {
#                         LM_inline[[i]] = ggplot_gtable(ggplot_build(LM_inline[[i]]))
#                         widths_var = append(widths_var, list(LM_inline[[i]]$widths))
#                     }
#                 }        
#                 maxWidth = do.call(grid::unit.pmax, widths_var)
#                 for (i in 1:nPlot) {
#                     if (LM_name_inline[i] %in% c(var_plotted, "Q", "\\sqrt{Q}")) {
#                         LM_inline[[i]]$widths = as.list(maxWidth)
#                     }
#                 }

#                 plot = grid.arrange(arrangeGrob(grobs=LM_inline,
#                                                 nrow=LMrow,
#                                                 ncol=LMcol,
#                                                 heights=heightLM,
#                                                 widths=widthLM,
#                                                 as.table=FALSE))
                

#                 eventName = chartr("áéèà", "aeea", tolower(event))
#                 filename = paste0(as.character(code), '_',
#                                   gsub(' ', '_', eventName),
#                                   '_', page)

#                 print("Saving")
                
#                 # Saving
#                 ggsave(plot=plot,
#                        path=outdirTmp_pdf,
#                        filename=paste0(filename, '.pdf'),
#                        width=width, height=height, units='cm', dpi=100)
                
#                 # Saving
#                 ggsave(plot=plot, 
#                        path=outdirTmp_png,
#                        filename=paste0(filename, '.png'),
#                        width=width, height=height, units='cm', dpi=400)

                
#             }
#         }

#         print("")

#         if (!is.null(df_page)) {
#             section = 'Fiche station'
#             subsection = code
#             if (nrow(df_page) == 0 | pdf_chunk == 'by_code') {
#                 n_page = page_code
#             } else {
#                 n_page = df_page$n[nrow(df_page)] + 1
#             }
#             df_page = bind_rows(
#                 df_page,
#                 tibble(section=section,
#                        subsection=subsection,
#                        n=n_page))
#         }   
#     }
