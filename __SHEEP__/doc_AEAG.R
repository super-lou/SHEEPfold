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


## 1. LAYOUT _________________________________________________________
# Generates a PDF that gather datasheets, map and summarize table about the trend analyses realised on selected stations
#' @title Layout panel
#' @export
layout_panel = function (data, meta,
                         PLOT,
                         structure,
                         to_plot=c('datasheet_ASHES', 'table_ASHES', 'map_ASHES',
                                   'map_regime_ASHES', 'map_trend_ASHES', 'map_mean_ASHES'),
                         tmpdir='', figdir='', docdir="doc",
                         level=0.1,
                         trend_period=NULL,
                         mean_period=NULL, colorForce=FALSE,
                         exXprob=0.01,
                         linetype_per='solid',
                         axis_xlim=NULL,
                         paper_size='A4',
                         time_header=NULL,
                         info_header=NULL, foot_note=TRUE,
                         info_height=2.8, time_height=3,
                         var_ratio=3, foot_height=1.25,
                         shapefile_list=NULL,
                         resdir=NULL,
                         logo_path=NULL,
                         zone_to_show=NULL,
                         pdf_chunk=c('all'),
                         show_colorEvent=FALSE) {

    outfile = paste0(docdir, ".pdf")
    
    outdir = file.path(figdir,  docdir)
    if (!(file.exists(outdir))) {
        dir.create(outdir)
    }

    outdirCode = file.path(outdir, "by_code")
    if (!(file.exists(outdir_code))) {
        dir.create(outdir_code)
    }
    
    outdirPDF = file.path(figdir,  docdir, "pdf")
    if (!(file.exists(outdirPDF))) {
        dir.create(outdirPDF)
    }

    outdirPNG = file.path(figdir,  docdir, "png")
    if (!(file.exists(outdirPNG))) {
        dir.create(outdirPNG)
    }
    


    if (any(grepl("[_]ASHES$", to_plot))) {
        # Number of type/variable
        nbp = length(dataEx)

        # Convert dataEx tibble to list of tibble if it is not the case
        if (all(is.list(dataEx))) {
            dataEx = list(dataEx)
        }

        if (all(is.list(trend))) {
            trend = list(trend)
        }

        # Creates a blank list to store all the dataEx of each type of plot
        TREND2plot = vector(mode='list', length=nbp)

        # For all the type of graph / number of studied variables
        for (i in 1:nbp) {
            # Creates a list that gather all the info for one type of graph
            trend2plot = list(dataEx=dataEx[[i]], 
                              trend=trend[[i]],
                              var=var[[i]],
                              event=event[[i]],
                              unit=unit[[i]],
                              samplePeriod=samplePeriod[[i]],
                              glose=glose[[i]])
            # Stores it
            TREND2plot[[i]] = trend2plot
        }
    }

    if (any(grepl("[_]Ex2D$", to_plot))) {

    }

    

    if ('summary' %in% to_plot) {
        df_page = tibble(section='Sommaire', subsection=NA, n=1)
    } else {
        df_page = tibble()
    }
    
    # If map needs to be plot
    if ('map_ASHES' %in% to_plot | 'map_regime_ASHES' %in% to_plot) {
        df_page = page_map(NULL, 
                           meta,
                           idPer_trend=length(trend_period),
                           trend_period=trend_period,
                           mean_period=mean_period,
                           colorForce=colorForce,
                           exXprob=exXprob,
                           mapType='regime',
                           shapefile_list=shapefile_list,
                           foot_note=foot_note,
                           foot_height=foot_height,
                           zone_to_show=zone_to_show,
                           logo_path=logo_path,
                           tmpdir=tmpdir,
                           outdirPDF=outdirPDF,
                           outdirPNG=outdirPNG,
                           df_page=df_page,
                           verbose=FALSE)
    }
    
    if ('map_ASHES' %in% to_plot | 'map_trend_ASHES' %in% to_plot) {
        df_page = page_map(TREND2plot, 
                           meta,
                           idPer_trend=length(trend_period),
                           trend_period=trend_period,
                           mean_period=mean_period,
                           colorForce=colorForce,
                           exXprob=exXprob,
                           mapType='trend',
                           shapefile_list=shapefile_list,
                           foot_note=foot_note,
                           foot_height=foot_height,
                           zone_to_show=zone_to_show,
                           logo_path=logo_path,
                           tmpdir=tmpdir,
                           outdirPDF=outdirPDF,
                           outdirPNG=outdirPNG,
                           df_page=df_page)
    }
    
    if ('map_ASHES' %in% to_plot | 'map_mean_ASHES' %in% to_plot) {     
        df_page = page_map(TREND2plot, 
                           meta,
                           idPer_trend=length(trend_period),
                           trend_period=trend_period,
                           mean_period=mean_period,
                           colorForce=colorForce,
                           exXprob=exXprob,
                           mapType='mean',
                           shapefile_list=shapefile_list,
                           foot_note=foot_note,
                           foot_height=foot_height,
                           zone_to_show=zone_to_show,
                           logo_path=logo_path,
                           tmpdir=tmpdir,
                           outdirPDF=outdirPDF,
                           outdirPNG=outdirPNG,
                           df_page=df_page)
    }

    # If summarize table needs to be plot
    if ('table_ASHES' %in% to_plot) {
        df_page = page_table(TREND2plot,
                             meta,
                             trend_period,
                             mean_period,
                             colorForce=colorForce,
                             exXprob=exXprob,
                             slice=19,
                             paper_size='A3',
                             foot_note=foot_note,
                             foot_height=foot_height,
                             resdir=resdir,
                             logo_path=logo_path,
                             tmpdir=tmpdir,
                             outdirPDF=outdirPDF,
                             outdirPNG=outdirPNG,
                             df_page=df_page)
    }

    # If datasheets needs to be plot
    if ('datasheet_ASHES' %in% to_plot) {
        df_page = page_datasheet_ASHES(dataEx=dataEx,
                                       meta=meta,
                                       trend=trend,
                                       var=var,
                                       event=event,
                                       unit=unit,
                                       samplePeriod=samplePeriod,
                                       glose=glose,
                                       trend_period=trend_period,
                                       mean_period=mean_period,
                                       linetype_per=linetype_per,
                                       axis_xlim=axis_xlim,
                                       colorForce=colorForce,
                                       exXprob=exXprob,
                                       info_header=info_header,
                                       time_header=time_header,
                                       foot_note=foot_note,
                                       structure=structure,
                                       info_height=info_height,
                                       time_height=time_height,
                                       var_ratio=var_ratio,
                                       foot_height=foot_height,
                                       paper_size=paper_size,
                                       shapefile_list=shapefile_list,
                                       logo_path=logo_path,
                                       zone_to_show=zone_to_show,
                                       show_colorEvent=show_colorEvent,
                                       tmpdir=tmpdir,
                                       outdirPDF=outdirPDF,
                                       outdirPNG=outdirPNG, 
                                       df_page=df_page,
                                       pdf_chunk=pdf_chunk)
    }

    if ('summary' %in% to_plot) {
        page_summary(df_page,
                     foot_note,
                     foot_height,
                     logo_path=logo_path,
                     tmpdir=tmpdir,
                     outdirPDF=outdirPDF,
                     outdirPNG=outdirPNG)
    }

    # Combine independant pages into one PDF
    details = file.info(list.files(outdirTmp_pdf, full.names=TRUE))
    details = details[with(details, order(as.POSIXct(mtime))),]
    listfile_path = rownames(details)

    if ('summary' %in% to_plot) {
        summary_path = listfile_path[length(listfile_path)]
        listfile_path = listfile_path[-length(listfile_path)]
        listfile_path = c(summary_path, listfile_path)
    }

    if (pdf_chunk == 'by_code') {
        Code = rle(dataEx[[1]]$Code)$value
        for (code in Code) {
            listfile_code_path = listfile_path[grepl(code, listfile_path)]
            pdf_combine(input=listfile_code_path,
                        output=file.path(outdirCode, paste0(code, '.pdf')))
        }
    }
    
    if (pdf_chunk == 'all') {
        pdf_combine(input=listfile_path,
                    output=file.path(outdir, outfile))
    }
} 
