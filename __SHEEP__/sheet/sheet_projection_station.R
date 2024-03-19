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
                                     icon_path="",
                                     Warnings=NULL,
                                     logo_path="",
                                     Pages=NULL,
                                     Shapefiles=NULL,
                                     figdir="",
                                     verbose=FALSE) {
    
    page_margin = c(t=0.5, r=0.5, b=0.5, l=0.5)

    info_height = 3
    medQJ_height = 7
    foot_height = 1.25

    free_height = 29.7 - page_margin["t"] - page_margin["b"] -
        info_height - medQJ_height - foot_height
    
    stripes_height = free_height/3 * 2/6
    spread_height = free_height/3 * 3/6
    signe_height = free_height/3 * 1/6
    
    width = 21 - page_margin["l"] - page_margin["r"]

    plan = matrix(c(
        "info", "info", "info",
        
        "medQJ_H0", "medQJ_H2", "medQJ_H3",
        
        "stripes_QJXA", "stripes_QJXA", "stripes_QJXA",
        "spread_QJXA", "spread_QJXA", "spread_QJXA",
        "signe_QJXA", "signe_QJXA", "signe_QJXA",
        
        "stripes_QA", "stripes_QA", "stripes_QA",
        "spread_QA", "spread_QA", "spread_QA",
        "signe_QA", "signe_QA", "signe_QA",
        
        "stripes_VCN10_summer", "stripes_VCN10_summer", "stripes_VCN10_summer", 
        "spread_VCN10_summer", "spread_VCN10_summer", "spread_VCN10_summer",
        "signe_VCN10_summer", "signe_VCN10_summer", "signe_VCN10_summer",
        
        "foot", "foot", "foot"
    ), ncol=3, byrow=TRUE)

    
    # data_obs =
    #     dplyr::summarise(dplyr::group_by(data, code, date),
    #                      Q=median(Q_obs, na.rm=TRUE),
    #                      .groups="drop")


    Code = levels(factor(dataEX_serie[[1]]$code))
    nCode = length(Code)

    Variables_medQJ = c("medQJ_H0", "medQJ_H1", "medQJ_H2")
    Variables = unique(metaEX_serie$variable_en)
    Variables = Variables[Variables %in% Variables_medQJ]

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
        
        info = panel_info_station(
            data_obs_code,
            dataEX_serieQM_obs_code$QM,
            regimeLight=regimeHydro$detail[regimeHydro$code == code],
            meta=meta,
            Shapefiles=Shapefiles,
            codeLight=code,
            to_do='all',
            zone_to_show='France',
            verbose=verbose)
        herd = add_sheep(herd,
                         sheep=info,
                         id="info",
                         height=info_height,
                         verbose=verbose)



        for (variable in Variable_medQJ) {

            medQJ = panel_spaghetti(verbose=verbose)
            medQJ = contour()
            herd = add_sheep(herd,
                             sheep=medQJ,
                             id=variable,
                             label="align",
                             height=medQJ_height,
                             verbose=verbose)

        }

        
        for (variable in Variable) {

            # stripes = panel_stripes(verbose=verbose)
            stripes = contour()
            herd = add_sheep(herd,
                             sheep=stripes,
                             id=paste0("stripes", variable),
                             label="align",
                             height=stripes_height,
                             verbose=verbose)

            # spread = panel_spread(verbose=verbose)
            spread = contour()
            herd = add_sheep(herd,
                             sheep=spread,
                             id=paste0("spread", variable),
                             label="align",
                             height=spread_height,
                             verbose=verbose)

            # signe = panel_signe(verbose=verbose)
            signe = contour()
            herd = add_sheep(herd,
                             sheep=signe,
                             id=paste0("signe_", variable),
                             label="align",
                             height=signe_height,
                             verbose=verbose)
        }


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
