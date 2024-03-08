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


### 4.1. Summary _____________________________________________________
#' @title Summary panel
#' @export
sheet_summary = function (Pages,
                          title="title",
                          subtitle="subtitle",
                          logo_info=NULL,
                          page_margin=c(t=0.5, r=0.5, b=0.5, l=0.5),
                          figdir="") {

    title_height = 1
    if (is.null(subtitle)) {
        subtitle_height = 0
    } else {
        subtitle_height = 1
    }
    foot_height = 1.25
    sum_height = 29.7 - title_height - subtitle_height - foot_height - page_margin["t"] - page_margin["b"]

    p_width = 2
    sec_width = (21 - p_width*2 - page_margin["l"] - page_margin["r"])/2
    width = 21 - page_margin["l"] - page_margin["r"]
    
    
    plan = matrix(c("title", "title", "title", "title",
                    "subtitle", "subtitle", "subtitle", "subtitle",
                    "sum1", "page1", "sum2", "page2",
                    "foot", "foot", "foot", "foot"),
                  nrow=4, byrow=TRUE)

    herd = bring_grass(verbose=verbose)
    herd = plan_of_herd(herd, plan,
                        verbose=verbose)

    
    text_title = paste0("<b>", title, "</b>")
    text_subtitle = subtitle

    Sec_name = rle(Pages$section)$values
    nSec = length(Sec_name)
    
    text_sum1 = ''
    text_page1 = ''
    text_sum2 = ''
    text_page2 = ''
    
    nline = 0
    nline_max = 58
    for (idS in 1:nSec) {
        sec_name = Sec_name[idS]
        subSec_name = rle(Pages$subsection[Pages$section == sec_name])$values
        n_page = Pages$n[Pages$section == sec_name][1]

        line = paste("<b>", idS, ". ", sec_name, "</b>", "<br>", sep='')
        page = paste("<b>p.", n_page, "</b><br>", sep='')
        
        if (nline <= nline_max) {
            text_sum1 = paste(text_sum1, line, sep='')
            text_page1 = paste(text_page1, page, sep='')
        } else {
            text_sum2 = paste(text_sum2, line, sep='')
            text_page2 = paste(text_page2, page, sep='')
        }

        nline = nline + 1
        
        nSSec = length(subSec_name)
        for (idSS in 1:nSSec) {
            subsec_name = subSec_name[idSS]
            if (!is.na(subsec_name)) {
                n_page = Pages$n[Pages$section == sec_name &
                                   Pages$subsection == subsec_name][1]

                line = paste("<b>", idS, ".", idSS, ".</b> ",
                             subsec_name, "<br>", sep='')
                page = paste("p.", n_page, "<br>", sep='')
                
                if (nline <= nline_max) {
                    text_sum1 = paste(text_sum1, line, sep='')
                    text_page1 = paste(text_page1, page, sep='')
                } else {
                    text_sum2 = paste(text_sum2, line, sep='')
                    text_page2 = paste(text_page2, page, sep='')
                }

                nline = nline + 1
            }
        }
        if (nline <= nline_max) {
            text_sum1 = paste(text_sum1, "<br>", sep='')
            text_page1 = paste(text_page1, "<br>", sep='')
        } else {
            text_sum2 = paste(text_sum2, "<br>", sep='')
            text_page2 = paste(text_page2, "<br>", sep='')
        }
        nline = nline + 1
    }

    # text_sum1 = gsub(" ", "<span style='color:white'>&#95;</span>",
                     # text_sum1)
    text_sum1 = gsub('[.]', '&#46;', text_sum1)
    text_page1 = gsub('[.]', '&#46;', text_page1)
    
    # text_sum2 = gsub(" ", "<span style='color:white'>&#95;</span>",
                     # text_sum2)
    text_sum2 = gsub('[.]', '&#46;', text_sum2)
    text_page2 = gsub('[.]', '&#46;', text_page2)

    
    # Converts all texts to graphical object in the right position
    title = richtext_grob(text_title,
                          x=0, y=1,
                          margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                          hjust=0, vjust=1,
                          gp=gpar(col=refCOL, fontsize=20))
    herd = add_sheep(herd,
                     sheep=title,
                     id="title",
                     height=title_height,
                     verbose=verbose)

    subtitle = richtext_grob(text_subtitle,
                             x=0, y=1,
                             margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                             hjust=0, vjust=1,
                             gp=gpar(col=refCOL, fontsize=15))
    herd = add_sheep(herd,
                     sheep=subtitle,
                     id="subtitle",
                     height=subtitle_height,
                     verbose=verbose)

    sum1 = richtext_grob(text_sum1,
                         x=0, y=1,
                         margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                         hjust=0, vjust=1,
                         gp=gpar(col=refCOL, fontsize=10))
    herd = add_sheep(herd,
                     sheep=sum1,
                     id="sum1",
                     height=sum_height,
                     width=sec_width,
                     verbose=verbose)
    
    page1 = richtext_grob(text_page1,
                          x=0, y=1,
                          margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                          hjust=0, vjust=1,
                          gp=gpar(col=refCOL, fontsize=10))
    herd = add_sheep(herd,
                     sheep=page1,
                     id="page1",
                     height=sum_height,
                     width=p_width,
                     verbose=verbose)

    sum2 = richtext_grob(text_sum2,
                         x=0, y=1,
                         margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                         hjust=0, vjust=1,
                         gp=gpar(col=refCOL, fontsize=10))
    herd = add_sheep(herd,
                     sheep=sum2,
                     id="sum2",
                     height=sum_height,
                     width=sec_width,
                     verbose=verbose)
    
    page2 = richtext_grob(text_page2,
                          x=0, y=1,
                          margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                          hjust=0, vjust=1,
                          gp=gpar(col=refCOL, fontsize=10))
    herd = add_sheep(herd,
                     sheep=page2,
                     id="page2",
                     height=sum_height,
                     width=p_width, 
                     verbose=verbose)


    
    foot = panel_foot('Sommaire', 1, foot_height, logo_info)
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

    filename = paste0("summary.pdf")

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
