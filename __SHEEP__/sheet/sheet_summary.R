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
sheet_summary = function (df_page, title="title", subtitle="subtitle", logo_path=NULL, figdir="") {


    if (is.null(title)) {
        title = ""
    }
    if (is.null(subtitle)) {
        subtitle = ""
    }
    
    foot_height = 1.25
    
    text_title = paste0("<b>", title, "</b>")
    text_subtitle = subtitle

    Sec_name = rle(df_page$section)$values
    nSec = length(Sec_name)
    
    text_sum1 = ''
    text_page1 = ''
    text_sum2 = ''
    text_page2 = ''
    
    nline = 0
    nline_max = 58
    for (idS in 1:nSec) {
        sec_name = Sec_name[idS]
        subSec_name = rle(df_page$subsection[df_page$section == sec_name])$values
        n_page = df_page$n[df_page$section == sec_name][1]

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
                n_page = df_page$n[df_page$section == sec_name &
                                   df_page$subsection == subsec_name][1]

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
    gtitle = richtext_grob(text_title,
                           x=0, y=1,
                           margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                           hjust=0, vjust=1,
                           gp=gpar(col="#00A3A8", fontsize=20))

    gsubtitle = richtext_grob(text_subtitle,
                              x=0, y=1,
                              margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                              hjust=0, vjust=1,
                              gp=gpar(col="#00A3A8", fontsize=15))

    gsum1 = richtext_grob(text_sum1,
                          x=0, y=1,
                          margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                          hjust=0, vjust=1,
                          gp=gpar(col="#00A3A8", fontsize=10))
    
    gpage1 = richtext_grob(text_page1,
                           x=0, y=1,
                           margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                           hjust=0, vjust=1,
                           gp=gpar(col="#00A3A8", fontsize=10))

    gsum2 = richtext_grob(text_sum2,
                          x=0, y=1,
                          margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                          hjust=0, vjust=1,
                          gp=gpar(col="#00A3A8", fontsize=10))
    
    gpage2 = richtext_grob(text_page2,
                           x=0, y=1,
                           margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                           hjust=0, vjust=1,
                           gp=gpar(col="#00A3A8", fontsize=10))
    
    
    # If there is a foot note
    if (!is.null(logo_path)) {
        footName = 'sommaire'
        foot = panel_foot(footName,
                          1, foot_height, logo_path)

        P = list(gtitle, gsubtitle, gsum1, gpage1, gsum2, gpage2, foot)
        LM = matrix(c(1, 1, 1, 1,
                      2, 2, 2, 2,
                      3, 4, 5, 6,
                      7, 7, 7, 7),
                    nrow=4, byrow=TRUE)
    } else {
        foot_height = 0
        P = list(gtitle, gsubtitle, gsum1, gpage1, gsum2, gpage2)
        LM = matrix(c(1, 1, 1, 1,
                      2, 2, 2, 2,
                      3, 4, 5, 6),
                    nrow=3, byrow=TRUE)
    }
    id_title = 1
    id_subtitle = 2
    id_page1 = 4
    id_page2 = 6
    id_foot = 7

    LMcol = ncol(LM)
    LMrow = nrow(LM)
    
    LM = rbind(rep(99, times=LMcol), LM, rep(99, times=LMcol))
    LMrow = nrow(LM)
    LM = cbind(rep(99, times=LMrow), LM, rep(99, times=LMrow))
    LMcol = ncol(LM)

    title_height = 0.75
    subtitle_height = 1.25
    margin_size = 0.5
    page_width = 2
    height = 29.7
    width = 21

    row_height = (height - 2*margin_size - foot_height - title_height - subtitle_height) / (LMrow - 5)

    Hcut = LM[, 2]
    heightLM = rep(row_height, times=LMrow)
    heightLM[Hcut == id_title] = title_height
    heightLM[Hcut == id_subtitle] = subtitle_height
    heightLM[Hcut == id_foot] = foot_height
    heightLM[Hcut == 99] = margin_size

    col_width = (width - 2*margin_size - 2*page_width) / (LMcol - 4)
    
    Wcut = LM[4,]
    widthLM = rep(col_width, times=LMcol)
    widthLM[Wcut ==  id_page1 | Wcut ==  id_page2] = page_width
    widthLM[Wcut == 99] = margin_size

    print(LM)
    print(heightLM)
    print(widthLM)
    
    # Arranges the graphical object
    plot = grid.arrange(grobs=P, layout_matrix=LM,
                        heights=heightLM, widths=widthLM)

    print(figdir)
    
    if (!(file.exists(figdir))) {
        dir.create(figdir, recursive=TRUE)
    }
    # Saves the plot
    ggsave(plot=plot,
           path=figdir,
           filename=paste0('sommaire', '.pdf'),
           width=width, height=height, units='cm', dpi=100)
} 
