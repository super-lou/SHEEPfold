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


calc_ttest_p_value = function(vec_a, vec_b){
  t.test(vec_a, vec_b, conf.level=0.9)$p.value
}



panel_correlation_matrix = function (dataEX,
                                     metaEX,
                                     icon_path,
                                     criteria_selection=NULL,
                                     level=0.1,
                                     margin=margin(t=0, r=0,
                                                   b=0, l=0,
                                                   "cm")) {

    lw_mat = 0.4
    d_W_mat = 0.25
    
    dy_L1 = 0.4
    lw_L1 = 0.25
    
    dy_I1 = 0.4
    size_I1 = 0.45
    # dr_I1 = 6
    dr_I1 = 0.23
    
    dy_T1 = 0.6
    size_T1 = 3.2
    ech_T1 = 0.95

    dy_L2_min = 1
    lw_L2 = 0.25
    
    dx_L3 = 0.5
    lw_L3 = 0.45

    dy_L4 = 0.5
    lw_L4 = 0.45
    
    dy_T2 = 0.3
    dy_T2line = 0.8
    size_T2 = 2.7
    ech_T2 = 2.5
    
    dy_I2 = 2
    size_I2 = 1

    ech = 25
    
    
    complete = function (X) {
        if (length(X) < 2) {
            X = c(X, NA)
        }
        return (X)
    }

    logicalCol = names(dataEX)[sapply(dataEX, class) == "logical"]
    dataEX = dataEX[!(names(dataEX) %in% logicalCol)]
    metaEX = metaEX[!(metaEX$variable_en %in% logicalCol),]
    
    Topic = strsplit(metaEX$topic, "[|]")
    Topic = lapply(Topic, complete)
    mainTopicVAR = sapply(Topic, '[[', 2)
    names(mainTopicVAR) = metaEX$variable_en
    lenMainTopic = rle(mainTopicVAR)$lengths
    nMainTopic = length(lenMainTopic)
    startMainTopic =
        cumsum(c(1, lenMainTopic[1:(nMainTopic-1)])) - 1 + dx_L3
    endMainTopic = cumsum(lenMainTopic) - dx_L3
    midMainTopic = (startMainTopic + endMainTopic)/2
    mainTopic = mainTopicVAR[!duplicated(mainTopicVAR)]

    mainTopic_icon = lapply(
        file.path(icon_path, paste0(gsub(" ", "_", mainTopic), ".svg")),
        svgparser::read_svg)

    variables2keep = names(dataEX)
    variables2keep = variables2keep[!grepl("([_]obs)|([_]sim)", variables2keep)]

    dataEX = dplyr::mutate(dataEX,
                           dplyr::across(where(is.logical),
                                         as.numeric),
                           .keep="all")
    dataEX = dplyr::select(dataEX, variables2keep)

    HM = levels(factor(dataEX$HM))
    nHM = length(HM)

    CORRmat = c()
    Pmat = c()
    for (i in 1:nHM) {
        dataEX_hm = dataEX[dataEX$HM == HM[i],]
        nameRow = dataEX_hm$code
        
        dataEX_hm = dplyr::select(dataEX_hm, -c(code, HM))

        matchVariable = match(names(dataEX_hm), metaEX$variable_en)
        matchVariable = matchVariable[!is.na(matchVariable)]
        
        dataEX_hm = dataEX_hm[matchVariable]
        
        nameCol = names(dataEX_hm)
        Variable = nameCol
        nVariable = ncol(dataEX_hm)

        nCol = ncol(dataEX_hm)
        col2rm = c()
        for (i in 1:nCol) {
            dataEX_hm[[i]][is.nan(dataEX_hm[[i]])] = NA
            if (sum(!is.na(dataEX_hm[[i]])) < 2) {
                col2rm = c(col2rm, names(dataEX_hm)[i])
            }
        }
        
        if (!is.null(col2rm)) {
            dataEX_hm = dplyr::select(dataEX_hm, -col2rm)
            nameCol = names(dataEX_hm)
        }


        # dataEX_hm = as.matrix(dataEX_hm)
        
        CORRmat_tmp = 
            as.matrix(
                dplyr::select(
                           corrr::correlate(dataEX_hm,
                                            method="spearman",
                                            use="pairwise.complete.obs",
                                            diagonal=1),
                           -"term"))
        name = colnames(CORRmat_tmp)
        rownames(CORRmat_tmp) = name

        Pmat_tmp =
            as.matrix(
                dplyr::select(
                           corrr::colpair_map(dataEX_hm,
                                              calc_ttest_p_value),
                           -"term"))
        name = colnames(Pmat_tmp)
        rownames(Pmat_tmp) = name

        # rownames(Pmat_tmp) = colnames(Pmat_tmp)

        # colnames(dataEX_hm) = nameCol
        # rownames(dataEX_hm) = nameRow
        
        CORRmat = c(CORRmat,
                    CORRmat_tmp)
        Pmat = c(Pmat,
                 Pmat_tmp)


        # CORRmat = c(CORRmat,
        #             c(cor(dataEX_hm,
        #                   method="spearman",
        #                   use="pairwise.complete.obs")))
        # Pmat = c(Pmat,
        #          c(corrplot::cor.mtest(
        #                          dataEX_hm,
        #                          conf.level=1-level,
        #                          method="spearman",
        #                          use="pairwise.complete.obs")$p))
    }
    
    CORRmat = array(CORRmat, c(nVariable, nVariable, nHM))
    Pmat = array(Pmat, c(nVariable, nVariable, nHM))

    CORRmat_hm = apply(CORRmat, 1:2, median, na.rm=TRUE)
    if (nHM > 1) {
        Pmat_hm = matrix(rep(0, nVariable*nVariable), ncol=nVariable)
    } else {
        Pmat_hm = matrix(Pmat, ncol=nVariable)
    }

    if (!is.null(col2rm)) {
        nCol2add = length(col2rm)
        nVariableCORR = length(colnames(CORRmat_hm))
        
        for (i in 1:nCol2add) {
            missVariable = col2rm[i]
            id = which(Variable == missVariable)
            CORRmat_hm = rbind(CORRmat_hm[1:(id-1),],
                                  rep(NA, nVariableCORR),
                                  CORRmat_hm[id:nrow(CORRmat_hm),])
            # print(CORRmat_hm)
            # print(id)
            # print(missVariable)
            # print(rownames(CORRmat_hm)[id])
            # rownames(CORRmat_hm)[id] = missVariable
            # print("ok")
            
            Pmat_hm = rbind(Pmat_hm[1:(id-1),],
                               rep(NA, nVariableCORR),
                               Pmat_hm[id:nrow(Pmat_hm),])
            # rownames(Pmat_hm)[id] = missVariable
        }
        
        for (i in 1:nCol2add) {
            missVariable = col2rm[i]
            id = which(Variable == missVariable)
            CORRmat_hm = cbind(CORRmat_hm[, 1:(id-1)],
                                  rep(NA, nVariableCORR+nCol2add),
                                  CORRmat_hm[, id:ncol(CORRmat_hm)])
            # colnames(CORRmat_hm)[id] = missVariable
            Pmat_hm = cbind(Pmat_hm[, 1:(id-1)],
                               rep(NA, nVariableCORR+nCol2add),
                               Pmat_hm[, id:ncol(Pmat_hm)])
            # colnames(Pmat_hm)[id] = missVariable
        }
    }


    Palette = get_IPCC_Palette("rainbow_6", reverse=TRUE)
    res = compute_colorBin(-1, 1,
                           colorStep=6,
                           center=0,
                           include=TRUE,
                           round=FALSE)
    bin = res$bin
    upBin = res$upBin
    lowBin = res$lowBin

    Colors = get_colors(CORRmat_hm,
                        upBin=upBin,
                        lowBin=lowBin,
                        Palette=Palette)

    COLORmat = matrix(Colors,
                      nrow=nrow(CORRmat_hm),
                      ncol=ncol(CORRmat_hm))
    SIZEmat = (abs(CORRmat_hm))^(1/6)*ech
    Xmat = matrix(rep(0:(nVariable-1)*ech, nVariable), nrow=nVariable, byrow=TRUE) + 0.5*ech
    Ymat = matrix(rep((nVariable-1):0*ech, nVariable), nrow=nVariable) + 0.5*ech
    XMINmat = Xmat - SIZEmat/2
    XMAXmat = Xmat + SIZEmat/2
    YMINmat = Ymat - SIZEmat/2
    YMAXmat = Ymat + SIZEmat/2

    COLOR = unlist(as.list(COLORmat))
    XMIN = unlist(as.list(XMINmat))
    XMAX = unlist(as.list(XMAXmat))
    YMIN = unlist(as.list(YMINmat))
    YMAX = unlist(as.list(YMAXmat))

    nOKPmat = Pmat_hm > level
    nOKPmat[!nOKPmat] = NA
    XPSIZEmat = SIZEmat*nOKPmat/ech
    XPmat = Xmat*nOKPmat
    YPmat = Ymat*nOKPmat

    XPSIZE = unlist(as.list(XPSIZEmat))
    XPSIZE = XPSIZE[!is.na(XPSIZE)]
    XP = unlist(as.list(XPmat))
    XP = XP[!is.na(XP)]
    YP = unlist(as.list(YPmat))
    YP = YP[!is.na(YP)]

    VariableTeX = convert2TeX(Variable)

    cm = ggplot() + theme_void() + coord_fixed(clip="off") +
        theme(text=element_text(family="Helvetica"),
              plot.margin=margin)

    cm = cm +
        annotate("rect", xmin=XMIN, xmax=XMAX,
                 ymin=YMIN, ymax=YMAX,
                 fill=COLOR)
    
    cm = cm +
        annotate("rect", xmin=0, xmax=nVariable*ech, ymin=0, ymax=nVariable*ech,
                 linewidth=lw_mat, color=IPCCgrey95, fill=NA)
    for (i in 1:(nVariable-1)) {
        cm = cm +
            annotate("line", x=c(0, nVariable)*ech, y=c(i, i)*ech,
                     linewidth=lw_mat, color=IPCCgrey95) +
            annotate("line", x=c(i, i)*ech, y=c(0, nVariable)*ech,
                     linewidth=lw_mat, color=IPCCgrey95)
    }

    cm = cm +
        annotate("point", x=XP, y=YP,
                 shape=4, size=XPSIZE, color="white")


    if (!is.null(criteria_selection)) {
        Variable_selected = apply(sapply(criteria_selection, grepl, x=Variable),
                             1, any)
        Variable_color = rep(IPCCgrey60, nVariable) 
        Variable_color[Variable_selected] = IPCCgrey40
    } else {
        Variable_color = rep(IPCCgrey60, nVariable) 
    }
    
    cm = cm +
        annotate("text",
                 x=rep(-d_W_mat*ech, nVariable),
                 y=(nVariable-1):0*ech + 0.5*ech,
                 hjust=1, vjust=0.5,
                 label=TeX(VariableTeX), size=size_T1,
                 color=Variable_color) +
        
        annotate("text",
                 x=0:(nVariable-1)*ech + 0.5*ech,
                 y=rep(-d_W_mat*ech, nVariable),
                 hjust=1, vjust=0.5,
                 angle=90,
                 label=TeX(VariableTeX), size=size_T1,
                 color=Variable_color)

    PX = get_alphabet_in_px(style="bold")

    VariableRAW = VariableTeX
    VariableRAW = gsub("normalsize", "", VariableRAW)
    VariableRAW = gsub("textit", "", VariableRAW)
    VariableRAW = gsub("textbf", "", VariableRAW)
    VariableRAW = gsub("bf", "", VariableRAW)
    VariableRAW = gsub("\\", "", VariableRAW, fixed=TRUE)
    VariableRAW = gsub("$", "", VariableRAW, fixed=TRUE)
    VariableRAW = gsub("{", "", VariableRAW, fixed=TRUE)
    VariableRAW = gsub("}", "", VariableRAW, fixed=TRUE)

    change_ = function (x) {
        if (any(grepl("[_]", x))) {
            paste0(gsub("[_].*$", "", x),
                   strrep("a",
                          nchar(
                              gsub("( )|([,])", "",
                                   gsub("^.*[_]", "", x)))))
        } else {
            x
        }
    }

    VariableRAW = sapply(VariableRAW, change_)
    VariableRAW = paste0(" ", VariableRAW)
    # VariableRAW = gsub("BFI$", "BFI ", VariableRAW)
    
    Space = sapply(VariableRAW, text2px, PX=PX)
    maxSpace = max(Space)
    
    
    dy = nVariable + d_W_mat
    
    for (i in 1:nVariable) {
        cm = cm +
            
            annotate("line",
                     x=rep((i-1) + 0.5, 2)*ech,
                     y=c(dy,
                         dy + dy_L1 + dy_I1/2)*ech,
                     linewidth=lw_L1, color=IPCCgrey67) +
            
            gg_circle(r=dr_I1*ech,
                      xc=((i-1) + 0.5)*ech,
                      yc=(dy + dy_L1 + dy_I1)*ech,
                      color=IPCCgrey67, linewidth=lw_L1,
                      fill="white") +
            
            # gg_circle(r=size_I1*(ech-dr_I1),
            #           xc=((i-1) + 0.5)*ech,
            #           yc=(dy + dy_L1 + dy_I1)*ech,
            #           color=NA, linewidth=0, fill="white") +
            
            # annotation_custom(
            #     subTopic_icon[[i]],
            #     xmin=((i-1) + 0.5 - size_I1)*ech,
            #     xmax=((i-1) + 0.5 + size_I1)*ech,
            #     ymin=(dy +
            #           dy_L1 + dy_I1 - size_I1)*ech,
            #     ymax=(dy +
            #           dy_L1 + dy_I1 + size_I1)*ech) +
            
            annotate("line",
                     x=rep((i-1) + 0.5, 2)*ech,
                     y=c(dy + d_W_mat +
                         dy_L1 + dy_I1 + dy_T1,
                         dy +
                         dy_L1 + dy_I1 + dy_T1 + 
                         maxSpace*ech_T1 + dy_L2_min)*ech,
                     linewidth=lw_L1, color=IPCCgrey67) +
            
            annotate("rect",
                     xmin=((i-1) + 0.1)*ech,
                     xmax=((i-1) + 0.9)*ech,
                     ymin=(dy +
                           dy_L1 + dy_I1 + dy_T1)*ech,
                     ymax=(dy +
                           dy_L1 + dy_I1 + dy_T1 +
                           Space[i]*ech_T1)*ech,
                     fill="white",
                     color=NA) +
            
            annotate("text",
                     x=((i-1) + 0.5)*ech,
                     y=(dy +
                        dy_L1 + dy_I1 + dy_T1)*ech,
                     label=TeX(VariableTeX[i]),
                     hjust=0, vjust=0.675,
                     angle=90,
                     size=size_T1,
                     color=Variable_color[i])
    }

    dy = dy + dy_L1 + dy_I1 + dy_T1 + maxSpace*ech_T1 + dy_L2_min

    nLine = c()
    for (i in 1:nMainTopic) {
        nLim = as.integer((endMainTopic[i] - startMainTopic[i])*ech_T2)
        label = guess_newline(mainTopic[i], px=NULL, nChar=nLim)
        nLine = c(nLine, length(label))
    }
    dy_I2 = dy_I2 + dy_T2line*max(nLine)

    for (i in 1:nMainTopic) {
        
        nLim = as.integer((endMainTopic[i] - startMainTopic[i])*ech_T2)
        label = guess_newline(mainTopic[i], px=NULL, nChar=nLim)
        label =  rev(unlist(strsplit(label, "\n")))
        nLine = length(label)
        
        cm = cm +
            annotation_custom(
                mainTopic_icon[[i]],
                xmin=(midMainTopic[i] - size_I2)*ech,
                xmax=(midMainTopic[i] + size_I2)*ech,
                ymin=(dy + 
                      dy_L4 + dy_I2 - size_I2)*ech,
                ymax=(dy + 
                      dy_L4 + dy_I2 + size_I2)*ech)

        for (j in 1:nLine) {
            cm = cm +
                annotate("text",
                         x=midMainTopic[i]*ech,
                         y=(dy + 
                            dy_L4 + dy_T2 +
                            (j-1)*dy_T2line)*ech,
                         hjust=0.5, vjust=0,
                         angle=0,
                         label=label[j],
                         fontface="bold",
                         size=size_T2,
                         color=IPCCgrey05)
        }

        cm = cm +
            annotate("line",
                     x=c(midMainTopic[i], midMainTopic[i])*ech,
                     y=c(dy,
                         dy + dy_L4)*ech,
                     linewidth=lw_L4, color=IPCCgrey48,
                     lineend="round") +

    annotate("line",
             x=c(startMainTopic[i], endMainTopic[i])*ech,
             y=rep(dy, 2)*ech,
             linewidth=lw_L3, color=IPCCgrey48,
             lineend="round")
    }
    
    cm = cm +
        scale_x_continuous(expand=c(0, 0)) + 
        scale_y_continuous(expand=c(0, 0))
    
    # subTopic_path = subTopic_path[!duplicated(subTopic_path)]
    # subTopic_label = subTopic[!duplicated(subTopic)]
    # names(subTopic_path) = subTopic_label

    # res = list(cm=cm, info=subTopic_path)
    return (cm)
}
