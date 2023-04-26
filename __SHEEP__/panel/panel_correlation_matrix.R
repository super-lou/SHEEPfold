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

panel_correlation_matrix = function (dataEX,
                                     metaEX,
                                     icon_path,
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
    ech_T1 = 0.5

    dy_L2_min = 1
    lw_L2 = 0.25
    
    dx_L3 = 0.5
    lw_L3 = 0.45

    dy_L4 = 0.5
    lw_L4 = 0.45
    
    dy_T2 = 0.3
    dy_T2line = 0.8
    size_T2 = 2.7
    ech_T2 = 2.1
    
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
    metaEX = metaEX[!(metaEX$var %in% logicalCol),]
    
    Topic = strsplit(metaEX$topic, "/")
    Topic = lapply(Topic, complete)
    mainTopicVAR = sapply(Topic, '[[', 1)
    names(mainTopicVAR) = metaEX$var
    lenMainTopic = rle(mainTopicVAR)$lengths
    nMainTopic = length(lenMainTopic)
    startMainTopic =
        cumsum(c(1, lenMainTopic[1:(nMainTopic-1)])) - 1 + dx_L3
    endMainTopic = cumsum(lenMainTopic) - dx_L3
    midMainTopic = (startMainTopic + endMainTopic)/2
    mainTopic = mainTopicVAR[!duplicated(mainTopicVAR)]

    # subTopic = sapply(Topic, '[[', 2)
    # names(subTopic) = metaEX$var
    
    mainTopic_icon = lapply(
        file.path(icon_path, paste0(gsub(" ", "_", mainTopic), ".svg")),
        svgparser::read_svg)

    # subTopic_path = file.path(icon_path, paste0(gsub(" ", "_", subTopic), ".svg"))
    # subTopic_icon = lapply(subTopic_path, svgparser::read_svg)
    
    # names(mainTopic_icon) = mainTopic
    # names(subTopic_icon) = subTopic

    vars2keep = names(dataEX)
    vars2keep = vars2keep[!grepl("([_]obs)|([_]sim)", vars2keep)]

    dataEX = dplyr::mutate(dataEX,
                           dplyr::across(where(is.logical),
                                         as.numeric),
                           .keep="all")
    
    dataEX = dplyr::select(dataEX, vars2keep)

    Model = levels(factor(dataEX$Model))
    nModel = length(Model)

    CORRmat = c()
    Pmat = c()
    for (i in 1:nModel) {
        dataEX_model = dataEX[dataEX$Model == Model[i],]
        nameRow = dataEX_model$Code
        
        dataEX_model = dplyr::select(dataEX_model, -c(Code, Model))

        matchVar = match(names(dataEX_model), metaEX$var)
        matchVar = matchVar[!is.na(matchVar)]
        dataEX_model = dataEX_model[matchVar]

        nameCol = names(dataEX_model)
        Var = nameCol
        nVar = ncol(dataEX_model)

        nCol = ncol(dataEX_model)
        col2rm = c()
        for (i in 1:nCol) {
            if (sum(!is.na(dataEX_model[[i]])) < 3) {
                col2rm = c(col2rm, names(dataEX_model)[i])
            }
        }
        if (!is.null(col2rm)) {
            dataEX_model = dplyr::select(dataEX_model, -col2rm)
            nameCol = names(dataEX_model)
        }
        
        dataEX_model = as.matrix(dataEX_model)
        colnames(dataEX_model) = nameCol
        rownames(dataEX_model) = nameRow

        CORRmat = c(CORRmat,
                    c(cor(dataEX_model,
                          method="spearman",
                          use="pairwise.complete.obs")))
        Pmat = c(Pmat,
                 c(corrplot::cor.mtest(
                                 dataEX_model,
                                 conf.level=1-level,
                                 method="spearman",
                                 use="pairwise.complete.obs")$p))
    }

    CORRmat = array(CORRmat, c(nVar, nVar, nModel))
    Pmat = array(Pmat, c(nVar, nVar, nModel))

    CORRmat_model = apply(CORRmat, 1:2, median, na.rm=TRUE)
    if (nModel > 1) {
        Pmat_model = matrix(rep(0, nVar*nVar), ncol=nVar)
    } else {
        Pmat_model = matrix(Pmat, ncol=nVar)
    }

    if (!is.null(col2rm)) {
        nCol2add = length(col2rm)
        nVarCORR = length(colnames(CORRmat_model))
        
        for (i in 1:nCol2add) {
            missVar = col2rm[i]
            id = which(Var == missVar)
            CORRmat_model = rbind(CORRmat_model[1:(id-1),],
                                  rep(NA, nVarCORR),
                                  CORRmat_model[id:nrow(CORRmat_model),])
            rownames(CORRmat_model)[id] = missVar
            Pmat_model = rbind(Pmat_model[1:(id-1),],
                               rep(NA, nVarCORR),
                               Pmat_model[id:nrow(Pmat_model),])
            rownames(Pmat_model)[id] = missVar
            
        }
        
        for (i in 1:nCol2add) {
            missVar = col2rm[i]
            id = which(Var == missVar)
            CORRmat_model = cbind(CORRmat_model[, 1:(id-1)],
                                  rep(NA, nVarCORR+nCol2add),
                                  CORRmat_model[, id:ncol(CORRmat_model)])
            colnames(CORRmat_model)[id] = missVar
            Pmat_model = cbind(Pmat_model[, 1:(id-1)],
                               rep(NA, nVarCORR+nCol2add),
                               Pmat_model[, id:ncol(Pmat_model)])
            colnames(Pmat_model)[id] = missVar
        }
    }

    Colors = get_color(CORRmat_model, -1, 1,
                       Palette=Palette_rainbow(),
                       colorStep=6, include=TRUE,
                       reverse=TRUE)
    
    COLORmat = matrix(Colors,
                      nrow=nrow(CORRmat_model),
                      ncol=ncol(CORRmat_model))
    SIZEmat = (abs(CORRmat_model))^(1/6)*ech
    Xmat = matrix(rep(0:(nVar-1)*ech, nVar), nrow=nVar, byrow=TRUE) + 0.5*ech
    Ymat = matrix(rep((nVar-1):0*ech, nVar), nrow=nVar) + 0.5*ech
    XMINmat = Xmat - SIZEmat/2
    XMAXmat = Xmat + SIZEmat/2
    YMINmat = Ymat - SIZEmat/2
    YMAXmat = Ymat + SIZEmat/2

    COLOR = unlist(as.list(COLORmat))
    XMIN = unlist(as.list(XMINmat))
    XMAX = unlist(as.list(XMAXmat))
    YMIN = unlist(as.list(YMINmat))
    YMAX = unlist(as.list(YMAXmat))

    nOKPmat = Pmat_model > level
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

    VarTEX = gsub("etiage", "étiage", Var)  
    for (i in 1:nVar) {
        var = VarTEX[i]
        
        if (grepl("[_]", var) & !grepl("[_][{]", var)) {
            var = gsub("[_]", "$_{$", var)
            var = paste0(var, "}")
        } else if (grepl("[_]", var) & grepl("[_][{]", var)) {
            var = gsub("[_][{]", "$_{$", var)
        }

        if (grepl("alpha", var)) {
            var = gsub("alpha", "\\\\bf{\u03b1}", var)
        }

        if (grepl("epsilon", var)) {
            var = gsub("epsilon", "\\\\bf{\u03b5}", var)
        }

        if (grepl("HYP", var)) {
            var = gsub("HYP", "\\\\textit{H}", var)
        }

        if (grepl("inv", var) & !grepl("inv[{]", var)) {
            var = gsub("inv", "\\\\textit{inv}", var)
        } else if (grepl("inv", var) & grepl("inv[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("inv[{]", "\\\\textit{inv}", var)
        } 

        if (grepl("log", var) & !grepl("log[{]", var)) {
            var = gsub("log", "\\\\textit{log}", var)
        } else if (grepl("log", var) & grepl("log[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("log[{]", "\\\\textit{log}", var)
        } 

        if (grepl("mean", var) & !grepl("mean[{]", var)) {
            var = gsub("mean", "\\\\textit{moy}", var)
        } else if (grepl("mean", var) & grepl("mean[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("mean[{]", "\\\\textit{moy}", var)
        } 

        if (grepl("median", var) & !grepl("median[{]", var)) {
            var = gsub("median", "\\\\textit{med}", var)
        } else if (grepl("median", var) & grepl("median[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("median[{]", "\\\\textit{med}", var)
        } 
        
        if (grepl("sqrt", var) & !grepl("sqrt[{]", var)) {
            var = gsub("sqrt", "\\\\textit{sqrt}", var)
        } else if (grepl("sqrt", var) & grepl("sqrt[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("sqrt[{]", "\\\\textit{sqrt}", var)
        } 
        
        VarTEX[i] = var
    }
    VarTEX = paste0("\\textbf{", VarTEX, "}")

    cm = ggplot() + theme_void() + coord_fixed(clip="off") +
        theme(text=element_text(family="Helvetica"),
              plot.margin=margin)

    cm = cm +
        annotate("rect", xmin=XMIN, xmax=XMAX,
                 ymin=YMIN, ymax=YMAX,
                 fill=COLOR)
    
    cm = cm +
        annotate("rect", xmin=0, xmax=nVar*ech, ymin=0, ymax=nVar*ech,
                 linewidth=lw_mat, color=IPCCgrey95, fill=NA)
    for (i in 1:(nVar-1)) {
        cm = cm +
            annotate("line", x=c(0, nVar)*ech, y=c(i, i)*ech,
                     linewidth=lw_mat, color=IPCCgrey95) +
            annotate("line", x=c(i, i)*ech, y=c(0, nVar)*ech,
                     linewidth=lw_mat, color=IPCCgrey95)
    }

    cm = cm +
        annotate("point", x=XP, y=YP,
                 shape=4, size=XPSIZE, color="white")
    
    cm = cm +
        annotate("text",
                 x=rep(-d_W_mat*ech, nVar),
                 y=(nVar-1):0*ech + 0.5*ech,
                 hjust=1, vjust=0.5,
                 label=TeX(VarTEX), size=size_T1,
                 color=IPCCgrey40) +
        
        annotate("text",
                 x=0:(nVar-1)*ech + 0.5*ech,
                 y=rep(-d_W_mat*ech, nVar),
                 hjust=1, vjust=0.5,
                 angle=90,
                 label=TeX(VarTEX), size=size_T1,
                 color=IPCCgrey40)

    VarRAW = metaEX$var
    VarRAW = gsub("median", "med", VarRAW)
    VarRAW = gsub("mean", "moy", VarRAW)
    VarRAW = gsub("HYP", "H", VarRAW)
    VarRAW = gsub("alpha", "A", VarRAW)
    VarRAW = gsub("epsilon", "E", VarRAW)
    OK_ = grepl("[_]", VarRAW)
    tmp = gsub("^.*[_]", "", VarRAW)
    tmp = gsub("([{])|([}])", "", tmp)
    tmp[!OK_] = ""
    tmp = gsub("[[:alnum:]]", "*", tmp)
    VarRAW[OK_] = gsub("[{].*[}]", "", VarRAW[OK_])
    VarRAW[!OK_] = gsub("([{])|([}])", "", VarRAW[!OK_])
    VarRAW = gsub("[_].*$", "", VarRAW)
    VarRAW = paste0(VarRAW, tmp)
    VarRAW = strsplit(VarRAW, "*")

    convert2space = function (X) {
        X = gsub("[[:digit:]]", "1.1", X)
        X = gsub("[[:upper:]]", "1.6", X)
        X = gsub("[[:lower:]]", "1.1", X)
        X = gsub("([-])|([,])", "0.5", X)
        X = gsub("([*])", "0.9", X)
        return (X)    
    }

    Space = lapply(VarRAW, convert2space)
    Space = lapply(Space, as.numeric)
    Space = lapply(Space, sum)
    Space = unlist(Space)
    maxSpace = max(Space)

    dy = nVar + d_W_mat
    
    for (i in 1:nVar) {
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
                     label=TeX(VarTEX[i]),
                     hjust=0, vjust=0.675,
                     angle=90,
                     size=size_T1,
                     color=IPCCgrey40)
    }

    dy = dy + dy_L1 + dy_I1 + dy_T1 + maxSpace*ech_T1 + dy_L2_min


    nLine = c()
    for (i in 1:nMainTopic) {
        nLim = as.integer((endMainTopic[i] - startMainTopic[i])*ech_T2)
        label = guess_newline(mainTopic[i], nLim=nLim)
        nLine = c(nLine, length(label))
    }
    dy_I2 = dy_I2 + dy_T2line*max(nLine)

    for (i in 1:nMainTopic) {
        
        nLim = as.integer((endMainTopic[i] - startMainTopic[i])*ech_T2)
        label = guess_newline(mainTopic[i], nLim=nLim)
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
