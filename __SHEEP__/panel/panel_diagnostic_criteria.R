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

panel_diagnostic_criteria = function (dataEXind,
                                      metaEXind,
                                      meta,
                                      Colors,
                                      codeLight=NULL,
                                      groupCode=NULL,
                                      icon_path="",
                                      Warnings=NULL,
                                      alpha_marker=0.85,
                                      Probs=0.1,
                                      Alpha=0.7,
                                      title="",
                                      dTitle=0,
                                      add_name=FALSE,
                                      group_name="dans la région",
                                      dx_interp=7.6,
                                      nLim_interp=130,
                                      margin_add=margin(t=0, r=0,
                                                        b=0, l=0,
                                                        "mm")) {

    Alpha = sort(c(Alpha, alpha_marker))
    alpha_marker = max(Alpha)
    Alpha = Alpha[Alpha != alpha_marker]
    Probs = sort(Probs)

    NP = length(Probs)
    
    ## 1. PARAMETERS _____________________________________________________
    dl_grid=0.10
    dr_grid=0.10

    dspace_label = 0.12
    dspace_grid = 0.13

    dx_rect = 0.12
    dy_rect = 0.05
    dx_label = -0.1
    dx_bar = 0.08
        
    dy_arrow = 0.14#0.045
    dl_arrow = 0.2


    x_title = 0

    dy_mod = 0.5
    dx_mod_subtitle = 4.4
    dy_mod_subtitle = 0.05
    dx_mod_name = 0
    dy_mod_name = 0.5
    dx_mod_space = 0.71
    dy_mod_line = 0.18
    dx_mod_line = 0.09
    dy_mod_surf = 0.3
    alpha_mod_line = c(Alpha, alpha_marker)
    dl_mod_line = seq(0.3,
                      0.15,
                      length.out=length(alpha_mod_line)) #c(0.3, 0.15)

    ech_text_mod = 0.36

    dy_leg = 1.5
    if (is.null(codeLight)) {
        dy_leg = dy_leg - dy_mod_surf
    }
    dh_leg = 3
    dx_leg_line = 0.3
    dy_leg_line = 0.25
    dl_leg_line = 0.85
    dy_leg_point = 0.4
    w_leg_line = 0.06#0.14
    dl_leg_line_grad = 0.2
    dr_leg_line_grad = 0.1
    w_leg_line_grad = 0.3
    dx_leg_arrow = 11.5
    dy_leg_arrow = 0.6
    dy_leg_arrow_gap = 0.1
    dx_leg_arrow_text = 0.1
    
    # dx_interp = 9.8
    dx_interp_text = 0.12
    dl_interp_text_line = 0.05
    dr_interp_text_line = 0.05
    w_interp_text_line = 0.2
    dy_interp_text = 0.3
    dy_interp_line = 0.35
    dy_interp_nline = 0.3
    # nLim_interp = 99
    
    
    dx_arrow = 0.8
    ech_bar = 5.5

    perfect_tick_val = list("KGE"=c(1),
                            "^Biais$"=c(0),
                            "(^epsilon)|(^alpha)"=c(1),
                            "default"=c(0))
    
    major_tick_val = list("KGE"=c(0.5),
                          "^Biais$"=c(-0.2, 0.2),
                          "(^epsilon)|(^alpha)"=c(0.5, 2),
                          "default"=c(-1, 1))
    
    minor_tick_val =
        list("KGE"=c(0, 0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9),
             "^Biais$"=c(-0.5, -0.4, -0.3, -0.1, 0.1, 0.3, 0.4, 0.5),
             "(^epsilon)|(^alpha)"=c(0, 0.2, 0.4, 0.6, 0.8, 1.2, 1.4, 1.6, 1.8),
             "default"=c(-0.8, -0.6, -0.4, -0.2, 0.2, 0.4, 0.6, 0.8))


    norm_tick_info = c()
    shift_tick_info = c()
    for (i in 1:length(major_tick_val)) {
        perfect_tick = perfect_tick_val[[i]]
        major_tick = major_tick_val[[i]]
        name = names(major_tick_val)[i]
        minor_tick = minor_tick_val[[i]]
        norm_tick_info = c(norm_tick_info,
                           1 / (max(c(perfect_tick, major_tick, minor_tick)) -
                                min(c(perfect_tick, major_tick, minor_tick))))
        names(norm_tick_info)[i] = name
        shift_tick_info = c(shift_tick_info,
                            -min(c(perfect_tick, major_tick, minor_tick)))
        names(shift_tick_info)[i] = name
        
    }

    norm_tick_info = norm_tick_info*ech_bar
    
    ymin_grid = 0#min(c(minor_tick, major_tick))*ech_bar
    ymax_grid = ech_bar#max(c(minor_tick, major_tick))*ech_bar

    dy_icon_out = 0.18
    
    fact = 1.15

    dy_gap = 0.35
    dy_Ind = ymax_grid + dy_gap
    
    lw_mat = 0.4
    
    dy_L1 = 0.2
    lw_L1 = 0.25
    
    dy_I1 = 0.2
    size_I1 = 0.45
    dr_I1 = 0.15
    
    dy_T1 = 0.1
    size_T1 = 3.2
    dy_T1_space = 0.6
    ech_text_var = 0.46

    dy_L2_min = 0.5
    lw_L2 = 0.25

    dl_L3 = 0.3
    dr_L3 = 0.3
    dx_L3 = 0.5
    lw_L3 = 0.45


    dx_T2 = 0.3
    dy_T2 = 0.15
    dy_T2line = 0.4
    size_T2 = 2.7
    ech_T2 = 7

    # dx_I2 = 0.2
    dy_I2 = 0.6
    size_I2 = 0.5
    
    ech_x = 2

## 2. DATA FORMATTING ________________________________________________
    complete = function (X) {
        if (length(X) < 2) {
            X = c(X, NA)
        }
        return (X)
    }

    logicalCol = names(dataEXind)[sapply(dataEXind, class) == "logical"]
    dataEXind = dataEXind[!(names(dataEXind) %in% logicalCol)]
    metaEXind = metaEXind[!(metaEXind$var %in% logicalCol),]
    
    Topic = strsplit(metaEXind$topic, "/")
    Topic = lapply(Topic, complete)
    mainTopicVAR = sapply(Topic, '[[', 1)
    names(mainTopicVAR) = metaEXind$var
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
    names(mainTopic_icon) = mainTopic

    vars2keep = names(dataEXind)
    vars2keep = vars2keep[!grepl("([_]obs)|([_]sim)", vars2keep)]

    dataEXind = dplyr::mutate(dataEXind,
                              dplyr::across(where(is.logical),
                                            as.numeric),
                              .keep="all")

    dataEXind = dplyr::select(dataEXind, vars2keep)


    CodeIN = c(codeLight, groupCode)
    
    Model = levels(factor(dataEXind$Model[dataEXind$Code %in% CodeIN]))
    nModel = length(Model)

    dataEXind_tmp = dataEXind
    dataEXind_tmp = dplyr::select(dataEXind_tmp, -c(Code, Model))

    matchVar = match(names(dataEXind_tmp), metaEXind$var)
    matchVar = matchVar[!is.na(matchVar)]
    dataEXind_tmp = dataEXind_tmp[matchVar]

    nameCol = names(dataEXind_tmp)
    Var = nameCol
    nVar = length(Var)
    
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
            var = gsub("mean", "", var)
        } else if (grepl("mean", var) & grepl("mean[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("mean[{]", "", var)
        } 

        if (grepl("median", var) & !grepl("median[{]", var)) {
            var = gsub("median", "", var)
        } else if (grepl("median", var) & grepl("median[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("median[{]", "", var)
        } 
        
        if (grepl("racine", var) & !grepl("racine[{]", var)) {
            var = gsub("racine", "\u221A", var)
        } else if (grepl("racine", var) & grepl("racine[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("racine[{]", "\u221A", var)
        }
        
        VarTEX[i] = var
    }
    VarTEX = paste0("\\textbf{", VarTEX, "}")

    Code = levels(factor(dataEXind$Code))    
    id_save = ""
    space = 0
    Spaces = c()


## 3. GRAPHICAL INITIALISATION _______________________________________
    Ind = ggplot() + theme_void() + coord_fixed(clip="off") +
        theme(plot.margin=margin_add,
              plot.title=element_text(size=9,
                                      vjust=0, hjust=dTitle,
                                      color=IPCCgrey25)) +
        ggtitle(title)
    
    x_limits =
        c((-dx_label-dspace_label)*ech_x,
        (nVar)*ech_x)
    dy = dy_Ind
    Ind = Ind 


## 4. INDICATOR BAR PLOT _____________________________________________
    for (i in 1:nVar) {
        var = Var[i]

        varRAW = gsub("[{]", "[{]", var)
        varRAW = gsub("[}]", "[}]", varRAW)
        varRAW = gsub("[_]", "[_]", varRAW)
        
        id = sapply(names(perfect_tick_val), grepl, x=varRAW)
        if (all(!id)) {
            id = "default"
        } else {
            id = names(perfect_tick_val)[id]
        }

        perfect_tick = perfect_tick_val[[id]]
        major_tick = major_tick_val[[id]]
        minor_tick = minor_tick_val[[id]]
        norm = norm_tick_info[id]
        shift = shift_tick_info[id]
        
        if (id != id_save) {

            space = space + dspace_label
            
            Ind = Ind +
                annotate("rect",
                         xmin=(i-1+dl_grid+space)*ech_x,
                         xmax=(i-dr_grid+space)*ech_x,
                         ymin=(max(c(major_tick,
                                     perfect_tick))+shift)*norm,
                         ymax=ymax_grid + dy_rect*ech_bar,
                         fill=IPCCgrey99,
                         size=0) +
                annotate("rect",
                         xmin=(i-1+dl_grid+space)*ech_x,
                         xmax=(i-dr_grid+space)*ech_x,
                         ymin=ymin_grid - dy_rect*ech_bar,
                         ymax=(min(c(major_tick,
                                   perfect_tick))+shift)*norm,
                         fill=IPCCgrey99,
                         size=0)

            Ind = Ind +
                annotate("rect",
                         xmin=(i-1+dl_grid+space)*ech_x,
                         xmax=(i-1-dx_label+space-dspace_label -
                               dx_rect)*ech_x,
                         ymin=ymin_grid - dy_rect*ech_bar,
                         ymax=ymax_grid + dy_rect*ech_bar,
                         fill=IPCCgrey95,
                         size=0)

            for (t in perfect_tick) {
                Ind = Ind +
                    annotate("line",
                             x=c(i-1+dl_grid+space,
                                 i-dr_grid+space)*ech_x,
                             y=(c(t, t)+shift)*norm,
                             color=IPCCgrey60,
                             size=0.4,
                             lineend="round")
            }
            
            for (t in major_tick) {
                Ind = Ind +
                    annotate("line",
                             x=c(i-1+dl_grid+space,
                                 i-dr_grid+space)*ech_x,
                             y=(c(t, t)+shift)*norm,
                             color=IPCCgrey60,
                             size=0.2,
                             lineend="round")
            }
            
            for (t in minor_tick) {
                Ind = Ind +
                    annotate("line",
                             x=c(i-1+dl_grid+space,
                                 i-dr_grid+space)*ech_x,
                             y=(c(t, t)+shift)*norm,
                             color=IPCCgrey85,
                             size=0.2,
                             lineend="round")
            }

            for (t in perfect_tick) {
                Ind = Ind +
                    annotate("text",
                             x=(i-1-dx_label+space)*ech_x,
                             y=(t+shift)*norm,
                             label=t,
                             fontface="bold",
                             hjust=1,
                             vjust=0.57,
                             color=IPCCgrey40,
                             size=2.2)
            }
            
            for (t in major_tick) {
                Ind = Ind +
                    annotate("text",
                             x=(i-1-dx_label+space)*ech_x,
                             y=(t+shift)*norm,
                             label=t,
                             hjust=1,
                             vjust=0.55,
                             color=IPCCgrey40,
                             size=2.2)
            }
            
            for (t in minor_tick) {
                Ind = Ind +
                    annotate("text",
                             x=(i-1-dx_label+space)*ech_x,
                             y=(t+shift)*norm,
                             label=t,
                             hjust=1,
                             vjust=0.49,
                             color=IPCCgrey67,
                             size=2)
            }
            
        } else {
            space = space - dspace_grid

            Ind = Ind +
                annotate("rect",
                         xmin=(i-1-dr_grid+space)*ech_x,
                         xmax=(i-dr_grid+space)*ech_x,
                         ymin=(max(c(major_tick,
                                     perfect_tick))+shift)*norm,
                         ymax=ymax_grid + dy_rect*ech_bar,
                         fill=IPCCgrey99,
                         size=0) +
                annotate("rect",
                         xmin=(i-1-dr_grid+space)*ech_x,
                         xmax=(i-dr_grid+space)*ech_x,
                         ymin=ymin_grid - dy_rect*ech_bar,
                         ymax=(min(c(major_tick,
                                     perfect_tick))+shift)*norm,
                         fill=IPCCgrey99,
                         size=0)
            
            for (t in perfect_tick) {
                Ind = Ind +
                    annotate("line",
                             x=c(i-1-dr_grid+space,
                                 i-dr_grid+space)*ech_x,
                             y=(c(t, t)+shift)*norm,
                             color=IPCCgrey60,
                             size=0.4,
                             lineend="round")
            }
            for (t in major_tick) {
                Ind = Ind +
                    annotate("line",
                             x=c(i-1-dr_grid+space,
                                 i-dr_grid+space)*ech_x,
                             y=(c(t, t)+shift)*norm,
                             color=IPCCgrey60,
                             size=0.2,
                             lineend="round")
            }
            for (t in minor_tick) {
                Ind = Ind +
                    annotate("line",
                             x=c(i-1-dr_grid+space,
                                 i-dr_grid+space)*ech_x,
                             y=(c(t, t)+shift)*norm,
                             color=IPCCgrey85,
                             size=0.2,
                             lineend="round")
            }
        }
        id_save = id
        Spaces = c(Spaces, space)
    }

    for (i in 1:nVar) {
        var = Var[i]

        varRAW = gsub("[{]", "[{]", var)
        varRAW = gsub("[}]", "[}]", varRAW)
        varRAW = gsub("[_]", "[_]", varRAW)
        
        id = sapply(names(perfect_tick_val), grepl, x=varRAW)
        if (all(!id)) {
            id = "default"
        } else {
            id = names(perfect_tick_val)[id]
        }

        perfect_tick = perfect_tick_val[[id]]
        major_tick = major_tick_val[[id]]
        minor_tick = minor_tick_val[[id]]
        norm = norm_tick_info[id]
        shift = shift_tick_info[id]

        space = Spaces[i]

        for (j in 1:nModel) {
            model = Model[j]
            dataEXind_model = dataEXind[dataEXind$Model == model,]
            dataEXind_model_group =
                dataEXind_model[dataEXind_model$Code %in% groupCode,]

            if (nrow(dataEXind_model_group) != 0) {
                Q = (quantile(dataEXind_model_group[[var]],
                              c(min(Probs), 1-min(Probs)),
                              na.rm=TRUE)+shift)*norm
                
                Q[Q > ymax_grid] = ymax_grid
                Q[ymin_grid > Q] = ymin_grid
                
                Ind = Ind +
                    annotate("line",
                             x=rep((i-1) + 0.5 -
                                   (nModel/2)*dx_bar+dx_bar/2 +
                                   (j-1)*dx_bar + space, 2)*ech_x,
                             y=Q,
                             color="white",
                             linewidth=2,
                             lineend="round")
            }
        }

        for (j in 1:nModel) {
            model = Model[j]
            dataEXind_model = dataEXind[dataEXind$Model == model,]
            dataEXind_model_group =
                dataEXind_model[dataEXind_model$Code %in% groupCode,]
            
            if (nrow(dataEXind_model_group) != 0) {
                for (k in 1:NP) {
                    Q = (quantile(dataEXind_model_group[[var]],
                                  c(Probs[k], 1-Probs[k]),
                                  na.rm=TRUE)+shift)*norm
                    
                    Q[Q > ymax_grid] = ymax_grid
                    Q[ymin_grid > Q] = ymin_grid
                    
                    Ind = Ind +
                        annotate("line",
                                 x=rep((i-1) + 0.5 -
                                       (nModel/2)*dx_bar+dx_bar/2 +
                                       (j-1)*dx_bar + space, 2)*ech_x,
                                 y=Q,
                                 color=
                                     Colors[names(Colors) == model],
                                 alpha=Alpha[k],
                                 linewidth=1.3,
                                 lineend="round")
                }
            }
        }
        

        
        for (j in 1:nModel) {
            model = Model[j]
            dataEXind_model = dataEXind[dataEXind$Model == model,]

            if (!is.null(codeLight)) {
                dataEXind_model_code =
                    dataEXind_model[dataEXind_model$Code ==
                                    codeLight,]
                value = dataEXind_model_code[[var]]
            } else {
                dataEXind_model_group =
                    dataEXind_model[dataEXind_model$Code %in%
                                    groupCode,]
                value = median(dataEXind_model_group[[var]],
                               na.rm=TRUE)
            }

            if (is.null(value)) {
                next
            }
            if (identical(value, numeric(0))) {
                next
            }
            if (is.na(value)) {
                next
            }
            
            above = (value+shift)*norm >
                ymax_grid
            below = ymin_grid >
                (value+shift)*norm

            if (!above & !below) {
                if (!is.null(codeLight)) {
                    Ind = Ind +
                        annotate("point",
                                 x=((i-1) + 0.5 -
                                    (nModel/2)*dx_bar +
                                    dx_bar/2 +
                                    (j-1)*dx_bar +
                                    space)*ech_x,
                                 y=(value+shift)*norm,
                                 color="white",
                                 size=2.2,
                                 stroke=0)
                } else {
                    Ind = Ind +
                        annotate("line",
                                 x=c(((i-1) + 0.5 -
                                      (nModel/2)*dx_bar +
                                      dx_bar/2 +
                                      (j-1)*dx_bar +
                                      space)*ech_x -
                                     w_leg_line/2,
                                 ((i-1) + 0.5 -
                                  (nModel/2)*dx_bar +
                                  dx_bar/2 +
                                  (j-1)*dx_bar +
                                  space)*ech_x +
                                 w_leg_line/2),
                                 y=rep(value +
                                       shift, 2)*norm,
                                 color="white",
                                 linewidth=1.5,
                                 lineend="round")
                }
            }
        }
        
        for (j in 1:nModel) {
            model = Model[j]
            dataEXind_model = dataEXind[dataEXind$Model == model,]

            if (!is.null(codeLight)) {
                dataEXind_model_code =
                    dataEXind_model[dataEXind_model$Code ==
                                    codeLight,]
                value = dataEXind_model_code[[var]]
            } else {
                dataEXind_model_group =
                    dataEXind_model[dataEXind_model$Code %in%
                                    groupCode,]
                value = median(dataEXind_model_group[[var]],
                               na.rm=TRUE)
            }

            if (is.null(value)) {
                next
            }
            if (identical(value, numeric(0))) {
                next
            }
            if (is.na(value)) {
                next
            }
            
            above = (value+shift)*norm >
                ymax_grid
            below = ymin_grid >
                (value+shift)*norm

            if (!above & !below) {
                if (!is.null(codeLight)) {
                    Ind = Ind +
                        annotate("point",
                                 x=((i-1) + 0.5 -
                                    (nModel/2)*dx_bar +
                                    dx_bar/2 +
                                    (j-1)*dx_bar +
                                    space)*ech_x,
                                 y=(value+shift)*norm,
                                 color=
                                     Colors[names(Colors) == model],
                                 alpha=alpha_marker,
                                 size=1.5,
                                 stroke=0)
                } else {
                    Ind = Ind +
                        annotate("line",
                                 x=c(((i-1) + 0.5 -
                                      (nModel/2)*dx_bar +
                                      dx_bar/2 +
                                      (j-1)*dx_bar +
                                      space)*ech_x -
                                     w_leg_line/2,
                                 ((i-1) + 0.5 -
                                  (nModel/2)*dx_bar +
                                  dx_bar/2 +
                                  (j-1)*dx_bar +
                                  space)*ech_x +
                                 w_leg_line/2),
                                 y=rep(value +
                                       shift, 2)*norm,
                                 alpha=alpha_marker,
                                 color=Colors[names(Colors) ==
                                              model],
                                 linewidth=0.9,
                                 lineend="round")
                }
            }
        }

        for (j in 1:nModel) {
            model = Model[j]
            dataEXind_model = dataEXind[dataEXind$Model == model,]

            if (!is.null(codeLight)) {
                dataEXind_model_code =
                    dataEXind_model[dataEXind_model$Code ==
                                    codeLight,]
                value = dataEXind_model_code[[var]]
            } else {
                dataEXind_model_group =
                    dataEXind_model[dataEXind_model$Code %in%
                                    groupCode,]
                value = median(dataEXind_model_group[[var]],
                               na.rm=TRUE)
            }

            if (is.null(value)) {
                next
            }
            if (identical(value, numeric(0))) {
                next
            }
            if (is.na(value)) {
                next
            }
            
            above = (value+shift)*norm >
                ymax_grid
            below = ymin_grid >
                (value+shift)*norm
            
            if (above | below) {
                x = ((i-1) + 0.5 -
                     (nModel/2)*dx_bar+dx_bar/2 +
                     (j-1)*dx_bar + space)*ech_x
                
                if (above) {
                    y = ymax_grid +
                        dy_arrow
                    yend = ymax_grid +
                        dy_arrow +
                        dl_arrow
                } else if (below) {
                    y = ymin_grid -
                        dy_arrow
                    yend = ymin_grid -
                        dy_arrow -
                        dl_arrow
                }
                Ind = Ind +
                    annotate("segment",
                             x=x, xend=x,
                             y=y, yend=yend,
                             color="white",
                             linewidth=0.8,
                             arrow=arrow(length=unit(dx_arrow,
                                                     "mm")),
                             lineend="round") + 
                    annotate("segment",
                             x=x, xend=x,
                             y=y, yend=yend,
                             color=
                                 Colors[names(Colors) == model],
                             alpha=alpha_marker,
                             linewidth=0.3,
                             arrow=arrow(length=unit(dx_arrow,
                                                     "mm")),
                             lineend="round")
            }
        }
    }

    
## 5. TOPIC INFO _____________________________________________________
    for (i in 1:nVar) { 

        space = Spaces[i]
        
        Ind = Ind +
            annotate("text",
                     x=((i-1) + 0.5 + space)*ech_x,
                     y=(dy +
                        dy_T1),
                     label=TeX(VarTEX[i]),
                     hjust=0.5, vjust=0,
                     angle=0,
                     size=size_T1,
                     color=IPCCgrey40)
    }

    dy = dy + dy_T1 + dy_T1_space

    for (i in 1:nMainTopic) {

        spaces = Spaces[mainTopicVAR == mainTopic[i]]
        space = spaces[1]
        
        nLim = as.integer((endMainTopic[i] - startMainTopic[i])*ech_T2)
        label = guess_newline(mainTopic[i], nLim=nLim)
        label =  rev(unlist(strsplit(label, "\n")))
        nLine = length(label)
        
        Ind = Ind +
            annotation_custom(
                mainTopic_icon[[i]],
                xmin=(startMainTopic[i] + space)*ech_x - size_I2,
                xmax=(startMainTopic[i] + space)*ech_x + size_I2,
                ymin=(dy + dy_I2 - size_I2),
                ymax=(dy + dy_I2 + size_I2))

        for (j in 1:nLine) {
            if (nLine == 1) {
                y = dy + dy_I2 - dy_T2 
            } else {
                y = dy + dy_I2 + (j-1)*dy_T2line -
                    dy_T2line/2 - dy_T2
            }
            Ind = Ind +
                annotate("text",
                         x=(startMainTopic[i] + space + dx_T2)*ech_x,
                         y=y,
                         hjust=0, vjust=0,
                         angle=0,
                         label=label[j],
                         fontface="bold",
                         size=size_T2,
                         color=IPCCgrey05)
        }

        Ind = Ind +
            annotate("line",
                     x=c(startMainTopic[i] - dl_L3 +
                         spaces[1],
                         endMainTopic[i] + dr_L3 + 
                         spaces[length(spaces)])*ech_x,
                     y=rep(dy, 2),
                     linewidth=lw_L3, color=IPCCgrey48,
                     lineend="round")
    }

    dy = dy + dy_I2 + size_I2

    
## 6. MODEL HYDRO LEGEND _____________________________________________
    title_mod = "MODÈLES HYDROLOGIQUES"
    Ind = Ind +
        annotate("text",
                 x=x_title,
                 y=ymin_grid - (dy_gap +
                                dy_mod),
                 label=title_mod,
                 color=IPCCgrey25,
                 hjust=0, vjust=0, size=2.5)
    if (!is.null(codeLight)) {
        Ind = Ind +
            annotate("richtext",
                     x=x_title + dx_mod_subtitle,
                     y=ymin_grid - (dy_gap +
                                    dy_mod) +
                         dy_mod_subtitle,
                     label="(Surface estimée par le modèle en km<sup>2</sup>)",
                     color=IPCCgrey25,
                     fill=NA, label.color=NA,
                     hjust=0, vjust=0.43, size=2.2)
    }

    PX = get_alphabet_in_px(save=TRUE)
    
    Span = lapply(strsplit(Model, "*"), X2px, PX=PX)
    Span = lapply(Span, sum)
    Span = unlist(Span)
    Span = c(0, Span)

    find = function (x, table) {
        which(grepl(x, table))[1]
    }
    
    color = Colors[sapply(Model, find, table=names(Colors))]
    for (i in 1:nModel) {

        for (k in 1:length(dl_mod_line)) {
            Ind = Ind +
                annotate("line",
                         x=c(dx_mod_name + dx_mod_space*(i-1) +
                             max(dl_mod_line) -
                             dl_mod_line[k],
                             dx_mod_name + dx_mod_space*(i-1) +
                             max(dl_mod_line))*ech_x +
                           sum(Span[1:i])*ech_text_mod,
                         y=rep(ymin_grid -
                               (dy_gap +
                                dy_mod +
                                dy_mod_name -
                                dy_mod_line),
                               2),
                         alpha=alpha_mod_line[k],
                         linewidth=1.5,
                         color=color[i],
                         lineend="round")
        }
        Ind = Ind +
            annotate("text",
                     x=(dx_mod_name + dx_mod_space*(i-1) +
                        max(dl_mod_line) +
                        dx_mod_line)*ech_x +
                       sum(Span[1:i])*ech_text_mod,
                     y=ymin_grid -
                         (dy_gap +
                          dy_mod +
                          dy_mod_name),
                     label=Model[i],
                     color=IPCCgrey25,
                     hjust=0, vjust=0, size=2.8)
        
        if (!is.null(codeLight)) {
            Ind = Ind +
                annotate("text",
                         x=(dx_mod_name + dx_mod_space*(i-1) +
                            max(dl_mod_line) +
                            dx_mod_line)*ech_x +
                           sum(Span[1:i])*ech_text_mod,
                         y=ymin_grid -
                             (dy_gap +
                              dy_mod +
                              dy_mod_name +
                              dy_mod_surf),
                         label="(xxxx)",
                         color=IPCCgrey25,
                         hjust=0, vjust=0, size=2)
        }
    }



## 7. BAR PLOT LEGEND ________________________________________________
    Ind = Ind +
        annotate("text",
                 x=x_title,
                 y=ymin_grid -
                     (dy_gap +
                      dy_mod +
                      dy_leg),
                 label="LÉGENDE",
                 color=IPCCgrey25,
                 hjust=0, vjust=0, size=2.5)

    for (k in 1:NP) {
        Ind = Ind +
            annotate("line",
                     x=rep(dx_leg_line, 2),
                     y=c(ymin_grid -
                         (dy_gap +
                          dy_mod +
                          dy_leg +
                          dy_leg_line +
                          (k-1)*dl_leg_line/3),
                         ymin_grid -
                         (dy_gap +
                          dy_mod +
                          dy_leg +
                          dy_leg_line +
                          dl_leg_line +
                          (NP-k)*dl_leg_line/3)),
                     color=IPCCgrey50,
                     alpha=Alpha[k],
                     linewidth=1.5,
                     lineend="round")
    }
    
    if (!is.null(codeLight)) {
        Ind = Ind +
            annotate("point",
                     x=rep(dx_leg_line, 2),
                     y=ymin_grid -
                         (dy_gap +
                          dy_mod +
                          dy_leg +
                          dy_leg_line +
                          dy_leg_point +
                          (NP-1)*dl_leg_line/3),
                     color="white",
                     size=2.4,
                     stroke=0) +
            annotate("point",
                     x=rep(dx_leg_line, 2),
                     y=ymin_grid -
                         (dy_gap +
                          dy_mod +
                          dy_leg +
                          dy_leg_line +
                          dy_leg_point +
                          (NP-1)*dl_leg_line/3),
                     alpha=alpha_marker,
                     color=IPCCgrey50,
                     size=1.5,
                     stroke=0)
    } else {
        Ind = Ind +
            annotate("line",
                     x=c(dx_leg_line - w_leg_line/2,
                         dx_leg_line + w_leg_line/2),
                     y=rep(ymin_grid -
                           (dy_gap +
                            dy_mod +
                            dy_leg +
                            dy_leg_line +
                            dy_leg_point +
                            (NP-1)*dl_leg_line/3), 2),
                     color="white",
                     linewidth=1.7,
                     lineend="round") +
            annotate("line",
                     x=c(dx_leg_line - w_leg_line/2,
                         dx_leg_line + w_leg_line/2),
                     y=rep(ymin_grid -
                           (dy_gap +
                            dy_mod +
                            dy_leg +
                            dy_leg_line +
                            dy_leg_point +
                            (NP-1)*dl_leg_line/3), 2),
                     alpha=alpha_marker,
                     color=IPCCgrey50,
                     linewidth=0.9,
                     lineend="round")
    }

    for (k in 1:NP) {
        if (k == 1) {
            end = paste0(" des résultats ", group_name)
        } else {
            end = ""
        }
        Ind = Ind +
            annotate("line",
                     x=c(dx_leg_line +
                         dl_leg_line_grad,
                         dx_leg_line +
                         dl_leg_line_grad + 
                         w_leg_line_grad),
                     y=rep(ymin_grid -
                           (dy_gap +
                            dy_mod +
                            dy_leg +
                            dy_leg_line +
                            (k-1)*dl_leg_line/3), 2),
                     color=IPCCgrey85,
                     linewidth=0.25) +
            annotate("richtext",
                     x=dx_leg_line +
                         dl_leg_line_grad +
                         w_leg_line_grad +
                         dr_leg_line_grad,
                     y=ymin_grid -
                         (dy_gap +
                          dy_mod +
                          dy_leg +
                          dy_leg_line +
                          (k-1)*dl_leg_line/3),
                     label=paste0("<b>Quantile à ",
                     (1-Probs[k])*100,
                     " %</b>", end),
                     fill=NA, label.color=NA,
                     color=IPCCgrey50,
                     hjust=0, vjust=0.6, size=2.4)
    }        

    if (!is.null(codeLight)) {
        label = "<b>Valeur</b> du critère à la station"
    } else {
        label = paste0("<b>Médiane</b> du critère ", group_name)
    }
        Ind = Ind +
            annotate("line",
                     x=c(dx_leg_line +
                         dl_leg_line_grad,
                         dx_leg_line +
                         dl_leg_line_grad + 
                         w_leg_line_grad),
                     y=rep(ymin_grid -
                           (dy_gap +
                            dy_mod +
                            dy_leg +
                            dy_leg_line +
                            dy_leg_point +
                            (NP-1)*dl_leg_line/3), 2),
                     color=IPCCgrey85,
                     linewidth=0.25) +
            annotate("richtext",
                     x=dx_leg_line +
                         dl_leg_line_grad +
                         w_leg_line_grad +
                         dr_leg_line_grad,
                     y=ymin_grid -
                         (dy_gap +
                          dy_mod +
                          dy_leg +
                          dy_leg_line +
                          dy_leg_point +
                          (NP-1)*dl_leg_line/3),
                     label=label,
                     fill=NA, label.color=NA,
                     color=IPCCgrey50,
                     hjust=0, vjust=0.6, size=2.4)

    for (k in 1:NP) {
        if (k == 1) {
            end = paste0(" des résultats ", group_name)
        } else {
            end = ""
        }
        Ind = Ind +
            annotate("line",
                     x=c(dx_leg_line +
                         dl_leg_line_grad,
                         dx_leg_line +
                         dl_leg_line_grad + 
                         w_leg_line_grad),
                     y=rep(ymin_grid -
                           (dy_gap +
                            dy_mod +
                            dy_leg +
                            dy_leg_line +
                            dl_leg_line +
                            (NP-k)*dl_leg_line/3), 2),
                     color=IPCCgrey85,
                     linewidth=0.25) +
            annotate("richtext",
                     x=dx_leg_line +
                         dl_leg_line_grad +
                         w_leg_line_grad +
                         dr_leg_line_grad,
                     y=ymin_grid -
                         (dy_gap +
                          dy_mod +
                          dy_leg +
                          dy_leg_line +
                          dl_leg_line +
                          (NP-k)*dl_leg_line/3),
                     label=paste0("<b>Quantile à ", Probs[k]*100,
                                  " %</b>", end),
                     fill=NA, label.color=NA,
                     color=IPCCgrey50,
                     hjust=0, vjust=0.6, size=2.4)
    }
    
    Ind = Ind + 
        annotate("segment",
                 x=dx_leg_line,
                 xend=dx_leg_line,
                 y=ymin_grid - (dy_gap +
                                dy_mod +
                                dy_leg +
                                dy_leg_line +
                                dl_leg_line +
                                dy_leg_arrow -
                                dy_leg_arrow_gap/2),
                 yend=ymin_grid - (dy_gap +
                                   dy_mod +
                                   dy_leg +
                                   dy_leg_line +
                                   dl_leg_line +
                                   dy_leg_arrow -
                                   dy_leg_arrow_gap/2 -
                                   dl_arrow),
                 color=IPCCgrey50,
                 alpha=alpha_marker,
                 linewidth=0.3,
                 arrow=arrow(length=unit(dx_arrow,
                                         "mm")),
                 lineend="round") +
        annotate("segment",
                 x=dx_leg_line,
                 xend=dx_leg_line,
                 y=ymin_grid - (dy_gap +
                                dy_mod +
                                dy_leg +
                                dy_leg_line +
                                dl_leg_line +
                                dy_leg_arrow +
                                dy_leg_arrow_gap/2),
                 yend=ymin_grid - (dy_gap +
                                   dy_mod +
                                   dy_leg +
                                   dy_leg_line +
                                   dl_leg_line +
                                   dy_leg_arrow +
                                   dy_leg_arrow_gap/2 +
                                   dl_arrow),
                 color=IPCCgrey50,
                 alpha=alpha_marker,
                 linewidth=0.3,
                 arrow=arrow(length=unit(dx_arrow,
                                         "mm")),
                 lineend="round") +
        annotate("richtext",
                 x=dx_leg_line + 
                     dx_leg_arrow_text,
                 y=ymin_grid - (dy_gap +
                                dy_mod +
                                dy_leg +
                                dy_leg_line +
                                dl_leg_line +
                                dy_leg_arrow),
                 label=paste0("<b>Valeur hors limite</b>"),
                 fill=NA, label.color=NA,
                 color=IPCCgrey50,
                 hjust=0, vjust=0.6, size=2.4)


## 8. INTERPRETATION BLOC ____________________________________________
    if (is.null(codeLight)) {
        Ind = Ind +
            annotate("text",
                     x=x_title + dx_interp,
                     y=ymin_grid - (dy_gap +
                                    dy_mod +
                                    dy_leg),
                     label="COMPLÉMENT",
                     color=IPCCgrey25,
                     hjust=0, vjust=0, size=2.5)

        Label = "Les stations choisies pour illustrer les résultats à l'échelle régionale illustrent la variabilité des performances obtenues sur les hydrogrammes des débits journaliers médians (stations associées aux maximum, quantile 75 % et 25 %, et minimum du KGE\u221A)."
        
        Label = guess_newline(Label, nLim=nLim_interp)
        Label = unlist(strsplit(Label, "\n"))
            
        for (j in 1:length(Label)) {
            Ind = Ind +
                annotate("richtext",
                         x=x_title + dx_interp - dx_interp_text,
                         y=ymin_grid - (dy_gap +
                                        dy_mod +
                                        dy_leg +
                                        dy_interp_text +
                                        (j-1)*dy_interp_nline),
                         label=Label[j],
                         fill=NA, label.color=NA,
                         color=IPCCgrey50,
                         hjust=0, vjust=0.55, size=2.4)
        }
        
    } else {
        Ind = Ind +
            annotate("text",
                     x=x_title + dx_interp,
                     y=ymin_grid - (dy_gap +
                                    dy_mod +
                                    dy_leg),
                     label="AVERTISSEMENTS",
                     color=IPCCgrey25,
                     hjust=0, vjust=0, size=2.5)

        Warnings_code = Warnings[Warnings$Code == codeLight,]
        nWar_lim = 7
        nWar = nrow(Warnings_code)
        nLine = 0
        for (i in 1:nWar_lim) {
            if (i > nWar) {
                break
            }

            Label = guess_newline(Warnings_code$warning[i],
                                  nLim=nLim_interp)
            Label = unlist(strsplit(Label, "\n"))

            if (i + nLine + length(Label)-1 > nWar_lim | i > nWar) {
                break
            }
            
            Ind = Ind +
                annotate("line",
                         x=c(x_title + dx_interp +
                             dl_interp_text_line,
                             x_title + dx_interp +
                             dl_interp_text_line + 
                             w_interp_text_line),
                         y=rep(ymin_grid - (dy_gap +
                                            dy_mod +
                                            dy_leg +
                                            dy_interp_text +
                                            (i-1)*dy_interp_line +
                                            nLine*dy_interp_nline), 2),
                         color=IPCCgrey85,
                         linewidth=0.25)
            for (j in 1:length(Label)) {
                Ind = Ind +
                    annotate("richtext",
                             x=x_title + dx_interp +
                                 dl_interp_text_line + 
                                 w_interp_text_line +
                                 dr_interp_text_line,
                             y=ymin_grid - (dy_gap +
                                            dy_mod +
                                            dy_leg +
                                            dy_interp_text +
                                            (i-1)*dy_interp_line +
                                            (nLine+(j-1))*dy_interp_nline),
                             label=Label[j],
                             fill=NA, label.color=NA,
                             color=IPCCgrey50,
                             hjust=0, vjust=0.55, size=2.4)
            }
            nLine = nLine + length(Label)-1
        }
    }


## 9. END OF GRAPH __________________________________________________
    y_limits=
        c(ymin_grid - (dy_gap +
                       dy_leg +
                       dy_mod +
                       dh_leg),
        (dy+dy_icon_out))
    
    Ind = Ind +
        scale_x_continuous(limits=x_limits,
                           expand=c(0, 0)) +
        scale_y_continuous(limits=y_limits,
                           expand=c(0, 0))

    return (Ind)
}
