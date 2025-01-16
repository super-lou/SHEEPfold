# Copyright 2022-2024 Louis Héraut (louis.heraut@inrae.fr)*1,
#                     Éric Sauquet (eric.sauquet@inrae.fr)*1,
#                     Jean-Philippe Vidal (jean-philippe.vidal@inrae.fr)*1
#
# *1   INRAE, UR RiverLy, Villeurbanne, France
#
# This file is part of SHEEPfold R toolbox.
#
# SHEEPfold R toolbox is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# SHEEPfold R toolbox is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with SHEEPfold R toolbox.
# If not, see <https://www.gnu.org/licenses/>.


sheet_precip_ratio = function (dataEX,
                               HMGroup=NULL,
                               Colors=refCOL,
                               refCOL=refCOL,
                               figdir="",
                               Pages=NULL,
                               verbose=FALSE) {

    paper_size = c(11, 11)
    page_margin = c(t=0.5, r=0.5, b=0.5, l=0.5)
    title_height = 0.7
    graph_height = 15 - title_height - 0.5 - 0.5
    
    plan = matrix(c("title", "graph"),
                  ncol=1)
    
    nGroup = length(HMGroup)

    if (length(Colors) == 1) {
        Colors = rep(Colors, nGroup)
    }

    HM = levels(factor(dataEX$PA_ratio$HM))
    
    for (i in 1:nGroup) {
        hms = HMGroup[[i]]

        hm1 = hms[1]
        hm2 = hms[2]
        name_hm1 = hms[1]
        name_hm2 = hms[2]
        
        if (hm1 == "SAFRAN") {
            hm1 = "SMASH"
        }
        if (hm2 == "SAFRAN") {
            hm2 = "SMASH"
        }

        color_hm1 = Colors[names(Colors) == name_hm1]
        color_hm2 = Colors[names(Colors) == name_hm2]

        if (length(color_hm1) == 0) {
            color_hm1 = refCOL  
        }
        if (length(color_hm2) == 0) {
            color_hm2 = refCOL    
        }
        

        herd = bring_grass(verbose=verbose)
        herd = plan_of_herd(herd, plan, verbose=verbose)
        
        title = ggplot() + theme_void_Lato() +
            theme(plot.margin=margin(t=0, r=0, b=0, l=0, "cm"))

        title = title +
            annotate("text",
                     x=0.14,
                     y=0.9,
                     label=TeX(paste0("Ratio de précipitations")),
                     size=3.5, hjust=0, vjust=1,
                     color=IPCCgrey20) + 
            annotate("text",
                     x=0.5,
                     y=0.88,
                     label=TeX(paste0("\\textbf{solides}")),
                     size=3.5, hjust=0, vjust=1,
                     color=EXPLORE2blue) +
            annotate("text",
                     x=0.633,
                     y=0.83,
                     label=TeX(paste0("et")),
                     size=3, hjust=0, vjust=1,
                     color=IPCCgrey20) +
            annotate("text",
                     x=0.67,
                     y=0.9,
                     label=TeX(paste0("\\textbf{liquides}")),
                     size=3.5, hjust=0, vjust=1,
                     color=IPCCblue)
        
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


        graph = ggplot() + theme_IPCC(is_panel.background=FALSE,
                                      isGridX=FALSE, isGridY=FALSE,
                                      isLabelX=TRUE, isLabelY=TRUE) +
            theme(plot.margin=margin(t=0, r=0, b=0, l=0, "cm")) +
            theme(axis.title.x = element_text(size=10,
                                              color=color_hm2),
                  axis.title.y = element_text(size=10,
                                              color=color_hm1))

        Pl_ratio_hm1 =
            dplyr::rename(
                       dplyr::select(
                                  dplyr::filter(dataEX$PA_ratio,
                                                HM == hm1),
                                  HM, Code, Pl_ratio_obs),
                       !!paste0("Pl_ratio_", name_hm1):=Pl_ratio_obs)
        Pl_ratio_hm2 =
            dplyr::rename(
                       dplyr::select(
                                  dplyr::filter(dataEX$PA_ratio,
                                                HM == hm2),
                                  HM, Code, Pl_ratio_obs),
                       !!paste0("Pl_ratio_", name_hm2):=Pl_ratio_obs)
        Pl_ratio_hms = dplyr::inner_join(Pl_ratio_hm1,
                                            Pl_ratio_hm2,
                                            by="Code")
        
        
        Ps_ratio_hm1 =
            dplyr::rename(
                       dplyr::select(dplyr::filter(dataEX$PA_ratio,
                                                   HM == hm1),
                                     HM, Code, Ps_ratio_obs),
                       !!paste0("Ps_ratio_", name_hm1):=Ps_ratio_obs)
        Ps_ratio_hm2 =
            dplyr::rename(
                       dplyr::select(dplyr::filter(dataEX$PA_ratio,
                                                   HM == hm2),
                                     HM, Code, Ps_ratio_obs),
                       !!paste0("Ps_ratio_", name_hm2):=Ps_ratio_obs)
        Ps_ratio_hms = dplyr::inner_join(Ps_ratio_hm1,
                                            Ps_ratio_hm2,
                                            by="Code")

        graph = graph +
            ggplot2::geom_point(data=Ps_ratio_hms,
                                aes(x=get(paste0("Ps_ratio_", name_hm2)),
                                    y=get(paste0("Ps_ratio_", name_hm1))),
                                shape=16, alpha=0.8,
                                color=EXPLORE2blue) +
            ggplot2::geom_point(data=Pl_ratio_hms,
                                aes(x=get(paste0("Pl_ratio_", name_hm2)),
                                    y=get(paste0("Pl_ratio_", name_hm1))),
                                shape=16, alpha=0.8,
                                color=IPCCblue) +
            
            ggplot2::geom_line(data=NULL,
                               aes(x=c(0, 1), y=c(0, 1)),
                               color="white",
                               linewidth=0.6) +  
            ggplot2::geom_line(data=NULL,
                               aes(x=c(0, 1), y=c(0, 1)),
                               color=IPCCgrey25,
                               linewidth=0.2) 

        graph = graph +
            ggplot2::xlab(TeX(paste0("\\textbf{", name_hm2, "}"))) + 
            ggplot2::ylab(TeX(paste0("\\textbf{", name_hm1, "}")))
        
        graph = graph +
            scale_x_continuous(expand=c(0, 0)) +
            scale_y_continuous(expand=c(0, 0))

        herd = add_sheep(herd,
                         sheep=graph,
                         id="graph",
                         height=graph_height,
                         verbose=verbose)

        res = return_to_sheepfold(herd,
                                  page_margin=page_margin,
                                  paper_size=paper_size,
                                  hjust=0, vjust=1,
                                  verbose=verbose)
        
        plot = res$plot
        paper_size = res$paper_size
        
        filename = paste0("precip_ratio_", name_hm1, "_", name_hm2, ".pdf")
            
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
