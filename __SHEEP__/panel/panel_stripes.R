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


panel_stripes = function (Value, Date,
                          min_value=NULL,
                          max_value=NULL,
                          palette_name="ground_8",
                          palette_reverse=FALSE,
                          palette_center=TRUE,
                          palette_q_extrem=0,
                          is_x_axis=FALSE,
                          x_limits=NULL,
                          x_size=NULL,
                          x_color=NULL,
                          x_hjust=NULL,
                          x_vjust=NULL,
                          x_breaks=seq.Date(as.Date("1972-01-01"),
                                            as.Date("1972-12-01"),
                                            "months"),
                          x_label_format="%m",
                          x_expand=ggplot2::expansion(add=c(0, 1)),
                          x_date_breaks=NULL,
                          margin_stripes=ggplot2::margin(0, 0, 0, 0,
                                                         unit="mm")) {


    
    if (is.null(x_date_breaks)) {
        x_date_breaks = ggplot2::waiver()
    }

    if (is.null(min_value)) {
        min_value_tmp = quantile(Value, palette_q_extrem)
    } else {
        min_value_tmp = min_value
    }
    if (is.null(max_value)) {
        max_value_tmp = quantile(Value, 1-palette_q_extrem)
    } else {
        max_value_tmp = max_value
    }
    min_value = min(c(min_value_tmp, max_value_tmp))
    max_value = max(c(min_value_tmp, max_value_tmp))

    if (palette_q_extrem %in% c(0, 1)) {
        include = TRUE
    } else {
        include = FALSE
    }

    Palette = get_IPCC_Palette(palette_name,
                               reverse=palette_reverse)
    colorStep = length(Palette)

    res = compute_colorBin(min_value, max_value,
                           colorStep,
                           center=palette_center,
                           include=include, round=TRUE)

    Color = get_colors(Value, res$upBin, res$lowBin, Palette)

    stripes = ggplot2::ggplot() + ggplot2::theme_void_Lato() +
        ggplot2::theme(plot.margin=margin_stripes)

    if (is.null(x_color)) {
        x_color = IPCCgrey40
    }
    
    if (is_x_axis) {
        stripes =
            stripes +
            ggplot2::theme(axis.text.x=
                               ggplot2::element_text(color=x_color,
                                                     size=x_size,
                                                     hjust=x_hjust,
                                                     vjust=x_vjust))
    } else {
        x_breaks = NULL
        x_date_breaks = ggplot2::waiver()
        x_label_format = ggplot2::waiver()
        x_expand = c(0, 0)
    }

    stripes = stripes +
        ggplot2::annotate("tile",
                          x=Date,
                          y=0.5,
                          fill=Color,
                          color=NA)

    stripes = stripes +
        ggplot2::scale_x_date(limits=x_limits,
                              breaks=x_breaks,
                              date_breaks=x_date_breaks,
                              date_labels=x_label_format,
                              expand=x_expand) +
        scale_y_continuous(limits=c(0, 1),
                           expand=c(0, 0))
    
    return (stripes)
}
