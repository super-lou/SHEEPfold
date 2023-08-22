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


## 1. PERSONAL PLOT __________________________________________________
### 1.1. Void plot ___________________________________________________
# A plot completly blank
#' @title Void plot
#' @export
void = function () {
    plot = ggplot() + geom_blank(aes(1,1)) +
        theme(
            plot.background = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(), 
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank()
        )
    return (plot)
}

### 1.2. Contour void plot ___________________________________________
# A plot completly blank with a contour
#' @title Contour plot
#' @export
contour = function () {
    plot = ggplot() + geom_blank(aes(1,1)) +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(), 
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            plot.background=element_rect(fill=NA, color="#EC4899"),
            plot.margin=margin(t=0, r=0, b=0, l=0, unit="mm"))
    return (plot)
}

### 1.3. Circle ______________________________________________________
# Allow to draw circle in ggplot2 with a radius and a center position
#' @title Circle
#' @export
gg_circle = function(r, xc, yc, color="black", fill=NA, ...) {
    x = xc + r*cos(seq(0, pi, length.out=100))
    ymax = yc + r*sin(seq(0, pi, length.out=100))
    ymin = yc + r*sin(seq(0, -pi, length.out=100))
    annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color,
             fill=fill, ...)
}

nbsp = function (n, size=NA) {
    if (is.na(size)) {
        paste0(rep("<span> </span>", times=n), collapse="")
    } else {
        paste0(rep(paste0("<span style='font-size:", size,
                          "pt'> </span>"), times=n),
               collapse="")
    }
}


## 3. NUMBER MANAGEMENT ______________________________________________
### 3.1. Number formatting ___________________________________________
# Returns the power of ten of the scientific expression of a value
#' @title Number formatting
#' @export
get_power = function (value) {

    if (is.na(value) | !is.finite(value)) {
        return (0)
    }
    
    if (length(value) > 1) {
        power = unlist(as.list(sapply(value, get_power),
                               recursive=TRUE,
                               use.names=FALSE))
    } else {
        if (!is.na(value)) {
            # Do not care about the sign
            value = abs(value)
            
            # If the value is greater than one
            if (value >= 1) {
                # The magnitude is the number of character of integer part
                # of the value minus one
                power = nchar(as.character(as.integer(value))) - 1
                # If value is zero
            } else if (value == 0) {
                # The power is zero
                power = 0
                # If the value is less than one
            } else {
                # Extract the decimal part
                dec = gsub('0.', '', as.character(value), fixed=TRUE)
                # Number of decimal with zero
                ndec = nchar(dec)
                # Number of decimal without zero
                nnum = nchar(as.character(
                    as.numeric(dec)))
                # Compute the power of ten associated
                power = -(ndec - nnum + 1)
            }
        } else {
            power = NA
        }
    }
    return (power)
}

### 3.2. Pourcentage of variable _____________________________________
# Returns the value corresponding of a certain percentage of a
# data serie
#' @title Pourcentage of variable
#' @export
gpct = function (pct, L, min_lim=NULL, shift=FALSE) {

    # If no reference for the serie is given
    if (is.null(min_lim)) {
        # The minimum of the serie is computed
        minL = min(L, na.rm=TRUE)
        # If a reference is specified
    } else {
        # The reference is the minimum
        minL = min_lim
    }

    # Gets the max
    maxL = max(L, na.rm=TRUE)
    # And the span
    spanL = maxL - minL
    # Computes the value corresponding to the percentage
    xL = pct/100 * as.numeric(spanL)

    # If the value needs to be shift by its reference
    if (shift) {
        xL = xL + minL
    }
    return (xL)
}


#' @title round_label
#' @export
round_label = function (labelRaw, direction="V", ncharLim=4) {
    labelRaw = round(labelRaw, 10)
    if (direction == "V") {
        label2 = signif(labelRaw, 2)
        label2[label2 >= 0] = paste0(" ", label2[label2 >= 0])
        label1 = signif(labelRaw, 1)
        label1[label1 >= 0] = paste0(" ", label1[label1 >= 0])
        label = label2
        label[nchar(label2) > ncharLim] =
            label1[nchar(label2) > ncharLim]
    } else if (direction == "H") {
        label2 = signif(labelRaw, 2)
        label1 = signif(labelRaw, 1)
        nCharLabel2 = nchar(label2)
        nCharLabel2[nCharLabel2 >= 0] =
            nCharLabel2[nCharLabel2 >= 0] + 1
        label = label2
        label[nCharLabel2 > ncharLim] = label1[nCharLabel2 > ncharLim]
    }
    return (label)
}

is.wholenumber = function (X, tol=.Machine$double.eps^0.5) {
    res = abs(X - round(X)) < tol
    return (res)
}

chr2op = function (x) {
    res = eval(parse(text=x))
    return (res)
}

float2frac = function (X, den) {
    Frac = paste0(round(X * den), "/", den)
    evalFrac = sapply(X, chr2op)
    OK = is.wholenumber(evalFrac)
    Frac[OK] = evalFrac[OK]
    return (Frac)
}

## 4. LOADING ________________________________________________________
### 4.1. Shapefile loading ___________________________________________
#' @title Shapefiles loading
#' @description  Generates a list of shapefiles to draw a hydrological
#' map of the France
#' @param resources_path Path to the resources directory.
#' @param france_dir Directory you want to use in ash\\resources_path\\
#' to get the France shapefile.
#' @param france_file Name of the France shapefile.
#' @param basinHydro_dir Directory you want to use in ash\\resources_path\\
#' to get the hydrological basin shapefile.
#' @param basinHydro_file Name of the hydrological basin shapefile.
#' @param regionHydro_dir Directory you want to use in
#' ash\\resources_path\\ to get the hydrological sub-basin shapefile.
#' @param regionHydro_file Name of the hydrological sub-basin shapefile.
#' @param river_dir Directory you want to use in ash\\resources_path\\
#' to get the hydrological network shapefile.
#' @param river_file  Name of the hydrological network shapefile.
#' @param show_river Boolean to indicate if the shapefile of the
#' hydrological network will be charge because it is a heavy one and
#' that it slows down the entire process (default : TRUE)
#' @return A list of shapefiles converted as tibbles that can be plot
#' with 'geom_polygon' or 'geom_path'.
#' @export
load_shapefile = function (computer_shp_path, Code,
                           france_shp_path,
                           basinHydro_shp_path,
                           regionHydro_shp_path,
                           secteurHydro_shp_path,
                           entiteHydro_shp_path, entiteHydro_coord,
                           entitePiezo_shp_path,
                           river_shp_path,
                           return_river=TRUE,
                           river_class=NULL,
                           river_length=NULL,
                           river_selection=NULL,
                           toleranceRel=10000) {
    
    # Path for shapefile
    france_path = file.path(computer_shp_path,
                            france_shp_path)
    basinHydro_path = file.path(computer_shp_path,
                                basinHydro_shp_path)
    regionHydro_path = file.path(computer_shp_path,
                                 regionHydro_shp_path)
    secteurHydro_path = file.path(computer_shp_path,
                                  secteurHydro_shp_path)
    entiteHydro_path = file.path(computer_shp_path,
                                 entiteHydro_shp_path)
    entitePiezo_path = file.path(computer_shp_path,
                                 entitePiezo_shp_path)
    river_path = file.path(computer_shp_path,
                           river_shp_path)
    
    # France
    france = st_read(france_path)
    france = st_union(france)
    france = st_transform(france, 2154)
    france = st_simplify(france,
                         preserveTopology=TRUE,
                         dTolerance=toleranceRel)
    
    # Hydrological basin
    basinHydro = st_read(basinHydro_path)
    basinHydro = st_transform(basinHydro, 2154)
    basinHydro = st_simplify(basinHydro,
                             preserveTopology=TRUE,
                             dTolerance=toleranceRel*0.6)
    
    # Hydrological sub-basin
    regionHydro = st_read(regionHydro_path)
    regionHydro = st_transform(regionHydro, 2154)
    regionHydro = st_simplify(regionHydro,
                              preserveTopology=TRUE,
                              dTolerance=toleranceRel*0.6)

    # Hydrological sector
    secteurHydro = st_read(secteurHydro_path)
    secteurHydro = st_transform(secteurHydro, 2154)
    secteurHydro = st_simplify(secteurHydro,
                               preserveTopology=TRUE,
                               dTolerance=toleranceRel*0.6)
    
    # Hydrological code bassin
    entiteHydro_list = lapply(entiteHydro_path, read_sf)
    entiteHydro_list = lapply(entiteHydro_list, st_transform, 2154)
    entiteHydro = do.call(rbind, entiteHydro_list)
    entiteHydro = entiteHydro[entiteHydro$Code %in% Code,]
    entiteHydro = st_simplify(entiteHydro,
                              preserveTopology=TRUE,
                              dTolerance=toleranceRel*0.4)

    # Piezo entity
    entitePiezo = st_read(entitePiezo_path)
    entitePiezo = st_transform(entitePiezo, 2154)
    entitePiezo = st_simplify(entitePiezo,
                              preserveTopology=TRUE,
                              dTolerance=toleranceRel*0.6)
    
    
    # If the river shapefile needs to be load
    if (return_river) {
        # Hydrographic network
        river = st_read(river_path)
        river = st_transform(river, 2154)
        
        if (!is.null(river_class)) {
            river = river[river$Classe %in% river_class,]

        }
        if (!is.null(river_length)) {
            river$length = as.numeric(st_length(river$geometry))
            river = river[river$length >= river_length,]
        }
        if (!is.null(river_selection)) {
            river = river[grepl(paste(river_selection, collapse='|'),
                                river$NomEntiteH),]
        }
        
        river = st_simplify(river,
                            preserveTopology=TRUE,
                            dTolerance=toleranceRel*0.4) 
    } else {
        river = NULL
    }

    return (list(france=france,
                 basinHydro=basinHydro,
                 regionHydro=regionHydro,
                 secteurHydro=secteurHydro,
                 entiteHydro=entiteHydro,
                 entitePiezo=entitePiezo,
                 river=river))
}

### 4.2. Logo loading ________________________________________________
#' @title Logo loading
#' @export
load_logo = function (resources_path, logo_dir, logo_to_show) {
    logo_path = c()
    nLogo = length(logo_to_show)
    for (i in 1:nLogo) { 
        logo_path = c(logo_path, file.path(resources_path,
                                           logo_dir,
                                           logo_to_show[i]))
        names(logo_path)[length(logo_path)] = names(logo_to_show)[i]
    }
    return (logo_path)
}

### 4.3. Font loading ________________________________________________
load_font = function (path=NULL, force_import=FALSE) {

    extrafont::font_import(paths=path)
    
    # if (is.null(extrafont::fonts()) | force_import) {
    # remotes::install_version("Rttf2pt1", version = "1.3.8")
    # extrafont::font_import(paths=path)
    # }
    # extrafont::loadfonts(device="all", quiet=TRUE)
    # theme = theme(text=element_text(family="frutiger-57-condensed"))
}


## 5. OTHER __________________________________________________________
#' @title Split filename
#' @export
splitext = function(file) { # tools::file_ext
    ex = strsplit(basename(file), split="\\.")[[1]]
    res = list(name=ex[1], extension=ex[2])
    return (res)
}

#' @title Split path
#' @export
split_path = function (path) {
    if (dirname(path) %in% c(".", path)) return(basename(path))
    return(c(basename(path), split_path(dirname(path))))
}


# X2px(unlist(strsplit(text, "")), PX)

    # PX = get_alphabet_in_px(save=TRUE)
    
    # Span = lapply(strsplit(Model, "*"), X2px, PX=PX)
    # Span = lapply(Span, sum)
    # Span = unlist(Span)


plotly_save = function (fig, path) {
    htmlwidgets::saveWidget(fig,
                            file=path,
                            selfcontained=TRUE)
    libdir = paste0(tools::file_path_sans_ext(basename(path)), "_files")
    unlink(file.path(dirname(path), libdir), recursive=TRUE)
}


strsplit_unlist = function (...) {unlist(strsplit(...))}
other_letters = c("é", "è", "à")
numbers = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")
symbols = c("-", "_", ".", ",", "*", "'", "%", "(", ")", "[", "]", "{", "}", "!", "?", "+", "=", "@", "|", "#", "&")
get_alphabet_in_px = function (alphabet=c(letters, LETTERS,
                                          other_letters,
                                          numbers, symbols),
                               size=50, font="sans",
                               style="normal",
                               isNorm=TRUE,
                               out_dir="letter",
                               save=FALSE) {
    library(magick)
    if (save &!dir.exists(out_dir)) {
        dir.create(out_dir)
    }
    find_id = function (X, a, where="") {
        if (any(a %in% X)) {
            id = which(X == a)
            if (where == "first") {
                id = id[1]
            } else if (where == "last") {
                id = id[length(id)]
            }
            return (id)
        } else {
            return (NA)
        }
    }

    if (style == "bold") {
        weight = 700
    } else {
        weight = 400
    }
    
    PX = c()
    for (letter in alphabet) {
        img = image_blank(width=size, height=size, color="white")
        img = image_annotate(img, letter, size=size, style="normal",
                             weight=weight, font=font, color="#000000")
        pixels = as.character(c(image_data(img, channel="gray")))
        pixels[pixels != "ff"] = "1"
        pixels[pixels == "ff"] = "0"
        pixels = as.numeric(pixels)
        pixels = matrix(pixels, ncol=size, byrow=TRUE)
        if (save) {
            write.table(pixels,
                        file=file.path(out_dir,
                                       paste0(letter, ".txt")),
                        row.names=FALSE, col.names=FALSE)
        }
        first_one = apply(pixels, 1, find_id, a=1, where="first")
        last_one = apply(pixels, 1, find_id, a=1, where="last")
        px = max(last_one, na.rm=TRUE) -
            min(first_one, na.rm=TRUE) + 1
        PX = c(PX, px)
        names(PX)[length(PX)] = letter
    }
    PX = c(PX, PX["_"])
    names(PX)[length(PX)] = ' '
    if (isNorm) {
        PX = PX/max(PX)
    }
    return (PX)
}


text2px = function (text, PX) {
    text = unlist(strsplit(text, ""))
    px = PX[text]
    px[is.na(px)] = mean(px, na.rm=TRUE)
    px = sum(px)
    return (px)
}

char2px = function (char, PX) {
    px = PX[char]
    px[is.na(px)] = mean(px, na.rm=TRUE)
    return (px)
}


select_good = function (X) {
    Xrle = rle(X)
    value = Xrle$values[Xrle$lengths == max(Xrle$lengths)]
    if (length(value) > 1) {
        value = mean(value, na.rm=TRUE)
    }
    return (value)
}

guess_newline = function (text, px=40, nChar=100,
                          PX=NULL, newlineId="\n") {

    if (is.null(px)) {
        lim = nChar
        estimator = nchar
    } else {
        lim = px
        if (is.null(PX)) {
            PX = get_alphabet_in_px()
        }
        estimator = function (text) {
            text2px(text, PX=PX)
        }
    }
    Newline = text
    distance = estimator(Newline)
    begin = 0
    
    while (distance > lim & sum(grepl(" ", text)) > 0) {        
        posSpace = which(strsplit(Newline, "")[[1]] == " ")
        posSpace_distance = sapply(lapply(
            posSpace, substr, x=Newline, start=1),
            estimator)
        idNewline = which.min(abs(posSpace_distance - lim))
        posNewline = posSpace[idNewline] + begin

        # print(posNewline)
        # print(Newline)
        # print(distance)
        # print(begin)
        # print("")
        
        text = paste(substring(text,
                               c(1, posNewline + 1),
                               c(posNewline - 1,
                                 nchar(text))),
                     collapse=newlineId)
        if (sum(grepl(" ", text)) == 0) {
            break
        }
        Newline = substr(text,
                         posNewline + 1,
                         nchar(text))
        distance = estimator(Newline)
        begin = nchar(text) - nchar(Newline) + begin
    }
    
    return (text)
}



convert2TeX = function (Var, size=NULL, is_it_small=FALSE, replace_space=FALSE, bold=TRUE) {
    nVar = length(Var)

    if (is_it_small) {
        ita = "\\\\small{"
        itb = "}"
    } else {
        ita = ""
        itb = ""
    }

    VarTEX = gsub("etiage", "étiage", Var)
    
    for (i in 1:nVar) {
        var = VarTEX[i]
        
        if (grepl("[_]", var) & !grepl("[_][{]", var)) {
            var = gsub("[_]", ", ", var)
            var = sub("[,] ", "$_{$", var)
            var = paste0(var, "}")           
        } else if (grepl("[_]", var) & grepl("[_][{]", var)) {
            var = gsub("[_]", ", ", var)
            var = sub("[,] [{]", "$_{$", var)
        }

        if (grepl("\\^[{]", var)) {
            var = gsub("\\^[{]", "$^{", var)
            var = gsub("[}]", "}$", var)
        }
        # if (grepl("\\^[{][$][-]", var)) {
            # var = gsub("\\^[{][$][-]", "^{-$", var)
        # }
        if (grepl("\\^[[:alnum:]]", var)) {
            var = gsub("\\^", "$^$", var)
        }

        if (grepl("alpha", var)) {
            var = gsub("alpha", "\\\\bf{\u03b1}", var)
        }

        if (grepl("epsilon", var)) {
            var = gsub("epsilon", "\\\\bf{\u03b5}", var)
        }

        if (grepl("HYP", var)) {
            var = gsub("HYP", "H", var)
        }

        if (grepl("inv", var) & !grepl("inv[{]", var)) {
            var = gsub("inv",
                       paste0(ita, "\\\\textit{inv}", itb),
                       var)
        } else if (grepl("inv", var) & grepl("inv[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("inv[{]", 
                       paste0(ita, "\\\\textit{inv}", itb),
                       var)
        } 

        if (grepl("log", var) & !grepl("log[{]", var)) {
            var = gsub("log", 
                       paste0(ita, "\\\\textit{log}", itb),
                       var)
        } else if (grepl("log", var) & grepl("log[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("log[{]", 
                       paste0(ita, "\\\\textit{log}", itb),
                       var)
        } 

        if (grepl("moy", var) & !grepl("moy[{]", var)) {
            var = gsub("moy", 
                       paste0(ita, "\\\\textit{moy}", itb),
                       var)
        } else if (grepl("moy", var) & grepl("moy[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("moy[{]", 
                       paste0(ita, "\\\\textit{moy}", itb),
                       var)
        } 

        if (grepl("med", var) & !grepl("med[{]", var)) {
            var = gsub("med", 
                       paste0(ita, "\\\\textit{med}", itb),
                       var)
        } else if (grepl("med", var) & grepl("med[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("med[{]", 
                       paste0(ita, "\\\\textit{med}", itb),
                       var)
        } 
        
        if (grepl("racine", var) & !grepl("racine[{]", var)) {
            var = gsub("racine", "\u221A", var)
        } else if (grepl("racine", var) & grepl("racine[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("racine[{]", "\u221A", var)
        }

        if (grepl("ips", var) & !grepl("ips[{]", var)) {
            var = gsub("ips", 
                       paste0(ita, "\\\\textit{ips}", itb),
                       var)
        } else if (grepl("ips", var) & grepl("ips[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("ips[{]", 
                       paste0(ita, "\\\\textit{ips}", itb),
                       var)
        }

        if (grepl("biais", var) & !grepl("biais[{]", var)) {
            var = gsub("biais", 
                       paste0(ita, "\\\\textit{biais}", itb),
                       var)
        } else if (grepl("biais", var) & grepl("biais[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("biais[{]", 
                       paste0(ita, "\\\\textit{biais}", itb),
                       var)
        }

        if (replace_space) {
            var = gsub(" ", "\\\\,", var)
        }
        
        VarTEX[i] = var
    }

    if (!is.null(size)) {
        VarTEX = paste0("\\", size, "{", VarTEX, "}")
    }
    
    if (bold) {
        VarTEX = paste0("\\textbf{", VarTEX, "}")
    }
    return (VarTEX)
}
