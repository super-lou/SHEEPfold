#!/usr/bin/env Rscript   
# use chmod ug+x to make this file executable


# Copyright 2022-2023 Louis Héraut (louis.heraut@inrae.fr)*1
#
# *1   INRAE, France
#
# This file is part of CARD R library.
#
# CARD R library is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# CARD R library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with CARD R library.
# If not, see <https://www.gnu.org/licenses/>.


suppressPackageStartupMessages(library(argparse))

parser = ArgumentParser()

parser$add_argument('-d', '--directory', nargs='+', type="character",
                    default="",
                    help="Directory in __all__ in which the variable will be search.")
parser$add_argument('-l', '--layout', nargs='+', type="character",
                    default="",
                    help="Layout of the chosen variables.")
parser$add_argument("-w", "--white", action="store_true", default=TRUE,
                    help="Underscores in directory names are replace by white spaces")
parser$add_argument("-b", "--blank", action="store_true", default=FALSE,
                    help="Remove id before variable names")
parser$add_argument("-v", "--verbose", action="store_true", default=FALSE,
                    help="Print information")

args = parser$parse_args()

# print(args$l)
# args$l = c("A", "[", "a", "[", "aa", "[", "aaa", "bbb", "]", "bb", "[", "ccc", "ddd", "]", "]", "]")

# Display the current parameters selected with argparse.
# parameters: 
#     args: argparse object
# Object generated by argparse with parser.parse_args() that
# contain all the selected parameters.
# returns: 
#     NULL
remind = function (args) {
    write("PARAMETERS:", stdout())
    n = length(args)
    args_name = names(args)
    for (i in 1:n) {
        write(paste0("    --", args_name[i], " ",
                     paste0(args[[i]], collapse=" ")), stdout())
    }
}

if (args$verbose) {
    remind(args)
}
if (all(args$l == "")) {
    write("Error : --layout is void\n", stderr())
    stop ()
}

source_dir = file.path("__SHEEP__", args$d)

OUT = unlist(args$l)
nOUT = length(OUT)
test1 = "[[]|[(]|[]]|[)]"
test2 = "[[]|[(]"
for (i in 1:nOUT) {
    if (i < nOUT & !grepl(test1, OUT[i]) & !grepl(test2, OUT[(i+1)])) {
        OUT[i] = paste0(OUT[i], ".(NA)")
    }
    if (i == nOUT & !grepl(test1, OUT[(i)])) {
        OUT[i] = paste0(OUT[i], ".(NA)")
    }
}
OUT = unlist(sapply(OUT, strsplit, split="[.]"),
            use.names=FALSE)

OUT = paste0(OUT, collapse="','")
OUT = gsub("[]]", ")", OUT)
OUT = gsub("[[]|[(]", "=list(", OUT)
OUT = gsub("[,]['][=]", "=", OUT)
OUT = gsub("[(]['][,]", "(", OUT)
OUT = gsub("[,]['][)]", ")", OUT)
OUT = gsub("[)][']", ")", OUT)
OUT = paste0("'", OUT)
OUT = paste0("list(", OUT, ")")
OUT = eval(parse(text=OUT))
OUT = unlist(OUT)
OUT = names(OUT)
OUT = gsub("[.]", "/", OUT)
OUT = paste0(OUT, ".R")

n = length(OUT)
SUB = c()
save = c()
IN = c()
DIR = c()
for (i in 1:n) {
    path = unlist(strsplit(OUT[i], "/"))
    len = length(path)
    nsd = len - 2

    if (nsd < 0) {
        write("Error : No directory detect\n", stderr())
        stop ()
        
    } else if (nsd == 0) {
        id = i
    
    } else if (nsd > 0) {

        for (j in 1:nsd) {
            
            if (!(path[(j+1)] %in% save)) {
                if (length(SUB) >= nsd) {
                    SUB[nsd] = SUB[nsd] + 1
                } else {
                    SUB = c(SUB, 1)
                }
                id = 1
                save = c(save, path[(j+1)])
            }

            obj = path[(j+1)]
            if (args$w) {
               obj = gsub("[_]", " ", obj)
            }

            if (!args$blank) {
                path[(j+1)] = paste0(SUB[j], "_", obj)
            }
        }
    }

    IN = c(IN, path[len])
    DIR = c(DIR, do.call(file.path, as.list(path[-len])))
    
    if (!args$blank) {
        idC = formatC(id, width=3, flag="0")
        path[len] = paste0(idC, "_", path[len])
    }
    
    id = id + 1
    OUT[i] = do.call(file.path, as.list(path))
}

DIR = DIR[!duplicated(DIR)]

if (any(file.exists(DIR))) {
    unlink(DIR, recursive=TRUE, force=TRUE)
}

for (i in 1:n) {
    dir.create(DIR[i], recursive=TRUE)
}

for (i in 1:n) {
    files = list.files(source_dir, recursive=TRUE)
    names(files) = basename(files)
    file.copy(file.path(source_dir, files[IN[i]]), OUT[i])
}

if (args$verbose) {
    write("done", stdout())
}
# warnings()
