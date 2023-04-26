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


## 1. BREAK PLOTTING _________________________________________________
### 1.1. Histogram ___________________________________________________
#' @title Histogram
#' @export
panel_break_histogram = function (df_break, meta, title='') {

    # Get all different stations code
    Code = rle(data$Code)$value
    nCode = length(Code)

    # Fix the major and minor date break between tick for axis
    datebreak = 10
    dateminbreak = 1

    Date = df_break$Date
    DateOk = df_break$Date[df_break$significant]

    ## All data
    # Computes histogram by year
    res_hist = hist(Date, breaks='years', plot=FALSE)
    # Gets the count by breaks
    counts = res_hist$counts
    # In pourcentage
    counts_pct = counts/nCode * 100
    # Gets the limits of the cells
    breaks = as.Date(res_hist$breaks)
    # Gets the middle of the cells
    mids = as.Date(res_hist$mids)

    ## Significant data
    # Computes histogram by year
    res_histOk = hist(DateOk, breaks='years', plot=FALSE)
    # Gets the count by breaks
    countsOk = res_histOk$counts
    # In pourcentage
    counts_pctOk = countsOk/nCode * 100
    # Gets the limits of the cells
    breaksOk = as.Date(res_histOk$breaks)
    # Gets the middle of the cells
    midsOk = as.Date(res_histOk$mids)

    # Open a new plot with personal theme
    plot = ggplot() + theme_IPCC() +
        
              # Y grid
        theme(panel.grid.major.y=element_line(color='grey80', size=0.15),
              # Remove y title
              axis.title.y=element_blank(),
              # Title
              plot.title = element_text(face="bold", colour="#00A3A8",
                                        size=10, hjust=0, vjust=-0.1)) +
        
        # Title
        ggtitle(title) +
        
        # Plot bar for all data
        geom_bar(aes(x=mids, y=counts_pct), 
                 stat='identity',
                 fill="#00A3A8", alpha=0.5) +
        
        # Plot bar for significant data
        geom_bar(aes(x=midsOk, y=counts_pctOk), 
                 stat='identity',
                 fill="#00A3A8") +
       
        # X axis 
        scale_x_date(date_breaks=paste(as.character(datebreak), 
                                       'year', sep=' '),
                     date_minor_breaks=paste(as.character(dateminbreak), 
                                             'year', sep=' '),
                     guide='axis_minor',
                     date_labels="%Y",
                     limits=c(min(Date) - lubridate::years(0), 
                              max(Date) + lubridate::years(0)),
                     expand=c(0, 0)) +
        
        # Y axis
        scale_y_continuous(limits=c(0,
                                    max(counts_pct)*1.1),
                           expand=c(0, 0))

    return (plot)
}

### 1.2. Cumulative __________________________________________________
#' @title Cumulative
#' @export
panel_break_cumulative = function (df_break, meta, title='', dyear=10) {

    # Get all different stations code
    Code = rle(data$Code)$value
    nCode = length(Code)

    # Fix the major and minor date break between tick for axis
    datebreak = 10
    dateminbreak = 1

    Date = df_break$Date
    DateOk = df_break$Date[df_break$significant]

    ## All data
    # Computes histogram by year
    res_hist = hist(Date, breaks='years', plot=FALSE)
    # Gets the count by breaks
    counts = res_hist$counts
    # Compute the cumulative sum
    cumul = cumsum(counts)
    # In percentage
    cumul_pct = cumul/nCode * 100
    # Gets the limits of the cells
    breaks = as.Date(res_hist$breaks)
    # Gets the middle of the cells
    midsRaw = as.Date(res_hist$mids)

    # Duplicates start and end value to extend graph
    mids = c(midsRaw[1] - lubridate::years(dyear), midsRaw[1] - lubridate::years(1),
             midsRaw,
             midsRaw[length(midsRaw)] + lubridate::years(dyear)) 
    cumul_pct = c(0, 0, 
                  cumul_pct,
                  cumul_pct[length(cumul_pct)])

    # Centers the middle date
    mids = mids + months(6)
    # Shifts the breaking date to be coherent with the start
    # of the rupture 
    breaks = breaks + 1
    # Remove the last date because it is too much
    breaks = breaks[-length(breaks)]

    ## Significant data
    # Computes histogram by year
    res_histOk = hist(DateOk, breaks='years', plot=FALSE)
    # Gets the count by breaks
    countsOk = res_histOk$counts
    # Compute the cumulative sum
    cumulOk = cumsum(countsOk)
    # In percentage
    cumul_pctOk = cumulOk/nCode * 100
    # Gets the limits of the cells
    breaksOk = as.Date(res_histOk$breaks)
    # Gets the middle of the cells
    midsOk = as.Date(res_histOk$mids)
    
    # Duplicates start and end value to extend graph
    midsOk = c(midsRaw[1] - lubridate::years(dyear), midsOk[1] - lubridate::years(1),
               midsOk,
               midsRaw[length(midsRaw)] + lubridate::years(dyear)) 
    cumul_pctOk = c(0, 0, 
                    cumul_pctOk,
                    cumul_pctOk[length(cumul_pctOk)])

    # Centers the middle date
    midsOk = midsOk + months(6)
    # Shifts the breaking date to be coherent with the start
    # of the rupture 
    breaksOk = breaksOk + 1
    # Remove the last date because it is too much
    breaksOk = breaksOk[-length(breaksOk)]

    # Creates a blank datebreak list to plot cumulative graph
    DBOk = c()
    # For all the date break cells
    for (i in 1:length(breaksOk)) {
        # Duplicates the date break for the number of times
        # it is counts in the histogram
        DBOk = c(DBOk, rep(breaksOk[i], times=countsOk[i]))
    }
    # Estimates the median
    q50 = as.Date(quantile(DBOk, probs=0.5)) + lubridate::years(1)
    # Print the median
    print(paste('mediane :', q50))
    

    # Open a new plot with personal theme
    plot = ggplot() + theme_IPCC() +
        
              # Y grid
        theme(panel.grid.major.y=element_line(color='grey80', size=0.15),
              # Remove y title
              axis.title.y=element_blank(),
              # Title
              plot.title = element_text(face="bold", colour="#00A3A8",
                                        size=10, hjust=0, vjust=-0.1)) +
        
        # Title
        ggtitle(title) +
        
        # Plot line of cumulative sum for all data
        geom_line(aes(x=mids, y=cumul_pct), 
                  color="#00A3A8", alpha=0.5) +
        
        # Plot line of cumulative sum for significant data
        geom_line(aes(x=midsOk, y=cumul_pctOk), 
                  color="#00A3A8") +
        
        # Plot the median line
        geom_line(aes(x=c(q50, q50), y=c(0, max(cumul_pctOk))), 
                  color="wheat", 
                  lty='dashed') +
        
        # X axis
        scale_x_date(date_breaks=paste(as.character(datebreak), 
                                       'year', sep=' '),
                     date_minor_breaks=paste(as.character(dateminbreak), 
                                             'year', sep=' '),
                     guide='axis_minor',
                     date_labels="%Y",
                     limits=c(min(mids) - lubridate::years(0), 
                              max(mids) + lubridate::years(0)),
                     expand=c(0, 0)) +
        
        # Y axis
        scale_y_continuous(limits=c(-1, 101),
                           expand=c(0, 0))

    if (title != '') {
        title = paste(title, '_', sep='')
    }
    
    return (plot)
}
