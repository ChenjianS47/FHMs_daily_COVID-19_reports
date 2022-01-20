library(tidyverse)
library(plotly)


plot_sheet_type_3 <- function(dataset_name,time_col_pos){
  g <- eval(as.symbol(dataset_name))
  
  year <- unlist(g[1])
  
  week <- str_pad(unlist(g[2]),2,side='left','0')
  
  yearweek <- paste(year,week,sep='')
  
  time_line <- as.Date(paste(yearweek," Sunday",sep='') , '%Y%U %A')
  
  colset <- color_23
  
  
  if (unlist(time_col_pos[dataset_name])!= 0){
    g[, 4:(length(colnames(g)))] <- suppressWarnings(sapply(g[, 4:(length(colnames(g)))], 
                                           as.numeric))
    p <- plot_ly()
    for (i in 4:(length(colnames(g)))){
      p <- p %>% add_trace(x = time_line, 
                           y = unlist(g[colnames(g)[i]]), 
                           name = colnames(g)[i], 
                           type = 'scatter', 
                           mode = 'lines',
                           transforms = list(
                             list(type = 'groupby',
                                  groups = unlist(g[3]))
                             )
                           )
    }
  }else{
    g[, -1:-2] <- suppressWarnings(sapply(g[, -1:-2], as.numeric))
    p <- plot_ly()
    for (i in 3:(length(colnames(g)))){
      p <- p %>% add_trace(x = time_line, 
                           y = unlist(g[colnames(g)[i]]), 
                           name = colnames(g)[i], 
                           type = 'scatter', 
                           mode = 'lines',
                           colors= colset) 
    }
  }
  
  
  return(p)
}