library(tidyverse)
library(plotly)


plot_sheet_type_2 <- function(dataset_name){
  g <- eval(as.symbol(dataset_name))
  time_line <- suppressWarnings(as.Date(as.POSIXct(unlist(g[1]), 
                                                   origin="1970-01-01")))
  
  g[,3:length(colnames(g))] <- sapply(g[,3:length(colnames(g))], 
                                      as.numeric)
  
  p <- plot_ly()
  for (i in 3:length(colnames(g))){
    p <- p %>% add_trace(x = time_line, 
                         y = unlist(g[colnames(g)[i]]), 
                         legendgroup = colnames(g)[i],
                         name = paste(colnames(g)[i],
                                      unlist(g[colnames(g)[2]]),
                                      sep='-'),
                         type = 'scatter', 
                         mode = 'lines',
                         color= (unlist(g[colnames(g)[2]]))
    )
  }
  return(p)
}