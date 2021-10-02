# Clear the environment
rm(list=ls())

# Library the relative package
library(Rcpp)
library(readxl)
library(config)
library(htmlwidgets)

source('./Core/plot_sheets_type_1.R')
source('./Core/plot_sheets_type_2.R')
source('./Core/read_data_file.R')

# Setting the language to English
Sys.setlocale("LC_TIME","english")

# Setting the character type to Swedish
Sys.setlocale("LC_CTYPE","swedish")

# Load the relative config of the data sheet in the excel file.
config <- config::get(file = "config.yml")

time <- seq.Date(from = as.Date(unlist(config['start_time']),format = "%Y/%m/%d"),
                 to = as.Date(unlist(config['end_time']),format = "%Y/%m/%d"),
                 by = "1 day")

file_add <- paste(paste(unlist(config['file_add_and_name']),
                        format(time, format="%b %d %Y"),
                        sep=''), '.xlsx',sep='')






fig_f <- list()
read_data_file(file_add[1],file_stats = 'first')


# Read the rest data
for (t in file_add[-1]){
  print(paste('Processing',t))
  if (file.exists(t)== TRUE){
    read_data_file(file_add = t,file_stats='others')
  }else{
    print(paste(t,' does not exist, continue to next file'))
  }
}


# Save the plot as the html
for (i in fig_f){
  p <- eval(as.symbol(i))
  saveWidget(as_widget(p), paste(i,'.html',sep=''))
}




  
