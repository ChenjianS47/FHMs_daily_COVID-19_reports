# Clear the environment
rm(list=ls())

# Library the relative package
library(config)
library(htmlwidgets)

# Library the function for process the data file
source('./Core/read_data_file.R')

# Setting the time language to English
Sys.setlocale("LC_TIME","english")

# Setting the character type to Swedish
Sys.setlocale("LC_CTYPE","swedish")

# Load the relative config of the data sheet in the excel file.
config <- config::get(file = "config.yml")

# Generate the date sequence
time <- seq.Date(from = as.Date(unlist(config['start_time']),format = "%Y/%m/%d"),
                 to = as.Date(unlist(config['end_time']),format = "%Y/%m/%d"),
                 by = "1 day")

# Generate the file path of the data
file_add <- paste(paste(unlist(config['file_add_and_name']),
                        format(time, format="%b %d %Y"),
                        sep=''), config['file_type'],sep='')

# Initialize the list for storing the names of plots
fig_f <- list()
col_name <- list()
data_date_seq <<- unlist(config['data_update_seq'])
data_series_set <<- 0
fig_output_type <- ((config['fig_output_type']))

print(paste('Processing',file_add[1]))

# Read the first file
read_data_file(file_add[1],file_stats = 'first')


# Read the other files
for (t in file_add[-1]){
  print(paste('Processing',t))
  if (file.exists(t)== TRUE){
    read_data_file(file_add = t,file_stats='others')
  }else{
    print(paste(t,' does not exist, continue to next file'))
  }
}


# Save the plot as the html

width = unlist(config['resolution'])[1]
height = unlist(config['resolution'])[2]

print('Start to plot the data and save as .html and .png file')
for (i in fig_f){
  p <- eval(as.symbol(i))
  saveWidget(as_widget(p), paste(i,'.html',sep=''))
  orca(p,file = paste(i,fig_output_type,sep=''),height = height,width = width)
}

print('Finished')



  
