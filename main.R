# Clear the environment
rm(list=ls())

library(Rcpp)
library(readxl)

# Setting the language to English
Sys.setlocale("LC_TIME", "English")

time <- seq.Date(from = as.Date("2020/04/03",format = "%Y/%m/%d"),
                to = as.Date("2021/09/17",format = "%Y/%m/%d"),
                by = "1 day")

file_name <- paste('Folkhalsomyndigheten_Covid19_',
                   format(time, format="%b %d %Y"),
                   sep='')

sheet_name <- excel_sheets('./data/Folkhalsomyndigheten_Covid19_Apr 01 2021.xlsx')

# Name of information page
day = as.integer(unlist(strsplit(format(as.Date('Apr 01 2021', "%b %d %Y"), format="%d_%b_%Y"),split = '_',fixed = TRUE))[1])

if ((day/10) >= 1.0){
  info_page_name <- paste("FOHM ",paste(as.character(day), format(as.Date('Apr 01 2021', "%b %d %Y"), format="%b %Y"),sep=' '),sep='')
} else {
  info_page_name <- paste("FOHM  ",paste(as.character(day), format(as.Date('Apr 01 2021', "%b %d %Y"), format="%b %Y"),sep=' '),sep='')
}

rm(day)
rm(file_name)


# Remove the information page from the sheet_name_list
for (i in 1:length(sheet_name)){
  if (sheet_name[i] == info_page_name){
    sheet_name = sheet_name[-i]
    break
  }
}

rm(info_page_name)

# Assign the data into the name of the sheet
for (i in 1:length(sheet_name)){
  temp = sheet_name[i]
  # Make sure the name of the sheet that would be shown correct as global environment
  Encoding(temp) <- "UTF-8"
  # Change ' ' into '_' for better manage of name of data frame
  temp = gsub(' ','_',temp)
  # Assign the data in sheet as the name of the sheet
  assign(temp, read_xlsx('./data/Folkhalsomyndigheten_Covid19_Apr 01 2021.xlsx',
                                             sheet = i,col_types = "list"))
}

rm(temp)

# Rename the name of sheet so that could use the data frame by get()
sheet_name = gsub(' ','_',sheet_name)



