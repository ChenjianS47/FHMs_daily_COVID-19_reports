library(readxl)

source('./Core/data_select_type_1.R')
source('./Core/data_select_type_2.R')
source('./Core/data_select_type_3.R')
source('./Core/data_select_type_4.R')

source('./Core/plot_sheets_type_1.R')
source('./Core/plot_sheets_type_2.R')
source('./Core/plot_sheets_type_3.R')
source('./Core/plot_sheets_type_4.R')

source("./Core/data_differ_type_1.R")

read_data_file <- function(file_add, file_stats){
  
  # Initialize the list for storing relative variables
  sheet_name_original <- list()
  sheet_name <- list()
  sheet_stats <- list()
  time_col_pos  <- list()
  data_col_start  <- list()
  data_col_end  <- list()
  
  # Get the original name of the sheets from the excel file
  sheet_name_original <- excel_sheets(file_add)
  
  # Assign the sheet name with the stats defined in the config.yml
  for (i in sheet_name_original){
    if (as.character(config[i]) == "NULL"){
    }else{
      sheet_name <- append(sheet_name, i)
      sheet_stats <- append(sheet_stats,unlist(config[i])[1])
      time_col_pos <- append(time_col_pos,unlist(config[i])[2])
      data_col_start <- append(data_col_start,unlist(config[i])[3])
      data_col_end <- append(data_col_end,unlist(config[i])[4])
    }
  }
  
  # Assign the data into the name of the sheet
  for (i in 1:length(sheet_name)){
    temp = sheet_name[i]
    # Change ' ' into '_' for better manage of name of data frame
    temp = gsub(' ','_',temp)
    # Assign the data in sheet as the name of the sheet
    assign(temp, 
           as.data.frame(read_xlsx(file_add,sheet = i,col_types = "list")),
           envir = .GlobalEnv)
  }
  
  # Rename the name of sheet so that could use the data frame by 
  # eval(as.symbol(sheet_name[1]))
  sheet_name_t = gsub(' ','_',sheet_name)
  
  # Rename the name of sheet_stats that contains the types defined in config.yml
  names(sheet_stats) <- unlist(sheet_name_t)
  # Rename the name of time_col_pos that contains the types defined in config.yml
  names(time_col_pos) <- unlist(sheet_name_t)
  # Rename the name of data_col_start that contains the types defined in config.yml
  names(data_col_start) <- unlist(sheet_name_t)
  # Rename the name of data_col_end that contains the types defined in config.yml
  names(data_col_end) <- unlist(sheet_name_t)
 
  # For first file
  if (file_stats=='first'){
    for (i in sheet_name_t){
      if(sheet_stats[i]==1){
        
        # If the stats of the sheet is defined as 1, it is already the data with 
        # the date(day) sequence
        data_select_type_1(i,time_col_pos,data_col_start,data_col_end)
        # Name the plots with the format of 'fig_xxx', where xxx is the name of
        # the sheet but with ' ' changed to '_'
        
        date_str <- str_replace_all( unlist(strsplit(unlist(strsplit(file_add[1],split = '_'))[3],split='.xlsx')),' ','_')
        
        assign(paste(i,date_str,sep='_'),eval(as.symbol(i)),envir = .GlobalEnv)
        
        assign(paste(i,'series',sep='_'),list(),envir = .GlobalEnv)
        
        data_series_temp <- eval(as.symbol(paste(i,'series',sep='_')))
        data_series_name <- paste(i,date_str,sep='_')
        data_series_temp <- append(data_series_temp,data_series_name)
        assign(paste(i,'series',sep='_'),data_series_temp,envir = .GlobalEnv)
        
        data_differ_type_1(i,data_col_start,data_col_end,file_stats,file_add)
        
        fig_name_temp <- paste('fig',i,sep='_')
        
        
        # Assign the variable fig_xxx to the string 'fig_xxx'
        assign(fig_name_temp, plot_sheet_type_1(i),envir = .GlobalEnv)
        
        # time_step_type_1(i,time_col_pos,data_col_start,data_col_end,file_stats)
        # Add the string to the list of the plots fig_f
        fig_f <<- append(fig_f,fig_name_temp)
        
      }else if(sheet_stats[i]==2){
        
        # If the stats of the sheet is defined as 2, it is the data without the 
        # date sequence, which means need to do the process for getting the 
        # data with date sequence.
        
        # When sheet stat is 2, the time_col_pos here means the column that 
        # contains name of the rows,which usually the type of the data.
        data_select_type_2(i,time_col_pos,data_col_start,data_col_end)
        # Name the sheet in the first file as the old sheet, and assign it as 
        # old_xxx, where xxx is the name of the sheet but with ' ' 
        # changed to '_'.
        assign(paste('final',i,sep='_'),eval(as.symbol(i)),envir = .GlobalEnv)
        # Calc the length of the data
        row_len <- length(rownames(eval(as.symbol(paste('final',i,sep='_')))))
        # Create a temp matrix for saving the time data
        temp_sets <- matrix(nrow = row_len,ncol=1)
        
        # Create the time col
        date_str_t <- unlist(strsplit(file_add,config['file_add_and_name']))[2]
        date_str <- unlist(strsplit(date_str_t,'.xlsx'))[1]
        temp_sets[,1] <-rep(format(as.Date(date_str, format='%b %d %Y'),'%Y/%m/%d'),row_len)
        colnames(temp_sets) <- 'Date'
        # Combine the time col and the data
        i_temp <- cbind(temp_sets,eval(as.symbol(paste('final',i,sep='_'))))
        # Update the final_xxx data
        assign(paste('final',i,sep='_'),i_temp,envir = .GlobalEnv)
        # Assign the variable fig_xxx to the string 'fig_xxx'
        
        
        assign(paste('fig',i,sep='_'),plot_sheet_type_2(paste('final',i,sep='_')),envir = .GlobalEnv)
        # Add the string to the list of the plots fig_f
        fig_f <<- append(fig_f,paste('fig',i,sep='_'))
        
      }else if(sheet_stats[i]==3){
        # When sheet stats is 3, it means it is the data with the date(weekly) 
        # sequence
        
        # When sheet stat is 3, the time_col_pos here means the column that 
        # contains name of the rows,which usually the type of the data.
        data_select_type_3(i,time_col_pos,data_col_start,data_col_end)
        # Name the plots with the format of 'fig_xxx', where xxx is the name of
        # the sheet but with ' ' changed to '_'
        fig_name_temp <- paste('fig',i,sep='_')
        # Assign the variable fig_xxx to the string 'fig_xxx'
        assign(fig_name_temp, plot_sheet_type_3(i,time_col_pos),envir = .GlobalEnv)
        # Add the string to the list of the plots fig_f
        fig_f <<- append(fig_f,fig_name_temp)
        
      }
      
    }
    # The loop below is used for processioning the combined sheet that defined 
    # in the config.yml
    for (i in 1:length(config)){
      if ((unlist(config[i])[1]) == 4){
        # Get the name of the combined sheet and assign it as name_temp
        name_temp<-gsub(' ','_',names(config[i]))
        # Get the data
        data_select_type_4(i,file_add,file_stats,sheet_name_t,time_col_pos,config)
        # Plot the plots
        assign(paste('fig',name_temp,sep='_'),
               plot_sheet_type_4(paste('final',name_temp,sep='_')),
               envir = .GlobalEnv)
        # Add the string to the list of the plots fig_f 
        fig_f <<- append(fig_f,paste('fig',name_temp,sep='_'))
      }
    }
  }else if (file_stats=='others'){
    for (i in sheet_name_t){
      if(sheet_stats[i]==1){
        # If the stats of the sheet is defined as 1, it is already the data with 
        # the date(day) sequence
        data_select_type_1(i,time_col_pos,data_col_start,data_col_end)
        # Name the plots with the format of 'fig_xxx', where xxx is the name of
        # the sheet but with ' ' changed to '_'
        # time_step_type_1(i,time_col_pos,data_col_start,data_col_end,file_stats)
        
        
        date_str <- str_replace_all( unlist(strsplit(unlist(strsplit(file_add[1],split = '_'))[3],split='.xlsx')),' ','_')
        assign(paste(i,date_str,sep='_'),eval(as.symbol(i)),envir = .GlobalEnv)
        
        data_series_temp <- eval(as.symbol(paste(i,'series',sep='_')))
        data_series_name <- paste(i,date_str,sep='_')
        data_series_temp <- append(data_series_temp,data_series_name)
        assign(paste(i,'series',sep='_'),data_series_temp,envir = .GlobalEnv)
        
        if (length(eval(as.symbol(paste(i,'series',sep='_')))) == data_date_seq) {
            data_differ_type_1(i,data_col_start,data_col_end,data_series_set,file_add)
        }
        
        fig_name_temp <- paste('fig',i,sep='_')
        # Assign the variable fig_xxx to the string 'fig_xxx'
        assign(fig_name_temp, plot_sheet_type_1(paste('series',i,sep='_')),envir = .GlobalEnv)
        
      }else if(sheet_stats[i]==2){
        
        # If the stats of the sheet is defined as 2, it is the data without the 
        # date sequence, which means need to do the process for getting the 
        # data with date sequence.
        
        # When sheet stat is 2, the time_col_pos here means the column that 
        # contains name of the rows,which usually the type of the data.
        data_select_type_2(i,time_col_pos,data_col_start,data_col_end)
        # Name the sheet in the first file as the old sheet, and assign it as 
        # old_xxx, where xxx is the name of the sheet but with ' ' 
        # changed to '_'.
        assign(paste('temp',i,sep='_'),eval(as.symbol(i)))
        # Calc the length of the data
        row_len <- length(rownames(eval(as.symbol(paste('temp',i,sep='_')))))
        # Create a temp matrix for saving the time data
        temp_sets <- matrix(nrow = row_len,ncol=1)
        # Create the time col
        date_str_t <- unlist(strsplit(file_add,config['file_add_and_name']))[2]
        date_str <- unlist(strsplit(date_str_t,'.xlsx'))[1]
        temp_sets[,1] <-rep(format(as.Date(date_str, format='%b %d %Y'),'%Y/%m/%d'),row_len)
        colnames(temp_sets) <- 'Date'
        # Combine the time col and the data
        i_temp <- cbind(temp_sets,eval(as.symbol(paste('temp',i,sep='_'))))
        # Update the final_xxx data
        i_temp <- rbind(eval(as.symbol(paste('final',i,sep='_'))),i_temp)
        assign(paste('final',i,sep='_'),i_temp,envir = .GlobalEnv)
        # Assign the variable fig_xxx to the string 'fig_xxx'
        assign(paste('fig',i,sep='_'),plot_sheet_type_2(paste('final',i,sep='_')),envir = .GlobalEnv)
        
      }else if(sheet_stats[i]==3){
        # When sheet stats is 3, it means it is the data with the date(weekly) 
        # sequence
        
        # When sheet stat is 3, the time_col_pos here means the column that 
        # contains name of the rows,which usually the type of the data.

        data_select_type_3(i,time_col_pos,data_col_start,data_col_end)
        # Name the plots with the format of 'fig_xxx', where xxx is the name of
        # the sheet but with ' ' changed to '_'
        fig_name_temp <- paste('fig',i,sep='_')
        # Assign the variable fig_xxx to the string 'fig_xxx'
        assign(fig_name_temp, plot_sheet_type_3(i,time_col_pos),envir = .GlobalEnv)
        
        if ((fig_name_temp %in% fig_f)==FALSE) {
          fig_f <<- append(fig_f,fig_name_temp)
        }
      }

    }
    for (i in 1:length(config)){
      if ((unlist(config[i])[1]) == 4){
        # Get the name of the combined sheet and assign it as name_temp
        name_temp<-gsub(' ','_',names(config[i]))
        # Get the data
        data_select_type_4(i,file_add,file_stats,sheet_name_t,time_col_pos,config)
        # Plot the plots
        assign(paste('fig',name_temp,sep='_'),
               plot_sheet_type_4(paste('final',name_temp,sep='_')),
               envir = .GlobalEnv)
      }
    }
  }
}