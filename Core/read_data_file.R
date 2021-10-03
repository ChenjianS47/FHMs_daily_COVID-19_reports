library(readxl)
source('./Core/plot_sheets_type_1.R')
source('./Core/plot_sheets_type_2.R')

read_data_file <- function(file_add, file_stats){
  
  # Initialize the list for storing relative variables
  sheet_name_original <- list()
  sheet_name <- list()
  sheet_stats <- list()
  
  # Get the original name of the sheets from the excel file
  sheet_name_original <- excel_sheets(file_add)
  
  # Assign the sheet name with the stats defined in the config.yml
  for (i in sheet_name_original){
    if (as.character(config[i]) == "NULL"){
    }else{
      sheet_name <- append(sheet_name, i)
      sheet_stats <- append(sheet_stats,config[i])
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
 
  # For first file
  if (file_stats=='first'){
    for (i in sheet_name_t){
      # If the stats of the sheet is defined as 1, it is already the data with 
      # the date(day) sequence, in this case, just plot the data without any 
      # process
      if(sheet_stats[i]==1){
        # Name the plots with the format of 'fig_xxx', where xxx is the name of
        # the sheet but with ' ' changed to '_'
        name_temp <- paste('fig',i,sep='_')
        # Assign the variable fig_xxx to the string 'fig_xxx'
        assign(name_temp, plot_sheet_type_1(i) ,envir = .GlobalEnv)
        # Add the string to the list of the plots fig_f
        fig_f <<- append(fig_f,name_temp)
        
      }else if(sheet_stats[i]==2){
        # If the stats of the sheet is defined as 2, it is the data without the 
        # date sequence, which means need to do the process for getting the 
        # data with date sequence.
        # Name the sheet in the first file as the old sheet, and assign it as 
        # old_xxx, where xxx is the name of the sheet but with ' ' 
        # changed to '_'.
        assign(paste('old',i,sep='_'),eval(as.symbol(i)),envir = .GlobalEnv)
        # Assign the labels of data to type_xxx, where xxx is the name of the 
        # sheet but with ' ' changed to '_'.
        assign(paste('type',i,sep='_'),
               unlist(eval(as.symbol(paste('old',i,sep='_')))[1]),
               envir = .GlobalEnv)
        # Name the data sheet with the name of final_xxx and assign it to the 
        # string, where xxx is the name of the sheet but with ' ' changed to '_'
        # Besides, initialize it as a matrix with 0 rows and n columns, which n
        # is the length(columns) dimension of the original data
        assign(paste('final',i,sep='_'),
               matrix(nrow = 0,ncol = (dim(eval(as.symbol(i)))[2]+1)),
               envir = .GlobalEnv)
        # Assign a temp variable for final_xxx
        ma_temp <- eval(as.symbol(paste('final',i,sep='_')))
        # Rename the temp variable
        colnames(ma_temp) <- c('Date',colnames(eval(as.symbol(i))))
        # Assign the temp variable to final_xxx
        assign(paste('final',i,sep='_'),ma_temp, envir = .GlobalEnv)
        # Add the string to the list of the plots fig_f
        fig_f <<- append(fig_f,paste('fig',i,sep='_'))
      }else if(sheet_stats[i]==3){
        # When sheet stats is 3, it means it is the data with the date(weekly) 
        # sequence, wait to complete in the next few days
      }
    }
    # The loop below is used for processioning the combined sheet that defined 
    # in the config.yml
    for (i in 1:length(config)) {
      # Get the position of the combined sheet in the config.yml
      if (length(unlist(config[i])) == 2){
        # Get the name of the combined sheet and assign it as name_temp
        name_temp<-gsub(' ','_',names(config[i]))
        # Get the sheets that need to combined
        combined_sheets <- unlist(config[i])
        # According to the define in the config, get the main sheet and the 
        # vice sheet
        main_sheet_name <- sheet_name_t[combined_sheets[1]]
        vice_sheet_name <- sheet_name_t[combined_sheets[2]]
        # Get the row length of the main sheet
        main_row_len <- dim(eval(as.symbol(main_sheet_name)))[1]
        # Get the row length of the vice sheet
        vice_row_len <- dim(eval(as.symbol(vice_sheet_name)))[1]
        # Get the row name of the vice sheet,according to the data sets' format,
        # the row name of the vice sheet is usually the first column
        vice_row_name <- unlist(eval(as.symbol(vice_sheet_name))[1])
        # Get the column length of the main sheet
        main_col_len <- dim(eval(as.symbol(main_sheet_name)))[2]
        # Get the column length of the vice sheet
        vice_col_len <- dim(eval(as.symbol(vice_sheet_name)))[2]
        # Get the column name of the main sheet
        main_col_name <- unlist(colnames(eval(as.symbol(main_sheet_name))))
        # Get the column name of the vice sheet
        vice_col_name <- unlist(colnames(eval(as.symbol(vice_sheet_name))))
        # Add the name of the columns to the col_name for the later rename of 
        # the relative data frame
        col_name <<- c('Date',main_col_name[1])
        
        # Get the combined columns name
        for (k in 2:length(main_col_name)){
          for (c in 1:length(vice_col_name)){
            if (main_col_name[k]==vice_col_name[c]){
              for (ck in vice_row_name){
                col_name <<- append(col_name, 
                                    paste(main_col_name[k],ck,sep='_'))
              }
            }
          }
        }
        
        # Get the same name column
        common_col <- intersect(main_col_name, vice_col_name)
        common_lenth <- length(common_col)
        
        # Filter the main sheet data and vice sheet data
        main_temp <- eval(as.symbol(main_sheet_name))[,common_col]
        vice_temp <- eval(as.symbol(vice_sheet_name))[,common_col]
        
        # Assign the combined data sheet with name final_xxx
        assign(paste('final',name_temp,sep='_'),
               matrix(nrow = 0,ncol = length(col_name)),
               envir = .GlobalEnv)
        # Rename the final_xxx in
        ma_temp <- eval(as.symbol(paste('final',name_temp,sep='_')))
        colnames(ma_temp) <- unlist(col_name)
        assign(paste('final',name_temp,sep='_'),ma_temp, envir = .GlobalEnv)
        # Get the date of the file
        date_str <- unlist(strsplit(
          unlist(strsplit(file_add,config['file_add_and_name']))[2],'.xlsx'))
        # Create a temp data set for store the data
        temp_sets <- matrix(nrow = main_row_len,ncol=2)
        temp_sets[,1] <-rep(format(as.Date(date_str, format='%b %d %Y'),
                                   '%Y/%m/%d'),main_row_len)
        temp_sets[,2] <- eval(as.symbol(paste('type',main_sheet_name,sep='_')))
        # Create temp matrix to store data
        col_temp <- matrix(nrow = main_row_len,ncol=1)
        col_final <- matrix(nrow = main_row_len,ncol=0)
        
        # Assign the data into each colunms and combined it into the final one
        for(k in 1:common_lenth){
          for (d in 1:vice_row_len){
            main_col_temp <- unlist(as.data.frame(main_temp[k]))
            vice_col_temp <- unlist(as.data.frame(vice_temp[k]))
            col_temp[,1] <- main_col_temp*((vice_col_temp)[d]/sum(vice_col_temp))
            col_final <- cbind(col_final,col_temp)
          }
        }
        # Combined the data to the temp_sets
        temp_sets <- cbind(temp_sets,col_final)
        # Rename the temp_sets
        colnames(temp_sets) <- unlist(col_name)
        # Create a temp variable as final_xxx
        temp_final <- eval(as.symbol(paste('final',name_temp,sep='_')))
        # Combined data
        temp_final <- rbind(temp_final,temp_sets)
        # Assign the final_xxx with the latest data
        assign(paste('final',name_temp,sep='_'),
               as.data.frame(temp_final), 
               envir = .GlobalEnv)
        # Plot the plots
        assign(paste('fig',name_temp,sep='_'),
               plot_sheet_type_2(paste('final',name_temp,sep='_')), 
               envir = .GlobalEnv)
        # Add the string to the list of the plots fig_f 
        fig_f <<- append(fig_f,paste('fig',name_temp,sep='_'))
      }
    }
  }else if (file_stats=='others'){
    for (i in sheet_name_t){
      if(sheet_stats[i]==1){
        assign(paste('fig',i,sep='_'),plot_sheet_type_1(i))
      }else if(sheet_stats[i]==2){
        assign(paste('new',i,sep='_'),eval(as.symbol(i)))
        row_len <- length(rownames(eval(as.symbol(paste('new',i,sep='_')))))
        temp_sets <- matrix(nrow = row_len,ncol=length(colnames(eval(as.symbol(paste('final',i,sep='_'))))))
        date_str <- unlist(strsplit(unlist(strsplit(t,config['file_add_and_name']))[2],'.xlsx'))
        temp_sets[,1] <-rep(format(as.Date(date_str, format='%b %d %Y'),'%Y/%m/%d'),row_len)
        
        # This is for avoiding some sheet records the data in a different length
        # for example, in the May 30 2020 and May 29 2020 files, the kon records
        # are different,etc
        if (dim(temp_sets)[1]!=length(eval(as.symbol(paste('type',i,sep='_'))))){
          fix_temp <- matrix(nrow = abs(dim(temp_sets)[1]-length(eval(as.symbol(paste('type',i,sep='_'))))),
                             ncol = dim(temp_sets)[2])
          fix_temp[,1] <- rep(format(as.Date(date_str, format='%b %d %Y'),'%Y/%m/%d'),dim(fix_temp)[1])
          colnames(fix_temp) <- c('Date',colnames(eval(as.symbol(i))))
          temp_sets <- rbind(temp_sets,fix_temp)
        }
        
        temp_sets[,2] <- eval(as.symbol(paste('type',i,sep='_')))
        new_temp <- as.data.frame(eval(as.symbol(paste('new',i,sep='_'))))
        # This is same to the above
        if (dim(new_temp)[1]!=length(eval(as.symbol(paste('type',i,sep='_'))))){
          fix_temp <- matrix(nrow = abs(dim(new_temp)[1]-length(eval(as.symbol(paste('type',i,sep='_'))))),
                             ncol = dim(new_temp)[2])
          fix_temp[,1] <- rep('Uppgift saknas',dim(fix_temp)[1])

          fix_temp[,2:(dim(fix_temp)[2])] <- rep(0,dim(fix_temp)[2]-1)
          
          colnames(fix_temp) <- unlist(colnames(new_temp))
          new_temp <- rbind(new_temp,fix_temp)
        }
        old_temp <- as.data.frame(eval(as.symbol(paste('old',i,sep='_'))))
        # This is same to the above
        if (dim(old_temp)[1]!=length(eval(as.symbol(paste('type',i,sep='_'))))){
          fix_temp <- matrix(nrow = abs(dim(old_temp)[1]-length(eval(as.symbol(paste('type',i,sep='_'))))),
                             ncol = dim(old_temp)[2])
          fix_temp[,1] <- rep('Uppgift saknas',dim(fix_temp)[1])
          fix_temp[,2:(dim(fix_temp)[2])] <- rep(0,dim(fix_temp)[2]-1)
          colnames(fix_temp) <- unlist(colnames(old_temp))
          old_temp <- rbind(old_temp,fix_temp)
        }

        for(k in 2:length(colnames(eval(as.symbol(i))))){
          temp_sets[,k+1] <- as.numeric(unlist(new_temp[k])) - as.numeric(unlist(old_temp[k]))
          }
        colnames(temp_sets) <- c('Date',colnames(eval(as.symbol(i))))


        temp_final <- eval(as.symbol(paste('final',i,sep='_')))
        temp_final <- rbind(temp_final,temp_sets)
        assign(paste('final',i,sep='_'),as.data.frame(temp_final), envir = .GlobalEnv)
        assign(paste('fig',i,sep='_'),plot_sheet_type_2(paste('final',i,sep='_')), envir = .GlobalEnv)
        assign(paste('old',i,sep='_'),eval(as.symbol(i)), envir = .GlobalEnv)
      }else{
        # When sheet stats is 3, it means it is the data with the date(weekly) 
        # sequence, wait to complete in the next few days
      }
    }
    for (i in 1:length(config)) {
      if (length(unlist(config[i])) == 2){
        name_temp<-gsub(' ','_',names(config[i]))
        combined_sheets <- unlist(config[i])

        main_sheet_name <- sheet_name_t[combined_sheets[1]]
        vice_sheet_name <- sheet_name_t[combined_sheets[2]]
        row_len <- length(rownames(eval(as.symbol(main_sheet_name))))
        vice_row_len <- length(rownames(eval(as.symbol(vice_sheet_name))))
        vice_row_name <- unlist(eval(as.symbol(vice_sheet_name))[1])
        main_col_len <- length(colnames(eval(as.symbol(main_sheet_name))))
        vice_col_len <- length(colnames(eval(as.symbol(vice_sheet_name))))
        main_col_name <- unlist(colnames(eval(as.symbol(main_sheet_name))))
        vice_col_name <- unlist(colnames(eval(as.symbol(vice_sheet_name))))
        col_name <<- c('Date',main_col_name[1])
        
        if(vice_col_len != length(eval(as.symbol(paste('type',vice_sheet_name,sep='_'))))){
          vice_row_name <- eval(as.symbol(paste('type',vice_sheet_name,sep='_')))
        }
        
        for (k in 2:length(main_col_name)){
          for (c in 1:length(vice_col_name)){
            if (main_col_name[k]==vice_col_name[c]){
              for (ck in vice_row_name){
                col_name <<- append(col_name, paste(main_col_name[k],ck,sep='_'))
              }
            }
          }
        }
        
        common_col <- intersect(main_col_name, vice_col_name)
        common_lenth <- length(common_col)
        
        main_temp <- eval(as.symbol(main_sheet_name))[,common_col]
        vice_temp <- eval(as.symbol(vice_sheet_name))[,common_col]
        
        if(dim(vice_temp)[1]!= length(eval(as.symbol(paste('type',vice_sheet_name,sep='_'))))){

          fix_temp <- matrix(data=rep(0,dim(vice_temp)[2]),
                             nrow = abs(dim(vice_temp)[1]- length(eval(as.symbol(paste('type',vice_sheet_name,sep='_'))))),
                             ncol = dim(vice_temp)[2])
          colnames(fix_temp) <- unlist(colnames(vice_temp))
          vice_temp <- rbind(vice_temp,fix_temp)
          vice_row_len <- dim(vice_temp)[1]
        }

        date_str <- unlist(strsplit(unlist(strsplit(file_add,config['file_add_and_name']))[2],'.xlsx'))
        temp_sets <- matrix(nrow = row_len,ncol=2)
        
        temp_sets[,1] <-rep(format(as.Date(date_str, format='%b %d %Y'),'%Y/%m/%d'),row_len)
        
        temp_sets[,2] <- eval(as.symbol(paste('type',main_sheet_name,sep='_')))
        

        col_temp <- matrix(nrow = row_len,ncol=1)
        col_final <- matrix(nrow = row_len,ncol=0)
        
        for(k in 1:common_lenth){
          for (d in 1:vice_row_len){
            main_col_temp <- unlist(as.data.frame(main_temp[k]))
            vice_col_temp <- unlist(as.data.frame(vice_temp[k]))
            col_temp[,1] <- main_col_temp*((vice_col_temp)[d]/sum(vice_col_temp))
            col_final <- cbind(col_final,col_temp)
          }
        }

        temp_sets <- cbind(temp_sets,col_final)
        colnames(temp_sets) <- unlist(col_name)

        temp_final <- eval(as.symbol(paste('final',name_temp,sep='_')))
        # Rename the final data for sometimes, the name of the groups changes
        colnames(temp_final) <- unlist(col_name)

        
        temp_final <- rbind(temp_final,temp_sets)
        assign(paste('final',name_temp,sep='_'),as.data.frame(temp_final), envir = .GlobalEnv)
        assign(paste('fig',name_temp,sep='_'),plot_sheet_type_2(paste('final',name_temp,sep='_')), envir = .GlobalEnv)
      }
    }
  }
}