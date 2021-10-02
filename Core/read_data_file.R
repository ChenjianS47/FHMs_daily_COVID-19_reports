
read_data_file <- function(file_add, file_stats){
  # Read the first file
  sheet_name_original <- list()
  sheet_name <- list()
  sheet_stats <- list()
  
  sheet_name_original <- excel_sheets(file_add)
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
    assign(temp, as.data.frame(read_xlsx(file_add,sheet = i,col_types = "list")),envir = .GlobalEnv)
  }
  
  # Rename the name of sheet so that could use the data frame by eval(as.symbol(sheet_name[1]))
  sheet_name_t = gsub(' ','_',sheet_name)
  names(sheet_stats) <- unlist(sheet_name_t)
 
  if (file_stats=='first'){
    for (i in sheet_name_t){
      if(sheet_stats[i]==1){
        name_temp <- paste('fig',i,sep='_')
        assign(name_temp, plot_sheet_type_1(i) ,envir = .GlobalEnv)
        fig_f <<- append(fig_f,name_temp)
      }else if(sheet_stats[i]==2){
        assign(paste('old',i,sep='_'),eval(as.symbol(i)),envir = .GlobalEnv)
        assign(paste('type',i,sep='_'),unlist(eval(as.symbol(paste('old',i,sep='_')))[1]),envir = .GlobalEnv)
        assign(paste('final',i,sep='_'),matrix(nrow = 0,ncol = length(colnames(eval(as.symbol(i))))+1),envir = .GlobalEnv)
        ma_temp <- eval(as.symbol(paste('final',i,sep='_')))
        colnames(ma_temp) <- c('Date',colnames(eval(as.symbol(i))))
        assign(paste('final',i,sep='_'),ma_temp, envir = .GlobalEnv)
        fig_f <<- append(fig_f,paste('fig',i,sep='_'))
      }else{
        
      }
    }
    # for (i in 1:length(config)) {
    #   if (length(unlist(config[i])) == 2){
    #     name_temp<-gsub(' ','_',names(config[i]))
    #     fig_f <<- append(fig_f,paste('fig',name_temp,sep='_'))
    #   }
    # }
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
        for(k in 2:length(colnames(eval(as.symbol(i))))){
          temp_sets[,k+1] <- unlist(as.data.frame(eval(as.symbol(paste('new',i,sep='_')))[k])) - 
            unlist(as.data.frame(eval(as.symbol(paste('old',i,sep='_')))[k]))
        }
        
        temp_sets[,2] <- eval(as.symbol(paste('type',i,sep='_')))
        colnames(temp_sets) <- c('Date',colnames(eval(as.symbol(i))))
        temp_final <- eval(as.symbol(paste('final',i,sep='_')))
        temp_final <- rbind(temp_final,temp_sets)
        assign(paste('final',i,sep='_'),as.data.frame(temp_final), envir = .GlobalEnv)
        assign(paste('fig',i,sep='_'),plot_sheet_type_2(paste('final',i,sep='_')), envir = .GlobalEnv)
        assign(paste('old',i,sep='_'),eval(as.symbol(i)), envir = .GlobalEnv)
      }else{
        
      }
    }
  }
}