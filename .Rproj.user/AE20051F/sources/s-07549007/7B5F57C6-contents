#### My commands ####

#' Summarize plea acceptance across conditions
#'
#' Build a descriptive summary table of guilty pleas by experimental condition. Table contains the total number of participants per condition, the total number of guilty pleas per condition (and the percentage of n), and the condition names and values.
#' @param data Your data set
#' @param dv The specific column within the data set to look in (e.g., a column of participant's names or emails)
#' @param ... A list of your IV variable names
#' @examples summarize_pleas(data=sonadata, dv=accept_reject, discount_level, pts_level, evidence_strength)
#' @export

summarize_pleas=function(data, dv, ...){
  table=data %>% group_by(...) %>% count()
  
  Accepted_deals_table=data %>% group_by(...) %>% 
    filter({{dv}}==1) %>% 
    count()
  
  table=table %>% add_column(GuiltyPleas=Accepted_deals_table$n) %>% 
    mutate(Percent_Accept=GuiltyPleas/n*100)
  
  table$Percent_Accept=round(table$Percent_Accept,1)
  
  return(table)
}




#' Load essential packages for analyzing data
#' 
#' This function sets up R for data anlysis by loading the following packages: The whole easystats suite, rstanarm, flextable, gt, and psych.
#' @export

prime_r=function(){
  pacman::p_load(bayestestR, bayesplot, performance, effectsize, rstanarm, see, parameters, insight, report, flextable, psych, gt)
  print("Ready!")
}

#' Find duplicates
#'
#' This function searches though a data frame for duplicate entries to ensure that extra responses don't mess up your data analysis. The function returns a list that shows a table with the observed counts of responses and the total count of the extra responses (assuming that each person is supposed to have one response each)
#' @param df Your data set
#' @param col The specific column within the data set to look in (e.g., a column of participant's names or emails)
#' @examples find_duplicates(mtcars,cyl)
#' @export

find_duplicates=function(df,col){
  
  dupe_table<-df %>% mutate(str_to_lower({{col}}) ) %>% # Step 1: Force all strings to lower case
    
    janitor::get_dupes({{col}}) %>%  # Step 2: Find duplicates using the dupe checking command from {janitor}
    tidyr::drop_na({{col}}) %>% # NA's are counted as duplicates, so remove them
    dplyr::select(c({{col}},dupe_count)) %>% # For easier summary view
    unique() %>% # only show first instance
    dplyr::rename("count"="dupe_count")
  
  i=dupe_table$count[]
  dupe.tally=sum(i[]-1) # tally up the number of duplicates, which is equal to the sum of i responses-1
  
  dupe_list_test=list(dupe_table,paste0('Number of extra responses that need to be removed: ', dupe.tally))
  
  if(dupe.tally>0) {print(dupe_list_test)}
  else{print("No duplicates found")}
}


#' Read in data from an SPSS file
#'
#' Read data in from any SPSS file and store it as a tidy tibble. 
#' @param file An SPSS .sav file. Note that the full file name and file extension should be included, and the whole thing quoted.
#' @examples read_spss("survey.sav")
#' @export

read_spss=function(file){
  spss_file=tidyverse::as_tibble(foreign::read.spss(file=file, quote = FALSE, sep = ","))
  return(spss_file)
}


#' Convert an SPSS .sav file to .csv
#'
#' Does what the name implies; converts an SPSS .sav file in the working directory to .csv file format. Please note that the argument names must be in quotes, and must include the file extensions. 
#' @param sav_file The name of the SPSS .sav file, including the .sav extension.
#' @param csv_name The name you want to give to the converted .csv file, including the .csv extension. Can be the same as or different from the original .sav name.
#' @examples convert_spss(sav_file="survey.sav", csv_name="converted_file.csv")
#' @export

convert_spss=function(sav_file, csv_name){
  write.table(foreign::read.spss(sav_file), file=csv_name, quote = FALSE, sep = ",",row.names = FALSE)
}


#' Find outliers in a sample of data
#'
#' Search through an indicated column for scores that are outliers, and marks them with a 0/1 coding. Depending on the formula used....
#' 
#' @param df The supplied data set
#' @param col The column to examine for outliers. If time_data is set to TRUE, \eqn{a + b}
#' @export

mark_outliers=function(df,col, time_data=TRUE){
  
  # DEFINE ALL FUNCTIONS
  #1. The Double-MADS-from-Median approach, for time data
  mads_method=function(df,col){
    
    DoubleMAD <- function(x, zero.mad.action="warn"){
      # The zero.mad.action determines the action in the event of an MAD of zero.
      # Possible values: "stop", "warn", "na" and "warn and na".
      x         <- x[!is.na(x)]
      m         <- median(x)
      abs.dev   <- abs(x - m)
      left.mad  <- median(abs.dev[x<=m])
      right.mad <- median(abs.dev[x>=m])
      if (left.mad == 0 || right.mad == 0){
        if (zero.mad.action == "stop") stop("MAD is 0")
        if (zero.mad.action %in% c("warn", "warn and na")) warning("MAD is 0")
        if (zero.mad.action %in% c(  "na", "warn and na")){
          if (left.mad  == 0) left.mad  <- NA
          if (right.mad == 0) right.mad <- NA
        }
      }
      return(c(left.mad, right.mad))
    }
    DoubleMADsFromMedian <- function(x, zero.mad.action="warn"){
      # The zero.mad.action determines the action in the event of an MAD of zero.
      # Possible values: "stop", "warn", "na" and "warn and na".
      two.sided.mad <- DoubleMAD(x, zero.mad.action)
      m <- median(x, na.rm=TRUE)
      x.mad <- rep(two.sided.mad[1], length(x))
      x.mad[x > m] <- two.sided.mad[2]
      mad.distance <- abs(x - m) / x.mad
      mad.distance[x==m] <- 0
      return(mad.distance)
    }
    
    df=df %>% mutate(double_mads= DoubleMADsFromMedian({{col}}),
                     outlier=if_else(double_mads>=3,1,0))
    
    outliers= df %>% filter(double_mads>=3)
    num.outliers=nrow(outliers)
    
    df=df %>% select(-(double_mads))
    
    message(paste0("Number of outliers: ",num.outliers, " (", scales::percent(num.outliers/(nrow(df)))," of n)"))
    return(df)
  }
  
  
  #2. The formula version from the SEM textbook
  formula_method=function(df,col){
    df=df %>% dplyr::mutate(outlier=dplyr::if_else(round(abs({{col}}-median({{col}}))/(1.483*mad({{col}}, constant = 1)),2)>2.24,1,0))
    
    outliers=df %>% filter(outlier==1)
    num.outliers=nrow(outliers)
    
    message(paste0("Number of outliers: ",num.outliers, " (", scales::percent(num.outliers/(nrow(df)))," of n)"))
    return(df)
  }
  
  
  # Run conditions
  if(time_data==TRUE) { df=mads_method({{df}},{{col}}) }
  
  if(time_data==FALSE) {df=formula_method({{df}}, {{col}}) }
  
  return(df)
}


########## Qualtrics formatting ##########

#' Remove the time stamps from a Qualtrics data files by mutating the start and end date columns 
#' 
#' @param df A survey import that has the columns "start_date" and "end_date", both of which will be subjected to mutation to reformat their respective times. 
#' @param remove_timestamps If this option is set to TRUE, the time will be removed from both columns and the formatted columns will contain the date only. Setting it to FALSE leaves the time and reformats the column from a character structure to a date-time structure.
#' @examples reformat_Qualtrics_datetime()
#' @export

reformat_Qualtrics_datetime=function(df,remove_timestamps=TRUE){
  if (remove_timestamps==FALSE) return(df %>% dplyr::mutate(dplyr::across(c(start_date,end_date),lubridate::ymd_hms)))
  if (remove_timestamps==TRUE) return(df %>% dplyr::mutate(dplyr::across(c(start_date,end_date),lubridate::date)))
}

#' Remove test runs and spam from Qualtrics data files 
#' 
#' In a Qualtrics survey, the "status" column contains the response type code. Responses will be flagged as 1 for real participants, or other numbers for spam responses or test runs. This command removes all test runs and spam responses (i.e., responses not equal to 1) from the data set, based on their coding. It then drops the status column when finished.
#' 
#' @param df A Qualtrics survey import that has the "status" column.
#' @examples drop_testruns(df)
#' @export

drop_junk_responses=function(df){
  df=df[!(df$status==1),]
  df=df %>% select(-status)
  return(df)
}

#' Remove seldom-used variables from a fresh Qualtrics survey import
#' 
#' Searches a Qualtrics import and removes the columns: "progress", "finished", "distribution_channel", "user_language", and "recorded_date".
#' @param df An imported Qualtrics survey
#' @examples drop_Qualtrics_JunkCols(df)
#' @export

drop_Qualtrics_JunkCols=function(df){
  df=df %>% dplyr::select(-c(progress,finished,distribution_channel,user_language,recorded_date, ip_address, recipient_first_name, recipient_last_name, recipient_email, external_reference, location_latitude, location_longitude))
  return(df)
}


#' Quick import Qualtrics files
#'
#' A quick-import option for Qualtrics surveys that performs a number of functions. It reads in a .csv file; cleans the column names; removes the extra two rows that Qualtrics includes underneath the header rows; reformats the date columns and removes their timestamps; drops junk columns; and drops test runs and spam responses from the data.
#' 
#' @param file A qualtrics .csv file export.
#' @examples data= read_Qualtrics("survey.csv")
#' @export

read_Qualtrics=function(file){
  file=readr::read_csv(here::here("Data",file)) %>% 
    janitor::clean_names() %>% 
    dplyr::slice(3:n()) %>% 
    reformat_Qualtrics_datetime(., remove_timestamps = TRUE) %>%
    drop_junk_responses() %>% 
    drop_Qualtrics_JunkCols()
  return(file)
}




################### Commands from others ###########################################################


#' Open a graphics window
#'
#' Opens the Plots viewer as a separate window, at the dimensions specified by the user.
#' 
#' @param width The width of the window
#' @param height The height of the window
#' @examples openGraph(width= 5, height= 6)
#' @export

# Kruschke graph commands
openGraph = function( width=7 , height=7 , mag=1.0 , ... ) {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    tryInfo = try( X11( width=width*mag , height=height*mag , type="cairo" , 
                        ... ) )
    if ( class(tryInfo)=="try-error" ) {
      lineInput = readline("WARNING: Previous graphics windows will be closed because of too many open windows.\nTO CONTINUE, PRESS <ENTER> IN R CONSOLE.\n")
      graphics.off() 
      X11( width=width*mag , height=height*mag , type="cairo" , ... )
    }
  } else { # Windows OS
    tryInfo = try( windows( width=width*mag , height=height*mag , ... ) )
    if ( class(tryInfo)=="try-error" ) {
      lineInput = readline("WARNING: Previous graphics windows will be closed because of too many open windows.\nTO CONTINUE, PRESS <ENTER> IN R CONSOLE.\n")
      graphics.off() 
      windows( width=width*mag , height=height*mag , ... )    
    }
  }
}


#' Save a graph in the Plots viewer/graphics window
#'
#' @param file The desired file name for the output
#' @param height Desired file type. Opetions are: PDF, jpg, or eps.
#' @examples saveGraph(file= "a_pretty_graph", type="jpg")
#' @export

saveGraph = function( file="saveGraphOutput" , type="pdf" , ... ) {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    if ( any( type == c("png","jpeg","jpg","tiff","bmp")) ) {
      sptype = type
      if ( type == "jpg" ) { sptype = "jpeg" }
      savePlot( file=paste0(file,".",type) , type=sptype , ... )     
    }
    if ( type == "pdf" ) {
      dev.copy2pdf(file=paste0(file,".",type) , ... )
    }
    if ( type == "eps" ) {
      dev.copy2eps(file=paste0(file,".",type) , ... )
    }
  } else { # Windows OS
    file=paste0(file,".",type) 
    savePlot( file=file , type=type , ... )
  }
}

