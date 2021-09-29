#### My commands ####

#' Count the percentage of missing data
#'
#' Quickly tabulates the number of NA's in a data set.
#' 
#' @param df A data frame (or a subset of a data frame) to look for missing data in.
#' @examples mtcars %>% count_missing()
#' @export


count_missing=function(df){
  df %>% 
    map_df(~sum(is.na(.))) %>% # count number of missing cases across the whole df
    pivot_longer( # Transpose lengthwise by:
      cols = everything(), # Taking ALL variable names...
      names_to="variable", # ...and dumping them into this new variable
      values_to="missing_count") %>% #...then place their values in this new column
    mutate(percent_missing=scales::percent(missing_count/NROW(df), 
                                           scale = 100, accuracy = .1)) %>% 
    arrange(desc(percent_missing))
}




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
#' This function sets up R for data analysis by loading the following packages: The whole easystats suite, rstanarm, flextable, psych, broom, and broom.mixed.
#' @export

prime_r=function(){
  pacman::p_load(bayestestR, bayesplot, performance, effectsize, rstanarm, see, parameters, insight, report, flextable, psych, broom, broom.mixed)
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
  #logical test to check if various columns exist in data; if true, it drops them.  
  crap_vars=c("ip_address", "recipient_first_name", "recipient_last_name", "recipient_email",
              "location_latitude", "location_longitude", "external_reference",
              "progress", "finished", "distribution_channel", "user_language", "recorded_date")
  
  df=df %>% select(-any_of(crap_vars))
  
  return(df)
}


#' Quick import Qualtrics files
#'
#' A quick-import option for Qualtrics surveys that performs a number of functions. It reads in a .csv file; cleans the column names; removes the extra two rows that Qualtrics includes underneath the header rows; reformats the date columns with lubridate and removes their time stamps; drops junk columns; and drops test runs and spam responses from the data.
#' 
#' @param file A qualtrics .csv file export.
#' @param remove_StartEnd_dates A separate toggle option to remove the start_date and end_date columns that indicate when a Qualtrics survey was opened and started, and finished, respectively. If set to TRUE, both columns are dropped upon import.
#' @examples data= read_Qualtrics("survey.csv")
#' @export

read_Qualtrics=function(file, remove_StartEnd_dates=TRUE){
  
  # define the crap to be removed
  crap_vars=c("ip_address", "recipient_first_name", "recipient_last_name", "recipient_email",
              "location_latitude", "location_longitude", "external_reference",
              "progress", "finished", "distribution_channel", "user_language", "recorded_date")
  
  #import data set
  file=readr::read_csv(file) %>% 
    janitor::clean_names() %>% 
    dplyr::select(-any_of(crap_vars)) %>% 
    dplyr::slice(3:n()) %>% 
    reformat_Qualtrics_datetime(., remove_timestamps = TRUE)
  
  #remove all junk responses
  file=file[!(file$status==1),]
  file=file %>% select(-status)
  
  # date drop check; if true, drop start and end dates for survey responses
  if(remove_StartEnd_dates==TRUE) return(file=file %>% select(-c(start_date, end_date)))
  if(remove_StartEnd_dates==FALSE) return(file)
}

#' Splice together data sets
#'
#' Splice together a numeric and a text-based Qualtrics survey. This function pastes a pattern after the variable names in the text data set, and then joins these data sets together
#' 
#' @param numeric_df  A qualtrics .csv file export containing the NUMERIC versions of responses
#' @param text_df A Qualtrics .csv file import containing the TEXT versions of responses
#' @examples data= read_Qualtrics("survey.csv")
#' @export


splice_text=function(numeric_df, text_df){
  
  # add _text suffix
  text_df=text_df %>% rename_with( ~ paste0(.x, "_text"))
  
  # join data sets together
  numeric_df=numeric_df %>% inner_join(text_df, by=c("response_id"="response_id_text"))
  
  # remove a few columns
  stuff=c("start_date_text", "end_date_text", "duration_in_seconds_text")
  numeric_df=numeric_df %>% select(-any_of(stuff))
  
  return(numeric_df)
}




#' Remove duplicate strings
#'
#' This function is designed to find and remove duplicate survey entries from SONA studies. You provide a data set and a column in which to check for duplicates (e.g., participant's names or email addresses), and this function removes all rows from the data that appear more than once. For example, if "Renee Johnson" completed the study three times, this function will keep only her first response and delete the later two entries.
#' 
#' @param df  A data frame
#' @param x The column to check for duplicates. Must be of class 'character'.
#' @examples my_survey = my_survey %>% drop_dupes(name)
#' @export


drop_dupes=function(df, x){
  df=df %>%
    mutate({{x}}:=str_to_lower({{x}})) %>%
    drop_na({{x}}) %>% 
    distinct({{x}}, .keep_all=TRUE)
  
  return(df)
}


#' Generate random ID tags
#'
#' Generate random ID's for participants to ensure confidentiality or any other purpose. Each random ID consists of a first name followed by a random number.
#' 
#' @param n  The number of names to generate. Set equal to the desired number. If inserting the generated list of names into a table, set to the same number of rows as the table.
#' @examples tibble(name=gen_id(n=3), fav_color=c("red", "blue", "green"))
#' @seealso The list of names used in this function were randomly drawn from the US Baby Names data set, downloadable here: https://www.kaggle.com/kaggle/us-baby-names
#' @export

gen_id=function(n){
  name=paste0(sample(legaldmlab::names$names, {{n}}, replace = TRUE),"_",sample(round(rnorm(n=5000, mean = 2500, sd=400)),{{n}},replace = TRUE))
  
  return(name)
}


#' Quickly tidy dates
#'
#' Combines two formating functions in one to make working with dates less terrible. The first thing the function does is take a character vector of the format MM/DD/YYYY and tell R to change it to the more stat-software-friendly-format YYYY-MM-DD. It then converts this reformatted character string into a date object with the lubridate package.
#' 
#' @param col  The date column you want to change.
#' @export
#' 
tidy_date=function(col){
  time_col=format(as.POSIXct({{col}},format='%m/%d/%y'),format='%Y-%m-%d')
  time_col=lubridate::date(time_col)
  
  return(time_col)
}

