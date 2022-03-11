#### RANDOM ####
#' Quickly tidy dates
#'
#' Combines two formating functions in one to make working with dates less terrible. The first thing the function does is take a character vector of the format MM/DD/YYYY and tell R to change it to the more stat-software-friendly-format YYYY-MM-DD. It then converts this reformatted character string into a date object with the lubridate package.
#' 
#' @param col  The date column you want to change.
#' @export
#' 
tidy_date=function(col, includes_timestamp=TRUE){
  
  if(includes_timestamp==TRUE) (time_col=format(as.POSIXct({{col}},format='%m/%d/%Y'),format='%Y-%m-%d'))
  if(includes_timestamp==FALSE) (time_col=format(as.POSIXct({{col}},format='%m/%d/%Y'),format='%Y-%m-%d'))
  
  time_col=lubridate::date(time_col)
  
  return(time_col)
}

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


#' Read and import all files in working directory
#'
#' Quickly import multiple files at once with this command. Note that this reads all files in and stores them as separate data frames.
#' 
#' @param path  The path in the working directory where the files are located. Best to specify with slashes rather than using the here package. BE SURE TO USE THE HERE PACKAGE TO INDICATE THE FILE PATH!
#' @param extension The file extension of the files you want to import. Can be either ".xlsx" for Excel or ".csv" for CSV
#' @example read_all(path= here::here("JLWOP/Data and Models/"), extension= ".xlsx")
#' @export
#' 

read_all=function(path, extension){
  file_path <- path
  
  # save only file names with the desired extension
  file_names=file_path %>% 
    list.files() %>% 
    .[str_detect(., extension)]
  
  if(str_detect(extension, pattern = ".csv")) (file_names %>%
                                                 purrr::map(function(file_name){ # iterate through each file name
                                                   assign(x = str_remove(file_name, ".csv"), # Remove file extension
                                                          value = readr::read_csv(file.path(file_path, file_name)),
                                                          envir = .GlobalEnv)}))
  
  if(str_detect(extension, pattern = ".xlsx")) (file_names %>%
                                                  purrr::map(function(file_name){ # iterate through each file name
                                                    assign(x = str_remove(file_name, ".xlsx"), # Remove file extension
                                                           value = readxl::read_excel(file.path(file_path, file_name)),
                                                           envir = .GlobalEnv)}))
  
  if(str_detect(extension, pattern = ".xlsx")) (file_names %>%
                                                  purrr::map(function(file_name){ # iterate through each file name
                                                    assign(x = str_remove(file_name, ".sav"), # Remove file extension
                                                           value = haven::read_sav(file.path(file_path, file_name)),
                                                           envir = .GlobalEnv)}))
}

#' Create a list of file info for quick writing
#'
#' Grab and store a list of file names, extensions, and output locations to be sent to purrr for writing many files. This function creates the second of the two lists needed for exporting many files at once with `walk2`
#' 
#' @param df_list A named list of data frames
#' @param output_location The desired folder location where the files are to be saved. Uses regular format, NOT the `here` package
#' @param file_type A string that contains the file type
#' @example paths=bundle_paths(df_list= my_dfs, output_location= "FolderA/FolderB", file_type= ".sav")
#' @export

bundle_paths=function(df_list, output_location, file_type){
  names=names(df_list)
  paths=rep(here::here(output_location), length(names))
  extension=rep(c(file_type), length(names))
  
  fixed_names=paste0("/",names)
  
  path_bundle=list(paths,fixed_names, extension) %>% 
    pmap(., paste0)
  
  return(path_bundle)
}



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
#' This function sets up R for data analysis by loading the following packages: The whole easystats suite, rstanarm, flextable, and psych.
#' @export

prime_r=function(){
  pacman::p_load(easystats, bayesplot, rstanarm, flextable, psych)
  message("Packages loaded, let's go!")
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



#' Find univariate outliers in a sample of data
#'
#' Search through the indicated column in a data set and mark all outliers with a new variable, coded 1 (outlier) or 0 (not an outlier). The formula used is from Rex Kline's Principles and Practice of Structural Equation Modeling, Fourth Edition (see details below).
#' 
#' "There is no single definition of 'extreme', but one heuristic is that scores more than three standard deviations beyond the mean may be outliers...but this method is susceptible to distortion by the very outliers that it is supposed to detect; that is, it is not robust...A more robust decision rule for detecting univariate outliers is: 
#' \deqn{\frac{|X|-Mdn}{1.483*MAD}>2.24}
#' 
#' where *Mdn designates the sample median--which is more robust against outliers than the mean--and MAD is the Median Absolute Deviation (MAD) of all scores from the sample median. The quantity MAD does not estimate the population standard deviation, but the product of MAD and the scale factor 1.43 is an unbiased estimator of \sigma in a normal distribution. The value of the ratio in this equation is the distance between a score and the median expressed in robust standard deviation units. The constant 2.24 in this equation is the square root of the approximate 97.5th percentile in a central Chi-square distribution with a  single degree of freedom. A potential outlier thus has a score on the ratio in this equation that exceeds 2.24." (Kline, 2016, p. 72).
#' 
#' 
#' @param df The supplied data set
#' @param col The column to examine for outliers.
#' @example ex_data=tibble::tibble(numbers=c(19, 25, 28, 32, 10000)
#' @example mark_outliers(df=ex_data, col=numbers)
#' 
#' @export

mark_outliers=function(df, col){
  
  df=df |> 
    dplyr::mutate(outlier=dplyr::if_else(round(abs({{col}}-median({{col}}))/(1.483*mad({{col}}, constant = 1)),2)>2.24,1,0))
  
  num.outliers=df |> 
    dplyr::filter(outlier==1) |> 
    dplyr::count()
  
  message(paste0(num.outliers, " outlier(s) detected"))
  return(df)
}


########## Qualtrics formatting ##########


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
              "progress", "finished", "distribution_channel", "user_language")
  
  #import data set
  file=readr::read_csv(file) |>  
    janitor::clean_names() |>  
    dplyr::select(-any_of(crap_vars)) |> 
    dplyr::slice(3:n())
  
  #remove all junk responses
  file=file[!(file$status==1),] # for numeric surveys
  part_1=part_1 |> filter(str_detect(Status, "IP Address", negate = FALSE)) #for text surveys
  file=file |>  select(-status)
  
  # date drop check; if true, drop start and end dates for survey responses
  if(remove_StartEnd_dates==TRUE) return(file=file |>  select(-c(start_date, end_date)))
  if(remove_StartEnd_dates==FALSE) return(file)
}


#### APA TABLES AND FLEXTABLE COMMANDS ####

#' Apply some standard APA format options to a flextable
#'
#' Quickly set the font size to 11; the font style to Times New Roman; add two header lines with the table number and title; and auto fit all columns.
#' 
#' @param flextable_object  The flextable to be modified
#' @param table_title The title you want to add to the table
#' @param table_number The table number
#' @param include_note Option to add a note to the end of the table as a footer. Can either be a string of text, or FALSE if you do not wish to add any notes.
#' @export
#' 

style_table=function(flextable_object, table_title, table_number, include_note){
  
  note=c(include_note)
  
  flextable_object=flextable::add_header_lines(flextable_object, values = c(table_title, paste0("Table ",table_number)))
  flextable_object=flextable::font(flextable_object,part = "all", fontname = "Times") # Font
  flextable_object=flextable::fontsize(flextable_object, size = 11, part = "all") # Font size
  flextable_object=flextable::autofit(flextable_object)
  flextable_object=flextable::bold(flextable_object, i=1, part = "header")
  
  if(include_note==FALSE) (return(flextable_object))
  if(include_note!=FALSE) flextable_object=add_footer_lines(flextable_object, values = paste0("Note."," ",note)) %>%
    fontsize(part = "footer", size = 11) %>% 
    font(part = "footer", fontname = "Times") 
  
  return(flextable_object)
}



#' Save a table for exporting
#'
#' Shortcut function that combines to commands into one. Makes it super easy to export a flextable object to MS Word.
#' 
#' @param flextable_object The flextable you want to export
#' @param file_path The path of the file directory you want to save it in; i.e., where the file should be saved.
#' @param file_name The name you want to give the file. Must also include the file extension (".docx") at the end
#' @export
#' 

save_table=function(flextable_object, file_path, file_name){
  
  docx_file <- file.path(here::here(file_path), file_name)
  save_as_docx(flextable_object, path = docx_file)
}


#### GGPLOT STUFF ####

#' Quick styling options
#'
#' Shortcut function that combines to commands into one: Center a title and remove grid lines on a ggplot figure.
#' 
#' @param gg_graph
#' @param center_title Centers the title on a ggplot figure. Two-option toggle that can be set to TRUE or FALSE.
#' @param remove_gridlines Removes the grid lines on a ggplot figure. Two-option toggle that can be set to TRUE or FALSE.
#' @export
#' 

style_ggplot=function(gg_graph, center_title, remove_gridlines){
  if(center_title==TRUE) (gg_graph=gg_graph + theme(plot.title = element_text(hjust = 0.5)))
  if(remove_gridlines==TRUE) (gg_graph=gg_graph + theme(panel.grid = element_blank()))
  return(gg_graph)
}
