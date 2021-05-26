#' Calculate and show variability table
#' 
#' This function accepts a list of numbers and returns (a) the Sum of Squares, (b) the sample variance and standard deviation, (c) the sample mean, and (d) a complete table showing every step of each calculation for (a) and (b). 
#' 
#' @param ... A list of numbers
#' @examples calc.variability.table(1,7,2,8,4,9,3,3,6,2)
#' 

calc.variability.table=function(...){
  data=as_tibble(c(...))
  m=mean(data$value)
  
  calc=data%>%mutate(deviation=value-m)
  calc=calc%>%mutate(deviation_squared=deviation^2)
  
  SS=sum(calc$deviation_squared)
  var.sample=(SS)/(length(calc$value)-1)
  SS=round(SS,digits=2)
  var.sample=round(var.sample,digits = 2)
  m=round(m,digits = 2)
  
  print(c("Sum of Squares:",SS))
  print(c("Sample variance:",var.sample))
  print(c("Sample mean:",m))
  
  calc=calc%>%rename("Raw_score"="value")
  
  return(calc)
}


#### z-scores and raw scores####
calc.x=function(mu,sd,z){
  xscore=(mu+(z*sd))
  return(xscore)
}

calc.z.single=function(mu,sd,x){
  zscore=((x-mu)/sd)
  print(zscore)
}


calc.z.all=function(df,col){
  df= df %>% mutate(zscore = ({{col}} - mean({{col}}))/sd({{col}}))
  return(df)
}

#### z-statistic and sample means ####

z.test=function(pop_mu, pop_sd, sample_mean, sample_n){
  se=pop_sd/(sqrt(sample_n))
  zscore=((sample_mean-pop_mu)/se)
  return(zscore)
}

calc.z.se=function(sd,n){
  se=sd/(sqrt(n))
  return(se)
}

#### t-statistics ####
sample.var=function(df,SS,var){
  df%>%(SS)/(length(var)-1)
}

calc.t.ese=function(sample_variance,n){
  ese=sqrt(sample_variance/(n))
  return(ese)
}


#####
roll.2d6=function(sides,dice,tosses){
  dice_1=sample(1:sides,size = tosses,replace = TRUE)
  dice_2=sample(1:sides,size = tosses,replace = TRUE)
  output=dice_1[]+dice_2[]
  print(output)
}

#####
sample.mean=function(col,sample_size){
  thing=sample({{col}},size=sample_size,replace = TRUE,prob = NULL)
  print(thing)
  
  print(c("sample mean:"=(mean(thing))))
}


# --------------------------------------------------------------------------------------------------
# ADMINISTRATIVE HELPER FUNCTIONS

import.quiz=function(quiz_file){
  quiz<- read_csv(here::here("Administrative", "grading",quiz_file), col_names=TRUE)%>%
    slice(1:n())%>% #when manual posting is enabled, this needs to be 3 instead of 2
    janitor::clean_names()%>%
    select(-(id:n_incorrect))%>%
    mutate(grade_score=(score/final.points.value)*100)%>%
    mutate(letter=case_when(grade_score>=90.000001 | grade_score==90 ~ "A",
                            grade_score>=80.001 &  grade_score<=89.999 ~ "B",
                            grade_score>=70.001 &  grade_score<=79.999 ~ "C",
                            grade_score>=60.001 &  grade_score<=69.999 ~ "D",
                            grade_score<=59.001 ~ "F",
                            TRUE ~ "Derp" ))
  
  quiz$grade_score=round(quiz$grade_score,digits = 1)
  
  return(quiz)
}

curve.grades=function(quiz_df,curve.points){
  
  quiz_df=quiz_df %>% mutate(curved_score=score+curve.points) %>% 
    mutate(curved_grade_score=(curved_score/final.points.value)*100) %>% 
    mutate(curved_letter=case_when(curved_grade_score>=90.000001 | curved_grade_score==90 ~ "A",
                                   curved_grade_score>=80.001 &  curved_grade_score<=89.999 ~ "B",
                                   curved_grade_score>=70.001 &  curved_grade_score<=79.999 ~ "C",
                                   curved_grade_score>=60.001 &  curved_grade_score<=69.999 ~ "D",
                                   curved_grade_score<=59.001 ~ "F",
                                   TRUE ~ "Derp" ))
  return(quiz_df)
}


check.attendence=function(zoom_log){
  class_roster=read_csv(here::here("Administrative", "Attendence stuff","class_roster.csv"),col_names = TRUE)
  
  zoom_log=zoom_log[!(zoom_log$name=="schneiderr7@montclair.edu"),] #remove professor's data
  
  minutes_tally = zoom_log %>% group_by(name) %>%
    summarize(minutes_total = sum(minutes_attended))
  
  summary_table=right_join(minutes_tally,class_roster)%>%
    arrange(name)
  
  summary_table=summary_table%>%
    mutate(logged_on=if_else(is.na(summary_table$minutes_total),0,1))%>%
    select(name,logged_on,minutes_total)
  
  return(summary_table)
}