# --------------------------------------------------------------------------------------------------
# ADMINISTRATIVE HELPER FUNCTIONS

#' Quick Import Canvas Quiz
#' 
#' Import the results of a Canvas quiz 
#' 
#' @param quiz_file A Canvas quiz .csv export (student analysis version only, not item analysis)
#' @param max_points The maximum total number of points possible on the quiz. If someone got this score, they would have a 100%.
#' @export

import.quiz=function(quiz_file, max_points){
  quiz<- read_csv(quiz_file, col_names=TRUE) %>%
    slice(1:n())%>% #when manual posting is enabled, this needs to be 3 instead of 2
    janitor::clean_names()%>%
    select(-(id:n_incorrect))%>%
    mutate(grade_score=(score/max_points)*100)%>%
    mutate(across(c(score, grade_score), as.double)) %>% 
    mutate(across(c(grade_score),round,2)) %>% 
    mutate(letter=case_when(grade_score>=90.000001 | grade_score==90 ~ "A",
                            grade_score>=80 &  grade_score<=89.999 ~ "B",
                            grade_score>=70 &  grade_score<=79.999 ~ "C",
                            grade_score>=60 &  grade_score<=69.999 ~ "D",
                            grade_score<60 ~ "F",
                            TRUE ~ "Derp" ))
  
  quiz$grade_score=round(quiz$grade_score,digits = 1)
  
  return(quiz)
}


#' Curve Quiz Grades
#' 
#' Apply a curve of a specified number of points to a quiz and see how it affects the results. The
#' 
#' @param quiz_df A Canvas quiz data frame imported with the import.quiz function
#' @param curve.points The number of points to add to the total. For example, if a quiz was worth 25 points and the user enters 5 as this value, each student's grade will be boosted by 5 points.
#' @param max_points The maximum total number of points possible on the quiz. If someone got this score, they would have a 100%.
#' @export

curve.grades=function(quiz_df,curve.points, max_points){
  
  quiz_df=quiz_df %>% mutate(curved_score=score+curve.points) %>% 
    mutate(curved_grade_score=(curved_score/max_points)*100) %>% 
    mutate(across(c(curved_grade_score),round,2)) %>% 
    mutate(curved_letter=case_when(curved_grade_score>=90.00 | curved_grade_score==90 ~ "A",
                                   curved_grade_score>=80.00 &  curved_grade_score<=89.999 ~ "B",
                                   curved_grade_score>=70.00 &  curved_grade_score<=79.999 ~ "C",
                                   curved_grade_score>=60.00 &  curved_grade_score<=69.999 ~ "D",
                                   curved_grade_score<60 ~ "F",
                                   TRUE ~ "Derp" ))
  return(quiz_df)
}

#' Quick Summary of Quiz Grades
#' 
#' This accepts a quiz data frame and returns a summary table of both the un-curved and curved quiz grades 
#' 
#' @param quiz_df A Canvas quiz data frame imported with the import.quiz function
#' @export

summarize_grades=function(quiz_df){
  pre_curve_stats=tabyl(df$letter)%>%
    adorn_pct_formatting() %>% 
    rename("Grade"="df$letter")
  
  post_curve_stats=tabyl(df$curved_letter)%>%
    adorn_pct_formatting() %>% 
    rename("Grade"="df$curved_letter")
  
  pre_curve_stats=pre_curve_stats %>% mutate(Curve_Group= "Original")
  post_curve_stats=post_curve_stats %>% mutate(Curve_Group="Curved")
  
  combined_data=rbind(pre_curve_stats, post_curve_stats)
  
  combined_data=flextable::as_grouped_data(combined_data, groups = "Curve_Group")
  
  combined_data=flextable::as_flextable(combined_data)
  combined_data=flextable::padding(combined_data, i=2:5, j=1, padding.left = 20)
  combined_data=flextable::padding(combined_data, i=7:11, j=1, padding.left = 20)
}