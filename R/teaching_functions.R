# --------------------------------------------------------------------------------------------------
# ADMINISTRATIVE HELPER FUNCTIONS

#' Quick Import Canvas Quiz
#' 
#' I'm feeling to lazy right now to right this description.
#' 
#' @param quiz_file A Canvas quiz .csv export
#' @export

import.quiz=function(quiz_file){
  quiz<- read_csv(quiz_file, col_names=TRUE) %>%
    dplyr::slice(1:n())%>% #when manual posting is enabled, this needs to be 3 instead of 2
    janitor::clean_names()%>%
    dplyr::select(-(id:n_incorrect))%>%
    dplyr::mutate(grade_score=(score/final.points.value)*100)%>%
    dplyr::mutate(across(c(score, grade_score), as.double)) %>% 
    dplyr::mutate(letter=dplyr::case_when(grade_score>=90.000001 | grade_score==90 ~ "A",
                            grade_score>=80.001 &  grade_score<=89.999 ~ "B",
                            grade_score>=70.001 &  grade_score<=79.999 ~ "C",
                            grade_score>=60.001 &  grade_score<=69.999 ~ "D",
                            grade_score<60 ~ "F",
                            TRUE ~ "Derp" ))
  
  quiz$grade_score=round(quiz$grade_score,digits = 1)
  
  return(quiz)
}


#' Curve Quiz Grades
#' 
#' I'm feeling to lazy right now to right this description.
#' 
#' @param quiz_df A Canvas quiz data frame
#' @export

curve.grades=function(quiz_df,curve.points){
  
  quiz_df=quiz_df %>% dplyr::mutate(curved_score=score+curve.points) %>% 
    dplyr::mutate(curved_grade_score=(curved_score/final.points.value)*100) %>% 
    dplyr::mutate(across(c(curved_grade_score),round,1)) %>% 
    dplyr::mutate(curved_letter=dplyr::case_when(curved_grade_score>=90.000001 | curved_grade_score==90 ~ "A",
                                   curved_grade_score>=80.001 &  curved_grade_score<=89.999 ~ "B",
                                   curved_grade_score>=70.001 &  curved_grade_score<=79.999 ~ "C",
                                   curved_grade_score>=60.001 &  curved_grade_score<=69.999 ~ "D",
                                   curved_grade_score<60 ~ "F",
                                   TRUE ~ "Derp" ))
  return(quiz_df)
}

