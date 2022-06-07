#### General helper functions ####

#' Workaround fix for a broken loo package command
#'
#' The standard loo command doesn't work on Windows because of a bug. This command gets around that bug by extracting the log likelihood first, and then running the loo command on that extracted object
#' @param model An rstanarm model object.
#' @export

loo_WindowsParallelFix=function(model){
  loglik1=rstanarm::log_lik(model)
  loo1=loo::loo(loglik1,save_psis=TRUE,cores=16)
  return(loo1)
}

#' Fancier model summary
#'
#' This function uses bayestestR::describe_posterior to generate a summary of a Bayesian rstan model. It adds an Odds Ratio column for Logistic Regressions, and neatens some columns up for nicier presentation.
#' @param model An rstanarm model object. Since this command adds an Odds Ratio column, it is made for Binomial Logistic Regression models.
#' @return The posterior summary.
#' @export


# custom version of describe_posterior() that adds Odds Ratios to the output and rounds the columns
describe_posterior_fancy=function(model){
  # Build table
  table1=bayestestR::describe_posterior(model)%>% 
    dplyr::mutate(Odds_Ratio=exp(Median)) %>% 
    dplyr::relocate(Odds_Ratio, .after="Parameter") %>% 
    dplyr::select(-(c(Rhat,ESS)))
  
  #round decimals
  table1= table1 %>% dplyr::mutate(dplyr::across(c(Odds_Ratio,Median,CI_low,CI_high,pd),round,2))
  table1=table1 %>% dplyr::mutate(dplyr::across(c(ROPE_low,ROPE_high),round,3))
  table1$ROPE_Percentage=round(table1$ROPE_Percentage,4)
  
  return(table1)
}


#' Grab and display odds ratio's for parameters in a Bayesian Logistic Regression
#'
#' Combination of bayestestR::describe_posterior and effectsize::interpret_oddsratio that gives a summary table of parameters and their respective odds ratios, for rstan BLR models
#' @param stan_glm_model An rstanarm model object (Binomial Logistic Regression models only).
#' @return A nice summary table.
#' @export

describe_odds=function(stan_glm_model){
  model_summary=describe_posterior_fancy(stan_glm_model) %>% 
    dplyr::select(Parameter:Median)
  
  model_summary=model_summary %>%  dplyr::mutate(effect_size=interpret_oddsratio(model_summary$OddsRatio[],log = TRUE))
  return(model_summary)  
}


#### Flextable commands ####-------------------------------------------------------------------------------------------------------------




#' Produce APA formatted Bayesian model summary table
#'
#' Accepts a Bayesian Logistic Regression rstanarm model and returns a perfectly formatted APA results table with flextable, ready to be exported to MS Word.
#' @param stan_glm_model An stan_glm model object. 
#' @export

table_BayesLogReg=function(stan_glm_model){
  glm_table=describe_posterior_fancy(stan_glm_model) %>% 
    dplyr::select(Parameter:pd,Rhat:ESS) %>% 
    tidyr::unite("Full_HDI", CI_low:CI_high, sep = ", ", remove = TRUE, na.rm = FALSE) %>% 
    dplyr::mutate("Full_HDI"=paste0('[',Full_HDI,']')) %>% 
    dplyr::select(-(CI))
  
  glm_table=flextable::flextable(glm_table)
  glm_table <-flextable::align(glm_table, align = "center", part = "all") #center align
  glm_table=flextable::add_header_lines(glm_table, values = c("Title","Table #"))
  glm_table<-flextable::font(glm_table,part = "all", fontname = "Times") # Font
  glm_table <-flextable::fontsize(glm_table, size = 11, part = "all") # Font size
  glm_table=flextable::autofit(glm_table)
  glm_table<-flextable::add_footer_lines(glm_table, values = "Note. ESS= Effective Sample Size; pd= Probability of Direction; HDI= Highest Density Interval.")
  glm_table=flextable::font(glm_table,part = "footer",fontname = "Times")
  glm_table=flextable::fontsize(glm_table,part = "footer",size = 11)
  glm_table=flextable::set_header_labels(glm_table,OddsRatio="Odds Ratio")
  glm_table=flextable::set_header_labels(glm_table,Full_HDI="89% HDI")
  return(glm_table)
}

#' Produce an APA formatted ROPE test table
#'
#' Accepts a Bayesian Logistic Regression rstanarm model and returns a perfectly formatted APA results table with flextable, ready to be exported to MS Word. 
#' @param stan_glm_model An stan_glm model object. 
#' @export

table_ROPE_test=function(rstan_model){
  model_summary=describe_posterior_fancy(rstan_model) %>% 
    select(Parameter:pd,Rhat:ESS) %>% 
    unite("89% HDI", CI_low:CI_high, sep = ", ", remove = TRUE, na.rm = FALSE) %>% 
    select(-(CI))
  
  ROPEtable=rope(rstan_model,range = c(-0.181,0.181), ci=0.89, ci_method = "HDI")%>% 
    select(Parameter,CI,ROPE_low:ROPE_Percentage) %>% 
    unite("ROPE Range", ROPE_low:ROPE_high, sep = ", ", remove = TRUE, na.rm = FALSE) %>% 
    mutate(Percent_in_ROPE=scales::percent(ROPE_Percentage,accuracy = 0.1,scale = 100))  %>% 
    select(-(ROPE_Percentage)) %>% 
    add_column(Practical_Significance=c("")) %>% 
    add_column('89% HDI'=model_summary$`89% HDI`, .before="Practical_Significance") %>% # this line is why "model_summary" is needed
    select(-(CI))
  
  ROPEtable=flextable(ROPEtable)
  ROPEtable=set_header_labels(ROPEtable,Percent_in_ROPE="Percent in ROPE", CI="HDI")
  ROPEtable=set_header_labels(ROPEtable,Practical_Significance="Practical Significance")
  ROPEtable<-align(ROPEtable, align = "center", part = "all")
  ROPEtable=add_header_lines(ROPEtable, values = c("Title","Table #"))
  ROPEtable<-font(ROPEtable,part = "all", fontname = "Times") # Font
  ROPEtable<-fontsize(ROPEtable, size = 11) # Font size
  ROPEtable=autofit(ROPEtable) # THIS LINE MUST BE BEFORE THE FOOTER
  ROPEtable<-add_footer_lines(ROPEtable, values = "Note. HDI= Highest Density Interval.")
  rm(model_summary)
  return(ROPEtable)
}


#' Produce an APA formatted Bayes factor models table
#'
#' Turn a single BF model object into a tidy tibble with interpretations, thanks to bayestestR 
#' @export

tidy_bf_models=function(bayestestR_obj){
  result=as_tibble(bayestestR_obj) |> 
    mutate(BF=exp(log_BF),
           Evidence=interpret_bf(BF)) |> 
    mutate(across(c(log_BF, BF), round,2))
  
  result$Evidence=stringr::str_to_sentence(result$Evidence) # Capitalize
  result$Evidence=stringr::str_replace(result$Evidence, "favour", "favor") #change to American English
  
  return(result)
}
