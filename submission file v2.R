TEAM_NAME="team_OSWGMCR"
VERSION="R version 3.5.2 (2019-06-24)"


load_data_001 <- function(path){
  library('readr')
  read_csv(path)
}

transformations_001 <- function(data){
  library('tidyverse')


  newdata <- data %>%
    replace_na(
      list(
        comp_int_bed_16="No",   #replace NA with "No" for comp_int_bed_16
        comp_noint_bed_16="No", #replace NA with "No" for comp_noint_bed_16
        comp_house="No",        #replace NA with "No" for comp_house
        tv_bed_16="No"          #replace NA with "No" for tv_bed_16
      )
    ) %>%

    mutate(

      #convert character variables to (possibly ordered) factors so that mice::mice works as intended
      sex=factor(sex),
      child_bull = factor(child_bull, levels=c("No", "Yes")),
      exercise = factor(exercise, levels=c("Never","Less than once a month","1-3 times a month", "1-4 times a week", "5 or more times a week"), ordered=TRUE),
      creat_14 = factor(creat_14, levels=c("sometimes", "often")),
      play_week = factor(play_week, levels=c("Less than 3 hours","3 or more hours")),
      play_wend = factor(play_wend, levels=c("Less than 3 hours","3 or more hours")),
      comp_week = factor(comp_week, levels=c("Not at all", "Less than 1 hour", "1-2 hours", "3 or more hours"), ordered=TRUE),
      comp_wend = factor(comp_wend, levels=c("Not at all", "Less than 1 hour", "1-2 hours", "3 or more hours"), ordered=TRUE),
      has_dep_diag = factor(has_dep_diag, levels=c("No ICD-10 diagnosis of depression", "Yes ICD-10 diagnosis of depression")),
      dep_band_13 = factor(dep_band_13, levels=c("<0.1%", "~0.5%", "~15%", "~50%", ">70%"), ordered=TRUE),
      dep_band_15 = factor(dep_band_15, levels=c("<0.1%", "~0.5%", "~15%", "~50%", ">70%"), ordered=TRUE),
      dep_score = factor(dep_score, levels=0:4, ordered=TRUE),
      dep_thoughts = if_else(dep_score==0, 0L, as.integer(dep_thoughts)), #assume if dep_score=0 then dep_thoughts=0
      dep_thoughts = factor(dep_thoughts, levels=0:5, ordered=TRUE),
      comp_bed = factor(case_when(comp_int_bed_16=="Yes" ~ "3 Yes, internet",
                                  comp_noint_bed_16=="Yes" ~ "2 Yes, no internet",
                                  comp_noint_bed_16=="No" | comp_int_bed_16=="No" ~ "1 No",
                                  is.na(comp_noint_bed_16) ~ NA_character_)), # convert comp_noint_bed_16 and comp_int_bed_16 to single factor

      bmi_16 = weight_16/((height_16/100)^2),

      # put highest 3 categories into one category
      dep_band_15_red = factor(fct_collapse(dep_band_15, '>15%'=c("~15%", "~50%", ">70%")), ordered=TRUE),

      # if any >3 hours then highest category, if only one >3 hours then middle category, any other
      social = factor(case_when(
        play_week=="3 or more hours" & play_wend == "3 or more hours" ~ "play3",
        play_week=="3 or more hours" | play_wend == "3 or more hours" ~ "play2",
        play_week=="Less than 3 hours" & play_wend == "Less than 3 hours" ~ "play1",
        TRUE ~ NA_character_),levels=c("play1", "play2", "play3"), ordered=TRUE
        ),

    )

  newdata
}


computer_use_001 <- function(data){
  library('tidyverse')

  newdata <- data %>%
    mutate(

      #combine first two categires into one
      comp_week_red = fct_collapse(comp_week, 'Less than 1 hour' = c('Not at all', 'Less than 1 hour')),
      comp_wend_red = fct_collapse(comp_wend, 'Less than 1 hour' = c('Not at all', 'Less than 1 hour')),

      #select highest computer use category for weekend and weekday as the dependent variable
      comp_comb = factor(c("Not at all", "Less than 1 hour", "1-2 hours", "3 or more hours")[pmax(as.integer(comp_week), as.integer(comp_wend), na.rm = TRUE)], levels=c("Not at all", "Less than 1 hour", "1-2 hours", "3 or more hours"), ordered=TRUE),
      comp_comb = factor(fct_collapse(comp_comb, 'Less than 1 hour' = c('Not at all', 'Less than 1 hour')), ordered=FALSE),

      #alternative
      comp_comb_alt = factor(case_when(comp_week=="3 or more hours" & comp_wend=="3 or more hours" ~ "comp3",
                                       comp_week=="3 or more hours" | comp_wend=="3 or more hours" ~ "comp2",
                                       comp_week!="3 or more hours" & comp_wend!="3 or more hours" ~ "comp1",
                                       TRUE ~ NA_character_), levels=c("comp1", "comp2", "comp"), ordered=TRUE),
    )

  newdata
}


depression_001 <- function(data){
  library('dplyr')
  mutate(data,
    depression = (has_dep_diag=="Yes ICD-10 diagnosis of depression")*1
  )
}

missing_001 <- function(data){
  library('dplyr')
  filter(data,
   !is.na(has_dep_diag)
  )
}

specify_model <- function(data){
  library('tidyverse')
  library('mice')

  #choose variables used in imputation model
  MIvars <- data %>%
    select(
      comp_comb,
      comp_bed,
      sex, iq, bmi_16,
      child_bull,
      exercise, creat_14,
      social,
      depression,
      dep_score, dep_thoughts,
      dep_band_15_red, dep_band_13,
      panic_score
    )

  set.seed(420) #set seed

  # impute m datasets taking the 10th interation for each
  # quickpred assumes zero correlation between two variables if |correlation| < 0.1
  mi.obj <- mice::mice(MIvars, m=10, maxit=10, pred=quickpred(MIvars))


  mi.fits <- with(
              mi.obj,
              glm(
                formula =
                  depression ~ comp_comb + dep_band_15_red + comp_bed + creat_14 + social + child_bull + exercise + sex ,
                family = binomial(),
                )
              )

  pooled <- summary(pool(mi.fits), conf.int = TRUE)

  or_1 <- exp(pooled['comp_comb1-2 hours','estimate'])
  or_2 <- exp(pooled['comp_comb3 or more hours','estimate'])
  p_1 <- pooled['comp_comb1-2 hours','p.value']
  p_2 <- pooled['comp_comb3 or more hours','p.value']
  ci_1 <- exp(pooled['comp_comb1-2 hours',c('2.5 %','97.5 %')])
  ci_2 <- exp(pooled['comp_comb3 or more hours',c('2.5 %','97.5 %')])
  aic <- mean(map_dbl(mi.fits$analyses, AIC))

  results <-
    list(
      or_1 = exp(pooled['comp_comb1-2 hours','estimate']),
      or_2 = exp(pooled['comp_comb3 or more hours','estimate']),
      p_1 = pooled['comp_comb1-2 hours','p.value'],
      p_2 = pooled['comp_comb3 or more hours','p.value'],
      ci_1 = c(exp(pooled['comp_comb1-2 hours',c('2.5 %')]), exp(pooled['comp_comb1-2 hours',c('97.5 %')])),
      ci_2 = c(exp(pooled['comp_comb3 or more hours',c('2.5 %')]), exp(pooled['comp_comb3 or more hours',c('97.5 %')])),
      aic = mean(map_dbl(mi.fits$analyses, AIC))
    )

  results

}


# run analysis

data1 <- load_data_001(here::here("data 20190510", "maps-synthetic-data-v1.1.csv"))
data2 <- transformations_001(data1)
data3 <- computer_use_001(data2)
data4 <- depression_001(data3)
data5 <- missing_001(data4)
results <- specify_model(data5)



MIvars <- data5 %>%
  select(
    comp_week, comp_wend,
    comp_bed,
    sex, iq, bmi_16,
    child_bull,
    exercise, creat_14,
    play_wend, play_week,
    depression,
    dep_score, dep_thoughts,
    dep_band_15, dep_band_13,
    panic_score
  )

map(MIvars, ~mean(!is.na(.)))
