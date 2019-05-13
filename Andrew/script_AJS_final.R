# “is computer use during weekdays and weekends at 16 years old associated with depression at 18 years old?”

library(tidyverse)
library(ordinal)

load_data <- function(path) {
  read_csv("maps-synthetic-data-v1.1.csv")
}

tidy_data <- function(data_name) {
    # Data tidying and wrangling ####
    # replace NAs with "No" for variable comp_int_bed_16 
    # and turn into factor
    data_name[is.na(data_name$comp_int_bed_16), ]$comp_int_bed_16 <- "No"
    data_name$comp_int_bed_16 <- factor(data_name$comp_int_bed_16)

    # recode variable anx_band_15 to numbers
    # include < 0.1 recoded as 0.1
    # recode as factors
    data_name <- data_name %>%
      mutate(anx_band_15 = recode_factor(anx_band_15, 
                                         "0" = "0",
                                         "<0.1%" = "0.1",
                                         "~0.5%" = ".5", 
                                         "~3%" = "3", 
                                         "~15%" = "15", 
                                         "~50%" = "50")) 
    
    # same as above for dep_band_15 measure
    data_name <- data_name %>%
      mutate(dep_band_15 = recode_factor(dep_band_15, 
                                         "0" = "0",
                                         "<0.1%" = "0.1",
                                         "~0.5%" = ".5", 
                                         "~15%" = "15", 
                                         "~50%" = "50", 
                                         ">70%" = "70")) 
    
    # recode the following as ordered factors
    data_name <- data_name %>%
      mutate(comp_wend = recode_factor(comp_wend, 
                                         "Not at all" = "0",
                                         "Less than 1 hour" = "1",
                                         "1-2 hours" = "2", 
                                         "3 or more hours" = "3")) 
    
    data_name <- data_name %>%
      mutate(comp_week = recode_factor(comp_week, 
                                       "Not at all" = "0",
                                       "Less than 1 hour" = "1",
                                       "1-2 hours" = "2", 
                                       "3 or more hours" = "3")) 
    
    data_name <- data_name %>%
      mutate(draw_wend = recode_factor(draw_wend, 
                                       "Not at all" = "0",
                                       "Less than 1 hour" = "1",
                                       "1 or more hours" = "2")) 
    
    data_name <- data_name %>%
      mutate(exercise = recode_factor(exercise, 
                                       "Never" = "0",
                                       "Less than once a month" = "1",
                                       "1-3 times a month" = "2",
                                       "1-4 times a week" = "3",
                                       "5 or more times a week" = "4")) 
}

model_data <- function(data_name) {
    # Build ordinal model ####
    # Examine predictors on depression diagnosis (has_dep_diag)
    # Computer with internet in bedroom controling for depression at age 15
    # and adding computer use at weekends, exercise and weekend drawing
    data_name$has_dep_diag  <- as.ordered(data_name$has_dep_diag )
    model <- clm(has_dep_diag  ~ dep_band_15 + comp_wend + comp_week + 
                   comp_int_bed_16 + exercise + draw_wend, 
                  data = data_name)
    
    summary_model <- summary(model)
    ci <- confint(model)
    model <- tidy(model)
    beta_coefs <- filter(model, coefficient_type == "beta")$estimate
    results <- list(summary_model, exp(cbind(OR = beta_coefs, ci)))
}

# mains script calls to functions ####
my_data <- load_data("maps-synthetic-data-v1.1.csv")
tidied_data <- tidy_data(my_data)
results <- model_data(tidied_data)

# Depression band 50 at age find has odds ratio of 5.14 and band 75
# has odds ration of 2.89
# computer use at weekend 3 or more hours has odds ratio of 1.87
# Previous measure of depression at 15 band 50 seems the biggest predictor
