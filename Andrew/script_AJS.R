# “is computer use during weekdays and weekends at 16 years old associated with depression at 18 years old?”

library(tidyverse)
library(visdat)
library(lme4)
library(ordinal)
library(arm)
library(lmSupport)
library(car)

maps_synthetic_data <- read_csv("maps-synthetic-data-v1.1.csv")

# Data tidying and wrangling ####
# replace NAs with "No" for variables comp_int_bed_16 and comp_noint_bed_16
# CHECK THE ABOVE ASSUMPTION
# and turn these two variables into factors
maps_synthetic_data[is.na(maps_synthetic_data$comp_int_bed_16), ]$comp_int_bed_16 <- "No"
maps_synthetic_data[is.na(maps_synthetic_data$comp_noint_bed_16), ]$comp_noint_bed_16 <- "No"

maps_synthetic_data$comp_int_bed_16 <- factor(maps_synthetic_data$comp_int_bed_16)
maps_synthetic_data$comp_noint_bed_16 <- factor(maps_synthetic_data$comp_noint_bed_16)

# turn the following into factors
maps_synthetic_data$sex <- as.factor(maps_synthetic_data$sex)
maps_synthetic_data$pat_pres <- as.factor(maps_synthetic_data$pat_pres)

# recode variable anx_band_15 to numbers
# include < 0.1 recoded as 0.1
# recode as factors
maps_synthetic_data <- maps_synthetic_data %>%
  mutate(anx_band_15 = recode_factor(anx_band_15, 
                                     "0" = "0",
                                     "<0.1%" = "0.1",
                                     "~0.5%" = ".5", 
                                     "~3%" = "3", 
                                     "~15%" = "15", 
                                     "~50%" = "50")) 

# same as above for dep_band_15 measure
maps_synthetic_data <- maps_synthetic_data %>%
  mutate(dep_band_15 = recode_factor(dep_band_15, 
                                     "0" = "0",
                                     "<0.1%" = "0.1",
                                     "~0.5%" = ".5", 
                                     "~15%" = "15", 
                                     "~50%" = "50", 
                                     ">70%" = "70")) 

# recode the following as ordered factors
maps_synthetic_data <- maps_synthetic_data %>%
  mutate(comp_week = recode_factor(comp_week, 
                                     "Not at all" = "0",
                                     "Less than 1 hour" = "1",
                                     "1-2 hours" = "2", 
                                     "3 or more hours" = "3")) 

maps_synthetic_data <- maps_synthetic_data %>%
  mutate(comp_wend = recode_factor(comp_wend, 
                                   "Not at all" = "0",
                                   "Less than 1 hour" = "1",
                                   "1-2 hours" = "2", 
                                   "3 or more hours" = "3")) 

maps_synthetic_data <- maps_synthetic_data %>%
  mutate(draw_wend = recode_factor(draw_wend, 
                                   "Not at all" = "0",
                                   "Less than 1 hour" = "1",
                                   "1 or more hours" = "2")) 

maps_synthetic_data <- maps_synthetic_data %>%
  mutate(draw_week = recode_factor(draw_week, 
                                   "Not at all" = "0",
                                   "Less than 1 hour" = "1",
                                   "1 or more hours" = "2")) 

maps_synthetic_data <- maps_synthetic_data %>%
  mutate(exercise = recode_factor(exercise, 
                                   "Never" = "0",
                                   "Less than once a month" = "1",
                                   "1-3 times a month" = "2",
                                   "1-4 times a week" = "3",
                                   "5 or more times a week" = "4")) 

# Visualise and summarise ####
maps_synthetic_data %>%
  filter(!is.na(dep_score)) %>%
  ggplot(aes(x = comp_int_bed_16, y = dep_score, colour = anx_band_15)) +
  geom_jitter(width = .2, alpha = .2) +
  labs(title = NULL, 
       x = "Computer with Internet in Bedroom at Age 16", 
       y = "Child's depression score on CIS-R")

maps_synthetic_data %>%
  filter(!is.na(dep_score)) %>%
  ggplot(aes(x = comp_noint_bed_16, y = dep_score, colour = anx_band_15 )) +
  geom_jitter(width = .2, alpha = .2) +
  labs(title = NULL, 
       x = "Computer with No Internet in Bedroom at Age 16", 
       y = "Child's depression score on CIS-R")

# Depression at 15 scores and anxiety 15 scores look pretty similar
maps_synthetic_data %>% 
  ggplot(aes(x = anx_band_15, y = dep_band_15)) + 
  geom_jitter(alpha = .05) +
  ggtitle("Looks like a clear +ve relationship between \ndepression at 15 and anxiety at 15")

# Computer with internet in bedroom controling for anxiety at age 15
filtered_data <- maps_synthetic_data 
filtered_data$dep_score <- as.ordered(filtered_data$dep_score)
model1 <- clm(dep_score ~ anx_band_15 + comp_int_bed_16, 
                 data = filtered_data)
summary(model1)

# CLM models ####
# Examine predictors on depression score (dep_score)
# Computer with internet in bedroom controling for anxiety at age 15 and depression at age 15
filtered_data <- maps_synthetic_data 
filtered_data$dep_score <- as.ordered(filtered_data$dep_score)
model1 <- clm(dep_score ~ anx_band_15 + dep_band_15 + comp_int_bed_16, 
              data = filtered_data)
summary(model1)

# Looks like singular fit issues so drop anxiety
model1a <- clm(dep_score ~ dep_band_15 + comp_int_bed_16, 
              data = filtered_data)
summary(model1a)

# Model with weekday hours added
model2 <- clm(dep_score ~ dep_band_15 + comp_week + comp_int_bed_16, 
              data = filtered_data)
summary(model2)

# Model with weekend hours added
model3 <- clm(dep_score ~ dep_band_15 + comp_wend + comp_int_bed_16, 
              data = filtered_data)
summary(model3)

# Model with both weekday and weekend hours added
model4 <- clm(dep_score ~ dep_band_15 + comp_week + comp_wend + comp_int_bed_16, 
              data = filtered_data)
summary(model4)

model5 <- clm(dep_score ~ agg_score + sex + iq + draw_wend + exercise + 
                pat_pres + dep_band_15 + comp_wend + comp_int_bed_16, 
              data = filtered_data)
summary(model5)


# Check BIC values of models
BIC(model1)
BIC(model1a)
BIC(model2)
BIC(model3) # This one looks best with BIC 5170.02 although AIC of model4 is lowest
BIC(model4)

# Examine predictor on depression diagnosis
filtered_data <- maps_synthetic_data 
filtered_data$has_dep_diag <- as.ordered(filtered_data$has_dep_diag)     
model1 <- clm(has_dep_diag ~ anx_band_15 + dep_band_15 + comp_int_bed_16, 
              data = filtered_data)
summary(model1)

# Looks like singular fit issues so drop anxiety
model1a <- clm(has_dep_diag ~ dep_band_15 + comp_int_bed_16, 
               data = filtered_data)
summary(model1a)

# Model with weekday hours added
model2 <- clm(has_dep_diag ~ dep_band_15 + comp_week + comp_int_bed_16, 
              data = filtered_data)
summary(model2)

# Model with weekend hours added
model3 <- clm(has_dep_diag ~ dep_band_15 + comp_wend + comp_int_bed_16, 
              data = filtered_data)
summary(model3)

# Model with both weekday and weekend hours added
model4 <- clm(has_dep_diag ~ dep_band_15 + comp_week + comp_wend + comp_int_bed_16, 
              data = filtered_data)
summary(model4)

model5 <- clm(has_dep_diag ~ agg_score + sex + iq + draw_wend + exercise + 
              pat_pres + dep_band_15 + comp_wend + comp_int_bed_16, 
              data = filtered_data)
summary(model5)

# Check BIC values of models - but it looks like we need to switch to AIC for the write-up
BIC(model1)
BIC(model1a)
BIC(model2)
BIC(model3) # This one looks best with BIC 1777.581 although AIC of model4 is lowest
BIC(model4)

# Logit regression models ####
# The above are cumulative link models - also try logit regression model
# try adding a few more obvious predictors
glm_data <- maps_synthetic_data %>%
  filter(!is.na(has_dep_diag)) %>%
  mutate(has_dep_diag = as.integer(dplyr::recode(has_dep_diag, 
                                          "No ICD-10 diagnosis of depression" = "0", 
                                          "Yes ICD-10 diagnosis of depression" = "1")))

model4 <- glm(has_dep_diag ~ agg_score + sex + iq + draw_wend + exercise + 
              pat_pres + dep_band_15 + comp_wend + comp_int_bed_16, 
              family = binomial,
              data = glm_data)
summary(model4)
modelEffectSizes(model4) 
vif(model4)
AIC(model4)

# calculate odds ratios of coefficients of predictors ####
epiDisplay::logistic.display(model4)

# Build Bayesian model to calculate DIC - doesn't work - issues with NAs
trials <- rep(1, nrow(glm_data))
model <- CARBayes::S.glm(has_dep_diag ~ agg_score + sex + iq + draw_wend + exercise + 
                        pat_pres + dep_band_15 + comp_wend + comp_int_bed_16, 
                        trials = trials,
                        family = "binomial", data = glm_data)


# So far on the basis of people having a computer with internet in their bedrooms,
# or the amount of time spent on computers during the week or at the weekend while 
# controlling for both depression at 15 has any impact on depression diagnosis at 18 or
# depression score at 18
# But lots more to investigate and I'm sure the models I'm builing are quite simplistic!



### scratch below ####

glm_data <- maps_synthetic_data %>%
  mutate(has_dep_diag = as.integer(dplyr::recode(has_dep_diag, 
                                          "No ICD-10 diagnosis of depression" = "0", 
                                          "Yes ICD-10 diagnosis of depression" = "1")))

modelx <- glm(has_dep_diag ~ dep_band_15 + comp_wend + comp_int_bed_16, 
              data = glm_data)
summary(modelx)

# Try k-fold cross validation to examine model ####
# but I'm not sure how to interpret
glm_data_filt <- maps_synthetic_data %>%
  filter(!is.na(has_dep_diag)) %>%
  mutate(has_dep_diag = as.ordered(dplyr::recode(has_dep_diag, 
                                                 "No ICD-10 diagnosis of depression" = "0", 
                                                 "Yes ICD-10 diagnosis of depression" = "1")))

# split df in half
length <- nrow(glm_data_filt)
training <- glm_data_filt[1:length/2, ]
testing <- glm_data_filt[(length/2 + 1):(length - 1),]

ctrl <- caret::trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <- caret::train(has_dep_diag ~ comp_int_bed_16, 
                 data = training, method = "glm", family = "binomial",
                 trControl = ctrl)

pred = predict(mod_fit, newdata = testing)
confusionMatrix(data = pred, testing$has_dep_diag)
