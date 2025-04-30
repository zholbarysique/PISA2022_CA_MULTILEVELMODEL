rm(list=ls())
setwd("/Users/assemzhulbarissova/Desktop/PISA2022/!PISA_2022_MATH_CENTRAL_ASIA")

library(lme4)
library(tidyverse)
library(dplyr)
library(haven)
########################## IMPORTING DATA ##########################
WORKING_DIRECTORY = "/Users/assemzhulbarissova/Desktop/PISA2022/!PISA_2022_MATH_CENTRAL_ASIA/"

kaz <- read_sav(paste0(WORKING_DIRECTORY, "PISA2022_KAZ_merged.sav"))
uzb <- read_sav(paste0(WORKING_DIRECTORY, "PISA2022_UZB_merged.sav"))

## delete NA columns
kaz <- kaz[, colSums(!is.na(kaz)) > 0]
uzb <- uzb[, colSums(!is.na(uzb)) > 0]


########################## PREPARING VARIABLES ##########################
## recode gender
#' ST004D01T: 1 - female, 2 - male
kaz$gender_female <-ifelse(kaz$ST004D01T==1,1,0)
uzb$gender_female <-ifelse(uzb$ST004D01T==1,1,0)

## recode study_program
#' ISCEDP: 244 and 344 are schools, 354, 453 are vocational
kaz$studyprog_vocational <- ifelse(kaz$ISCEDP==244, 0,
                                   ifelse(kaz$ISCEDP==344, 0,
                                          ifelse(kaz$ISCEDP==354, 1, NA)))
uzb$studyprog_vocational <- ifelse(uzb$ISCEDP==244, 0,
                                   ifelse(uzb$ISCEDP==344, 0,
                                          ifelse(uzb$ISCEDP==453, 1, NA)))
table(kaz$studyprog_vocational, kaz$ISCEDP)
table(uzb$studyprog_vocational, uzb$ISCEDP)

## recode school location
#' SC001Q01TA: 1-rural
kaz$location_rural <- ifelse(kaz$SC001Q01TA == 1, 1,
                             ifelse(kaz$SC001Q01TA == 2, 0,
                                    ifelse(kaz$SC001Q01TA == 3, 0,
                                           ifelse(kaz$SC001Q01TA == 4, 0,
                                                  ifelse(kaz$SC001Q01TA == 5, 0,
                                                         ifelse(kaz$SC001Q01TA == 6, 0, NA))))))
uzb$location_rural <- ifelse(uzb$SC001Q01TA == 1, 1,
                             ifelse(uzb$SC001Q01TA == 2, 0,
                                    ifelse(uzb$SC001Q01TA == 3, 0,
                                           ifelse(uzb$SC001Q01TA == 4, 0,
                                                  ifelse(uzb$SC001Q01TA == 5, 0,
                                                         ifelse(uzb$SC001Q01TA == 6, 0, NA))))))

##### grand mean centering of variables #####
library(intsvy)
## grand mean centering of ESCS
kaz_ESCS_weighted_mean <-pisa.mean(variable="ESCS",data=kaz)[1,2]
uzb_ESCS_weighted_mean <-pisa.mean(variable="ESCS",data=uzb)[1,2]

kaz$ESCS_grand_mean <- with(kaz, ifelse(is.na(ESCS), NA, ESCS - kaz_ESCS_weighted_mean))[]
uzb$ESCS_grand_mean <- with(uzb, ifelse(is.na(ESCS), NA, ESCS - uzb_ESCS_weighted_mean))[]


## grand mean centering of ANXMAT
kaz_ANXMAT_weighted_mean <-pisa.mean(variable="ANXMAT",data=kaz)[1,2]
uzb_ANXMAT_weighted_mean <-pisa.mean(variable="ANXMAT",data=uzb)[1,2]

kaz$ANXMAT_grand_mean <- with(kaz, ifelse(is.na(ANXMAT), NA, ANXMAT - kaz_ANXMAT_weighted_mean))[]
uzb$ANXMAT_grand_mean <- with(uzb, ifelse(is.na(ANXMAT), NA, ANXMAT - uzb_ANXMAT_weighted_mean))[]

## grand mean centering of MATHEF21
kaz_MATHEF21_weighted_mean <-pisa.mean(variable="MATHEF21",data=kaz)[1,2]
uzb_MATHEF21_weighted_mean <-pisa.mean(variable="MATHEF21",data=uzb)[1,2]

kaz$MATHEF21_grand_mean <- with(kaz, ifelse(is.na(MATHEF21), NA, MATHEF21 - kaz_MATHEF21_weighted_mean))[]
uzb$MATHEF21_grand_mean <- with(uzb, ifelse(is.na(MATHEF21), NA, MATHEF21 - uzb_MATHEF21_weighted_mean))[]

## grand mean centering of MATHPERS
kaz_MATHPERS_weighted_mean <-pisa.mean(variable="MATHPERS",data=kaz)[1,2]
uzb_MATHPERS_weighted_mean <-pisa.mean(variable="MATHPERS",data=uzb)[1,2]

kaz$MATHPERS_grand_mean <- with(kaz, ifelse(is.na(MATHPERS), NA, MATHPERS - kaz_MATHPERS_weighted_mean))[]
uzb$MATHPERS_grand_mean <- with(uzb, ifelse(is.na(MATHPERS), NA, MATHPERS - uzb_MATHPERS_weighted_mean))[]


## grand mean centering of TEACHSUP
kaz_TEACHSUP_weighted_mean <-pisa.mean(variable="TEACHSUP",data=kaz)[1,2]
uzb_TEACHSUP_weighted_mean <-pisa.mean(variable="TEACHSUP",data=uzb)[1,2]

kaz$TEACHSUP_grand_mean <- with(kaz, ifelse(is.na(TEACHSUP), NA, TEACHSUP - kaz_TEACHSUP_weighted_mean))[]
uzb$TEACHSUP_grand_mean <- with(uzb, ifelse(is.na(TEACHSUP), NA, TEACHSUP - uzb_TEACHSUP_weighted_mean))[]


## grand mean centering of FAMSUP
kaz_FAMSUP_weighted_mean <-pisa.mean(variable="FAMSUP",data=kaz)[1,2]
uzb_FAMSUP_weighted_mean <-pisa.mean(variable="FAMSUP",data=uzb)[1,2]

kaz$FAMSUP_grand_mean <- with(kaz, ifelse(is.na(FAMSUP), NA, FAMSUP - kaz_FAMSUP_weighted_mean))[]
uzb$FAMSUP_grand_mean <- with(uzb, ifelse(is.na(FAMSUP), NA, FAMSUP - uzb_FAMSUP_weighted_mean))[]

## grand mean centering of READ PVs
kaz_READ_weighted_mean <- pisa.mean.pv(pvlabel = paste0("PV",1:10,"READ"), data = kaz)[1,2]
uzb_READ_weighted_mean <- pisa.mean.pv(pvlabel = paste0("PV",1:10,"READ"), data = uzb)[1,2]

kaz$READ_student_mean <- rowMeans(kaz[paste0("PV", 1:10, "READ")], na.rm = TRUE)
uzb$READ_student_mean <- rowMeans(uzb[paste0("PV", 1:10, "READ")], na.rm = TRUE)

kaz$READ_centered <- kaz$READ_student_mean - kaz_READ_weighted_mean
uzb$READ_centered <- uzb$READ_student_mean - uzb_READ_weighted_mean

## public-private schools
kaz$school_private <- ifelse(kaz$SC013Q01TA == 1, 0,
                             ifelse(kaz$SC013Q01TA == 2, 1, NA))
uzb$school_private <- ifelse(uzb$SC013Q01TA == 1, 0,
                             ifelse(uzb$SC013Q01TA == 2, 1, NA))

########################## PICKING VARIABLES ##########################

###### MATH data frame #####
math_vars <- paste0("PV", 1:10, "MATH")
#scie_vars <- paste0("PV", 1:10, "SCIE")
#read_vars <- paste0("PV", 1:10, "READ")

rep_vars <- paste0("W_FSTURWT", 1:80)
schid_ver <- "CNTSCHID"

expl_vars <- c("gender_female", "GRADE", "ESCS_grand_mean",
               "ANXMAT", "ANXMAT_grand_mean", 
               "MATHEF21", 'MATHEF21_grand_mean', 
               'MATHMOT', "READ_centered",
               "MATHPERS", 'MATHPERS_grand_mean', 
               'school_private',
               'MCLSIZE', 'EDUSHORT')
weight <- c('W_FSTUWT', 'W_SCHGRNRABWT')

kaz_data <- kaz%>%select(schid_ver, expl_vars, weight, math_vars, rep_vars)
kaz_data <- kaz_data[order(kaz_data$CNTSCHID), ]
kaz_data <- na.omit(kaz_data)
str(kaz_data)
kaz_girls_data <- kaz_data[kaz_data$gender_female==1,]
kaz_boys_data <- kaz_data[kaz_data$gender_female==0,]

uzb_data <- uzb%>%select(schid_ver, expl_vars, weight, math_vars, rep_vars)

uzb_data <- uzb_data[order(uzb_data$CNTSCHID), ]
uzb_data <- na.omit(uzb_data)
str(uzb_data)
uzb_girls_data <- uzb_data[uzb_data$gender_female==1,]
uzb_boys_data <- uzb_data[uzb_data$gender_female==0,]


##########################  COMPARE MEAN VALUES BY GENDER 
## for checking the mean values by groups
# Function to calculate Mean Difference, Cohen's d, Standard Error of Difference, and z-statistic
calculate_stats <- function(varname,mean_0, mean_1, se_0, se_1, sd_0, sd_1, n_0, n_1) {
  # Calculate Mean Difference
  mean_diff <- mean_1 - mean_0
  
  # Calculate Pooled Standard Deviation
  sd_pooled <- sqrt(((n_0 - 1) * sd_0^2 + (n_1 - 1) * sd_1^2) / (n_0 + n_1 - 2))
  
  # Calculate Cohen's d
  cohen_d <- mean_diff / sd_pooled
  
  # Calculate Standard Error of the Difference
  
  se_diff <- sqrt((sd_0^2 / n_0) + (sd_1^2 / n_1))
  
  # Calculate z-statistic
  z_stat <- mean_diff / se_diff
  
  # Return the results as a list
  return(data.frame(
    varname = varname,
    mean_female = mean_1,
    sd_female = sd_1,
    n_female = n_1,
    mean_male = mean_0,
    sd_male = sd_0,
    n_male = n_0,
    mean_diff = mean_diff,
    cohen_d = cohen_d,
    se_diff = se_diff,
    z_stat = z_stat
  ))
}
library(intsvy)
kaz_anxmat<-pisa.mean(variable="ANXMAT", by = "gender_female", data = kaz_data)
uzb_anxmat<-pisa.mean(variable="ANXMAT", by = "gender_female", data = uzb_data)
kaz_result <- calculate_stats(varname = "ANXMAT", 
                              mean_0 = kaz_anxmat$Mean[1], mean_1 = kaz_anxmat$Mean[2],
                              se_0 = kaz_anxmat$s.e.[1], se_1 = kaz_anxmat$s.e.[2],
                              sd_0 = kaz_anxmat$SD[1], sd_1 = kaz_anxmat$SD[2],
                              n_0 = kaz_anxmat$Freq[1], n_1 = kaz_anxmat$Freq[2])
uzb_result <- calculate_stats(varname = "ANXMAT", 
                              mean_0 = uzb_anxmat$Mean[1], mean_1 = uzb_anxmat$Mean[2],
                              se_0 = uzb_anxmat$s.e.[1], se_1 = uzb_anxmat$s.e.[2],
                              sd_0 = uzb_anxmat$SD[1], sd_1 = uzb_anxmat$SD[2],
                              n_0 = uzb_anxmat$Freq[1], n_1 = uzb_anxmat$Freq[2])

kaz_mathef<-pisa.mean(variable="MATHEF21", by = "gender_female", data = kaz_data)
uzb_mathef<-pisa.mean(variable="MATHEF21", by = "gender_female", data = uzb_data)
kaz_result[2,] <- calculate_stats(varname = "MATHEF21", 
                                  mean_0 = kaz_mathef$Mean[1], mean_1 = kaz_mathef$Mean[2],
                                  se_0 = kaz_mathef$s.e.[1], se_1 = kaz_mathef$s.e.[2],
                                  sd_0 = kaz_mathef$SD[1], sd_1 = kaz_mathef$SD[2],
                                  n_0 = kaz_mathef$Freq[1], n_1 = kaz_mathef$Freq[2])
uzb_result[2,] <- calculate_stats(varname = "MATHEF21", 
                                  mean_0 = uzb_mathef$Mean[1], mean_1 = uzb_mathef$Mean[2],
                                  se_0 = uzb_mathef$s.e.[1], se_1 = uzb_mathef$s.e.[2],
                                  sd_0 = uzb_mathef$SD[1], sd_1 = uzb_mathef$SD[2],
                                  n_0 = uzb_mathef$Freq[1], n_1 = uzb_mathef$Freq[2])

kaz_mathpers<-pisa.mean(variable="MATHPERS", by = "gender_female", data = kaz_data)
uzb_mathpers<-pisa.mean(variable="MATHPERS", by = "gender_female", data = uzb_data)
kaz_result[3,] <- calculate_stats(varname = "MATHPERS", 
                                  mean_0 = kaz_mathpers$Mean[1], mean_1 = kaz_mathpers$Mean[2],
                                  se_0 = kaz_mathpers$s.e.[1], se_1 = kaz_mathpers$s.e.[2],
                                  sd_0 = kaz_mathpers$SD[1], sd_1 = kaz_mathpers$SD[2],
                                  n_0 = kaz_mathpers$Freq[1], n_1 = kaz_mathpers$Freq[2])
uzb_result[3,] <- calculate_stats(varname = "MATHPERS", 
                                  mean_0 = uzb_mathpers$Mean[1], mean_1 = uzb_mathpers$Mean[2],
                                  se_0 = uzb_mathpers$s.e.[1], se_1 = uzb_mathpers$s.e.[2],
                                  sd_0 = uzb_mathpers$SD[1], sd_1 = uzb_mathpers$SD[2],
                                  n_0 = uzb_mathpers$Freq[1], n_1 = uzb_mathpers$Freq[2])

kaz_mathpers<-pisa.mean(variable="MATHMOT", by = "gender_female", data = kaz_data)
uzb_mathpers<-pisa.mean(variable="MATHMOT", by = "gender_female", data = uzb_data)
kaz_result[4,] <- calculate_stats(varname = "MATHMOT", 
                                  mean_0 = kaz_mathpers$Mean[1], mean_1 = kaz_mathpers$Mean[2],
                                  se_0 = kaz_mathpers$s.e.[1], se_1 = kaz_mathpers$s.e.[2],
                                  sd_0 = kaz_mathpers$SD[1], sd_1 = kaz_mathpers$SD[2],
                                  n_0 = kaz_mathpers$Freq[1], n_1 = kaz_mathpers$Freq[2])
uzb_result[4,] <- calculate_stats(varname = "MATHMOT", 
                                  mean_0 = uzb_mathpers$Mean[1], mean_1 = uzb_mathpers$Mean[2],
                                  se_0 = uzb_mathpers$s.e.[1], se_1 = uzb_mathpers$s.e.[2],
                                  sd_0 = uzb_mathpers$SD[1], sd_1 = uzb_mathpers$SD[2],
                                  n_0 = uzb_mathpers$Freq[1], n_1 = uzb_mathpers$Freq[2])

writexl::write_xlsx(kaz_result, "PISA2022_KAZ_MEAN_DIFF_27042025.xlsx")
writexl::write_xlsx(uzb_result, "PISA2022_UZB_MEAN_DIFF_27042025.xlsx")


########################## SCALING SCH WEIGHTS TO ADJUST FOR NONRESPONSE ##########################
## https://unstats.un.org/unsd/demographic/meetings/egm/sampling_1203/docs/no_5.pdf
library(dplyr)

##### KAZAKHSTAN ####
## first, i count students per school in the original dataset (before NA were removed)
original_counts <- kaz %>%
  group_by(CNTSCHID) %>%
  summarize(
    n_orig = n(), 
    .groups = 'drop'
  )

## then, i count students per school in the cleaned dataset (after NA were removed)
response_counts <- kaz_data %>%
  group_by(CNTSCHID) %>%
  summarize(
    n_total = n(),
    .groups = 'drop'
  )

## third, i join counts and compute nonresponse adjustment factor
response_adjustment <- left_join(original_counts, response_counts, by = "CNTSCHID") %>%
  mutate(
    n_total = ifelse(is.na(n_total), 0, n_total),  # handle missing matches
    adj_factor = ifelse(n_total > 0, n_orig / n_total, 1)  # default to 1 if no students remained
  )

## finally, i apply adjustment to W_SCHGRNRABWT in kaz_data
kaz_data <- kaz_data %>%
  left_join(response_adjustment %>% select(CNTSCHID, adj_factor), by = "CNTSCHID") %>%
  mutate(
    W_SCHGRNRABWT_rescaled = W_SCHGRNRABWT * adj_factor
  )


##### UZBEKISTAN ####
## first, i count students per school in the original dataset (before NA were removed)
original_counts <- uzb %>%
  group_by(CNTSCHID) %>%
  summarize(
    n_orig = n(), 
    .groups = 'drop'
  )

## then, i count students per school in the cleaned dataset (after NA were removed)
response_counts <- uzb_data %>%
  group_by(CNTSCHID) %>%
  summarize(
    n_total = n(),
    .groups = 'drop'
  )

## third, i join counts and compute nonresponse adjustment factor
response_adjustment <- left_join(original_counts, response_counts, by = "CNTSCHID") %>%
  mutate(
    n_total = ifelse(is.na(n_total), 0, n_total),  # handle missing matches
    adj_factor = ifelse(n_total > 0, n_orig / n_total, 1)  # default to 1 if no students remained
  )

## finally, i apply adjustment to W_SCHGRNRABWT in uzb_data
uzb_data <- uzb_data %>%
  left_join(response_adjustment %>% select(CNTSCHID, adj_factor), by = "CNTSCHID") %>%
  mutate(
    W_SCHGRNRABWT_rescaled = W_SCHGRNRABWT * adj_factor
  )

###### STUDENT WEIGHT READJUSTMENT FOR NONRESPONSE
### Kazakhstan ####
## first, get original total student weights per school (before cleaning)
stu_weights_orig <- kaz %>%
  group_by(CNTSCHID) %>%
  summarize(
    total_stu_weight_orig = sum(W_FSTUWT, na.rm = TRUE),
    .groups = 'drop'
  )

## second, get total student weights after cleaning
stu_weights_cleaned <- kaz_data %>%
  group_by(CNTSCHID) %>%
  summarize(
    total_stu_weight_cleaned = sum(W_FSTUWT, na.rm = TRUE),
    .groups = 'drop'
  )

## third, compute adjustment factor
stu_response_adjustment <- left_join(stu_weights_orig, stu_weights_cleaned, by = "CNTSCHID") %>%
  mutate(
    total_stu_weight_cleaned = ifelse(is.na(total_stu_weight_cleaned), 0, total_stu_weight_cleaned),
    stu_adj_factor = ifelse(total_stu_weight_cleaned > 0,
                            total_stu_weight_orig / total_stu_weight_cleaned, 1)
  )

## fourth, apply adjustment
kaz_data <- kaz_data %>%
  left_join(stu_response_adjustment %>% select(CNTSCHID, stu_adj_factor), by = "CNTSCHID") %>%
  mutate(
    W_FSTUWT_rescaled = W_FSTUWT * stu_adj_factor
  )

### Uzbekistan ####
## first, get original total student weights per school (before cleaning)
stu_weights_orig <- uzb %>%
  group_by(CNTSCHID) %>%
  summarize(
    total_stu_weight_orig = sum(W_FSTUWT, na.rm = TRUE),
    .groups = 'drop'
  )

## second, get total student weights after cleaning
stu_weights_cleaned <- uzb_data %>%
  group_by(CNTSCHID) %>%
  summarize(
    total_stu_weight_cleaned = sum(W_FSTUWT, na.rm = TRUE),
    .groups = 'drop'
  )

## third, compute adjustment factor
stu_response_adjustment <- left_join(stu_weights_orig, stu_weights_cleaned, by = "CNTSCHID") %>%
  mutate(
    total_stu_weight_cleaned = ifelse(is.na(total_stu_weight_cleaned), 0, total_stu_weight_cleaned),
    stu_adj_factor = ifelse(total_stu_weight_cleaned > 0,
                            total_stu_weight_orig / total_stu_weight_cleaned, 1)
  )

## fourth, apply adjustment
uzb_data <- uzb_data %>%
  left_join(stu_response_adjustment %>% select(CNTSCHID, stu_adj_factor), by = "CNTSCHID") %>%
  mutate(
    W_FSTUWT_rescaled = W_FSTUWT * stu_adj_factor
  )

########################## REGRESSION MODEL FUNCTION ##########################
library(lme4)
library(dplyr)
library(WeMix)

library(future.apply)
plan(multisession, workers = 6)  # or multicore if Linux/Mac

my_regression <- function(data, y_names, x_formula, cluster_var, stu_weights_var, sch_weights_var, rep_weights) {
  
  M <- length(y_names)  # number of plausible values
  R <- length(rep_weights)  # number of replicate weights
  
  stu_weights <- stu_weights_var
  sch_weights <- sch_weights_var
  
  # Prepare formulas once
  formula_list <- lapply(y_names, function(pv) {
    as.formula(paste(pv, "~", x_formula, "+ (1 |", cluster_var, ")"))
  })
  names(formula_list) <- y_names
  
  ## Main estimates
  estimates_main <- list()
  var_main <- list()

  for (pv in y_names) {
    formula <- formula_list[[pv]]
    
    model <- mix(
      formula = formula,
      data = data,
      weights = c(stu_weights, sch_weights)
    )
    
    if (!is.null(model)) {
      estimates_main[[pv]] <- coef(model)
      var_main[[pv]] <- diag(model[["vars"]])
    }
  }
  
  if (length(estimates_main) == 0) stop("All models failed")
  
  est_matrix_main <- do.call(rbind, estimates_main)
  pooled_mean <- colMeans(est_matrix_main)
  
  W <- colMeans(do.call(rbind, var_main))  # Within-imputation variance
  B <- colSums((est_matrix_main - pooled_mean)^2) / (M - 1)  # Between-imputation variance
  
  ## Replication part using for loop
  replicate_estimates <- matrix(NA, nrow = R, ncol = length(pooled_mean))
  
  for (i in seq_along(rep_weights)) {
    rw <- rep_weights[[i]]
    est_matrix_rep <- matrix(NA, nrow = M, ncol = length(pooled_mean))
    
    for (m in seq_along(y_names)) {
      formula <- formula_list[[m]]
      
      model <- tryCatch(
        mix(
          formula = formula,
          data = data,
          weights = c(rw, sch_weights)
        ),
        error = function(e) NULL
      )
      
      if (!is.null(model)) {
        est_matrix_rep[m, ] <- coef(model)
      }
    }
    replicate_estimates[i, ] <- colMeans(est_matrix_rep, na.rm = TRUE)
  }
  
  ## Replication variance BRR
  replicate_diff <- sweep(replicate_estimates, 2, pooled_mean)
  V_brr <- 0.05 * colSums(replicate_diff^2)
  
  ## Total variance
  total_variance <- B + V_brr
  pooled_se <- sqrt(total_variance)
  
  ## t-values, degrees of freedom, p-values
  t_values <- pooled_mean / pooled_se
  
  df_raw <- ((1 + 1/M)^2 * B^2) / ((B^2)/(M - 1) + (V_brr^2)/R)
  df <- ifelse(B < 1e-10, M - 1, df_raw)
  df <- pmax(df, 5)
  
  p_values <- 2 * pt(abs(t_values), df, lower.tail = FALSE)
  
  ## Assemble results
  results <- data.frame(
    Variable = names(pooled_mean),
    Estimate = pooled_mean,
    SE = pooled_se,
    t_value = t_values,
    df = df,
    p_value = p_values
  )
  
  return(results)
}




########################## MODEL ##########################

######### null model #########
y_names <- paste0("PV", 1:10, "MATH")
cluster_var = "CNTSCHID"
stu_weights_var = "W_FSTUWT"
sch_weights_var = "W_SCHGRNRABWT_rescaled"
rep_weights = paste0("W_FSTURWT", 1:80)

x_formula <- "1"

kz_d = kaz_data ## kaz dataset
uz_d = uzb_data ## uzb dataset

kaz_m0_results <- my_regression(
  data = kz_d,
  y_names = y_names,
  x_formula = x_formula,
  cluster_var = cluster_var,
  stu_weights_var = stu_weights_var,
  sch_weights_var = sch_weights_var,
  rep_weights = rep_weights
)
print(kaz_m0_results)

uzb_m0_results <- my_regression(
  data = uz_d,
  y_names = y_names,
  x_formula = x_formula,
  cluster_var = cluster_var,
  stu_weights_var = stu_weights_var,
  sch_weights_var = sch_weights_var,
  rep_weights = rep_weights
)
print(uzb_m0_results)

############## model 1 ###########
x_formula <- c("gender_female  + ESCS_grand_mean + GRADE + READ_centered +
                ANXMAT_grand_mean + MATHEF21_grand_mean + MATHMOT + 
                MATHPERS_grand_mean")
kaz_m1_results <- my_regression(
  data = kz_d,
  y_names = y_names,
  x_formula = x_formula,
  cluster_var = cluster_var,
  stu_weights_var = stu_weights_var,
  sch_weights_var = sch_weights_var,
  rep_weights = rep_weights
)
print(kaz_m1_results)

uzb_m1_results <- my_regression(
  data = uz_d,
  y_names = y_names,
  x_formula = x_formula,
  cluster_var = cluster_var,
  stu_weights_var = stu_weights_var,
  sch_weights_var = sch_weights_var,
  rep_weights = rep_weights
)
print(uzb_m1_results)

## model with interaction
x_formula <- c("ESCS_grand_mean + GRADE + 
                gender_female*READ_centered + gender_female*ANXMAT_grand_mean + 
                gender_female*MATHEF21_grand_mean + gender_female*MATHMOT + 
               gender_female*MATHPERS_grand_mean")
kaz_m1.1_results <- my_regression(
  data = kz_d,
  y_names = y_names,
  x_formula = x_formula,
  cluster_var = cluster_var,
  stu_weights_var = stu_weights_var,
  sch_weights_var = sch_weights_var,
  rep_weights = rep_weights
)
print(kaz_m1.1_results)

uzb_m1.1_results <- my_regression(
  data = uz_d,
  y_names = y_names,
  x_formula = x_formula,
  cluster_var = cluster_var,
  stu_weights_var = stu_weights_var,
  sch_weights_var = sch_weights_var,
  rep_weights = rep_weights
)
print(uzb_m1.1_results)


############## model 2 ###########
x_formula <- c("gender_female + ESCS_grand_mean + GRADE + READ_centered + ANXMAT_grand_mean + 
               MATHEF21_grand_mean + MATHMOT + MATHPERS_grand_mean + 
              school_private + MCLSIZE + EDUSHORT")
kaz_m2_results <- my_regression(
  data = kz_d,
  y_names = y_names,
  x_formula = x_formula,
  cluster_var = cluster_var,
  stu_weights_var = stu_weights_var,
  sch_weights_var = sch_weights_var,
  rep_weights = rep_weights
)
print(kaz_m2_results)

uzb_m2_results <- my_regression(
  data = uz_d,
  y_names = y_names,
  x_formula = x_formula,
  cluster_var = cluster_var,
  stu_weights_var = stu_weights_var,
  sch_weights_var = sch_weights_var,
  rep_weights = rep_weights
)
print(uzb_m2_results)



x_formula <- c("ESCS_grand_mean + GRADE + 
                gender_female*READ_centered + gender_female*ANXMAT_grand_mean + 
                gender_female*MATHEF21_grand_mean + gender_female*MATHMOT + 
                gender_female*MATHPERS_grand_mean + 
                school_private +
                MCLSIZE + EDUSHORT")
kaz_m2.1_results <- my_regression(
  data = kz_d,
  y_names = y_names,
  x_formula = x_formula,
  cluster_var = cluster_var,
  stu_weights_var = stu_weights_var,
  sch_weights_var = sch_weights_var,
  rep_weights = rep_weights
)
print(kaz_m2.1_results)

uzb_m2.1_results <- my_regression(
  data = uz_d,
  y_names = y_names,
  x_formula = x_formula,
  cluster_var = cluster_var,
  stu_weights_var = stu_weights_var,
  sch_weights_var = sch_weights_var,
  rep_weights = rep_weights
)
print(uzb_m2.1_results)



## final model
x_formula <- c("GRADE + gender_female*READ_centered + 
                gender_female*ANXMAT_grand_mean + 
                MATHEF21_grand_mean + 
               school_private + EDUSHORT")
kaz_m3_results <- my_regression(
  data = kz_d,
  y_names = y_names,
  x_formula = x_formula,
  cluster_var = cluster_var,
  stu_weights_var = stu_weights_var,
  sch_weights_var = sch_weights_var,
  rep_weights = rep_weights
)
print(kaz_m3_results)


x_formula <- c("gender_female + READ_centered + ANXMAT_grand_mean + 
               MATHEF21_grand_mean + MATHMOT + MCLSIZE")
uzb_m3_results <- my_regression(
  data = uz_d,
  y_names = y_names,
  x_formula = x_formula,
  cluster_var = cluster_var,
  stu_weights_var = stu_weights_var,
  sch_weights_var = sch_weights_var,
  rep_weights = rep_weights
)
print(uzb_m3_results)




library(writexl)
# Create a named list of your data frames
kaz_results_list <- list(
  "Model_0"   = kaz_m0_results,
  "Model_1"   = kaz_m1_results,
  "Model_1.1" = kaz_m1.1_results,
  "Model_2"   = kaz_m2_results,
  "Model_2.1" = kaz_m2.1_results,
  "Model_3" = kaz_m3_results
)
uzb_results_list <- list(
  "Model_0"   = uzb_m0_results,
  "Model_1"   = uzb_m1_results,
  "Model_1.1" = uzb_m1.1_results,
  "Model_2"   = uzb_m2_results,
  "Model_2.1" = uzb_m2.1_results,
  "Model_3" = uzb_m3_results
)

# Write to Excel file
write_xlsx(kaz_results_list, path = "PISA2022_KAZ_model_results_29042025.xlsx")
write_xlsx(uzb_results_list, path = "PISA2022_UZB_model_results_29042025.xlsx")




