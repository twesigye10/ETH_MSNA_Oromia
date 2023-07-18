library(tidyverse)
library(srvyr)
library(supporteR)  

source("R/composite_indicators.R")
source("R/make_weights.R")

# packages to install incase
# devtools::install_github("zackarno/butteR")
# devtools::install_github("twesigye10/supporteR")

df_pop_data <- read_csv("inputs/msna_population_oromia.csv")

# clean data
data_path <- "inputs/clean_data_eth_msha_oromia.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000, sheet = "cleaned_main_data"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, sheet = "cleaned_main_data", col_types = c_types, na = "NA") |> 
    select(-starts_with("i.")) |> 
    create_composite_indicators() |> 
    mutate(strata = hh_woreda) |> 
    mutate(across(.cols = starts_with("i."), .fns = ~ ifelse((is.infinite(.x)|is.nan(.x)), NA, .)))

# weights table
weight_table <- make_weight_table(input_df = df_main_clean_data, 
                                  input_pop = df_pop_data)
# add weights to data
df_main_clean_data_with_weights <- df_main_clean_data |>  
    left_join(weight_table, by = "strata")

loop_support_data <- df_main_clean_data_with_weights |> select(uuid, hh_woreda, i.hoh_gender, strata,weights)

education_loop <- readxl::read_excel(path = data_path, sheet = "cleaned_education_loop", na = "NA")
df_education_data <- loop_support_data |> 
    inner_join(education_loop, by = c("uuid" = "_submission__uuid") ) 

health_loop <- readxl::read_excel(path = data_path, sheet = "cleaned_health_loop", na = "NA") |> 
    create_composite_indicators_health()
df_health_clean_data <- loop_support_data |> 
    inner_join(health_loop, by = c("uuid" = "_submission__uuid") ) 

# tool
loc_tool <- "inputs/ETH2301_MSHA_Oromia_tool.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

df_tool_data_support <- df_survey |> 
  select(type, name, label = `label::English`) |> 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) |> 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# dap
dap <- read_csv("inputs/r_dap_msha_eth.csv")


# main dataset ------------------------------------------------------------

# set up design object
ref_svy <- as_survey(.data = df_main_clean_data_with_weights, strata = strata, weights = weights)

# analysis

df_main_analysis <- analysis_after_survey_creation(input_svy_obj = ref_svy,
                                                   input_dap = dap |> 
                                                       filter(!variable %in% c("hh_living_district", 
                                                                               "when_settlement_est", 
                                                                               "idp_name",
                                                                               "wash_watertime1",
                                                                               "hh_shocks_affect11"))
                                                   ) |> 
    mutate(level = "Household")

# education loop ----------------------------------------------------------

# set up design object
ref_svy_education_loop <- as_survey(.data = df_education_data, strata = strata, weights = weights)
# analysis 
df_analysis_education_loop <- analysis_after_survey_creation(input_svy_obj = ref_svy_education_loop,
                                                   input_dap = dap |> 
                                                       filter(variable %in% c("edu_modality", "edu_attendance", "edu_attendance_remote", 
                                                                              "edu_non_access_reason", "edu_safe_environment", "edu_safe_environment_reasons",         
                                                                              "edu_learning_conditions", "edu_learning_conditions_reasons", "edu_pre_primary"))
                                                   ) |> 
    mutate(level = "Individual")

# health loop -------------------------------------------------------------

# set up design object
ref_svy_health_loop <- as_survey(.data = df_health_clean_data, strata = strata, weights = weights)
# analysis
df_analysis_health_loop <- analysis_after_survey_creation(input_svy_obj = ref_svy_health_loop,
                                                   input_dap = dap |> 
                                                       filter(variable %in% c("i.disability"))
                                                   ) |> 
    mutate(level = "Individual")


# roster ------------------------------------------------------------------

df_dap_roster <- bind_rows(tibble::tribble(~variable,
                                           "i.individual_age_cat")) |> 
    mutate(split = "all",
           subset_1 = "hh_woreda",
           subset_2 = "i.individual_gender"
           ) |> 
    pivot_longer(cols = starts_with("subset"), names_to = "subset_no", values_to = "subset_1") |> 
    filter(!is.na(subset_1), !subset_1 %in% c("NA")) |> 
    select(-subset_no)
    
df_main_pivot <- df_main_clean_data_with_weights |> 
    pivot_longer(cols = num_males_0to6:num_females_66plusyrs, names_to = "i.num_gender_age", values_to = "i.hh_size_based_on_gender_age")

df_roster_extract <- df_main_pivot |> 
    filter(i.hh_size_based_on_gender_age > 0) |> 
    uncount(i.hh_size_based_on_gender_age) |> 
    mutate(i.individual_gender = ifelse(str_detect(string = i.num_gender_age, pattern = "females"), "Female", "Male"),
           i.individual_age_cat = case_when(str_detect(string = i.num_gender_age, pattern = "0to6|7to3yrs|4to6") ~ "cat_0_6",
                                  str_detect(string = i.num_gender_age, pattern = "7to13|14to17") ~ "cat_7_17",
                                  str_detect(string = i.num_gender_age, pattern = "18to49|50to65") ~ "cat_18_65",
                                  str_detect(string = i.num_gender_age, pattern = "66plusyrs") ~ "cat_66+" ))

# set up design object
ref_svy_roster <- as_survey(.data = df_roster_extract, strata = strata, weights = weights)
# analysis
df_analysis_roster <- analysis_after_survey_creation(input_svy_obj = ref_svy_roster,
                                                          input_dap = df_dap_roster ) |> 
    mutate(level = "Individual")

# merge and format analysis ----------------------------------------------------------

combined_analysis <- bind_rows(df_main_analysis, df_analysis_education_loop, df_analysis_health_loop, df_analysis_roster)


integer_cols_i <- c("i.fcs", "i.rcsi", "i.hhs", "i.hh_composition_size",  "i.adults_permanent_job", "i.adults_temporary_job", "i.adults_casual_lobour",
                    "i.adults_own_bisuness", "i.children_permanent_job", "i.children_temporary_job", "i.children_casual_lobour",
                    "i.children_own_bisuness", "i.boys_early_marriege", "i.girls_early_marriege", "i.boys_work_outside_home",
                    "i.girls_work_outside_home")
integer_cols_int <- c("int.fcs", "int.rcsi", "int.hhs", "int.hh_composition_size", "int.adults_permanent_job", "int.adults_temporary_job", "int.adults_casual_lobour",
                      "int.adults_own_bisuness", "int.children_permanent_job", "int.children_temporary_job", "int.children_casual_lobour",
                      "int.children_own_bisuness", "int.boys_early_marriege", "int.girls_early_marriege", "int.boys_work_outside_home",
                      "int.girls_work_outside_home")

# formatting the analysis, adding question labels
full_analysis_long <- combined_analysis |> 
  mutate(variable = ifelse(is.na(variable) | variable %in% c(""), variable_val, variable),
         int.variable = ifelse(str_detect(string = variable, pattern = "^i\\."), str_replace(string = variable, pattern = "^i\\.", replacement = ""), variable)) |> 
  left_join(df_tool_data_support, by = c("int.variable" = "name")) |> 
  relocate(label, .after = variable) |> 
  mutate(variable = ifelse(variable %in% integer_cols_i, str_replace(string = variable, pattern = "i.", replacement = "int."), variable),
         select_type = ifelse(variable %in% integer_cols_int, "integer", select_type),
         label = ifelse(is.na(label), variable, label),
         # `mean/pct` = ifelse(select_type %in% c("integer") & !variable %in% integer_cols_i & !str_detect(string = variable, pattern = "^i\\."), `mean/pct`, `mean/pct`*100),
         `mean/pct` = round(`mean/pct`, digits = 3)) |> 
  mutate(variable = ifelse(variable %in% integer_cols_int, str_replace(string = variable, pattern = "int.", replacement = "i."), variable),
         label = ifelse(label %in% integer_cols_int, str_replace(string = label, pattern = "int.", replacement = "i."), label)) |> 
  select(`Question`= label, 
         variable, 
         `choices/options` = variable_val, 
         `Results(mean/percentage)` = `mean/pct`, 
         n_unweighted, 
         population, 
         subset_1_name, 
         subset_1_val, 
         select_type,
         level)

# output analysis
write_csv(full_analysis_long, paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_lf_eth_msna_oromia.csv"), na="")
write_csv(full_analysis_long, paste0("outputs/full_analysis_lf_eth_msna_oromia.csv"), na="")
write_csv(combined_analysis, paste0("outputs/combined_analysis_lf_eth_msna_oromia.csv"), na="")
