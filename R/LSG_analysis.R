library(tidyverse)
library(openxlsx)

# clean data
data_path <- "inputs/clean_data_eth_msha_oromia.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000, sheet = "cleaned_main_data"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, sheet = "cleaned_main_data", col_types = c_types, na = "NA") |> 
    select(-starts_with("i.")) |> 
    create_composite_indicators() |> 
    mutate(across(.cols = starts_with("i."), .fns = ~ ifelse((is.infinite(.x)|is.nan(.x)), NA, .)))

# Food security -----------------------------------------------------------

# calculated using: Fewsnet matrix (combining FCS, rCSI and HHS scores)



# Cash markets and livelihoods --------------------------------------------

# LCSI : yes, no_had_no_need, no_exhausted, not_applicable
# i.lost_job
# hh_tot_income
# hh_basic_needs

df_lsg_cash <- df_main_clean_data |> 
    unite("int.lcsi", livh_stress_lcsi_1:livh_emerg_lcsi_3, sep = ":", remove = FALSE) |> 
    unite("int.lcsi_stress", livh_stress_lcsi_1:livh_stress_lcsi_4, sep = ":", remove = FALSE) |> 
    unite("int.lcsi_crisis", livh_crisis_lcsi_1:livh_crisis_lcsi_3, sep = ":", remove = FALSE) |> 
    unite("int.lcsi_emergency", livh_emerg_lcsi_1:livh_emerg_lcsi_3, sep = ":", remove = FALSE) |>
    rowwise() |> 
    mutate(across(any_of(c("income_casual_labour", "income_own_production", "income_govt_benefits", "income_rent", "income_remittances",
                           "income_loans_family_friends", "income_loans_community_members", "income_hum_assistance")), .fns = ~ifelse(.x > 0, 1, 0), .names = "int.{.col}")) |> 
    mutate(int.income_props_seasonal = sum(c_across(c(int.income_casual_labour:int.income_remittances)), na.rm = T),
           int.income_props_receiving = sum(c_across(c(int.income_loans_family_friends:int.income_hum_assistance)), na.rm = T)) |> 
    ungroup() |> 
    mutate(crit_score_cash = case_when(str_detect(string = int.lcsi, pattern = "yes|no_exhausted", negate = TRUE) & 
                                           i.lost_job %in% c("no") &
                                           if_any(.cols = c(income_salaried_work, income_business), .fns = ~ . > 0) &
                                           hh_basic_needs %in% c("all") ~ "1",
                                       str_detect(string = int.lcsi_stress, pattern = "yes|no_exhausted") &
                                           str_detect(string = int.lcsi_crisis, pattern = "yes|no_exhausted", negate = TRUE) &
                                           str_detect(string = int.lcsi_emergency, pattern = "yes|no_exhausted", negate = TRUE) &
                                           int.income_props_seasonal > 1 &
                                           hh_basic_needs %in% c("almost_all") ~ "2",
                                       str_detect(string = int.lcsi_crisis, pattern = "yes|no_exhausted") &
                                           str_detect(string = int.lcsi_emergency, pattern = "yes|no_exhausted", negate = TRUE) &
                                           i.lost_job %in% c("yes") &
                                           int.income_props_seasonal == 1 &
                                           hh_basic_needs %in% c("some", "many") ~ "3",
                                       str_detect(string = int.lcsi_emergency, pattern = "yes|no_exhausted") &
                                           int.income_props_receiving >= 1 &
                                           hh_basic_needs %in% c("none", "few") ~ "4",
                                       ))

