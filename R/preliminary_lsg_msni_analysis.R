library(tidyverse)
library(srvyr)
library(supporteR)  

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
    mutate(strata = hh_woreda) 

# weights table
weight_table <- make_weight_table(input_df = df_main_clean_data, 
                                  input_pop = df_pop_data)
# add weights to data
df_main_clean_data_with_weights <- df_main_clean_data |>  
    left_join(weight_table, by = "strata")

support_data <- df_main_clean_data_with_weights |> select(uuid, hh_woreda, i.hoh_gender, strata,weights)

# lsg/msni data
df_lsg_msni <- read_csv("outputs/lsg_and_msni_oromia.csv") |> 
    mutate(across(matches("_lsg$|^msni$"), .fns = ~ case_when(.x %in% c("1") ~ "severity level 1",
                                                      .x %in% c("2") ~ "severity level 2",
                                                      .x %in% c("3") ~ "severity level 3",
                                                      .x %in% c("4") ~ "severity level 4",
                                                      .x %in% c("5") ~ "severity level 4+",
                                                      TRUE ~ as.character(.x)
                                                      )))

# dap
df_dap_lsg <- bind_rows(tibble::tribble(~variable,
                                        "fs_lsg",
                                        "cash_lsg",
                                        "wash_lsg",
                                        "health_lsg",
                                        "shelter_lsg",
                                        "edu_lsg",
                                        "prot_lsg",
                                        "msni",
                                        "i.fs_sl3_above",
                                        "i.cash_sl3_above",
                                        "i.wash_sl3_above",
                                        "i.health_sl3_above",
                                        "i.shelter_sl3_above",
                                        "i.edu_sl3_above",
                                        "i.prot_sl3_above",
                                        "i.msni_sl3_above",
)) |> 
    mutate(split = "all",
           subset_1 = "hh_woreda"
    ) |> 
    pivot_longer(cols = starts_with("subset"), names_to = "subset_no", values_to = "subset_1") |> 
    filter(!is.na(subset_1), !subset_1 %in% c("NA")) |> 
    select(-subset_no)


# main dataset ------------------------------------------------------------

df_combined_data <- df_main_clean_data_with_weights |> 
    left_join(df_lsg_msni)

# set up design object
ref_svy <- as_survey(.data = df_combined_data, strata = strata, weights = weights)

# analysis

df_lsg_analysis <- analysis_after_survey_creation(input_svy_obj = ref_svy,
                                                   input_dap = df_dap_lsg
                                                   ) |> 
    mutate(level = "Household")


# output analysis
write_csv(df_lsg_analysis, paste0("outputs/", butteR::date_file_prefix(), "_lsg_msni_analysis_eth_msna_oromia.csv"), na="")
write_csv(df_lsg_analysis, paste0("outputs/lsg_msni_analysis_eth_msna_oromia.csv"), na="")
