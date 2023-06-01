# Applying the cleaning log to clean the data
library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)

# source("R/composite_indicators.R")

# Read data and checking log 

df_cleaning_log <- read_csv("inputs/combined_checks_eth_msna_oromia.csv", col_types = cols(sheet = "c", index = "i")) |> 
  filter(!adjust_log %in% c("delete_log"), reviewed %in% c("1")) |>
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log),
         value = ifelse(is.na(value) & str_detect(string = issue_id, pattern = "logic_c_"), "blank", value),
         value = ifelse(type %in% c("remove_survey"), "blank", value),
         name = ifelse(is.na(name) & type %in% c("remove_survey"), "point_number", name)
  ) |> 
  filter(!is.na(value), !is.na(uuid)) |>
  mutate(value = ifelse(value %in% c("blank"), NA, value),
         # sheet = NA,
         # index = NA,
         relevant = NA) |>
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)

# raw data
loc_data <- "inputs/ETH2301_MSNA_Oromia_data.xlsx"

cols_to_escape <- c("index", "start", "end", "today", "starttime",	"endtime", "_submission_time", "_submission__submission_time")

data_nms <- names(readxl::read_excel(path = loc_data, n_max = 2000))
c_types <- case_when(str_detect(string = data_nms, pattern = "_other$") ~ "text",
                        TRUE ~ "guess")

df_raw_data <- readxl::read_excel(path = loc_data, col_types = c_types) |> 
  mutate(across(.cols = -c(any_of(cols_to_escape)), 
                .fns = ~ifelse(str_detect(string = ., 
                                          pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .)))

# tool
loc_tool <- "inputs/ETH2301_MSNA_Oromia_tool.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices") |> 
  mutate(label = `label::English`)


# main dataset ------------------------------------------------------------

df_cleaning_log_main <-  df_cleaning_log |> 
  filter(is.na(sheet))

df_cleaning_step <- supporteR::cleaning_support(input_df_raw_data = df_raw_data,
                                              input_df_survey = df_survey,
                                              input_df_choices = df_choices,
                                              input_df_cleaning_log = df_cleaning_log_main)

df_cleaned_data <- df_cleaning_step |> 
  mutate(across(.cols = -c(any_of(cols_to_escape), matches("_age$|^age_|uuid")),
                .fns = ~ifelse(str_detect(string = ., pattern = "^[9]{3,9}$"), "NA", .)))

# Add composite indicators at this stage ----------------------------------

df_main_with_composites <- create_composite_indicators(input_df = df_cleaned_data) |> 
  select(-starts_with("int."))

# # deletion log ------------------------------------------------------------
# 
# df_deletion_log <- df_cleaning_log |> 
#   filter(type %in% c("remove_survey")) |> 
#   group_by(uuid) |> 
#   filter(row_number() == 1) |> 
#   ungroup()

# write final datasets out -----------------------------------------------

list_of_raw_datasets <- list("Raw_main" = df_raw_data |> select(-starts_with("int.")))

openxlsx::write.xlsx(x = list_of_raw_datasets,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_raw_data_eth_msna_oromia.xlsx"))

list_of_clean_datasets <- list("cleaned_data" = df_main_with_composites
)

openxlsx::write.xlsx(x = list_of_clean_datasets,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_clean_data_eth_msna_oromia.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")

# openxlsx::write.xlsx(x = list_of_clean_datasets,
#                      file = paste0("inputs/clean_data_eth_msna_oromia.xlsx"), 
#                      overwrite = TRUE, keepNA = TRUE, na.string = "NA")
