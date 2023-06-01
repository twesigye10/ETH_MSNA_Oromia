library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)

# read data and tool ----------------------------------------------------------
# data
data_path <- "inputs/ETH2301_MSNA_Oromia_data.xlsx"
    
df_tool_data <- readxl::read_excel(data_path) |>  
    mutate(start = as_datetime(start),
           end = as_datetime(end)) |> 
    checks_add_extra_cols(input_enumerator_id_col = "enumerator_id",
                          input_location_col = "hh_kebele") |> 
    rowwise() |> 
    mutate( 
        int.hh_number = sum(c_across(num_males_0to6:num_females_66plusyrs))
    ) |>
    ungroup()

# loops
# loop_educ
loop_educ <- readxl::read_excel(path = data_path, sheet = "grp_education_loop")

df_raw_data_loop_educ <- df_tool_data |> 
    select(-`_index`) |> 
    inner_join(loop_educ, by = c("_uuid" = "_submission__uuid") )

# health_loop
loop_health <- readxl::read_excel(path = data_path, sheet = "grp_health_loop")

df_raw_data_loop_health <- df_tool_data |> 
    select(-`_index`) |> 
    inner_join(loop_health, by = c("_uuid" = "_submission__uuid") )

# tool
loc_tool <- "inputs/ETH2301_MSNA_Oromia_tool.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

# checks ------------------------------------------------------------------

checks_output <- list()

# testing data ------------------------------------------------------------

df_testing_data <- df_tool_data |> 
    filter(i.check.start_date < as_date("2023-05-19")) |> 
    mutate(i.check.type = "remove_survey",
           i.check.name = "",
           i.check.current_value = "",
           i.check.value = "",
           i.check.issue_id = "logic_c_testing_data",
           i.check.issue = "testing_data",
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    dplyr::select(starts_with("i.check.")) |> 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_testing_data")

# Time checks -------------------------------------------------------------

# Time interval for the survey
min_time_of_survey <- 30
max_time_of_survey <- 120

df_c_survey_time <-  supporteR::check_survey_time(input_tool_data = df_tool_data, 
                                                  input_enumerator_id_col = "enumerator_id",
                                                  input_location_col = "hh_kebele",
                                                  input_min_time = min_time_of_survey, 
                                                  input_max_time = max_time_of_survey)

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_survey_time")

# check duplicate uuids ---------------------------------------------------

df_c_duplicate_uuid <-  supporteR::checks_duplicate_uuids(input_tool_data = df_tool_data)

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_duplicate_uuid")

# outliers ----------------------------------------------------------------

df_c_outliers <- supporteR::check_outliers_cleaninginspector(input_tool_data = df_tool_data,
                                                             input_enumerator_id_col = "enumerator_id",
                                                             input_location_col = "hh_kebele")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_outliers")

# other_specify -----------------------------------------------------------

df_others_data <- supporteR::extract_other_specify_data(input_tool_data = df_tool_data, 
                                                        input_enumerator_id_col = "enumerator_id",
                                                        input_location_col = "hh_kebele",
                                                        input_survey = df_survey,  
                                                        input_choices = df_choices)

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_others_data")


# logical checks ----------------------------------------------------------

# log 999
cols_with_integer_values <- df_survey |> filter(type %in% c("integer")) |> pull(name)

df_999_data <- purrr::map_dfr(.x = cols_with_integer_values, 
                              .f = ~ {df_tool_data |> 
                                      dplyr::filter(str_detect(string = !!sym(.x), pattern = "^-[9]{2,4}$|^[9]{2,4}$")) |> 
                                      dplyr::mutate(i.check.type = "change_response",
                                                    i.check.name = .x,
                                                    i.check.current_value = as.character(!!sym(.x)),
                                                    i.check.value = "NA",
                                                    i.check.issue_id = "logic_c_handle_999",
                                                    i.check.issue = "remove 999 added during data collection",
                                                    i.check.other_text = "",
                                                    i.check.checked_by = "GG",
                                                    i.check.checked_date = as_date(today()),
                                                    i.check.comment = "",
                                                    i.check.reviewed = "1",
                                                    i.check.adjust_log = "",
                                                    i.check.so_sm_choices = "") |>
                                      dplyr::select(starts_with("i.check."))}) |> 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_999_data")

# more loop data than main dataset
df_count_hh_number_less_1 <- df_raw_data_loop_educ |>
    group_by(`_uuid`) |>
    mutate(int.loop_count = n()) |>
    filter(row_number() == 1) |>
    filter(int.loop_count > int.hh_number) |>
    ungroup() |>
    mutate(i.check.type = "remove_loop_entry",
           i.check.name = "",
           int.loop_count_difference = int.loop_count - int.hh_number,
           i.check.current_value = as.character(int.hh_number),
           i.check.value = "",
           i.check.issue_id = "logic_c_count_hh_number_less_educ_loop",
           i.check.issue = glue("int.loop_count : {int.loop_count}, hh_count not equal to loop composition"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "grp_education_loop",
           i.check.index = int.hh_number) |>
    slice(rep(1:n(), each = 2)) |> 
    group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |> 
    mutate(rank = row_number(),
           i.check.name = ifelse(rank == 2, "children_size", i.check.name),
           i.check.value = ifelse(rank == 2, as.character(int.hh_number), i.check.value),
           i.check.sheet = ifelse(rank == 2, NA_character_, i.check.sheet),
           i.check.index = ifelse(rank == 2, NA_character_, i.check.index)
    ) |>
    dplyr::select(starts_with("i.check.")) |>
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_count_hh_number_less_1")

# health loop count greater than given hh_number 
df_count_hh_number_less_2 <- df_raw_data_loop_health |>
    group_by(`_uuid`) |>
    mutate(int.loop_count = n()) |>
    filter(row_number() == 1) |>
    filter(int.loop_count > int.hh_number) |>
    ungroup() |>
    mutate(i.check.type = "remove_survey",
           i.check.name = "",
           int.loop_count_difference = int.loop_count - int.hh_number,
           i.check.current_value = as.character(int.hh_number),
           i.check.value = "",
           i.check.issue_id = "logic_c_count_hh_number_less_health_loop",
           i.check.issue = glue("int.loop_count : {int.loop_count}, hh_count not equal to loop composition"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") |>
    dplyr::select(starts_with("i.check.")) |>
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_count_hh_number_less_2")

# same value of fcs components
df_fd_consumption_score_same <- df_tool_data |>  
    filter(if_all(c(fs_fcs_cerealgrainroottuber, fs_fcs_beansnuts, fs_fcs_vegetableleave, fs_fcs_fruit, fs_fcs_condiment, 
                    fs_fcs_meatfishegg, fs_fcs_dairy, fs_fcs_sugar, fs_fcs_fat), ~ fs_fcs_cerealgrainroottuber == .x))  |> 
    mutate(i.check.type = "change_response",
           i.check.name = "fs_fcs_cerealgrainroottuber",
           i.check.current_value = as.character(fs_fcs_cerealgrainroottuber),
           i.check.value = "NA",
           i.check.issue_id = "logic_c_fd_consumption_score_same",
           i.check.issue = glue("fs_fcs_cerealgrainroottuber :{fs_fcs_cerealgrainroottuber}, fs_fcs_beansnuts :{fs_fcs_beansnuts}, fs_fcs_vegetableleave :{fs_fcs_vegetableleave}, fs_fcs_fruit :{fs_fcs_fruit}, fs_fcs_condiment :{fs_fcs_condiment}, fs_fcs_meatfishegg :{fs_fcs_meatfishegg}, fs_fcs_dairy :{fs_fcs_dairy}, fs_fcs_sugar :{fs_fcs_sugar}, fs_fcs_fat :{fs_fcs_fat}"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    slice(rep(1:n(), each = 9)) |>  
    group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
    mutate(rank = row_number(),
           i.check.name = case_when(rank == 1 ~ "fs_fcs_cerealgrainroottuber", 
                                    rank == 2 ~ "fs_fcs_beansnuts",
                                    rank == 3 ~ "fs_fcs_vegetableleave", 
                                    rank == 4 ~ "fs_fcs_fruit", 
                                    rank == 5 ~ "fs_fcs_condiment", 
                                    rank == 6 ~ "fs_fcs_meatfishegg", 
                                    rank == 7 ~ "fs_fcs_dairy", 
                                    rank == 8 ~ "fs_fcs_sugar", 
                                    TRUE ~ "fs_fcs_fat"),
           i.check.current_value = case_when(rank == 1 ~ as.character(fs_fcs_cerealgrainroottuber),
                                             rank == 2 ~ as.character(fs_fcs_beansnuts),
                                             rank == 3 ~ as.character(fs_fcs_vegetableleave), 
                                             rank == 4 ~ as.character(fs_fcs_fruit), 
                                             rank == 5 ~ as.character(fs_fcs_condiment), 
                                             rank == 6 ~ as.character(fs_fcs_meatfishegg), 
                                             rank == 7 ~ as.character(fs_fcs_dairy), 
                                             rank == 8 ~ as.character(fs_fcs_sugar), 
                                             TRUE ~ as.character(fs_fcs_fat))
    ) |> 
    dplyr::select(starts_with("i.check.")) |> 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_fd_consumption_score_same")



# combined  checks --------------------------------------------------------

df_combined_checks <- bind_rows(checks_output)

# output the log
write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_eth_msna_oromia.csv"), na = "")

