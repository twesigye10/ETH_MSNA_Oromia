library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)
library(cluster)


source("R/composite_indicators.R")
source("R/enumerator_behavior.R")
# read data and tool ----------------------------------------------------------
# data
data_path <- "inputs/ETH2301_MSHA_Oromia_data.xlsx"
    
df_tool_data <- readxl::read_excel(data_path) |>  
    mutate(start = as_datetime(start),
           end = as_datetime(end),
           enumerator_id = ifelse(is.na(enumerator_id), enum_code, enumerator_id)) |> 
    checks_add_extra_cols(input_enumerator_id_col = "enumerator_id",
                          input_location_col = "hh_kebele") |> 
    rowwise() |> 
    mutate( 
        i.hh_number = sum(c_across(num_males_0to6:num_females_66plusyrs), na.rm = T),
        i.tot_income_percent = sum(c_across(income_salaried_work:income_other_income), na.rm = T),
        i.tot_expenditure = sum(c_across(food_items:expenditure_other_frequent), na.rm = T),
    ) |>
    ungroup() |> 
    create_composite_indicators() |> 
    rename(int.hh_number = i.hh_number,
           int.tot_income_percent = i.tot_income_percent,
           int.tot_expenditure = i.tot_expenditure)

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
loc_tool <- "inputs/ETH2301_MSHA_Oromia_tool.xlsx"

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
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_testing_data")


# fix enumerator_id data --------------------------------------------------

df_logic_c_enumerator_id_harmonization <- df_tool_data |> 
    filter(is.na(enumerator_id), i.check.start_date > as_date("2023-05-18")) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "enumerator_id",
           i.check.current_value = "NA",
           i.check.value = enum_code,
           i.check.issue_id = "logic_c_enumerator_id_harmonization",
           i.check.issue = "enumerator_id_harmonization",
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_enumerator_id_harmonization")

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

# log 999
cols_with_text_values <- df_survey |> filter(type %in% c("text"), name %in% colnames(df_tool_data)) |> pull(name)

df_999_data_other <- purrr::map_dfr(.x = cols_with_text_values, 
                              .f = ~ {df_tool_data |> 
                                      dplyr::filter(str_detect(string = !!sym(.x), pattern = "^-[9]{2,4}$|^[9]{2,4}$")) |> 
                                      dplyr::mutate(i.check.type = "change_response",
                                                    i.check.name = .x,
                                                    i.check.current_value = as.character(!!sym(.x)),
                                                    i.check.value = "NA",
                                                    i.check.issue_id = "logic_c_handle_999_other",
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

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_999_data_other")

# logic_c_sell_livestock_but_not_owning_any_livestock
df_logic_c_sell_livestock_but_not_owning_any_livestock <- df_tool_data |> 
    filter(livh_stress_lcsi_4 %in%  c("yes"),
           hh_own_livestock %in% c( "no")) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "livh_stress_lcsi_4",
           i.check.current_value = livh_stress_lcsi_4,
           i.check.value = "",
           i.check.issue_id = "logic_c_sell_livestock_but_not_owning_any_livestock",
           i.check.issue = glue("livh_stress_lcsi_4: {livh_stress_lcsi_4} but hh_own_livestock: {hh_own_livestock}"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "")  |> 
    batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_sell_livestock_but_not_owning_any_livestock")

# logic_c_sell_female_animal_but_not_owning_any_livestock
df_logic_c_sell_female_animal_but_not_owning_any_livestock <- df_tool_data |> 
    filter(livh_emerg_lcsi_2 %in%  c("yes"),
           hh_own_livestock %in% c( "no")) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "livh_emerg_lcsi_2",
           i.check.current_value = livh_emerg_lcsi_2,
           i.check.value = "",
           i.check.issue_id = "logic_c_sell_female_animal_but_not_owning_any_livestock",
           i.check.issue = glue("livh_emerg_lcsi_2: {livh_emerg_lcsi_2} but hh_own_livestock: {hh_own_livestock}"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "")  |> 
    batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_sell_female_animal_but_not_owning_any_livestock")

# logic_c_children_withdrawn_from_school_but_not_report_dropout
df_logic_c_children_withdrawn_from_school_but_not_report_dropout <- df_tool_data |> 
    filter(livh_crisis_lcsi_3 %in%  c("yes"),
           edu_dropout_due_drought %in% c( "no")) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "livh_crisis_lcsi_3",
           i.check.current_value = livh_crisis_lcsi_3,
           i.check.value = "",
           i.check.issue_id = "logic_c_children_withdrawn_from_school_but_not_report_dropout",
           i.check.issue = glue("livh_crisis_lcsi_3: {livh_crisis_lcsi_3} but edu_dropout_due_drought: {edu_dropout_due_drought}"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "")  |> 
    batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_children_withdrawn_from_school_but_not_report_dropout")

# more loop data than main dataset
df_count_hh_number_less_1 <- df_raw_data_loop_educ |>
    group_by(`_uuid`) |>
    mutate(int.loop_count = n(), int.index = max(`_index`)) |>
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
           i.check.index = int.index) |>
    slice(rep(1:n(), each = 2)) |> 
    group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |> 
    mutate(rank = row_number(),
           i.check.type = ifelse(rank == 2, "change_response", i.check.type),
           i.check.name = ifelse(rank == 2, "children_size", i.check.name),
           i.check.value = ifelse(rank == 2, as.character(int.hh_number), i.check.value),
           i.check.sheet = ifelse(rank == 2, NA_character_, i.check.sheet),
           i.check.index = ifelse(rank == 2, NA_character_, i.check.index)
    ) |>
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

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
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

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
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_fd_consumption_score_same")

# same value of rcsi components and more than 0 times of rcsi. suspect that enumerators may have just filled
df_fd_rcsi_same <- df_tool_data |>  
    filter(if_all(c(rCSILessQlty, rCSIMealSize, rCSIMealAdult, rCSIMealNb,
                    rCSIBorrow), ~ rCSILessQlty == .x & rCSILessQlty > 0))  |> 
    mutate(i.check.type = "change_response",
           i.check.name = "rCSILessQlty",
           i.check.current_value = as.character(rCSILessQlty),
           i.check.value = "NA",
           i.check.issue_id = "logic_c_fd_rcsi_same",
           i.check.issue = glue("rCSILessQlty :{rCSILessQlty}, rCSIMealSize :{rCSIMealSize}, rCSIMealAdult :{rCSIMealAdult}, rCSIMealNb :{rCSIMealNb}, rCSIBorrow :{rCSIBorrow}"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    slice(rep(1:n(), each = 5)) |>  
    group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
    mutate(rank = row_number(),
           i.check.name = case_when(rank == 1 ~ "rCSILessQlty", 
                                    rank == 2 ~ "rCSIMealSize",
                                    rank == 3 ~ "rCSIMealAdult", 
                                    rank == 4 ~ "rCSIMealNb", 
                                    TRUE ~ "rCSIBorrow"),
           i.check.current_value = case_when(rank == 1 ~ as.character(rCSILessQlty),
                                             rank == 2 ~ as.character(rCSIMealSize),
                                             rank == 3 ~ as.character(rCSIMealAdult), 
                                             rank == 4 ~ as.character(rCSIMealNb), 
                                             TRUE ~ as.character(rCSIBorrow))
    ) |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_fd_rcsi_same")

# logic_c_hh_report_debt_repayment_but_doesnot_have_debt
df_logic_c_hh_report_debt_repayment_but_doesnot_have_debt <- df_tool_data |> 
    filter(hh_debt %in% c("no"), repaying_debts > 0) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "hh_debt",
           i.check.current_value = hh_debt,
           i.check.value = "",
           i.check.issue_id = "logic_c_hh_report_debt_repayment_but_doesnot_have_debt",
           i.check.issue = glue("hh_debt: {hh_debt}, but repaying_debts: {repaying_debts}"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "")  |> 
    batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_report_debt_repayment_but_doesnot_have_debt")

# logic_c_hh_report_debt_repayment_but_doesnot_have_debt_2
df_logic_c_hh_report_debt_repayment_but_doesnot_have_debt_2 <- df_tool_data |> 
    filter(hh_debt %in% c("no"), debt_repayement > 0) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "hh_debt",
           i.check.current_value = hh_debt,
           i.check.value = "",
           i.check.issue_id = "logic_c_hh_report_debt_repayment_but_doesnot_have_debt_2",
           i.check.issue = glue("hh_debt: {hh_debt}, but debt_repayement: {debt_repayement}"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "")  |> 
    batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_report_debt_repayment_but_doesnot_have_debt_2")

# logic_c_experienced_security_restrictions_but_boys_safety_concerns_none
df_logic_c_experienced_security_restrictions_but_boys_safety_concerns_none <- df_tool_data |> 
    filter(prot_saftey %in%  c("yes"),
           str_detect(string = boys_security_concerns, pattern = "none")) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "prot_saftey",
           i.check.current_value = prot_saftey,
           i.check.value = "",
           i.check.issue_id = "logic_c_experienced_security_restrictions_but_boys_safety_concerns_none",
           i.check.issue = glue("prot_saftey: {prot_saftey} but boys_security_concerns: {boys_security_concerns}"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "")  |> 
    batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_experienced_security_restrictions_but_boys_safety_concerns_none")

# logic_c_experienced_security_restrictions_but_girls_safety_concerns_none
df_logic_c_experienced_security_restrictions_but_girls_safety_concerns_none <- df_tool_data |> 
    filter(prot_saftey %in%  c("yes"),
           str_detect(string = girls_security_concerns, pattern = "none")) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "prot_saftey",
           i.check.current_value = prot_saftey,
           i.check.value = "",
           i.check.issue_id = "logic_c_experienced_security_restrictions_but_girls_security_concerns_none",
           i.check.issue = glue("prot_saftey: {prot_saftey} but girls_security_concerns: {girls_security_concerns}"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "")  |> 
    batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_experienced_security_restrictions_but_girls_safety_concerns_none")

# logic_c_experienced_security_restrictions_but_women_security_concerns_none
df_logic_c_experienced_security_restrictions_but_women_security_concerns_none <- df_tool_data |> 
    filter(prot_saftey %in%  c("yes"),
           str_detect(string = women_security_concerns, pattern = "none")) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "prot_saftey",
           i.check.current_value = prot_saftey,
           i.check.value = "",
           i.check.issue_id = "logic_c_experienced_security_restrictions_but_women_security_concerns_none",
           i.check.issue = glue("prot_saftey: {prot_saftey} but women_security_concerns: {women_security_concerns}"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "")  |> 
    batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_experienced_security_restrictions_but_women_security_concerns_none")

# logic_c_experienced_security_restrictions_but_men_security_concerns_none
df_logic_c_experienced_security_restrictions_but_men_security_concerns_none <- df_tool_data |> 
    filter(prot_saftey %in%  c("yes"),
           str_detect(string = men_security_concerns, pattern = "none")) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "prot_saftey",
           i.check.current_value = prot_saftey,
           i.check.value = "",
           i.check.issue_id = "logic_c_experienced_security_restrictions_but_men_security_concerns_none",
           i.check.issue = glue("prot_saftey: {prot_saftey} but men_security_concerns: {men_security_concerns}"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "")  |> 
    batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_experienced_security_restrictions_but_men_security_concerns_none")


# handling hhs questions harmonisation ------------------------------------

# fs_hhs_no_food
df_logic_c_hhs_harmonisation_no_food <- df_tool_data |> 
    filter(fs_hhs_no_food %in% c("yes", "no")) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "fs_hhs_no_food",
           i.check.current_value = as.character(fs_hhs_no_food),
           i.check.value = ifelse(fs_hhs_no_food %in% c("yes"), as.character(1), as.character(0)),
           i.check.issue_id = "logic_c_hhs_harmonisation_no_food",
           i.check.issue = glue("hhs_harmonisation_no_food"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    slice(rep(1:n(), each = 2)) |>  
    group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
    mutate(rank = row_number(),
           i.check.name = ifelse(rank == 1, "fs_hhs_no_food", "fs_hhs_no_food_freq"),
           i.check.current_value = ifelse(rank == 1, as.character(fs_hhs_no_food), as.character(fs_hhs_no_food_freq)),
           i.check.value = ifelse(rank == 1, i.check.value, case_when(i.check.current_value %in% c("rarely_1_2") ~ "1",
                                                                      i.check.current_value %in% c("sometimes_3_10") ~ "2",
                                                                      i.check.current_value %in% c("often_10_times") ~ "3"))
    ) |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hhs_harmonisation_no_food")

# fs_hhs_sleephungry
df_logic_c_hhs_harmonisation_sleephungry <- df_tool_data |> 
    filter(fs_hhs_sleephungry %in% c("yes", "no")) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "fs_hhs_sleephungry",
           i.check.current_value = as.character(fs_hhs_sleephungry),
           i.check.value = ifelse(fs_hhs_sleephungry %in% c("yes"), as.character(1), as.character(0)),
           i.check.issue_id = "logic_c_hhs_harmonisation_sleephungry",
           i.check.issue = glue("hhs_harmonisation_sleephungry"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    slice(rep(1:n(), each = 2)) |>  
    group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
    mutate(rank = row_number(),
           i.check.name = ifelse(rank == 1, "fs_hhs_sleephungry", "fs_hhs_sleephungry_freq"),
           i.check.current_value = ifelse(rank == 1, as.character(fs_hhs_sleephungry), as.character(fs_hhs_sleephungry_freq)),
           i.check.value = ifelse(rank == 1, i.check.value, case_when(i.check.current_value %in% c("rarely_1_2") ~ "1",
                                                                      i.check.current_value %in% c("sometimes_3_10") ~ "2",
                                                                      i.check.current_value %in% c("often_10_times") ~ "3"))
    ) |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hhs_harmonisation_sleephungry")

# fs_hhs_daynoteating
df_logic_c_hhs_harmonisation_daynoteating <- df_tool_data |> 
    filter(fs_hhs_daynoteating %in% c("yes", "no")) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "fs_hhs_daynoteating",
           i.check.current_value = as.character(fs_hhs_daynoteating),
           i.check.value = ifelse(fs_hhs_daynoteating %in% c("yes"), as.character(1), as.character(0)),
           i.check.issue_id = "logic_c_hhs_harmonisation_daynoteating",
           i.check.issue = glue("hhs_harmonisation_daynoteating"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    slice(rep(1:n(), each = 2)) |>  
    group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
    mutate(rank = row_number(),
           i.check.name = ifelse(rank == 1, "fs_hhs_daynoteating", "fs_hhs_daynoteating_freq"),
           i.check.current_value = ifelse(rank == 1, as.character(fs_hhs_daynoteating), as.character(fs_hhs_daynoteating_freq)),
           i.check.value = ifelse(rank == 1, i.check.value, case_when(i.check.current_value %in% c("rarely_1_2") ~ "1",
                                                                      i.check.current_value %in% c("sometimes_3_10") ~ "2",
                                                                      i.check.current_value %in% c("often_10_times") ~ "3"))
    ) |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hhs_harmonisation_daynoteating")


# extra checks income -----------------------------------------------------


df_logic_c_expenditure_greater_than_income <- df_main_extra_data |> 
    filter(int.tot_expenditure > 2*hh_tot_income_amount) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "int.tot_expenditure",
           i.check.current_value = as.character(int.tot_expenditure),
           i.check.value = "NA",
           i.check.issue_id = "logic_c_expenditure_greater_than_income",
           i.check.issue = glue("expenditure_greater_than_income, int.tot_expenditure: {int.tot_expenditure} but hh_tot_income_amount: {hh_tot_income_amount}"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_expenditure_greater_than_income")

# income composition greater than 100 percent

df_logic_c_income_greater_than_100 <- df_main_extra_data |> 
    filter(int.tot_income_percent > 100) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "income_salaried_work",
           i.check.current_value = as.character(income_salaried_work),
           i.check.value = "",
           i.check.issue_id = "logic_c_income_greater_than_100",
           i.check.issue = glue("income_greater_than_100_percent, int.tot_income_percent: {int.tot_income_percent}"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    slice(rep(1:n(), each = 12)) |>  
    group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
    mutate(rank = row_number(),
           i.check.name = case_when(rank == 1 ~ "income_salaried_work", 
                                    rank == 2 ~ "income_casual_labour",
                                    rank == 3 ~ "income_business", 
                                    rank == 4 ~ "income_own_production", 
                                    rank == 5 ~ "income_govt_benefits", 
                                    rank == 6 ~ "income_rent", 
                                    rank == 7 ~ "income_remittances", 
                                    rank == 8 ~ "income_loans_family_friends", 
                                    rank == 9 ~ "income_loans_community_members", 
                                    rank == 10 ~ "income_hum_assistance", 
                                    rank == 11 ~ "income_other_income", 
                                    TRUE ~ "income_other_income_other"),
           i.check.current_value = case_when(rank == 1 ~ as.character(income_salaried_work),
                                             rank == 2 ~ as.character(income_casual_labour),
                                             rank == 3 ~ as.character(income_business), 
                                             rank == 4 ~ as.character(income_own_production), 
                                             rank == 5 ~ as.character(income_govt_benefits), 
                                             rank == 6 ~ as.character(income_rent), 
                                             rank == 7 ~ as.character(income_remittances), 
                                             rank == 8 ~ as.character(income_loans_family_friends), 
                                             rank == 9 ~ as.character(income_loans_community_members), 
                                             rank == 10 ~ as.character(income_hum_assistance), 
                                             rank == 11 ~ as.character(income_other_income), 
                                             TRUE ~ as.character(income_other_income_other))
    ) |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_income_greater_than_100")


# number of surveys -------------------------------------------------------

# column differences less than 60
df_sim_data <- readxl::read_excel("outputs/20230612_most_similar_analysis_msha.xlsx") 

df_suspected_data_60 <- df_sim_data |> 
    filter(number.different.columns < 60)

df_logic_c_similar_data_across_interviews <- df_main_extra_data |> 
    filter(uuid %in% df_suspected_data_60$`_uuid`) |> 
    mutate(i.check.type = "remove_survey",
           i.check.name = "",
           i.check.current_value = "",
           i.check.value = "",
           i.check.issue_id = "logic_c_similar_data_across_interviews",
           i.check.issue = glue("similar_data_across_interviews"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_similar_data_across_interviews")

# excluding un trusted enumerators

df_logic_c_excluded_enums <- df_main_extra_data |> 
    filter(!uuid %in% df_logic_c_similar_data_across_interviews$uuid, enumerator_id %in% c("ETH06", "ETH04", "ETH48", "6")) |> 
    mutate(i.check.type = "remove_survey",
           i.check.name = "",
           i.check.current_value = "",
           i.check.value = "",
           i.check.issue_id = "logic_c_excluded_enums",
           i.check.issue = glue("enumerators with un satisfactory quality"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_excluded_enums")

# handling other_job
df_logic_c_reclassifying_other_job <- df_main_extra_data |> 
    filter(other_job %in% c("yes")) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "other_job",
           i.check.current_value = as.character(other_job),
           i.check.value = "",
           i.check.issue_id = "logic_c_reclassifying_other_job",
           i.check.issue = glue("Reclassifying other_job"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = paste(c("permanent_job_female", "permanent_job_male", "temporary_job_female", "temporary_job_male", "casual_lobour_female", "casual_lobour_male", "own_bisuness_female", "own_bisuness_male"), collapse = " : ")) |> 
    slice(rep(1:n(), each = 2)) |>  
    group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
    mutate(rank = row_number(),
           i.check.name = case_when(rank == 1 ~ "other_job_female", 
                                    TRUE ~ "other_job_male"),
           i.check.current_value = case_when(rank == 1 ~ as.character(other_job_female),
                                             TRUE ~ as.character(other_job_male))
    ) |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_reclassifying_other_job")

# handling other_job_children
df_logic_c_reclassifying_other_job_children <- df_main_extra_data |> 
    filter(other_job_children %in% c("yes")) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "other_job_children",
           i.check.current_value = as.character(other_job_children),
           i.check.value = "",
           i.check.issue_id = "logic_c_reclassifying_other_job_children",
           i.check.issue = glue("Reclassifying other_job_children"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = paste(c("permanent_children_job_female", "permanent_children_job_male", "temporary_children_job_female", "temporary_children_job_male", "casual_lobour_children_female", "casual_lobour_children_male", "own_bisuness_children_female", "own_bisuness_children_male"), collapse = " : ")) |> 
    slice(rep(1:n(), each = 2)) |>  
    group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
    mutate(rank = row_number(),
           i.check.name = case_when(rank == 1 ~ "other_job_children_female", 
                                    TRUE ~ "other_job_children_male"),
           i.check.current_value = case_when(rank == 1 ~ as.character(other_job_children_female),
                                             TRUE ~ as.character(other_job_children_male))
    ) |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_reclassifying_other_job_children")

# handling expenditure_other_infrequent
df_logic_c_harmonise_expenditure_other_infrequent <- df_main_extra_data |> 
    filter(expenditure_other_infrequent > 0,  expenditure_other_infrequent_other %in% c("0", "9", "NA", "No", "o")) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "expenditure_other_infrequent",
           i.check.current_value = as.character(expenditure_other_infrequent),
           i.check.value = "NA",
           i.check.issue_id = "logic_c_harmonise_expenditure_other_infrequent",
           i.check.issue = glue("harmonise expenditure_other_infrequent"),
           i.check.other_text = expenditure_other_infrequent_other,
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = paste(c("shelter", "nfi_purchase", "healthcare_expenditures", "education_expenditures", "debt_repayement"), collapse = " : ")) |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_harmonise_expenditure_other_infrequent")

# handling expenditure_other_infrequent_other
df_logic_c_reclassifying_expenditure_other_infrequent_other <- df_main_extra_data |> 
    filter(!is.na(expenditure_other_infrequent_other), !expenditure_other_infrequent_other %in% c("0", "9", "NA", "No", "o")) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "expenditure_other_infrequent_other",
           i.check.current_value = as.character(expenditure_other_infrequent_other),
           i.check.value = "",
           i.check.issue_id = "logic_c_reclassifying_expenditure_other_infrequent_other",
           i.check.issue = glue("Reclassifying expenditure_other_infrequent_other"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = paste(c("shelter", "nfi_purchase", "healthcare_expenditures", "education_expenditures", "debt_repayement"), collapse = " : ")) |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_reclassifying_expenditure_other_infrequent_other")


# extra other checks ------------------------------------------------------
# handling other_vegetables
df_logic_c_reclassifying_other_vegetables <- df_main_extra_data |> 
    filter(!is.na(other_vegetables)) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "other_vegetables",
           i.check.current_value = as.character(other_vegetables),
           i.check.value = ifelse(other_vegetables %in% c("0", "9", "NA", "o")|str_detect(string = other_vegetables, 
                                                                                          pattern = regex("Lakki|Hin |No|none|Nothing", ignore_case = TRUE)), "NA", ""),
           i.check.issue_id = "logic_c_reclassifying_other_vegetables",
           i.check.issue = glue("Reclassifying other_vegetables"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_reclassifying_other_vegetables")

# handling other_fruits
df_logic_c_reclassifying_other_fruits <- df_main_extra_data |> 
    filter(!is.na(other_fruits)) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "other_fruits",
           i.check.current_value = as.character(other_fruits),
           i.check.value = ifelse(other_fruits %in% c("0", "9", "NA", "o")|str_detect(string = other_fruits, 
                                                                                      pattern = regex("Lakki|Hin |No|none|Nothing", ignore_case = TRUE)), "NA", ""),
           i.check.issue_id = "logic_c_reclassifying_other_fruits",
           i.check.issue = glue("Reclassifying other_fruits"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_reclassifying_other_fruits")

# handling other_meats
df_logic_c_reclassifying_other_meats <- df_main_extra_data |> 
    filter(!is.na(other_meats)) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "other_meats",
           i.check.current_value = as.character(other_meats),
           i.check.value = ifelse(other_meats %in% c("0", "9", "NA", "o")|str_detect(string = other_meats, 
                                                                                     pattern = regex("Lakki|Hin |No|none|Nothing", ignore_case = TRUE)), "NA", ""),
           i.check.issue_id = "logic_c_reclassifying_other_meats",
           i.check.issue = glue("Reclassifying other_meats"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_reclassifying_other_meats")

# handling income_other_income_other
df_logic_c_reclassifying_income_other_income_other <- df_main_extra_data |> 
    filter(!is.na(income_other_income_other)) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "income_other_income_other",
           i.check.current_value = as.character(income_other_income_other),
           i.check.value = ifelse(income_other_income_other %in% c("0", "9", "NA", "o")|str_detect(string = income_other_income_other, 
                                                                                                   pattern = regex("Lakki|Hin |No|none|Nothing", ignore_case = TRUE)), "NA", ""),
           i.check.issue_id = "logic_c_reclassifying_income_other_income_other",
           i.check.issue = glue("Reclassifying income_other_income_other"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_reclassifying_income_other_income_other")

# handling expenditure_other_infrequent_other
df_logic_c_reclassifying_expenditure_other_infrequent_other <- df_main_extra_data |> 
    filter(!is.na(expenditure_other_infrequent_other)) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "expenditure_other_infrequent_other",
           i.check.current_value = as.character(expenditure_other_infrequent_other),
           i.check.value = ifelse(expenditure_other_infrequent_other %in% c("0", "9", "NA", "o")|str_detect(string = expenditure_other_infrequent_other, 
                                                                                                            pattern = regex("Lakki|Hin |No|none|Nothing", ignore_case = TRUE)), "NA", ""),
           i.check.issue_id = "logic_c_reclassifying_expenditure_other_infrequent_other",
           i.check.issue = glue("Reclassifying expenditure_other_infrequent_other"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_reclassifying_expenditure_other_infrequent_other")

# handling hh_other_livestock_other
df_logic_c_reclassifying_hh_other_livestock_other <- df_main_extra_data |> 
    filter(!is.na(hh_other_livestock_other)) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "hh_other_livestock_other",
           i.check.current_value = as.character(hh_other_livestock_other),
           i.check.value = ifelse(hh_other_livestock_other %in% c("0", "9", "NA", "o")|str_detect(string = hh_other_livestock_other, 
                                                                                                  pattern = regex("Lakki|Hin |No|none|Nothing", ignore_case = TRUE)), "NA", ""),
           i.check.issue_id = "logic_c_reclassifying_hh_other_livestock_other",
           i.check.issue = glue("Reclassifying hh_other_livestock_other"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_reclassifying_hh_other_livestock_other")

# handling hh_previous_location
df_logic_c_reclassifying_hh_previous_location <- df_main_extra_data |> 
    filter(!is.na(hh_previous_location)) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "hh_previous_location",
           i.check.current_value = as.character(hh_previous_location),
           i.check.value = ifelse(hh_previous_location %in% c("0", "9", "NA", "o")|str_detect(string = hh_previous_location, 
                                                                                              pattern = regex("Lakki|Hin |No|none|Nothing", ignore_case = TRUE)), "NA", ""),
           i.check.issue_id = "logic_c_reclassifying_hh_previous_location",
           i.check.issue = glue("Reclassifying hh_previous_location"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_reclassifying_hh_previous_location")

# handling water_based_liquids_other
df_logic_c_reclassifying_water_based_liquids_other <- df_main_extra_data |> 
    filter(!is.na(water_based_liquids_other)) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "water_based_liquids_other",
           i.check.current_value = as.character(water_based_liquids_other),
           i.check.value = ifelse(water_based_liquids_other %in% c("0", "9", "NA", "o")|str_detect(string = water_based_liquids_other, 
                                                                                                   pattern = regex("Lakki|Hin |No|none|Nothing", ignore_case = TRUE)), "NA", ""),
           i.check.issue_id = "logic_c_reclassifying_water_based_liquids_other",
           i.check.issue = glue("Reclassifying water_based_liquids_other"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_reclassifying_water_based_liquids_other")

# handling water_based_liquids_sweetened
df_logic_c_reclassifying_water_based_liquids_sweetened <- df_main_extra_data |> 
    filter(!is.na(water_based_liquids_sweetened)) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "water_based_liquids_sweetened",
           i.check.current_value = as.character(water_based_liquids_sweetened),
           i.check.value = ifelse(water_based_liquids_sweetened %in% c("0", "9", "NA", "o")|str_detect(string = water_based_liquids_sweetened, 
                                                                                                       pattern = regex("Lakki|Hin |No|none|Nothing", ignore_case = TRUE)), "NA", ""),
           i.check.issue_id = "logic_c_reclassifying_water_based_liquids_sweetened",
           i.check.issue = glue("Reclassifying water_based_liquids_sweetened"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_reclassifying_water_based_liquids_sweetened")

# handling snfi_core_nfis_other
df_logic_c_reclassifying_snfi_core_nfis_other <- df_main_extra_data |> 
    filter(!is.na(snfi_core_nfis_other)) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "snfi_core_nfis_other",
           i.check.current_value = as.character(snfi_core_nfis_other),
           i.check.value = ifelse(snfi_core_nfis_other %in% c("0", "9", "NA", "o")|str_detect(string = snfi_core_nfis_other, 
                                                                                              pattern = regex("Lakki|Hin |No|none|Nothing", ignore_case = TRUE)), "NA", ""),
           i.check.issue_id = "logic_c_reclassifying_snfi_core_nfis_other",
           i.check.issue = glue("Reclassifying snfi_core_nfis_other"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_reclassifying_snfi_core_nfis_other")

# handling hh_face_barriers_other
df_logic_c_reclassifying_hh_face_barriers_other <- df_main_extra_data |> 
    filter(!is.na(hh_face_barriers_other)) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "hh_face_barriers_other",
           i.check.current_value = as.character(hh_face_barriers_other),
           i.check.value = ifelse(hh_face_barriers_other %in% c("0", "9", "NA", "o")|str_detect(string = hh_face_barriers_other, 
                                                                                                pattern = regex("Lakki|Hin |No|none|Nothing", ignore_case = TRUE)), "NA", ""),
           i.check.issue_id = "logic_c_reclassifying_hh_face_barriers_other",
           i.check.issue = glue("Reclassifying hh_face_barriers_other"),
           i.check.other_text = "",
           i.check.checked_by = "AT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") |> 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_reclassifying_hh_face_barriers_other")


# combined  checks --------------------------------------------------------

df_combined_checks <- bind_rows(checks_output) 
    
# output the log
write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_eth_msha_oromia.csv"), na = "")



# similarity and silhouette analysis --------------------------------------

# silhouette analysis

# NOTE: the column for "col_admin" is kept in the data
omit_cols_sil <- c("start", "end", "today", "consent","hoh", "hoh_equivalent",
                   "deviceid", "audit", "audit_URL", "instance_name", "end_survey", 
                   "gps", "_gps_latitude", "_gps_longitude", "_gps_altitude", "_gps_precision", "_id" ,"_submission_time","_validation_status","_notes","_status","_submitted_by","_tags","_index",
                   "i.check.enumerator_id",
                   "__version__", 
                   "enum_gender", "enum_phonenum", "note_consent_statement", "respondent_gender", "respondent_age", 
                   "hh_woreda", "hoh_marital_status", "hoh_no", "hoh_no_marital_status", "hoh_gender", "hoh_age")

data_similartiy_sil <- df_tool_data |> 
    filter(!is.na(enumerator_id)) |> 
    select(- any_of(omit_cols_sil), 
           - c("...1198"), 
           -starts_with("i."), 
           -starts_with("note_"), 
           -starts_with("int."))

df_sil_data <- calculateEnumeratorSimilarity(data = data_similartiy_sil,
                                             input_df_survey = df_survey, 
                                             col_enum = "enumerator_id",
                                             col_admin = "hh_kebele") |> 
    mutate(si2= abs(si))

df_sil_data[order(df_sil_data$`si2`, decreasing = TRUE),!colnames(df_sil_data)%in%"si2"] |>  
    openxlsx::write.xlsx(paste0("outputs/", butteR::date_file_prefix(), "_silhouette_analysis_msha.xlsx"))


# similarity analysis

data_similartiy <- df_tool_data |> 
    select(- any_of(c(omit_cols_sil, "hh_kebele")), 
           - c("...1198"), 
           -starts_with("i."), 
           -starts_with("note_"), 
           -starts_with("int."))

df_sim_data <- calculateDifferences(data = data_similartiy, 
                                    input_df_survey = df_survey)

openxlsx::write.xlsx(df_sim_data, paste0("outputs/", butteR::date_file_prefix(), 
                                "_most_similar_analysis_msha.xlsx"))

# check surveys with less than 70

df_surveys_per_enum <- df_tool_data |> 
    group_by(enumerator_id) |> 
    summarise(num_surveys = n())

df_suspected_data <- df_sim_data |> 
    filter(number.different.columns < 60) |> 
    group_by(enumerator_id) |> 
    summarise(similar_count = n()) |> 
    arrange(desc(similar_count)) |> 
    left_join(df_surveys_per_enum)

df_suspected_data_similarity <- df_sim_data |> 
    filter(number.different.columns < 70) |> 
    group_by(enumerator_id) |> 
    summarise(similar_count = n()) |> 
    arrange(desc(similar_count)) |> 
    left_join(df_surveys_per_enum)

openxlsx::write.xlsx(list("similar_60" = df_suspected_data,
                          "similar_70" = df_suspected_data_similarity), 
                     paste0("outputs/", butteR::date_file_prefix(), 
                                         "_most_similar_data_extract.xlsx"))
