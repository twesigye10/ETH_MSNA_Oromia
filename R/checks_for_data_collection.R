library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)

# read data and tool ----------------------------------------------------------
# data
df_tool_data <- readxl::read_excel("inputs/ETH2301_MSNA_Oromia_data.xlsx") |>  
    mutate(start = as_datetime(start),
           end = as_datetime(end)) |> 
    checks_add_extra_cols(input_enumerator_id_col = "enumerator_id",
                          input_location_col = "hh_kebele")

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
                                      dplyr::filter(str_detect(string = !!sym(.x), pattern = "^-[9]{2,4}$")) |> 
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

# logic_c_hh_report_debt_repayment_but_doesnot_have_debt
df_logic_c_hh_report_debt_repayment_but_doesnot_have_debt <- df_tool_data |> 
    filter(hh_debt %in% c("no"), repaying_debts > 0) |> 
    mutate(i.check.type = "change_response",
           i.check.name = "hh_debt",
           i.check.current_value = hh_debt,
           i.check.value = "no",
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
           i.check.value = "no",
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
           !str_detect(string = boys_security_concerns, pattern = "none")) |> 
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
           !str_detect(string = girls_security_concerns, pattern = "none")) |> 
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
           !str_detect(string = women_security_concerns, pattern = "none")) |> 
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
           !str_detect(string = men_security_concerns, pattern = "none")) |> 
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

# combined  checks --------------------------------------------------------

df_combined_checks <- bind_rows(checks_output) |> 
    mutate(int.issue_id = str_extract(string = issue_id, pattern = "[0-9]{1,3}")) |> 
    left_join(df_logical_check_description, by = c("int.issue_id" = "check_number")) |> 
    mutate(issue = ifelse(str_detect(string = issue_id, pattern = "[0-9]{1,3}"), paste("[", issue, "].", check_description), issue)) |> 
    select(-c(int.issue_id, check_description))

# output the log
write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_eth_msna_oromia.csv"), na = "")

