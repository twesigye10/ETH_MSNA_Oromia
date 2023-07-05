library(tidyverse)
library(srvyr)
library(supporteR)  

# packages to install incase
# devtools::install_github("zackarno/butteR")
# devtools::install_github("twesigye10/supporteR")

# clean data
data_path <- "inputs/clean_data_eth_msha_oromia.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000, sheet = "cleaned_main_data"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, sheet = "cleaned_main_data", col_types = c_types, na = "NA")

# tool
df_survey <- readxl::read_excel("inputs/ETH2301_MSHA_Oromia_tool.xlsx", sheet = "survey") 

df_tool_data_support <- df_survey |> 
  select(type, name, label = `label::English`) |> 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) |> 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# dap
dap <- read_csv("inputs/r_dap_msha_eth.csv")

# set up design object
ref_svy <- as_survey(.data = df_main_clean_data)

# analysis

df_main_analysis <- analysis_after_survey_creation(input_svy_obj = ref_svy,
                                                   input_dap = dap |> 
                                                       filter(!variable %in% c("hh_living_district", 
                                                                               "when_settlement_est", 
                                                                               "idp_name",
                                                                               "wash_watertime1",
                                                                               "hh_shocks_affect11"))
                                                   )
# merge analysis

combined_analysis <- df_main_analysis

integer_cols_i <- c("i.fcs", "i.rcsi", "i.hhs", "i.adults_permanent_job", "i.adults_temporary_job", "i.adults_casual_lobour",
                    "i.adults_own_bisuness", "i.children_permanent_job", "i.children_temporary_job", "i.children_casual_lobour",
                    "i.children_own_bisuness", "i.boys_early_marriege", "i.girls_early_marriege", "i.boys_work_outside_home",
                    "i.girls_work_outside_home")
integer_cols_int <- c("int.fcs", "int.rcsi", "int.hhs", "int.adults_permanent_job", "int.adults_temporary_job", "int.adults_casual_lobour",
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
         `mean/pct` = ifelse(select_type %in% c("integer") & !variable %in% integer_cols_i & !str_detect(string = variable, pattern = "^i\\."), `mean/pct`, `mean/pct`*100),
         `mean/pct` = round(`mean/pct`, digits = 2)) |> 
  mutate(variable = ifelse(variable %in% integer_cols_int, str_replace(string = variable, pattern = "int.", replacement = "i."), variable),
         label = ifelse(label %in% integer_cols_int, str_replace(string = label, pattern = "int.", replacement = "i."), label)) |> 
  select(`Question`= label, 
         variable, 
         `choices/options` = variable_val, 
         `Results(mean/percentage)` = `mean/pct`, 
         n_unweighted, 
         population, 
         subset_1_name, 
         subset_1_val)

# output analysis
write_csv(full_analysis_long, paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_lf_eth_msna_oromia.csv"), na="")
write_csv(full_analysis_long, paste0("outputs/full_analysis_lf_eth_msna_oromia.csv"), na="")
