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
