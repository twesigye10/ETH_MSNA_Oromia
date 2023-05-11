library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)

# read data and tool ----------------------------------------------------------
# data
df_tool_data <- readxl::read_excel("inputs/ETH2002_H2R_data.xlsx") |>  
    mutate(start = as_datetime(start),
           end = as_datetime(end)) |> 
    checks_add_extra_cols(input_enumerator_id_col = "enumerator_id",
                          input_location_col = "loc_zone")

# tool
loc_tool <- "inputs/ETH2002_H2R_tool.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")