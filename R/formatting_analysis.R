library(tidyverse)
library(openxlsx)

# global options can be set to further simplify things
options("openxlsx.borderStyle" = "thin")
# options("openxlsx.borderColour" = "#4F81BD")
options("openxlsx.withFilter" = FALSE)

# analysis
df_analysis <- read_csv("outputs/full_analysis_lf_eth_msna_oromia.csv") |> 
    filter(is.na(subset_1_name))

# tool
df_survey <- readxl::read_excel("inputs/ETH2301_MSHA_Oromia_tool.xlsx", sheet = "survey") 

# extract groups
df_tool_questions <- df_survey |> 
    mutate(int.group = ifelse(str_detect(string = name, pattern = "^grp_"), name, NA_character_),
           i.group = int.group) |> 
    fill(i.group) |> 
    filter(!str_detect(string = name, pattern = "_other$"),
           !str_detect(string = type, pattern = "group|repeat|text|geopoint|^gps$|^note$"),
           !is.na(i.group)
    ) |> 
    select(type, name, `label::English`, i.group, in_number)

# identify indicators
df_dap_questions <- readxl::read_excel("support_files/ETH2301_DAP_Validated.xlsx", sheet = "ETH2301_DAP") |> 
    filter(!is.na(`Indicator / Variable`)) |> 
    janitor::clean_names() |> 
    select(in_number, indicator_group_sector, indicator_variable, questionnaire_question) |> 
    mutate(indicator_group_sector = str_replace_all(string = indicator_group_sector, pattern = "\\/|\\?", replacement = "_"))


df_tool_dap_info <- df_tool_questions |> 
    left_join(df_dap_questions)


# format the data ---------------------------------------------------------

df_analysis_dap_info <- df_analysis |> 
    left_join(df_tool_dap_info |> 
                  select(name, indicator_group_sector, indicator_variable), by = c("variable" = "name")) |> 
    select(-c(population, subset_1_name, subset_1_val))

# split data based on groups or sectors
output <- split(df_analysis_dap_info, df_analysis_dap_info$indicator_group_sector)

wb <- createWorkbook()

hs1 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", fontColour = "white", fontSize = 14, wrapText = T)
hs2 <- createStyle(fgFill = "#808080", halign = "CENTER", textDecoration = "Bold", fontColour = "white", wrapText = T)
hs3 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", border = "Bottom", fontColour = "white")

for (i in 1:length(output)) {
    addWorksheet(wb, sheetName=names(output[i]))
    
    # add header to sheet
    mergeCells(wb, sheet = names(output[i]), rows = 1, cols = 1:5)
    writeData(wb, sheet = names(output[i]), names(output[i]), startCol = 1, startRow = 1, headerStyle = hs1)
    addStyle(wb, sheet = names(output[i]), hs1, rows = 1, cols = 1:5, gridExpand = TRUE)
    # get current data for the group or sector
    current_sheet_data <- output[[i]] |> 
        mutate(row_id = row_number())
    
    # split variables to be written in different tables with in a sheet
    sheet_variables_data <- split(current_sheet_data, current_sheet_data$variable)
    
    previous_max_row <- 2
        
    for (j in 1:length(sheet_variables_data)) {
        
        current_variable_data <- sheet_variables_data[[j]]
        
        get_question <- current_variable_data |> select(Question) |> unique() |> pull()
        
        variable_data_length <- previous_max_row + 3
        # current_variable_data
        print(variable_data_length)
        
        # add header for variable
        mergeCells(wb, sheet = names(output[i]), rows = previous_max_row + 2, cols = 1:5)
        writeData(wb, sheet = names(output[i]), get_question, startCol = 1, startRow = previous_max_row + 2)
        addStyle(wb, sheet = names(output[i]), hs2, rows = previous_max_row + 2, cols = 1:5, gridExpand = TRUE)
        writeDataTable(wb = wb, 
                       sheet = names(output[i]), 
                       x = current_variable_data |> select(-c(Question, indicator_group_sector, indicator_variable, row_id)), 
                       startRow = variable_data_length, 
                       startCol = 1, 
                       tableStyle = "TableStyleLight9", 
                       headerStyle = hs3)
        
        previous_max_row <- variable_data_length + 1 + max(current_variable_data$row_id) - min(current_variable_data$row_id)
    }
    
}

saveWorkbook(wb, paste0("outputs/", butteR::date_file_prefix(),"_formatted_analysis_eth_msna_oromia.xlsx"), overwrite = TRUE)
openXL(file = paste0("outputs/", butteR::date_file_prefix(),"_formatted_analysis_eth_msna_oromia.xlsx"))
