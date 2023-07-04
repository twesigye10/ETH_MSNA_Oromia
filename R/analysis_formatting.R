library(tidyverse)
library(openxlsx)

# analysis
df_analysis <- read_csv("outputs/full_analysis_lf_eth_msna_oromia.csv") |> 
    filter(is.na(subset_1_name))

# tool
df_survey <- readxl::read_excel("inputs/ETH2301_MSHA_Oromia_tool.xlsx", sheet = "survey") 

df_tool_data_support <- df_survey |> 
    select(type, name, label = `label::English`, in_number) |> 
    filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) |> 
    separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

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

output <- split(df_analysis_dap_info, df_analysis_dap_info$indicator_group_sector)

wb <- createWorkbook()

for (i in 1:length(output)) {
    addWorksheet(wb, sheetName=names(output[i]))
    
    current_sheet_data <- output[[i]] |> 
        mutate(row_id = row_number())
    # view(current_sheet_data)
    
    sheet_variables_data <- split(current_sheet_data, current_sheet_data$variable)
    
    previous_max_row <- 0
        
    for (j in 1:length(sheet_variables_data)) {
        
        current_variable_data <- sheet_variables_data[[j]]
        # view(current_variable_data)
        
        variable_data_length <- previous_max_row + 2
        # current_variable_data
        print(variable_data_length)
        writeDataTable(wb = wb, 
                       sheet = names(output[i]), 
                       x = current_variable_data |> select(-c(indicator_group_sector, indicator_variable, row_id)), 
                       startRow = variable_data_length, 
                       startCol = 2, 
                       tableStyle = "TableStyleMedium21")
        
        previous_max_row <- variable_data_length + 1 + max(current_variable_data$row_id) - min(current_variable_data$row_id)
    }
    
}

saveWorkbook(wb, "outputs/analysis_tables_styles_4.xlsx", overwrite = TRUE)