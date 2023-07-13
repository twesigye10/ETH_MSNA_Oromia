library(tidyverse)
library(openxlsx)

# global options can be set to further simplify things
options("openxlsx.borderStyle" = "thin")
# options("openxlsx.borderColour" = "#4F81BD")
options("openxlsx.withFilter" = FALSE)

# tool
loc_tool <- "inputs/ETH2301_MSHA_Oromia_tool.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices") |> 
    select(list_name, choice_name = name,   choice_label =`label::English`)

# extract select types
df_tool_select_type <- df_survey |> 
    select(type, qn_name = name) |> 
    filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) |> 
    separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# extract choice ids and labels
df_choices_support <- df_choices |> 
    left_join(df_tool_select_type) |> 
    unite("survey_choice_id", qn_name, choice_name, sep = "_", remove = FALSE) |> 
    select(survey_choice_id, choice_label) 

# extract groups
df_tool_groups <- df_survey |> 
    mutate(int.group = ifelse(str_detect(string = name, pattern = "^grp_"), name, NA_character_),
           i.group = int.group) |> 
    fill(i.group) |> 
    filter(!str_detect(string = name, pattern = "_other$"),
           !str_detect(string = type, pattern = "group|repeat|text|geopoint|^gps$|^note$"),
           !is.na(i.group)
    ) |> 
    select(type, name, `label::English`, i.group, in_number)

# support composite grps and labels
df_support_composite_grps <- readxl::read_excel("support_files/support composite grps and labels.xlsx")
df_support_integer_col_labs <- readxl::read_excel("support_files/support integer column names.xlsx") |> 
    filter(!is.na(integer_column_label))

# analysis
df_analysis <- read_csv("outputs/full_analysis_lf_eth_msna_oromia.csv") |> 
    mutate(analysis_choice_id = case_when(select_type %in% c("select_multiple", "select multiple") ~ str_replace(string = `choices/options`, 
                                                                                                                 pattern = "\\/", replacement = "_"),
                                          select_type %in% c("select_one", "select one") ~ paste0(variable, "_", `choices/options`)),
           analysis_choice_id = ifelse(variable %in% c("i.hoh_age"), paste0("hoh_age_", `choices/options`), analysis_choice_id),
           analysis_choice_id = ifelse(variable %in% c("i.hoh_gender"), paste0("hoh_gender_", `choices/options`), analysis_choice_id),
           analysis_choice_id = ifelse(variable %in% c("i.lost_job", "i.boys_anxiety", "i.girls_anxiety", 
                                                       "i.adults_anxiety", "i.chronic_illiness_male", 
                                                       "i.chronic_illiness_female", "i.mental_heath_male", 
                                                       "i.mental_heath_female"), paste0("consent_", `choices/options`), analysis_choice_id),
           Question = ifelse(variable %in% df_support_composite_grps$composite_code, recode(variable, !!!setNames(df_support_composite_grps$composite_qn_label, df_support_composite_grps$composite_code)), Question)
           )

# identify indicators
df_dap_questions <- readxl::read_excel("support_files/ETH2301_DAP_Validated.xlsx", sheet = "ETH2301_DAP") |> 
    filter(!is.na(`Indicator / Variable`)) |> 
    janitor::clean_names() |> 
    select(in_number, indicator_group_sector, indicator_variable, questionnaire_question) |> 
    mutate(indicator_group_sector = str_replace_all(string = indicator_group_sector, pattern = "\\/|\\?", replacement = "_"),
           indicator_group_sector = case_when(indicator_group_sector %in% c("Presentation and consent", "Respondent information","HH information") ~ "HH information",
                                              indicator_group_sector %in% c("Cash & Markets, Livelihoods", "Cash and Markets") ~ "Cash and Markets",
                                              indicator_group_sector %in% c("Protection", "Child Protection") ~ "Protection",
                                              indicator_group_sector %in% c("PSNP", "Shock or vulnerability") ~ "Shock or vulnerability",
                                              TRUE ~ indicator_group_sector)
    )


df_tool_dap_info <- df_tool_groups |> 
    left_join(df_dap_questions) |> 
    mutate(qn_number = row_number())


# format the data ---------------------------------------------------------

df_analysis_dap_info <- df_analysis |> 
    left_join(df_tool_dap_info |> 
                  select(name, indicator_group_sector, indicator_variable, qn_number), by = c("variable" = "name")) |> 
    mutate(response_lable = recode(analysis_choice_id, !!!setNames(df_choices_support$choice_label, df_choices_support$survey_choice_id)),
           choices = ifelse(is.na(response_lable), `choices/options`, response_lable),
           subset_1_val_label = recode(subset_1_val, !!!setNames(df_choices$choice_label, df_choices$choice_name)),
           subset_1_val_label =  ifelse(is.na(subset_1_val_label), "Zonal", subset_1_val_label),
           indicator_group_sector = ifelse(is.na(indicator_group_sector) & variable %in% df_support_composite_grps$composite_code, recode(variable, !!!setNames(df_support_composite_grps$grp_label, df_support_composite_grps$composite_code)), indicator_group_sector), 
           select_type = ifelse((is.na(select_type) | variable %in% c("i.chronic_illiness_male", 
                                                                      "i.chronic_illiness_female", 
                                                                      "i.mental_heath_male", 
                                                                      "i.mental_heath_female",
                                                                      "i.boys_anxiety",
                                                                      "i.girls_anxiety",
                                                                      "i.adults_anxiety")) & variable %in% df_support_composite_grps$composite_code, recode(variable, !!!setNames(df_support_composite_grps$composite_type, df_support_composite_grps$composite_code)), select_type),
           choices = ifelse(variable %in% df_support_integer_col_labs$variable, recode(variable, !!!setNames(df_support_integer_col_labs$integer_column_label, df_support_integer_col_labs$variable)), choices)
           ) |> 
    select(-c(n_unweighted, subset_1_name, subset_1_val)) |> 
    filter(!is.na(indicator_group_sector))

# split data based on groups or sectors
output <- split(df_analysis_dap_info, df_analysis_dap_info$indicator_group_sector)

wb <- createWorkbook()

hs1 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", fontColour = "white", fontSize = 14, wrapText = T)
hs2 <- createStyle(fgFill = "#808080", halign = "CENTER", textDecoration = "Bold", fontColour = "white", wrapText = T)
hs3 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", border = "Bottom", fontColour = "white")
# numbers
number_2digit_style <- openxlsx::createStyle(numFmt = "0.00")
number_1digit_style <- openxlsx::createStyle(numFmt = "0.0")
number_style <- openxlsx::createStyle(numFmt = "0")

cols_for_special_formatting <- c("Zonal", "Gasera", "Sinana", "Goba", "Harena Buluk", "Delo Mena", "Berbere", "Goro")

for (i in 1:length(output)) {
    addWorksheet(wb, sheetName=names(output[i]))
    
    # add header to sheet
    mergeCells(wb, sheet = names(output[i]), rows = 1, cols = 1:10)
    writeData(wb, sheet = names(output[i]), names(output[i]), startCol = 1, startRow = 1, headerStyle = hs1)
    addStyle(wb, sheet = names(output[i]), hs1, rows = 1, cols = 1:10, gridExpand = TRUE)
    
    setColWidths(wb = wb, sheet = names(output[i]), cols = 2, widths = 60)
    
    # get current data for the group or sector
    current_sheet_data <- output[[i]] |> 
        pivot_wider(names_from = subset_1_val_label, values_from = `Results(mean/percentage)`) |> 
        arrange(qn_number) |> 
        mutate(row_id = row_number())
    
    # split variables to be written in different tables with in a sheet
    sheet_variables_data <- split(current_sheet_data, factor(current_sheet_data$variable, levels = unique(current_sheet_data$variable)))
    
    previous_row_end <- 2
    
    for (j in 1:length(sheet_variables_data)) {
        
        current_variable_data <- sheet_variables_data[[j]]
        
        get_question <- current_variable_data |> select(Question) |> unique() |> pull()
        get_qn_type <- current_variable_data |> select(select_type) |> unique() |> pull()
        
        if(get_qn_type %in% c("select_one", "select one", "select_multiple", "select multiple")){
            class(current_variable_data$Zonal) <- "percentage"
            class(current_variable_data$Gasera) <- "percentage"
            class(current_variable_data$Sinana) <- "percentage"
            class(current_variable_data$Goba) <- "percentage"
            class(current_variable_data$`Harena Buluk`) <- "percentage"
            class(current_variable_data$`Delo Mena`) <- "percentage"
            class(current_variable_data$Berbere) <- "percentage"
            class(current_variable_data$Goro) <- "percentage"
        }else{
            class(current_variable_data$Zonal) <- "numeric"
            class(current_variable_data$Gasera) <- "numeric"
            class(current_variable_data$Sinana) <- "numeric"
            class(current_variable_data$Goba) <- "numeric"
            class(current_variable_data$`Harena Buluk`) <- "numeric"
            class(current_variable_data$`Delo Mena`) <- "numeric"
            class(current_variable_data$Berbere) <- "numeric"
            class(current_variable_data$Goro) <- "numeric"
        }
        
        current_row_start <- previous_row_end + 3
        
        print(current_row_start)
        
        # add header for variable
        mergeCells(wb, sheet = names(output[i]), rows = previous_row_end + 2, cols = 1:10)
        writeData(wb, sheet = names(output[i]), get_question, startCol = 1, startRow = previous_row_end + 2)
        addStyle(wb, sheet = names(output[i]), hs2, rows = previous_row_end + 2, cols = 1:10, gridExpand = TRUE)
        
        current_data_length <- max(current_variable_data$row_id) - min(current_variable_data$row_id)
        
        addStyle(wb, sheet = names(output[i]), number_1digit_style, rows = current_row_start + 1 : current_row_start + 1 + current_data_length, cols = 1:10, gridExpand = TRUE)

        writeDataTable(wb = wb, 
                       sheet = names(output[i]), 
                       x = current_variable_data |> 
                           select(-c(Question, `choices/options`, 
                                     population, analysis_choice_id, 
                                     indicator_group_sector,response_lable,
                                     row_id, variable, select_type, indicator_variable, qn_number)
                           ), 
                       startRow = current_row_start, 
                       startCol = 1, 
                       tableStyle = "TableStyleLight9", 
                       headerStyle = hs3)
        
        previous_row_end <- current_row_start + 1 + current_data_length
    }
    # hide grid lines
    showGridLines(wb,  names(output[i]), showGridLines = FALSE)  
}

# worksheets order
worksheetOrder(wb) <- c(7, 2, 8, 4, 3, 10, 13, 11, 6, 5, 1, 9, 12)

activeSheet(wb) <- 7

saveWorkbook(wb, paste0("outputs/", butteR::date_file_prefix(),"_formatted_analysis_eth_msna_oromia.xlsx"), overwrite = TRUE)
openXL(file = paste0("outputs/", butteR::date_file_prefix(),"_formatted_analysis_eth_msna_oromia.xlsx"))
