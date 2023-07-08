library(tidyverse)
library(openxlsx)

# global options can be set to further simplify things
options("openxlsx.borderStyle" = "thin")
# options("openxlsx.borderColour" = "#4F81BD")
options("openxlsx.withFilter" = FALSE)

# analysis
df_analysis <- read_csv("outputs/full_analysis_lf_eth_msna_oromia.csv") |> 
    mutate(analysis_choice_id = case_when(select_type %in% c("select_multiple", "select multiple") ~ str_replace(string = `choices/options`, 
                                                                                                              pattern = "\\/", replacement = "_"),
                                        select_type %in% c("select_one", "select one") ~ paste0(variable, "_", `choices/options`)
                                        ))

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

df_support_composite_grps <- tibble::tribble(
                                             ~composite_code,         ~grp_label, ~composite_type,
                                                     "i.fcs",    "Food security",       "integer",
                                                 "i.fcs_cat",    "Food security",    "select_one",
                                                    "i.rcsi",    "Food security",       "integer",
                                                "i.rcsi_cat",    "Food security",    "select_one",
                                                     "i.hhs",    "Food security",       "integer",
                                                 "i.hhs_cat",    "Food security",    "select_one",
                                     "i.hh_composition_size",   "HH information",       "integer",
                                              "i.hoh_gender",   "HH information",    "select_one",
                                                 "i.hoh_age",   "HH information",    "select_one",
                                    "i.adults_permanent_job", "Cash and Markets",       "integer",
                                    "i.adults_temporary_job", "Cash and Markets",       "integer",
                                    "i.adults_casual_lobour", "Cash and Markets",       "integer",
                                     "i.adults_own_bisuness", "Cash and Markets",       "integer",
                                  "i.children_permanent_job", "Cash and Markets",       "integer",
                                  "i.children_temporary_job", "Cash and Markets",       "integer",
                                  "i.children_casual_lobour", "Cash and Markets",       "integer",
                                   "i.children_own_bisuness", "Cash and Markets",       "integer",
                                                "i.lost_job", "Cash and Markets",    "select_one",
                                     "i.boys_early_marriege",       "Protection",    "select_one",
                                    "i.girls_early_marriege",       "Protection",    "select_one",
                                  "i.boys_work_outside_home",       "Protection",    "select_one",
                                 "i.girls_work_outside_home",       "Protection",    "select_one",
                                            "i.boys_anxiety",       "Protection",    "select_one",
                                           "i.girls_anxiety",       "Protection",    "select_one",
                                          "i.adults_anxiety",       "Protection",    "select_one",
                                           "i.snfi_no_rooms",      "Shelter_NFI",       "integer",
                                   "i.chronic_illiness_male",           "Health",    "select_one",
                                 "i.chronic_illiness_female",           "Health",    "select_one",
                                       "i.mental_heath_male",           "Health",    "select_one",
                                     "i.mental_heath_female",           "Health",    "select_one",
                                              "i.disability",           "Health",    "select_one"
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
    left_join(df_dap_questions)


# format the data ---------------------------------------------------------

df_analysis_dap_info <- df_analysis |> 
    left_join(df_tool_dap_info |> 
                  select(name, indicator_group_sector, indicator_variable), by = c("variable" = "name")) |> 
    mutate(response_lable = recode(analysis_choice_id, !!!setNames(df_choices_support$choice_label, df_choices_support$survey_choice_id)),
           choices = ifelse(is.na(response_lable), `choices/options`, response_lable),
           subset_1_val_label = recode(subset_1_val, !!!setNames(df_choices$choice_label, df_choices$choice_name)),
           subset_1_val_label =  ifelse(is.na(subset_1_val_label), "Zonal", subset_1_val_label),
           indicator_group_sector = ifelse(is.na(indicator_group_sector) & variable %in% df_support_composite_grps$composite_code, recode(variable, !!!setNames(df_support_composite_grps$grp_label, df_support_composite_grps$composite_code)), indicator_group_sector), 
           select_type = ifelse(is.na(select_type) & variable %in% df_support_composite_grps$composite_code, recode(variable, !!!setNames(df_support_composite_grps$composite_type, df_support_composite_grps$composite_code)), select_type)) |> 
    select(-c(n_unweighted, subset_1_name, subset_1_val))

# split data based on groups or sectors
output <- split(df_analysis_dap_info, df_analysis_dap_info$indicator_group_sector)

wb <- createWorkbook()

hs1 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", fontColour = "white", fontSize = 14, wrapText = T)
hs2 <- createStyle(fgFill = "#808080", halign = "CENTER", textDecoration = "Bold", fontColour = "white", wrapText = T)
hs3 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", border = "Bottom", fontColour = "white")

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
        mutate(row_id = row_number())
    
    # split variables to be written in different tables with in a sheet
    sheet_variables_data <- split(current_sheet_data, current_sheet_data$variable)
    
    previous_max_row <- 2
        
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
        
        variable_data_length <- previous_max_row + 3
    
        print(variable_data_length)
        
        # add header for variable
        mergeCells(wb, sheet = names(output[i]), rows = previous_max_row + 2, cols = 1:10)
        writeData(wb, sheet = names(output[i]), get_question, startCol = 1, startRow = previous_max_row + 2)
        addStyle(wb, sheet = names(output[i]), hs2, rows = previous_max_row + 2, cols = 1:10, gridExpand = TRUE)
        writeDataTable(wb = wb, 
                       sheet = names(output[i]), 
                       x = current_variable_data |> 
                           select(-c(Question, `choices/options`, 
                                     population, analysis_choice_id, 
                                     indicator_group_sector,response_lable,
                                     row_id, variable, select_type, indicator_variable)
                                                           ), 
                       startRow = variable_data_length, 
                       startCol = 1, 
                       tableStyle = "TableStyleLight9", 
                       headerStyle = hs3)
        
        previous_max_row <- variable_data_length + 1 + max(current_variable_data$row_id) - min(current_variable_data$row_id)
    }
    
}

# worksheets order
worksheetOrder(wb) <- c(7, 2, 8, 4, 3, 10, 13, 11, 6, 5, 1, 9, 12)

activeSheet(wb) <- 7

saveWorkbook(wb, paste0("outputs/", butteR::date_file_prefix(),"_formatted_analysis_eth_msna_oromia.xlsx"), overwrite = TRUE)
openXL(file = paste0("outputs/", butteR::date_file_prefix(),"_formatted_analysis_eth_msna_oromia.xlsx"))
