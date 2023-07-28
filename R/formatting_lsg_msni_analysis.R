library(tidyverse)
library(openxlsx)

# global options can be set to further simplify things
options("openxlsx.borderStyle" = "thin")
# options("openxlsx.borderColour" = "#4F81BD")
options("openxlsx.withFilter" = FALSE)

# analysis
df_lsg_msni_analysis <- read_csv("outputs/lsg_msni_analysis_eth_msna_oromia.csv") |> 
    rename(Severity = variable_val) |> 
    relocate(level, .before = Severity) |> 
    select(-c(`mean/pct_low`, `mean/pct_upp`, n_unweighted, subset_1_name))

# tool
loc_tool <- "inputs/ETH2301_MSHA_Oromia_tool.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices") |> 
    select(list_name, choice_name = name,   choice_label =`label::English`) |> 
    filter(list_name %in% c("woreda"))

# format the data ---------------------------------------------------------

df_analysis_extra_info <- df_lsg_msni_analysis |> 
    mutate(select_type = ifelse(variable %in% c("lsg_count", "lsg_profiles"), "integer", "select_one"),
           subset_1_val =  ifelse(is.na(subset_1_val), "Zonal", subset_1_val),
           subset_1_val =  ifelse(subset_1_val %in% c("ET041114", "ET041112",
                                                      "ET041106", "ET041110",
                                                      "ET041116", "ET041111",
                                                      "ET041109"), 
                                  recode(subset_1_val, !!!setNames(df_choices$choice_label, df_choices$choice_name)), subset_1_val),
           group_sector = case_when(str_detect(string = variable, pattern = "_lsg$|^msni$") ~ "All Severity Levels",
                                    str_detect(string = variable, pattern = "_sl3_above$|_sl4_above$|lsg_count|lsg_profiles") ~ "Higher Severity Levels",)
           )

# split data based on groups
output <- split(df_analysis_extra_info, df_analysis_extra_info$group_sector)

wb <- createWorkbook()

hs1 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", fontColour = "white", fontSize = 14, wrapText = T)
hs2 <- createStyle(fgFill = "#808080", halign = "CENTER", textDecoration = "Bold", fontColour = "white", wrapText = T)
hs3 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", border = "Bottom", fontColour = "white")
# numbers
number_2digit_style <- openxlsx::createStyle(numFmt = "0.00")
number_1digit_style <- openxlsx::createStyle(numFmt = "0.0")
number_style <- openxlsx::createStyle(numFmt = "0")


for (i in 1:length(output)) {
    addWorksheet(wb, sheetName=names(output[i]))
    
    # add header to sheet
    mergeCells(wb, sheet = names(output[i]), rows = 1, cols = 1:10)
    writeData(wb, sheet = names(output[i]), names(output[i]), startCol = 1, startRow = 1, headerStyle = hs1)
    addStyle(wb, sheet = names(output[i]), hs1, rows = 1, cols = 1:10, gridExpand = TRUE)
    
    setColWidths(wb = wb, sheet = names(output[i]), cols = 2, widths = 30)
    
    # get current data for the group or sector
    current_sheet_data <- output[[i]] |> 
        pivot_wider(names_from = subset_1_val, values_from = `mean/pct`) |> 
        mutate(row_id = row_number())
    
    # split variables to be written in different tables with in a sheet
    sheet_variables_data <- split(current_sheet_data, factor(current_sheet_data$variable, levels = unique(current_sheet_data$variable)))
    
    previous_row_end <- 2
    
    for (j in 1:length(sheet_variables_data)) {
        
        current_variable_data <- sheet_variables_data[[j]]
        
        get_question <- current_variable_data |> select(variable) |> unique() |> pull()
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
                           select(-c(population,  
                                     group_sector,
                                     row_id, variable, select_type)
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

saveWorkbook(wb, paste0("outputs/", butteR::date_file_prefix(),"_formatted_lsg_analysis_eth_msna_oromia.xlsx"), overwrite = TRUE)
openXL(file = paste0("outputs/", butteR::date_file_prefix(),"_formatted_lsg_analysis_eth_msna_oromia.xlsx"))
