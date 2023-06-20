# creating composite indicators -------------------------------------------

create_composite_indicators <- function(input_df) {
    input_df |> 
        mutate(i.fcs = (fs_fcs_cerealgrainroottuber*2 + fs_fcs_beansnuts*3 + fs_fcs_vegetableleave +  
                            fs_fcs_fruit + fs_fcs_meatfishegg*4 + fs_fcs_dairy*4 + 
                            fs_fcs_sugar*0.5 + fs_fcs_fat*0.5),
               i.fcs_cat = case_when(i.fcs <= 21 ~ "Poor",
                                     i.fcs <= 35 ~ "Borderline",
                                     i.fcs <= 112 ~ "Acceptable"),
               i.rcsi = rCSILessQlty  + (2 * rCSIBorrow) + rCSIMealSize + (3 * rCSIMealAdult) + rCSIMealNb,
               i.rcsi_cat = case_when(i.rcsi < 4 ~ "rcsi_0_3",
                                      i.rcsi < 19 ~ "rcsi_4_18",
                                      i.rcsi >= 19 ~ "rcsi_19+"),
               int.freq_no_food_lack_resources = case_when(fs_hhs_no_food_freq %in% c("1") ~ 0,
                                                           fs_hhs_no_food_freq %in% c("2") ~ 1,
                                                           fs_hhs_no_food_freq %in% c("3") ~ 2),
               int.freq_sleep_hungry = case_when(fs_hhs_sleephungry_freq %in% c("1") ~ 0,
                                                 fs_hhs_sleephungry_freq %in% c("2") ~ 1,
                                                 fs_hhs_sleephungry_freq %in% c("3") ~ 2),
               int.freq_day_and_night_no_food = case_when(fs_hhs_daynoteating_freq %in% c("1") ~ 0,
                                                          fs_hhs_daynoteating_freq %in% c("2") ~ 1,
                                                          fs_hhs_daynoteating_freq %in% c("3") ~ 2) 
        ) |> 
        rowwise() |> 
        mutate( 
            i.hhs = sum(c_across(int.freq_no_food_lack_resources:int.freq_day_and_night_no_food), na.rm = T)
        ) |>
        ungroup() |>
        mutate(i.hhs_cat = case_when(i.hhs <= 1 ~ "Little to no hunger",
                                     i.hhs <= 3 ~ "Moderate hunger",
                                     i.hhs <= 6 ~ "Severe hunger"),
               i.fewsnet_phase = case_when(i.hhs == 0 ~ "Phase 1",
                                           i.hhs == 1 ~ "Phase 2",
                                           i.hhs <= 3 ~ "Phase 3",
                                           i.hhs == 4 ~ "Phase 4",
                                           i.hhs <= 6 ~ "Phase 5")) |> 
        select(-c(starts_with("int.")))
}

