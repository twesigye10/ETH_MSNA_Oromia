# creating composite indicators -------------------------------------------

create_composite_indicators <- function(input_df) {
    input_df |> 
        mutate(int.fcs_cereals_tubers = fs_fcs_cerealgrainroottuber*2,
               int.fcs_pulses = fs_fcs_beansnuts*3,
               int.fcs_vegetables = fs_fcs_vegetableleave,
               int.fcs_fruit = fs_fcs_fruit,
               int.fcs_meat_fish = fs_fcs_meatfishegg*4,
               int.fcs_dairy = fs_fcs_dairy*4,
               int.fcs_sugar = fs_fcs_sugar*0.5,
               int.fcs_oils = fs_fcs_fat*0.5,
               int.rCSILessQlty = rCSILessQlty,
               int.rCSIBorrow = 2 * rCSIBorrow,
               int.rCSIMealSize = rCSIMealSize,
               int.rCSIMealAdult = 3 * rCSIMealAdult,
               int.rCSIMealNb = rCSIMealNb,
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
        mutate(i.fcs = sum(c_across(int.fcs_cereals_tubers:int.fcs_oils), na.rm = T),
               i.rcsi = sum(c_across(int.rCSILessQlty:int.rCSIMealNb)),
               i.hhs = sum(c_across(int.freq_no_food_lack_resources:int.freq_day_and_night_no_food), na.rm = T)
        ) |>
        ungroup() |>
        mutate(i.fcs_cat = case_when(i.fcs <= 21 ~ "Poor",
                                     i.fcs <= 35 ~ "Borderline",
                                     i.fcs <= 112 ~ "Acceptable"),
               i.rcsi_cat = case_when(i.rcsi < 4 ~ "rcsi_0_3",
                                      i.rcsi < 19 ~ "rcsi_4_18",
                                      i.rcsi >= 19 ~ "rcsi_19+"),
               i.hhs_cat = case_when(i.hhs <= 1 ~ "Little to no hunger",
                                     i.hhs <= 3 ~ "Moderate hunger",
                                     i.hhs <= 6 ~ "Severe hunger"),
               i.fewsnet_phase = case_when(i.hhs == 0 ~ "Phase 1",
                                           i.hhs == 1 ~ "Phase 2",
                                           i.hhs <= 3 ~ "Phase 3",
                                           i.hhs == 4 ~ "Phase 4",
                                           i.hhs <= 6 ~ "Phase 5"),
        ) |> 
        relocate(i.fcs_cat, .after = i.fcs) |> 
        relocate(i.rcsi_cat, .after = i.rcsi) |> 
        select(-c(starts_with("int.")))
}

