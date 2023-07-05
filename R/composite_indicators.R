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
        mutate(int.fcs = sum(c_across(int.fcs_cereals_tubers:int.fcs_oils)),
               int.rcsi = sum(c_across(int.rCSILessQlty:int.rCSIMealNb)),
               int.hhs = sum(c_across(int.freq_no_food_lack_resources:int.freq_day_and_night_no_food), na.rm = T),
               int.hh_size = sum(c_across(num_males_0to6:num_females_66plusyrs), na.rm = T),
               int.adults_permanent_job = sum(c_across(permanent_job_female : permanent_job_male), na.rm = T),
               int.adults_temporary_job = sum(c_across(temporary_job_female : temporary_job_male), na.rm = T),
               int.adults_casual_lobour = sum(c_across(casual_lobour_female : casual_lobour_male), na.rm = T),
               int.adults_own_bisuness = sum(c_across(own_bisuness_female : own_bisuness_male), na.rm = T),
               int.hh_no_4_work = sum(c_across(num_males_18to49:num_females_66plusyrs), na.rm = T),
               int.children_permanent_job = sum(c_across(permanent_children_job_female : permanent_children_job_male), na.rm = T),
               int.children_temporary_job = sum(c_across(temporary_children_job_female : temporary_children_job_male), na.rm = T),
               int.children_casual_lobour = sum(c_across(casual_lobour_children_female : casual_lobour_children_male), na.rm = T),
               int.children_own_bisuness = sum(c_across(own_bisuness_children_female : own_bisuness_children_male), na.rm = T),
               int.hh_number_children = sum(c_across(num_males_0to6:num_females_14to17), na.rm = T),
               int.lost_job = sum(c_across(permanent_lost_job_female : temporary_lost_job_male), na.rm = T),
               int.hh_number_children_male = sum(c_across(c("num_males_0to6", "num_males_7to3yrs", "num_males_4to6", "num_males_7to13", "num_males_14to17")), na.rm = T),
               int.hh_number_children_female = sum(c_across(c("num_females_0to6", "num_females_7to3yrs", "num_females_4to6", "num_females_7to13", "num_females_14to17")), na.rm = T),
               int.boys_work_outside_home = sum(c_across(c("boys_between2_7years", "boys_between8_13years", "boys_between14_17years")), na.rm = T),
               int.girls_work_outside_home = sum(c_across(c("girls_between2_7years", "girls_between8_13years", "girls_between14_17years")), na.rm = T),
               
               
        ) |>
        ungroup() |>
        mutate(i.fcs = int.fcs,
               i.fcs_cat = case_when(i.fcs <= 21 ~ "Poor",
                                     i.fcs <= 35 ~ "Borderline",
                                     i.fcs <= 112 ~ "Acceptable"),
               i.rcsi = int.rcsi,
               i.rcsi_cat = case_when(i.rcsi < 4 ~ "rcsi_0_3",
                                      i.rcsi < 19 ~ "rcsi_4_18",
                                      i.rcsi >= 19 ~ "rcsi_19+"),
               i.hhs = int.hhs,
               i.hhs_cat = case_when(i.hhs == 0 ~ "None",
                                     i.hhs == 1 ~ "Slight",
                                     i.hhs <= 3 ~ "Moderate",
                                     i.hhs == 4 ~ "Severe",
                                     i.hhs <= 6 ~ "Very severe"),
               i.hh_composition_size = int.hh_size,
               i.hoh_gender = ifelse(is.na(hoh_gender), respondent_gender, hoh_gender),
               i.hoh_age = ifelse(is.na(hoh_age), respondent_age, hoh_age),
               i.adults_permanent_job = (int.adults_permanent_job/int.hh_no_4_work) * 100,
               i.adults_temporary_job = (int.adults_temporary_job/int.hh_no_4_work) * 100,
               i.adults_casual_lobour = (int.adults_casual_lobour/int.hh_no_4_work) * 100,
               i.adults_own_bisuness = (int.adults_own_bisuness/int.hh_no_4_work) * 100,
               i.children_permanent_job = (int.children_permanent_job/int.hh_number_children) * 100,
               i.children_temporary_job = (int.children_temporary_job/int.hh_number_children) * 100,
               i.children_casual_lobour = (int.children_casual_lobour/int.hh_number_children) * 100,
               i.children_own_bisuness = (int.children_own_bisuness/int.hh_number_children) * 100,
               i.lost_job = case_when(int.lost_job == 0 ~ "no",
                                        int.lost_job > 0 ~ "yes"),
               i.boys_early_marriege = (boys_early_marriege/int.hh_number_children_male) * 100,
               i.girls_early_marriege = (girls_early_marriege/int.hh_number_children_female) * 100,
               i.boys_work_outside_home = (int.boys_work_outside_home/int.hh_number_children_male) * 100,
               i.girls_work_outside_home = (int.girls_work_outside_home/int.hh_number_children_female) * 100,
               i.boys_anxiety = case_when(boys_anxiety == 0 ~ "no",
                                          boys_anxiety > 0 ~ "yes"), 
               i.girls_anxiety = case_when(girls_anxiety == 0 ~ "no",
                                           girls_anxiety > 0 ~ "yes"), 
               i.adults_anxiety = case_when(adults_anxiety == 0 ~ "no",
                                            adults_anxiety > 0 ~ "yes"),
               i.snfi_no_rooms = i.hh_composition_size/snfi_no_rooms,
               i.chronic_illiness_male = case_when(chronic_illiness_male == 0 ~ "no",
                                                   chronic_illiness_male > 0 ~ "yes"), 
               i.chronic_illiness_female = case_when(chronic_illiness_female == 0 ~ "no",
                                                     chronic_illiness_female > 0 ~ "yes"), 
               i.mental_heath_male = case_when(mental_heath_male == 0 ~ "no",
                                               mental_heath_male > 0 ~ "yes"), 
               i.mental_heath_female = case_when(mental_heath_female == 0 ~ "no",
                                                 mental_heath_female > 0 ~ "yes"),
               
        ) |> 
        select(-c(starts_with("int.")))
}


create_composite_indicators_health <- function(input_df) {
    input_df |> 
        mutate(int.disability = paste(difficulty_seeing, difficulty_hearing, difficulty_walking, 
                                   difficulty_remembering, difficulty_self_care, difficulty_communicating),
               i.disability = ifelse(str_detect(string = int.disability, pattern = "a_lot_of_difficulty|cannot_do_at_all"), "yes", "no"),
               
        ) |>  
        select(-c(starts_with("int.")))
}

