library(tidyverse)
library(msni2022)
library(openxlsx)

source("R/composite_indicators.R")

# check drivers of LSG
check_lsg_driver <- function(df, crit_ind_pattern = "crit_", none_crit_ind_pattern = "none_crit_"){
    set.seed(2023)
    df %>% 
        select(starts_with(crit_ind_pattern), starts_with(none_crit_ind_pattern)) %>% 
        replace(is.na(.), 0) %>% 
        mutate(indicator = names(.)[max.col(.)]) %>% 
        count(indicator) %>% 
        mutate(rel_freq = round(n/sum(n), digits = 2))}

check_msni_driver <- function(df, lsg_ind_pattern = "_lsg"){
    set.seed(2023)
    df %>% 
        select(ends_with(lsg_ind_pattern)) %>% 
        replace(is.na(.), 0) %>% 
        mutate(indicator = names(.)[max.col(.)]) %>% 
        count(indicator) %>% 
        mutate(rel_freq = round(n/sum(n), digits = 2))}

# clean data
data_path <- "inputs/clean_data_eth_msha_oromia.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000, sheet = "cleaned_main_data"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, sheet = "cleaned_main_data", col_types = c_types, na = "NA") |> 
    select(-starts_with("i.")) |> 
    create_composite_indicators() |> 
    mutate(across(.cols = starts_with("i."), .fns = ~ ifelse((is.infinite(.x)|is.nan(.x)), NA, .)))

education_loop <- readxl::read_excel(path = data_path, sheet = "cleaned_education_loop", na = "NA")

health_loop <- readxl::read_excel(path = data_path, sheet = "cleaned_health_loop", na = "NA")

df_hh_disability <- health_loop |> 
    create_composite_indicators_health() |> 
    group_by(`_submission__uuid`) |> 
    mutate(int.hh_disability = paste(i.disability, collapse = " "),
           i.hh_disability = ifelse(str_detect(string = int.hh_disability, pattern = "yes"), "yes", "no")) |> 
    filter(row_number() == 1) |> 
    select(uuid = `_submission__uuid`, i.hh_disability)

# Food security -----------------------------------------------------------

# calculated using: Fewsnet matrix (combining FCS, rCSI and HHS scores)
df_lsg_fs <- df_main_clean_data |> 
    mutate(int.crit_fs_ind1 = case_when(i.fc_matrix_cat %in% c("Phase 1") ~ 1,
                                        i.fc_matrix_cat %in% c("Phase 2") ~ 2,
                                        i.fc_matrix_cat %in% c("Phase 3") ~ 3,
                                        i.fc_matrix_cat %in% c("Phase 4") ~ 4,
                                        i.fc_matrix_cat %in% c("Phase 5") ~ 5)
    )

df_lsg_fs_extract <- df_lsg_fs |> select(uuid, contains("int.crit_"), contains("none_crit_"))
df_lsg_fs_extract$fs_lsg <- make_lsg(dataset = df_lsg_fs_extract, crit_to_4plus = c("int.crit_fs_ind1"))

df_drivers_fs <- check_lsg_driver(df = df_lsg_fs_extract, crit_ind_pattern = "int.crit_", none_crit_ind_pattern = "none_crit_")

# Cash markets and livelihoods --------------------------------------------

# LCSI : yes, no_had_no_need, no_exhausted, not_applicable
# i.lost_job
# hh_tot_income
# hh_basic_needs

df_lsg_cash <- df_main_clean_data |> 
    unite("int.lcsi", livh_stress_lcsi_1:livh_emerg_lcsi_3, sep = ":", remove = FALSE) |> 
    unite("int.lcsi_stress", livh_stress_lcsi_1:livh_stress_lcsi_4, sep = ":", remove = FALSE) |> 
    unite("int.lcsi_crisis", livh_crisis_lcsi_1:livh_crisis_lcsi_3, sep = ":", remove = FALSE) |> 
    unite("int.lcsi_emergency", livh_emerg_lcsi_1:livh_emerg_lcsi_3, sep = ":", remove = FALSE) |>
    rowwise() |> 
    mutate(across(any_of(c("income_casual_labour", "income_own_production", "income_govt_benefits", "income_rent", "income_remittances",
                           "income_loans_family_friends", "income_loans_community_members", "income_hum_assistance")), .fns = ~ifelse(.x > 0, 1, 0), .names = "int.{.col}")) |> 
    mutate(int.income_props_seasonal = sum(c_across(c(int.income_casual_labour:int.income_remittances)), na.rm = T),
           int.income_props_receiving = sum(c_across(c(int.income_loans_family_friends:int.income_hum_assistance)), na.rm = T)) |> 
    ungroup() |> 
    mutate(int.crit_cash_ind1_lcsi = case_when(str_detect(string = int.lcsi, pattern = "yes|no_exhausted", negate = TRUE) ~ 1,
                                               str_detect(string = int.lcsi_stress, pattern = "yes|no_exhausted") ~ 2,
                                               str_detect(string = int.lcsi_crisis, pattern = "yes|no_exhausted") ~ 3,
                                               str_detect(string = int.lcsi_emergency, pattern = "yes|no_exhausted") ~ 4),
    int.crit_cash_ind2_hh_lost_job = case_when(i.lost_job %in% c("no") ~ 1,
                                               i.lost_job %in% c("yes") ~ 3),
    int.crit_cash_ind3_hh_tot_income = case_when(if_any(.cols = c(income_salaried_work, income_business), .fns = ~ . > 0) ~ 1,
                                                 int.income_props_seasonal > 1 ~ 2,
                                                 int.income_props_seasonal == 1 ~ 3,
                                                 int.income_props_receiving >= 1 ~ 4),
    int.crit_cash_ind4_hh_basic_needs = case_when(hh_basic_needs %in% c("all") ~ 1,
                                                  hh_basic_needs %in% c("almost_all") ~ 2,
                                                  hh_basic_needs %in% c("some", "many") ~ 3,
                                                  hh_basic_needs %in% c("none", "few") ~ 4)
    )

df_lsg_cash_extract <- df_lsg_cash |> select(uuid, contains("int.crit_"), contains("none_crit_"))
df_lsg_cash_extract$cash_lsg <- make_lsg(dataset = df_lsg_cash_extract, crit_to_4 = c("int.crit_cash_ind1_lcsi", "int.crit_cash_ind3_hh_tot_income", "int.crit_cash_ind4_hh_basic_needs"), 
                                        crit_to_3 = c("int.crit_cash_ind2_hh_lost_job"))

df_drivers_cash <- check_lsg_driver(df = df_lsg_cash_extract, crit_ind_pattern = "int.crit_", none_crit_ind_pattern = "none_crit_")

# WASH --------------------------------------------------------------------

# wash_drinkingwatersource
# wash_watertime

# wash_waterfreq

# wash_sanitationfacility #combined with
# wash_sanitationsharing_yn #and
# wash_sanitationsharing_number

# wash_handwashingfacility #combined with 
# wash_handwashing_water_available #and
# wash_handwashing_soap_available

df_lsg_wash <- df_main_clean_data |> 
    mutate(int.crit_wash_ind1 = case_when(wash_drinkingwatersource %in% c("piped_into_dwelling", "piped_into_compound") ~ 1,
                                          (wash_drinkingwatersource %in% c("piped_to_neighbour", "public_tap", "borehole", 
                                                                           "protected_well", "protected_spring", 
                                                                           "rain_water_collection", "tanker_trucks", 
                                                                           "cart_with_tank_drum", "water_kiosk", 
                                                                           "bottled_water", "sachet_water") & wash_watertime <= 30) ~ 2,
                                          (wash_drinkingwatersource %in% c("piped_to_neighbour", "public_tap", "borehole", 
                                                                           "protected_well", "protected_spring", 
                                                                           "rain_water_collection", "tanker_trucks", 
                                                                           "cart_with_tank_drum", "water_kiosk", 
                                                                           "bottled_water", "sachet_water") & wash_watertime > 30) ~ 3,
                                          wash_drinkingwatersource %in% c("unprotected_well", "unprotected_spring") ~ 4,
                                          wash_drinkingwatersource %in% c("surface_water") ~ 5),
           int.crit_wash_ind2 = case_when(wash_waterfreq %in% c("never") ~ 1,
                                          wash_waterfreq %in% c("rarely") ~ 2,
                                          wash_waterfreq %in% c("sometimes") ~ 3,
                                          wash_waterfreq %in% c("often") ~ 4,
                                          wash_waterfreq %in% c("always") ~ 5),
           int.crit_wash_ind3 = case_when((wash_sanitationfacility %in% c("flush_to_piped", "flush_to_septic", "flush_to_pit", "pit_latrine_with_slab", "composting_toilet") & 
                                               wash_sanitationsharing_yn %in% c("no")) ~ 1,
                                          (wash_sanitationfacility %in% c("flush_to_piped", "flush_to_septic", "flush_to_pit", "pit_latrine_with_slab", "composting_toilet") & 
                                               wash_sanitationsharing_number <= 20) ~ 2,
                                          (wash_sanitationfacility %in% c("flush_to_piped", "flush_to_septic", "flush_to_pit", "pit_latrine_with_slab", "composting_toilet") & 
                                               (wash_sanitationsharing_number > 20 & wash_sanitationsharing_number <= 50)) ~ 3,
                                          (wash_sanitationfacility %in% c("flush_to_open", "flush_to_elsewhere", "flush_to_dnt_where", "pit_latrine_without_slab", "plastic_bag", "buket", "hanging_toiletlatrine") |
                                               (wash_sanitationfacility %in% c("flush_to_piped", "flush_to_septic", "flush_to_pit", "pit_latrine_with_slab", "composting_toilet") & 
                                               wash_sanitationsharing_number > 50)) ~ 4,
                                          wash_sanitationfacility %in% c("no_facility") ~ 5),
           int.crit_wash_ind4 = case_when((wash_handwashingfacility %in% c("fixed_or_mobile_handwashing") &
                                               wash_handwashing_water_available %in% c("water_available") & wash_handwashing_soap_available %in% c("soap_available")) ~ 1,
                                          (wash_handwashingfacility %in% c("no_handwashing") |
                                               wash_handwashing_water_available %in% c("water_not_available") | wash_handwashing_soap_available %in% c("soap_not_available")) ~ 2)
    )

df_lsg_wash_extract <- df_lsg_wash |> select(uuid, contains("int.crit_"), contains("none_crit_"))
df_lsg_wash_extract$wash_lsg <- make_lsg(dataset = df_lsg_wash_extract, crit_to_4plus = c("int.crit_wash_ind1", "int.crit_wash_ind2", "int.crit_wash_ind3"), 
                                        crit_to_3 = c("int.crit_wash_ind4"))

df_drivers_wash <- check_lsg_driver(df = df_lsg_wash_extract, crit_ind_pattern = "int.crit_", none_crit_ind_pattern = "none_crit_")

# Health ------------------------------------------------------------------

# healthcare_needed
# healthcare_received

# health_last3months_barriers
# health_last3months_barriers_healthcare
health_options_cols <- c("no_functional_health_facility_nearby", "specific_medicine", "long_waiting_time", 
                         "could_not_afford_cost_treatment", "could_not_afford_consultation_cost", 
                         "could_not_afford_transportation", "health_facility_is_too_far_away", 
                         "disability_prevents_access_to_health_facility", "no_means_of_transport", 
                         "unsafe_security_at_health_facility", "unsafe_security_while_travelling_to_health_facility", 
                         "didnt_receive_correct_meducations", "not_trained_staff_at_health_facility", "not_enough_staff_at_health_facility", 
                         "wanted_to_wait_and_see_if_problem_got_better_on_its_own", "fear_distrust_of_health_workers", "couldnt_take_time_off_work", 
                         "language_barriers_or_issues", "minority_clan_affilation_prevents_access_to_health_facility")

df_lsg_health <- df_main_clean_data |> 
    left_join(df_hh_disability) |> 
    mutate(int.crit_health_ind1 = case_when(healthcare_needed %in% c("no") ~ 1,
                                            healthcare_needed %in% c("yes") & healthcare_received %in% c("yes") ~ 2,
                                            ((healthcare_needed %in% c("yes") & healthcare_received %in% c("no")) | i.hh_disability %in% c("yes")) ~ 3),
           int.crit_health_ind1 = ifelse(((healthcare_needed %in% c("yes") & healthcare_received %in% c("no")) & i.hh_disability %in% c("yes")), 4, int.crit_health_ind1), # did not work inside case when because of the | operation
           int.none_crit_health_ind1 = case_when(health_last3months_barriers %in% c("no_barriers_faced") ~ 0,
                                                 str_detect(string = health_last3months_barriers, pattern = paste0(health_options_cols, collapse = "|")) ~ 1),
           int.none_crit_health_ind2 = case_when(health_last3months_barriers_healthcare %in% c("no_barriers_faced") ~ 0,
                                                 str_detect(string = health_last3months_barriers_healthcare, pattern = paste0(health_options_cols, collapse = "|")) ~ 1)
    ) |> 
    rowwise() |> 
    mutate(int.means_none_crit_health = round(mean(c_across(starts_with("int.none_crit_health")), na.rm = FALSE), digits = 2)
    ) |> 
    ungroup() |>
    mutate(none_crit_health = case_when(between(int.means_none_crit_health, 0, 0.33) ~ 1,
                                        between(int.means_none_crit_health, 0.34, 0.66) ~ 2,
                                        between(int.means_none_crit_health, 0.67, 1) ~ 3)
           )

df_lsg_health_extract <- df_lsg_health |> select(uuid, contains("int.crit_"), contains("none_crit_"))
df_lsg_health_extract$health_lsg <- make_lsg(dataset = df_lsg_health_extract, crit_to_4 = c("int.crit_health_ind1"),
                                            non_crit = c("none_crit_health"))

df_drivers_health <- check_lsg_driver(df = df_lsg_health_extract, crit_ind_pattern = "int.crit_", none_crit_ind_pattern = "none_crit_")

# Shelter & NFI -----------------------------------------------------------

# snfi_sheltertype #combined with
# snfi_shelter_issues #and
# snfi_occupancy_arrangement

adequate_shelter_cols <- c("plastic_sheet", "cgi_sheet", "mud_and_stick_wall", "stone_or_birk")
inadequate_shelter_cols <- c("buul", "tent", "emergency_shelter", "hybrid_or_transitional_shelters", "stick_wall", "collective_shelter")

# snfi_living_space_cooking
# snfi_living_space_sleeping
# snfi_living_space_storing_food_water
# snfi_living_space_electricity

df_lsg_shelter <- df_main_clean_data |> 
    mutate(int.crit_shelter_ind1 = case_when(snfi_sheltertype %in% adequate_shelter_cols &
                                                 str_detect(string = snfi_shelter_issues, pattern = "none") &
                                                 snfi_occupancy_arrangement %in% c("ownership", "rented") ~ 1,
                                             
                                             ((snfi_sheltertype %in% inadequate_shelter_cols)|(snfi_sheltertype %in% adequate_shelter_cols)) &
                                                 (str_detect(string = snfi_shelter_issues, pattern = paste0(c("minor_damage_roof", "major_damage_roof", "damage_floors", "damage_walls", "damage_windows"), collapse = "|")) |
                                                      str_detect(string = snfi_shelter_issues, pattern = paste0(c("lack_privacy", "lack_space", "lack_of_insulation", "limited_ventilation", "leaks_during_rain", "unable_to_lock", "lack_light"), collapse = "|")) |
                                                      snfi_occupancy_arrangement %in% c("hosted", "squatting")) ~ 2
    ),
    int.crit_shelter_ind1 = case_when((snfi_sheltertype %in% inadequate_shelter_cols) &
                                          (str_detect(string = snfi_shelter_issues, pattern = paste0(c("minor_damage_roof", "major_damage_roof", "damage_floors", "damage_walls", "damage_windows"), collapse = "|")) |
                                               str_detect(string = snfi_shelter_issues, pattern = paste0(c("lack_privacy", "lack_space", "lack_of_insulation", "limited_ventilation", "leaks_during_rain", "unable_to_lock", "lack_light"), collapse = "|")) |
                                               snfi_occupancy_arrangement %in% c("hosted", "squatting")) ~ 3,
                                      
                                      (snfi_sheltertype %in% c("none_or_sleep_in_the_open") |
                                           str_detect(string = snfi_shelter_issues, pattern = "collapse_or_unsafe")) ~ 5,
                                      TRUE ~ int.crit_shelter_ind1
    ),
    int.none_crit_shelter_ind1 = case_when(snfi_living_space_cooking %in% c("no_issues", "can_do_with_issues") ~ 0,
                                           snfi_living_space_cooking %in% c("cannot_do") ~ 1),
    int.none_crit_shelter_ind2 = case_when(snfi_living_space_sleeping %in% c("no_issues", "can_do_with_issues") ~ 0,
                                           snfi_living_space_sleeping %in% c("cannot_do") ~ 1),
    int.none_crit_shelter_ind3 = case_when(snfi_living_space_storing_food_water %in% c("no_issues", "can_do_with_issues") ~ 0,
                                           snfi_living_space_storing_food_water %in% c("cannot_do") ~ 1),
    int.none_crit_shelter_ind4 = case_when(snfi_living_space_electricity %in% c("no_issues", "can_do_with_issues") ~ 0,
                                           snfi_living_space_electricity %in% c("cannot_do") ~ 1)
    ) |> 
    rowwise() |> 
    mutate(int.means_none_crit_shelter = round(mean(c_across(starts_with("int.none_crit_shelter")), na.rm = FALSE), digits = 2)
    ) |> 
    ungroup() |>
    mutate(none_crit_shelter = case_when(between(int.means_none_crit_shelter, 0, 0.33) ~ 1,
                                        between(int.means_none_crit_shelter, 0.34, 0.66) ~ 2,
                                        between(int.means_none_crit_shelter, 0.67, 1) ~ 3)
    )

df_lsg_shelter_extract <- df_lsg_shelter |> select(uuid, contains("int.crit_"), contains("none_crit_"))
df_lsg_shelter_extract$shelter_lsg <- make_lsg(dataset = df_lsg_shelter_extract, crit_to_4plus = c("int.crit_shelter_ind1"),
                                              non_crit = c("none_crit_shelter"))

df_drivers_shelter <- check_lsg_driver(df = df_lsg_shelter_extract, crit_ind_pattern = "int.crit_", none_crit_ind_pattern = "none_crit_")

# Education ---------------------------------------------------------------

## loop
# edu_attendance #combined with
# edu_non_access_reason
# edu_safe_environment #combined with
# edu_learning_conditions

# main data
# edu_dropout_due_drought
# edu_dropout_due_drought_yes

edu_learning_conditions_reasons_cols <- c("overcrowding", "curriculum_not_adapted", "lack_teachers", "lack_qualified_staff", "lack_materials", "poor_wash", "discrimination", "displacement", "drought_related_challanges", "curriculum_not_adapted_remote", "internt_isnt_reliable", "equipment_sahred_with_others")
edu_safe_environment_reasons_cols <- c("security_concerns_travel", "attacks", "armed_groups", "gbv", "verbal_bullying", "physical_bullying", "physical_punishment", "unsafe_infrastructure", "lack_staff_psychosocial_support", "lack_referral_mechanism", "discrimination")
child_support_cols <- c("excemption_from_school_fees", "cash_for_school_supplies", "cash_for_school_transportation_to_schhol", "cash_for_children_food", "cash_to_offset_opportunity", "direct_provision_of_school", "direct_provision_of_transportation", "direct_provision_of_water_for_children", "direct_provision_of_food_for_children", "healthcare_at_school", "provision_of_alterntavive_learning", "assistance_for_children_of_minority_groups", "Ȁ")

df_lsg_edu_loop <- education_loop |> 
    group_by(`_submission__uuid`) |> 
    mutate(int.hh_loop_size = n(),
           int.edu_attendance = paste(edu_attendance, collapse = " "),
           int.edu_attendance_yes_count = str_count(int.edu_attendance, pattern = "yes"),
           int.edu_non_access_reason = supporteR::mode_with_out_nc(paste(edu_non_access_reason)),
           int.edu_safe_environment = case_when(!is.na(edu_safe_environment) & str_detect(string = paste(edu_safe_environment, collapse = " "), pattern = "no") ~ "no",
                                                !is.na(edu_safe_environment) & !str_detect(string = paste(edu_safe_environment, collapse = " "), pattern = "no") ~ "yes"),
           int.edu_learning_conditions = case_when(!is.na(edu_learning_conditions) & str_detect(string = paste(edu_learning_conditions, collapse = " "), pattern = "no") ~ "no",
                                                   !is.na(edu_learning_conditions) & !str_detect(string = paste(edu_learning_conditions, collapse = " "), pattern = "no") ~ "yes"),
           int.edu_learning_conditions_reasons = paste(edu_learning_conditions_reasons, collapse = " "),
           int.edu_safe_environment_reasons = paste(edu_safe_environment_reasons, collapse = " ")
    ) |> 
    filter(row_number() == 1) |> 
    ungroup() |> 
    mutate(int.crit_edu_ind1 = case_when(int.hh_loop_size == int.edu_attendance_yes_count ~ 1,
                                         int.hh_loop_size > int.edu_attendance_yes_count ~ 3),
           int.crit_edu_ind1 = case_when(int.hh_loop_size > int.edu_attendance_yes_count &
                                             int.edu_non_access_reason %in% c("protection_risks", "child_marriage") ~ 4,
                                         TRUE ~ int.crit_edu_ind1),
           int.crit_edu_ind2 = case_when(int.edu_safe_environment %in% c("yes") &
                                             int.edu_learning_conditions %in% c("yes") ~ 1,
                                         int.edu_learning_conditions %in% c("no") &
                                             str_detect(string = int.edu_learning_conditions_reasons, pattern = paste0(edu_learning_conditions_reasons_cols, collapse = "|")) ~ 2,
                                         int.edu_safe_environment %in% c("no")&
                                             str_detect(string = int.edu_safe_environment_reasons, pattern = paste0(edu_safe_environment_reasons_cols, collapse = "|")) ~ 4)
    ) |> 
    select(uuid = `_submission__uuid`, starts_with("int."))

df_lsg_edu <- df_main_clean_data |> 
    mutate(int.none_crit_edu_ind1 = case_when(edu_dropout_due_drought %in% c("no") ~ 0,
                                          edu_dropout_due_drought %in% c("yes") ~ 1),
           int.none_crit_edu_ind2 = case_when(str_detect(string = edu_dropout_due_drought_yes, pattern = "no_support_needed") ~ 0,
                                          str_detect(string = edu_dropout_due_drought_yes, pattern = paste0(child_support_cols, collapse = "|")) ~ 1)
    ) |> 
    left_join(df_lsg_edu_loop, by = "uuid") |> 
    rowwise() |> 
    mutate(int.means_none_crit_edu = round(mean(c_across(starts_with("int.none_crit_edu")), na.rm = FALSE), digits = 2)
    ) |> 
    ungroup() |>
    mutate(none_crit_edu = case_when(between(int.means_none_crit_edu, 0, 0.33) ~ 1,
                                         between(int.means_none_crit_edu, 0.34, 0.66) ~ 2,
                                         between(int.means_none_crit_edu, 0.67, 1) ~ 3)
    )

df_lsg_edu_extract <- df_lsg_edu |> select(uuid, contains("int.crit_"), contains("none_crit_"))
df_lsg_edu_extract$edu_lsg <- make_lsg(dataset = df_lsg_edu_extract, crit_to_4 = c("int.crit_edu_ind1", "int.crit_edu_ind2"),
                                       non_crit = c("none_crit_edu"))

df_drivers_edu <- check_lsg_driver(df = df_lsg_edu_extract, crit_ind_pattern = "int.crit_", none_crit_ind_pattern = "none_crit_")


# Protection --------------------------------------------------------------

# hh_separated
# hh_reason_left
# hh_early_marriege
# prot_id

df_lsg_prot <- df_main_clean_data |> 
    mutate(int.crit_prot_ind1 = case_when(hh_separated %in% c("no") ~ 1,
                                          hh_separated %in% c("yes") &
                                              str_detect(string = hh_reason_left, pattern = "living_with_relatives|study_leave") ~ 3,
                                       hh_separated %in% c("yes") &
                                           str_detect(string = hh_reason_left, pattern = "married|seek_employment") ~ 4,
                                       hh_separated %in% c("yes") &
                                           str_detect(string = hh_reason_left, pattern = "engage_army_group|abducted|missing") ~ 5 ),
    int.crit_prot_ind2 = case_when(hh_early_marriege %in% c("no") ~ 1,
                                       hh_early_marriege %in% c("yes") ~ 4),
    int.crit_prot_ind3 = case_when(prot_id %in% c("yes_all_have_id") ~ 1,
                                       prot_id %in% c("atleast_child_havent_id", "all_children_havent_id") ~ 2,
                                       prot_id %in% c("atleast_adult_havent_id", "atleast_child_and_adult_havent_id", "no_all_havent_id", "no_all_adults_havent_id") ~ 3),
    int.none_crit_prot_ind1 = case_when(!is.na(child_services_available) ~ 0,
                                    is.na(child_services_available) ~ 1),
    int.none_crit_prot_ind2 = case_when(hh_anxiety %in% c("no") ~ 0,
                                    hh_anxiety %in% c("yes") ~ 1),
    int.none_crit_prot_ind3 = case_when(work_outside_home %in% c("no") ~ 0,
                                    work_outside_home %in% c("yes") ~ 1)
    ) |> 
    rowwise() |> 
    mutate(int.means_none_crit_prot = round(mean(c_across(starts_with("int.none_crit_prot")), na.rm = FALSE), digits = 2)
    ) |> 
    ungroup() |>
    mutate(none_crit_prot = case_when(between(int.means_none_crit_prot, 0, 0.33) ~ 1,
                                     between(int.means_none_crit_prot, 0.34, 0.66) ~ 2,
                                     between(int.means_none_crit_prot, 0.67, 1) ~ 3)
    )

df_lsg_prot_extract <- df_lsg_prot |> select(uuid, contains("int.crit_"), contains("none_crit_"))
df_lsg_prot_extract$prot_lsg <- make_lsg(dataset = df_lsg_prot_extract, crit_to_4plus = c("int.crit_prot_ind1"), crit_to_4 = c("int.crit_prot_ind2"),
                                         crit_to_3 = c("int.crit_prot_ind3"), non_crit = c("none_crit_prot"))

df_drivers_prot <- check_lsg_driver(df = df_lsg_prot_extract, crit_ind_pattern = "int.crit_", none_crit_ind_pattern = "none_crit_")

# join all data -----------------------------------------------------------

df_all_lsg_datasets_list <- list(df_lsg_fs_extract, df_lsg_cash_extract, 
                            df_lsg_wash_extract, df_lsg_health_extract, df_lsg_shelter_extract,
                            df_lsg_edu_extract, df_lsg_prot_extract)

df_all_lsg_datasets <- purrr::reduce(.f = left_join, .x = df_all_lsg_datasets_list)


# calculate msni ----------------------------------------------------------

df_msni <- df_all_lsg_datasets |> 
    rowwise() |> 
    mutate(msni = max(c_across(ends_with("_lsg")), na.rm = TRUE)
    ) |> 
    ungroup() |> 
    mutate(i.fs_sl3_above = case_when(fs_lsg < 3 ~ "No LSG identified", fs_lsg >= 3 ~ "LSG identified"),
           i.cash_sl3_above = case_when(cash_lsg < 3 ~ "No LSG identified", cash_lsg >= 3 ~ "LSG identified"),
           i.wash_sl3_above = case_when(wash_lsg < 3 ~ "No LSG identified", wash_lsg >= 3 ~ "LSG identified"),
           i.health_sl3_above = case_when(health_lsg < 3 ~ "No LSG identified", health_lsg >= 3 ~ "LSG identified"),
           i.shelter_sl3_above = case_when(shelter_lsg < 3 ~ "No LSG identified", shelter_lsg >= 3 ~ "LSG identified"),
           i.edu_sl3_above = case_when(edu_lsg < 3 ~ "No LSG identified", edu_lsg >= 3 ~ "LSG identified"),
           i.prot_sl3_above = case_when(prot_lsg < 3 ~ "No LSG identified", prot_lsg >= 3 ~ "LSG identified"),
           i.msni_sl3_above = case_when(msni < 3 ~ "No LSG identified", msni >= 3 ~ "LSG identified"),
           i.fs_sl4_above = case_when(fs_lsg < 4 ~ "No LSG identified", fs_lsg >= 4 ~ "LSG identified"),
           i.cash_sl4_above = case_when(cash_lsg < 4 ~ "No LSG identified", cash_lsg >= 4 ~ "LSG identified"),
           i.wash_sl4_above = case_when(wash_lsg < 4 ~ "No LSG identified", wash_lsg >= 4 ~ "LSG identified"),
           i.health_sl4_above = case_when(health_lsg < 4 ~ "No LSG identified", health_lsg >= 4 ~ "LSG identified"),
           i.shelter_sl4_above = case_when(shelter_lsg < 4 ~ "No LSG identified", shelter_lsg >= 4 ~ "LSG identified"),
           i.edu_sl4_above = case_when(edu_lsg < 4 ~ "No LSG identified", edu_lsg >= 4 ~ "LSG identified"),
           i.prot_sl4_above = case_when(prot_lsg < 4 ~ "No LSG identified", prot_lsg >= 4 ~ "LSG identified"),
           i.msni_sl4_above = case_when(msni < 4 ~ "No LSG identified", msni >= 4 ~ "LSG identified")
           ) |> 
    mutate(across(.cols = matches("_lsg$"), 
                  .fns = ~ifelse(.x >= 3, cur_column(), NA_character_),
                  .names = "profile.{.col}")) |>
    unite("lsg_profiles", matches("^profile."), sep = " ", remove = T , na.rm = TRUE) |> 
    mutate(lsg_count = case_when(msni < 3 ~ 0,
                                    msni >= 3 ~ str_count(string = lsg_profiles, pattern = boundary("word")),
                                    TRUE ~ NA),
           lsg_profiles = case_when(msni < 3 ~ "No needs profile",
                                    msni >= 3 ~ lsg_profiles,
                                    TRUE ~ NA_character_))

df_drivers_msni <- check_msni_driver(df = df_msni, lsg_ind_pattern = "_lsg")


# export msni dataset -----------------------------------------------------

write_csv(df_msni, paste0("outputs/", butteR::date_file_prefix(), "_lsg_and_msni_data_oromia.csv"), na="")
write_csv(df_msni, "outputs/lsg_and_msni_data_oromia.csv", na="")

