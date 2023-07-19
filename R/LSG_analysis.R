library(tidyverse)
library(openxlsx)

source("R/composite_indicators.R")
# clean data
data_path <- "inputs/clean_data_eth_msha_oromia.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000, sheet = "cleaned_main_data"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, sheet = "cleaned_main_data", col_types = c_types, na = "NA") |> 
    select(-starts_with("i.")) |> 
    create_composite_indicators() |> 
    mutate(across(.cols = starts_with("i."), .fns = ~ ifelse((is.infinite(.x)|is.nan(.x)), NA, .)))

education_loop <- readxl::read_excel(path = data_path, sheet = "cleaned_education_loop", na = "NA")

# Food security -----------------------------------------------------------

# calculated using: Fewsnet matrix (combining FCS, rCSI and HHS scores)



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
    mutate(crit_score_cash = case_when(str_detect(string = int.lcsi, pattern = "yes|no_exhausted", negate = TRUE) & 
                                           i.lost_job %in% c("no") &
                                           if_any(.cols = c(income_salaried_work, income_business), .fns = ~ . > 0) &
                                           hh_basic_needs %in% c("all") ~ "1",
                                       
                                       str_detect(string = int.lcsi_stress, pattern = "yes|no_exhausted") &
                                           str_detect(string = int.lcsi_crisis, pattern = "yes|no_exhausted", negate = TRUE) &
                                           str_detect(string = int.lcsi_emergency, pattern = "yes|no_exhausted", negate = TRUE) &
                                           int.income_props_seasonal > 1 &
                                           hh_basic_needs %in% c("almost_all") ~ "2",
                                       
                                       str_detect(string = int.lcsi_crisis, pattern = "yes|no_exhausted") &
                                           str_detect(string = int.lcsi_emergency, pattern = "yes|no_exhausted", negate = TRUE) &
                                           i.lost_job %in% c("yes") &
                                           int.income_props_seasonal == 1 &
                                           hh_basic_needs %in% c("some", "many") ~ "3",
                                       
                                       str_detect(string = int.lcsi_emergency, pattern = "yes|no_exhausted") &
                                           int.income_props_receiving >= 1 &
                                           hh_basic_needs %in% c("none", "few") ~ "4",
                                       ))


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
    mutate(crit_score_wash = case_when(wash_drinkingwatersource %in% c("piped_into_dwelling", "piped_into_compound") &
                                           wash_waterfreq %in% c("never") &
                                       (wash_sanitationfacility %in% c("flush_to_piped", "flush_to_septic", "flush_to_pit", "flush_to_dnt_where", "pit_latrine_with_slab", "composting_toilet") & 
                                            wash_sanitationsharing_yn %in% c("no")) &
                                          (wash_handwashingfacility %in% c("fixed_or_mobile_handwashing") &
                                               wash_handwashing_water_available %in% c("water_available") & wash_handwashing_soap_available %in% c("soap_available")) ~ "1",
                                       
                                       (wash_drinkingwatersource %in% c("piped_to_neighbour", "public_tap", "borehole", "protected_well", "protected_spring", "rain_water_collection", "tanker_trucks", "cart_with_tank_drum", "water_kiosk", "bottled_water", "sachet_water") &
                                           wash_watertime <= 30) &
                                           wash_waterfreq %in% c("rarely") &
                                           (wash_sanitationfacility %in% c("flush_to_piped", "flush_to_septic", "flush_to_pit", "flush_to_dnt_where", "pit_latrine_with_slab", "composting_toilet") & 
                                                wash_sanitationsharing_number <= 20) &
                                           (wash_handwashingfacility %in% c("no_handwashing") |
                                                wash_handwashing_water_available %in% c("water_not_available") | wash_handwashing_soap_available %in% c("soap_not_available")) ~ "2",
                                       
                                       (wash_drinkingwatersource %in% c("piped_to_neighbour", "public_tap", "borehole", "protected_well", "protected_spring", "rain_water_collection", "tanker_trucks", "cart_with_tank_drum", "water_kiosk", "bottled_water", "sachet_water") &
                                            wash_watertime > 30) &
                                           wash_waterfreq %in% c("sometimes") &
                                           (wash_sanitationfacility %in% c("flush_to_piped", "flush_to_septic", "flush_to_pit", "flush_to_dnt_where", "pit_latrine_with_slab", "composting_toilet") & 
                                                wash_sanitationsharing_number > 20) ~ "3",
                                       
                                       wash_drinkingwatersource %in% c("piped_to_neighbour", "public_tap", "borehole", "protected_well", "protected_spring", "rain_water_collection", "tanker_trucks", "cart_with_tank_drum", "water_kiosk", "bottled_water", "sachet_water") &
                                           wash_waterfreq %in% c("often") &
                                           (wash_sanitationfacility %in% c("flush_to_piped", "flush_to_septic", "flush_to_pit", "flush_to_open", "flush_to_elsewhere", "flush_to_dnt_where", "pit_latrine_with_slab", "pit_latrine_without_slab", "composting_toilet", "plastic_bag", "buket", "hanging_toiletlatrine") & 
                                                wash_sanitationsharing_number > 50) ~ "4",
                                       
                                       wash_drinkingwatersource %in% c("surface_water") &
                                           wash_waterfreq %in% c("always") &
                                           wash_sanitationfacility %in% c("no_facility") ~ "4+",
))


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
    mutate(crit_score_health = case_when(healthcare_needed %in% c("no") ~ "1",
                                         healthcare_needed %in% c("yes") & healthcare_received %in% c("yes") ~ "2",
                                         ((healthcare_needed %in% c("yes") & healthcare_received %in% c("no")) | i.disability %in% c("yes")) ~ "3",
                                         ((healthcare_needed %in% c("yes") & healthcare_received %in% c("no")) & i.disability %in% c("yes")) ~ "4",
    ),
    non_crit_score_health = case_when(health_last3months_barriers %in% c("no_barriers_faced") & 
                                          health_last3months_barriers_healthcare %in% c("no_barriers_faced")~ "0",
                                  str_detect(string = health_last3months_barriers, pattern = paste0(health_options_cols, collapse = "|")) &
                                      str_detect(string = health_last3months_barriers_healthcare, pattern = paste0(health_options_cols, collapse = "|"))~ "1"
    )
    )


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
    mutate(crit_score_shelter = case_when(snfi_sheltertype %in% adequate_shelter_cols &
                                              str_detect(string = snfi_shelter_issues, pattern = "none") &
                                              snfi_occupancy_arrangement %in% c("ownership", "rented") ~ "1",
                                          
                                          ((snfi_sheltertype %in% inadequate_shelter_cols &)|(snfi_sheltertype %in% adequate_shelter_cols &)) &
                                              (str_detect(string = snfi_shelter_issues, pattern = paste0("(?=.*", paste0(c("minor_damage_roof", "major_damage_roof", "damage_floors", "damage_walls"), collapse = ")(?=.*"), ")")) |
                                                   str_detect(string = snfi_shelter_issues, pattern = paste0("(?=.*", paste0(c("lack_privacy", "lack_space", "lack_of_insulation", "limited_ventilation", "leaks_during_rain", "unable_to_lock", "lack_light"), collapse = ")(?=.*"), ")")) |
                                              snfi_occupancy_arrangement %in% c("hosted", "squatting")) ~ "2",
                                          
                                          (snfi_sheltertype %in% inadequate_shelter_cols &) &
                                              (str_detect(string = snfi_shelter_issues, pattern = paste0("(?=.*", paste0(c("minor_damage_roof", "major_damage_roof", "damage_floors", "damage_walls"), collapse = ")(?=.*"), ")")) |
                                                   str_detect(string = snfi_shelter_issues, pattern = paste0("(?=.*", paste0(c("lack_privacy", "lack_space", "lack_of_insulation", "limited_ventilation", "leaks_during_rain", "unable_to_lock", "lack_light"), collapse = ")(?=.*"), ")")) |
                                                   snfi_occupancy_arrangement %in% c("hosted", "squatting")) ~ "3",
                                          
                                          (snfi_sheltertype %in% c("none_or_sleep_in_the_open") |
                                              str_detect(string = snfi_shelter_issues, pattern = "collapse_or_unsafe")) ~ "4+",
    ),
    non_crit_score_shelter = case_when(snfi_living_space_cooking %in% c("no_issues", "can_do_with_issues") &
                                           snfi_living_space_sleeping %in% c("no_issues", "can_do_with_issues") &
                                           snfi_living_space_storing_food_water %in% c("no_issues", "can_do_with_issues") &
                                           snfi_living_space_electricity %in% c("no_issues", "can_do_with_issues")
                                            ~ "0",
                                       snfi_living_space_cooking %in% c("cannot_do") &
                                           snfi_living_space_sleeping %in% c("cannot_do") &
                                           snfi_living_space_storing_food_water %in% c("cannot_do") &
                                           snfi_living_space_electricity %in% c("cannot_do")
                                           ~ "1"
                                      
    )
    )
# still need clarification on level 2 and level 3


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
    group_by(uuid) |> 
    mutate(int.hh_loop_size = n(),
           int.edu_attendance = paste(edu_attendance, collapse = " "),
           int.edu_attendance_yes_count = str_count(int.edu_attendance, pattern = "yes")) |> 
    ungroup() |> 
    mutate(crit_score_edu = case_when(int.hh_loop_size == int.edu_attendance_yes_count &
                                          edu_safe_environment %in% c("yes") &
                                          edu_learning_conditions %in% c("yes") ~ "1",
                                       
                                      edu_safe_environment %in% c("no") &
                                          str_detect(string = edu_learning_conditions_reasons, pattern = paste0(edu_learning_conditions_reasons_cols, collapse = "|")) ~ "2",
                                       
                                      int.hh_loop_size > int.edu_attendance_yes_count ~ "3",
                                       
                                      int.hh_loop_size > int.edu_attendance_yes_count &
                                          edu_non_access_reason %in% c("protection_risks", "child_marriage") &
                                          edu_safe_environment %in% c("no")&
                                          str_detect(string = edu_safe_environment_reasons, pattern = paste0(edu_safe_environment_reasons_cols, collapse = "|")) ~ "4"
    )
    )

df_lsg_edu <- df_main_clean_data |> 
    mutate(non_crit_score_edu = case_when(edu_dropout_due_drought %in% c("no") &
                                              str_detect(string = edu_dropout_due_drought_yes, pattern = "no_support_needed") ~ "0",
                                          edu_dropout_due_drought %in% c("yes") &
                                              str_detect(string = edu_dropout_due_drought_yes, pattern = paste0(child_support_cols, collapse = "|")) ~ "1",
                                       
                                      
    )
    )



# Protection --------------------------------------------------------------

# hh_separated
# hh_reason_left
# hh_early_marriege
# prot_id


df_lsg_prot <- df_main_clean_data |> 
    mutate(crit_score_prot = case_when(hh_separated %in% c("no") &
                                           hh_early_marriege %in% c("no") &
                                           prot_id %in% c("yes_all_have_id") ~ "1",
                                          
                                       prot_id %in% c("atleast_child_havent_id", "all_children_havent_id") ~ "2",
                                          
                                       prot_id %in% c("atleast_adult_havent_id", "atleast_child_and_adult_havent_id", "no_all_havent_id", "no_all_adults_havent_id") ~ "3",
                                          
                                       hh_separated %in% c("yes") &
                                           str_detect(string = hh_reason_left, pattern = "married|seek_employment") &
                                           hh_early_marriege %in% c("yes")
                                                ~ "4",
                                    
                                       hh_separated %in% c("yes") &
                                           str_detect(string = hh_reason_left, pattern = "engage_army_group|abducted|missing")
                                                ~ "4+"
    ),
    non_crit_score_prot = case_when(!is.na(child_services_available)  &
                                        hh_anxiety %in% c("no") &
                                        work_outside_home %in% c("no") 
                                       ~ "0",
                                    is.na(child_services_available) &
                                        hh_anxiety %in% c("yes") &
                                        work_outside_home %in% c("yes") 
                                       ~ "1"
                                       
    )
    )

