library(tidyverse)

load("data/data-cleaned/data-cleaned.rda")

predictor_list <- data_patient_complete |>
  select(
    patient_age, patient_sex, patient_weight, baseline_CLCR,
    dept_ER, dept_ICU, baseline_HGB, baseline_WBC, baseline_PLT,
    LZD_duration, LZD_route, starts_with("invasive"), starts_with("comorb") & -comorb_hematological,
    starts_with("infect"), comed_aspirin, comed_diclofenac, comed_ibuprofen,
    comed_paracetamol, comed_penicillin, comed_cepha, comed_carbapenem,
    comed_cotrimoxazol, comed_vancomycin, comed_levofloxacin, comed_teicoplanin,
    comed_ethambutol, comed_pyrazinamid, comed_rifampin, comed_heparin,
    comed_clopidogrel, comed_enoxaparin, comed_dexamethason, comed_amiodaron,
    comed_furosemid, comed_haloperidol, comed_valproic
    # select only comeds which univariate odds ratio can be calculated
  ) |>
  colnames()

data_patient_transformed <- data_patient_complete |>
  mutate(
    baseline_HGB_per_10 = baseline_HGB / 10,
    baseline_WBC_per_10 = baseline_WBC / 10,
    baseline_PLT_per_10 = baseline_PLT / 10,
    baseline_CLCR_per_10 = baseline_CLCR / 10,
    patient_age_65 = patient_age >= 65,
    patient_age_60 = patient_age >= 60,
    patient_weight_50 = patient_weight < 50,
    baseline_CLCR_60 = baseline_CLCR < 60,
    baseline_CLCR_30 = baseline_CLCR < 30,
    baseline_PLT_200 = baseline_PLT < 200,
    baseline_PLT_150 = baseline_PLT < 150,
    LZD_duration_7 = LZD_duration >= 7,
    LZD_duration_10 = LZD_duration >= 10,
    LZD_duration_14 = LZD_duration >= 14,
    LZD_route_IV = LZD_route == "IV"
  )

predictor_transformed <- data_patient_transformed |>
  select(
    patient_age, patient_age_60, patient_age_65,
    patient_sex, patient_weight, patient_weight_50,
    baseline_CLCR_per_10, baseline_CLCR_60, baseline_CLCR_30,
    dept_ER, dept_ICU, baseline_HGB_per_10, baseline_WBC_per_10,
    baseline_PLT_per_10, baseline_PLT_200, baseline_PLT_150,
    LZD_duration, LZD_duration_7, LZD_duration_10, LZD_duration_14, LZD_route,
    starts_with("invasive"), starts_with("comorb") & -comorb_hematological,
    starts_with("infect"), comed_aspirin, comed_diclofenac, comed_ibuprofen,
    comed_paracetamol, comed_penicillin, comed_cepha, comed_carbapenem,
    comed_cotrimoxazol, comed_vancomycin, comed_levofloxacin, comed_teicoplanin,
    comed_ethambutol, comed_pyrazinamid, comed_rifampin, comed_heparin,
    comed_clopidogrel, comed_enoxaparin, comed_dexamethason, comed_amiodaron,
    comed_furosemid, comed_haloperidol, comed_valproic
  ) |>
  colnames()

predictor_list_short <- data_patient_complete |>
  select(
    patient_age, patient_sex, patient_weight, baseline_CLCR,
    dept_ER, dept_ICU, baseline_HGB, baseline_WBC, baseline_PLT,
    LZD_duration, LZD_route, starts_with("invasive"), starts_with("comorb") & -comorb_hematological,
    starts_with("infect"), comed_ibuprofen, comed_carbapenem,
    comed_vancomycin, comed_levofloxacin, comed_teicoplanin,
    comed_pyrazinamid, comed_rifampin, comed_heparin,
    comed_clopidogrel, comed_enoxaparin, comed_valproic
  ) |>
  colnames()
