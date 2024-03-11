library(tidyverse)

load("data/data-cleaned/data-cleaned.rda")

predictor_list <- data_patient_complete |>
  select(
    patient_age, patient_sex, LZD_dose_per_weight, baseline_CLCR,
    dept_ER, dept_ICU, baseline_HGB, baseline_WBC, baseline_PLT,
    LZD_duration, starts_with("invasive"), starts_with("comorb"),
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
    baseline_CLCR_30 = baseline_CLCR <= 30
  )

predictor_transformed <- data_patient_transformed |>
  select(
    patient_age, patient_sex, LZD_dose_per_weight, baseline_CLCR_30, # note
    dept_ER, dept_ICU, baseline_HGB, baseline_WBC, baseline_PLT,
    LZD_duration, starts_with("invasive"), starts_with("comorb"),
    starts_with("infect"), comed_aspirin, comed_diclofenac, comed_ibuprofen,
    comed_paracetamol, comed_penicillin, comed_cepha, comed_carbapenem,
    comed_cotrimoxazol, comed_vancomycin, comed_levofloxacin, comed_teicoplanin,
    comed_ethambutol, comed_pyrazinamid, comed_rifampin, comed_heparin,
    comed_clopidogrel, comed_enoxaparin, comed_dexamethason, comed_amiodaron,
    comed_furosemid, comed_haloperidol, comed_valproic
  ) |>
  colnames()