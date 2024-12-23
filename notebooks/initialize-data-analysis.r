library(tidyverse)

load("data/data-cleaned/data-cleaned.rda")

predictor_list <- data_patient_complete |>
  select(
    patient_age, patient_sex, patient_weight, baseline_CLCR,
    dept_ICU_ER, baseline_HGB, baseline_WBC, baseline_PLT,
    LZD_duration, LZD_route, starts_with("invasive"),
    starts_with("comorb") & -c(comorb_hematological, comorb_SLE),
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
    patient_age_per_10 = patient_age / 10,
    patient_age_65 = patient_age >= 65,
    patient_age_60 = patient_age >= 60,
    patient_weight_per_10 = patient_weight / 10,
    patient_weight_50 = patient_weight < 50,
    baseline_CLCR_60 = baseline_CLCR < 60,
    baseline_CLCR_30 = baseline_CLCR < 30,
    baseline_PLT_200 = baseline_PLT < 200,
    baseline_PLT_150 = baseline_PLT < 150,
    LZD_duration_factor = case_when(
      LZD_duration < 10 ~ 1,
      LZD_duration >= 10 & LZD_duration < 14 ~ 2,
      LZD_duration >= 14 ~ 3
    ) |> as.factor(),
    LZD_duration_7 = LZD_duration >= 7,
    LZD_duration_10 = LZD_duration >= 10,
    LZD_duration_14 = LZD_duration >= 14,
    LZD_route_IV = LZD_route == "IV"
  ) |>
  mutate(
    patient_age_group = case_when(
      patient_age >= 18 & patient_age <= 44 ~ "18-44",
      patient_age >= 45 & patient_age <= 64 ~ "45-64",
      patient_age >= 65 ~ "> 65"
    )
  ) |>
  mutate(
    baseline_CLCR_factor = case_when(
      baseline_CLCR < 30 ~ "CLCR < 30",
      baseline_CLCR >= 30 & baseline_CLCR < 60 ~ "30 <= CLCR < 60",
      baseline_CLCR >= 60 & baseline_CLCR < 90 ~ "60 <= CLCR < 90",
      baseline_CLCR >= 90 & baseline_CLCR < 130 ~ "90 <= CLCR < 130",
      baseline_CLCR >= 130 ~ "CLCR >= 130"
    ) |> as.factor(),
    baseline_CLCR_factor = factor(
      baseline_CLCR_factor,
      levels = c(
        "CLCR < 30",
        "30 <= CLCR < 60",
        "60 <= CLCR < 90",
        "90 <= CLCR < 130",
        "CLCR >= 130"
    ))
  )


predictor_transformed <- data_patient_transformed |>
  select(
    patient_age_per_10, patient_age_60, patient_age_65,
    patient_sex, patient_weight_per_10, patient_weight_50,
    baseline_CLCR_per_10, baseline_CLCR_60, baseline_CLCR_30,
    dept_ER, dept_ICU, baseline_HGB_per_10, baseline_WBC_per_10,
    baseline_PLT_per_10, baseline_PLT_200, baseline_PLT_150,
    LZD_duration, LZD_duration_7, LZD_duration_10, LZD_duration_14, LZD_route,
    starts_with("invasive"), starts_with("comorb") & -c(comorb_hematological, comorb_SLE),
    starts_with("infect"), comed_ibuprofen, comed_carbapenem,
    comed_vancomycin, comed_levofloxacin, comed_teicoplanin,
    comed_pyrazinamid, comed_rifampin, comed_heparin,
    comed_clopidogrel, comed_enoxaparin, comed_valproic
  ) |>
  colnames()

predictor_list_short <- data_patient_complete |>
  select(
    patient_age, patient_sex, patient_weight, baseline_CLCR,
    dept_ICU_ER, baseline_HGB, baseline_WBC, baseline_PLT,
    LZD_duration, LZD_route, starts_with("invasive"), starts_with("comorb") & -comorb_hematological,
    starts_with("infect"), comed_ibuprofen, comed_carbapenem,
    comed_vancomycin, comed_levofloxacin, comed_teicoplanin,
    comed_pyrazinamid, comed_rifampin, comed_heparin,
    comed_clopidogrel, comed_enoxaparin, comed_valproic
  ) |>
  colnames()
