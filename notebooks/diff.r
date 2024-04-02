library(tidyverse)
library(readxl)

df1 <- read_xlsx("D:/R Working Directory/LZD-TP-pred-model-backup/All_data1.xlsx") |>
  select(-"patient_name") |>
  select(starts_with("patient"), site, charlson, starts_with("dept"), starts_with("invasive"), starts_with("baseline"), starts_with("LZD"), starts_with("comorb"), starts_with("comed"), starts_with("infect"), flag_ADR_TP_ID) |>
  arrange(site) |>
  group_by(site) |>
  arrange(patient_ID, .by_group = TRUE) |>
  ungroup()

df2 <- data_patient_complete |>
  select(-starts_with("flag"), -starts_with("ADR"), flag_ADR_TP_ID) |>
  mutate(across(where(is.logical), as.numeric)) |>
  select(starts_with("patient"), site, charlson, starts_with("dept"), starts_with("invasive"), starts_with("baseline"), starts_with("LZD"), starts_with("comorb"), starts_with("comed"), starts_with("infect"), flag_ADR_TP_ID) |>
  arrange(site) |>
  group_by(site) |>
  arrange(patient_ID, .by_group = TRUE) |>
  ungroup() |>
  mutate(patient_ID = str_replace(patient_ID, "_1", ""))

(compare_results <- waldo::compare(df1, df2, max_diffs = Inf))
