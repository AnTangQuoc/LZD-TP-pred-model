library(tidyverse)
library(readxl)

# nolint start: object_usage_linter

inputs <- c(
  "data/data-original/Data linezolid_Benh nhiet doi gđ1.xlsx",
  "data/data-original/Data linezolid_Benh nhiet doi gđ2.xlsx",
  "data/data-original/Data linezolid_Giang Bạch Mai.xlsx",
  "data/data-original/Data linezolid_Nhi Bach Mai 2023.xlsx",
  "data/data-original/Data linezolid_Thuy Thanh Nhan 2020.xlsx",
  "data/data-original/Data linezolid_Thanh Nhan 2023.xlsx"
) # not uploaded to GitHub

# This function processes data from Excel files in the "data_patient" sheet.
# It select() specific columns then mutate() data type of the selected columns.
process_data_patient <- function(path) {
  read_excel(path, sheet = "data_patient") |>
    select(
      starts_with("patient"), "site", "charlson",
      starts_with("dept"), starts_with("invasive"), starts_with("baseline"),
      starts_with("LZD"), starts_with("comorb"),
      starts_with("comed"), starts_with("infect"),
      -c("patient_name") # patient_name is not used in the analysis
    ) |>
    mutate(
      across(c("patient_ID", "site"), ~ as.character(.x)),
      across(
        c(
          "patient_sex", starts_with("dept_"), starts_with("invasive"),
          starts_with("comorb") & -(c("comorb_K", "comorb_hematological", "comorb_hema")),
          starts_with("comed"), starts_with("infect")
        ), ~ as.logical(.x)
      ),
      across(
        c(
          "patient_age", "patient_weight", "charlson", "LZD_dose_per_weight",
          starts_with("baseline") & -contains("baseline_date")
        ), ~ as.numeric(.x)
      ),
      across(c("baseline_date", "LZD_start", "LZD_end"), ~ as_date(.x)),
      across(
        c("comorb_K", "comorb_hematological", "comorb_hema"),
        ~ as.logical(if_else(.x == "0" | .x == 0 | is.na(.x), FALSE, TRUE))
      )
    )
}

# Apply 'process_data_patient' function to each Excel file
# and bind resulting rows together.
inputs_general <- map(inputs, process_data_patient) |> bind_rows()
write_csv(inputs_general, "data/data-original-processed/data-patient.csv")

# This function processes the data from an Excel file from the "data_lab_test" sheet.
# It select() specific columns then mutate() data type of the selected columns.
# It also fill() other infos in the selected columns.
process_data_lab_test <- function(path) {
  read_excel(path, sheet = "data_lab_test") |>
    select(starts_with("patient"), starts_with("LZD"), starts_with("test"), -c("patient_name")) |>
    mutate(
      across(c("test_date", starts_with("LZD")), ~ as_date(.x)),
      across(starts_with("test") & -contains("test_date"), ~ as.numeric(.x))
    ) |>
    fill(!starts_with("test"), .direction = "down")
}

# Apply 'process_data_lab_test' function to each Excel file
# and bind resulting rows together.
inputs_labs_general <- map(inputs, process_data_lab_test) |> bind_rows()
write_csv(inputs_labs_general, "data/data-original-processed/data-labs.csv")

# Save to a .rda file for better loading
save(inputs_general, inputs_labs_general, file = "data/data-original-processed/data-processed.rda")

# nolint end