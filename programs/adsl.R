library(haven)
library(admiral)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(metacore)
library(metatools)
library(xportr)

# read source -------------------------------------------------------------
# When SAS datasets are imported into R using read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values


dm <- convert_blanks_to_na(read_xpt(file.path("sdtm", "dm.xpt")))
ds <- convert_blanks_to_na(read_xpt(file.path("sdtm", "ds.xpt")))
ex <- convert_blanks_to_na(read_xpt(file.path("sdtm", "ex.xpt")))
qs <- convert_blanks_to_na(read_xpt(file.path("sdtm", "qs.xpt")))
sv <- convert_blanks_to_na(read_xpt(file.path("sdtm", "sv.xpt")))
vs <- convert_blanks_to_na(read_xpt(file.path("sdtm", "vs.xpt")))
sc <- convert_blanks_to_na(read_xpt(file.path("sdtm", "sc.xpt")))
mh <- convert_blanks_to_na(read_xpt(file.path("sdtm", "mh.xpt")))

## placeholder for origin=predecessor, use metatool::build_from_derived()
metacore <- spec_to_metacore("metadata/specs.xlsx", where_sep_sheet = FALSE)
# Get the specifications for the dataset we are currently building
adsl_spec <- metacore %>%
  select_dataset("ADSL")

adsl_preds <- dm %>%
              transmute(AGE = AGE, AGEU = AGEU, ARM = ARM, DTHFL = DTHFL, ETHNIC = ETHNIC,
              RACE = RACE, RFENDTC = RFENDTC, RFSTDTC = RFSTDTC, SEX = SEX,
              SITEID = SITEID, STUDYID = STUDYID, SUBJID = SUBJID, USUBJID = USUBJID,
              TRT01P = ARM, TRT01A = ACTARM, ARMCD = ARMCD)

ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )

adsl <- adsl_preds %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = vars(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = vars(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = vars(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = vars(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = vars(STUDYID, USUBJID)
  )

adsl <- adsl %>%
  derive_vars_dtm_to_dt(source_vars = vars(TRTSDTM, TRTEDTM))

adsl <- adsl %>%
  derive_var_trtdurd()

# convert character date to numeric date without imputation
ds_ext <- derive_vars_dt(
  ds,
  dtc = DSSTDTC,
  new_vars_prefix = "DSST"
)

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(EOSDT = DSSTDT),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE"
  )

adsl <- adsl %>%
  derive_var_disposition_status(
    dataset_ds = ds,
    new_var = EOSSTT,
    status_var = DSDECOD,
    filter_ds = DSCAT == "DISPOSITION EVENT"
  )

format_agegr1 <- function(var_input) {
  case_when(
    var_input <65 ~ 1,
    between(var_input, 65, 80) ~ 2,
    var_input > 80 ~ 3,
    TRUE ~ NA
  )
}

adsl <- adsl %>%
  mutate(
    AGEGR1 = format_agegr1(AGE),

  )

format_agegr1n <- function(var_input) {
  case_when(
    var_input =1 ~ "1",
    var_input =2 ~ "2",
    var_input =3 ~ "3",
    TRUE ~ "missing"
  )
}

adsl <- adsl %>%
  mutate(
    AGEGR1N = format_agegr1n(AGEGR1),

  )
