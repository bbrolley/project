library(haven)
library(admiral)
library(dplyr)
library(tidyr)
library(metacore)
library(metatools)
library(xportr)


# Read specs ----
metacore <- spec_to_metacore("./metadata/specs.xlsx", where_sep_sheet = FALSE)

# Import data ----
ae <- read_xpt("sdtm/ae.xpt")
adsl <- read_xpt("adam/adsl.xpt")

ae <- convert_blanks_to_na(ae)
adsl <- convert_blanks_to_na(adsl)


# Merge with ADSL ----
# Check predecessor from metacore
der <- metacore$derivations
check <- der %>%
  filter(str_detect(derivation_id, "ADAE")) %>%
  filter(str_detect(derivation, "ADSL"))


# Add ADSL vars
adsl_vars <- vars(AGE, AGEGR1, AGEGR1N, RACE, RACEN, SAFFL, SEX, SITEID,
                  STUDYID, TRT01A, TRT01AN, TRTEDT, TRTSDT, USUBJID)

adae <- derive_vars_merged(
  ae,
  dataset_add = adsl,
  new_vars = adsl_vars,
  by_vars = vars(STUDYID, USUBJID)
) %>%
  mutate(TRTA = TRT01A,
         TRTAN = TRT01AN)


# Convert DTC to numeric date ----
adae <- adae %>%
  derive_vars_dt(
    dtc = AESTDTC,
    new_vars_prefix = "AST",
    highest_imputation = "D",
    date_imputation = "first"
  ) %>%
  derive_vars_dt(
    dtc = AEENDTC,
    new_vars_prefix = "AEN"
  )%>%
  derive_vars_dy(
    reference_date = TRTSDT,
    source_vars = vars(ASTDT, AENDT)
  )

# Duration ----
adae <- adae %>%
  derive_vars_duration(
    new_var = ADURN,
    new_var_unit = ADURU,
    start_date = ASTDT,
    end_date = AENDT
  )

# Treatment emergent flag ----
adae <- adae %>%
  mutate(
    TRTEMFL = case_when(
      TRTSDT <= ASTDT ~ "Y",
      TRUE ~ NA_character_
    )
  )

# Check
checkTRTEMFL <- adae %>%
  group_by(ASTDY,  TRTEMFL) %>%
  summarize(n = n()) %>%
  ungroup()

# Customized Query ----

adae <- adae %>%
  mutate(
    CQ01NAM = case_when(
      str_detect(AEDECOD, "APPLICATION")                            ~ "DERMATOLOGIC EVENTS",
      str_detect(AEDECOD, "DERMATITIS")                             ~ "DERMATOLOGIC EVENTS",
      str_detect(AEDECOD, "ERYTHEMA")                               ~ "DERMATOLOGIC EVENTS",
      str_detect(AEDECOD, "BLISTER")                                ~ "DERMATOLOGIC EVENTS",
      AEBODSYS == "SKIN AND SUBC UTANEOUS TISSUE DISORDERS" &
        ! (AEDECOD %in% c("COLD SWEAT","HYPERHIDROSIS", "ALOPECIA")) ~ "DERMATOLOGIC EVENTS",
      TRUE ~ NA_character_
    )
  )
# Check
checkCQNAM <- adae %>%
  filter(CQ01NAM == "DERMATOLOGIC EVENTS") %>%
  group_by(AEBODSYS,  AEDECOD) %>%
  summarize(n = n()) %>%
  ungroup()

# First occurrence flag ----

adae <- adae %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID),
      order = vars(ASTDT, AESEQ),
      new_var = AOCC01FL,
      mode = "first"
    ),
    filter = TRTEMFL == "Y" & CQ01NAM == "DERMATOLOGIC EVENTS"
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID),
      order = vars(ASTDT, AESEQ),
      new_var = AOCC02FL,
      mode = "first"
    ),
    filter = TRTEMFL == "Y" & AESER == "Y"
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS),
      order = vars(ASTDT, AESEQ),
      new_var = AOCC03FL,
      mode = "first"
    ),
    filter = TRTEMFL == "Y" & AESER == "Y"
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS, AEDECOD),
      order = vars(ASTDT, AESEQ),
      new_var = AOCC04FL,
      mode = "first"
    ),
    filter = TRTEMFL == "Y" & AESER == "Y"
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID),
      order = vars(ASTDT, AESEQ),
      new_var = AOCCFL,
      mode = "first"
    ),
    filter = TRTEMFL == "Y"
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS, AEDECOD),
      order = vars(ASTDT, AESEQ),
      new_var = AOCCPFL,
      mode = "first"
    ),
    filter = TRTEMFL == "Y"
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS),
      order = vars(ASTDT, AESEQ),
      new_var = AOCCSFL,
      mode = "first"
    ),
    filter = TRTEMFL == "Y"
  )

# Apply spec properties, create xpt ----
adae <- adae %>%
  drop_unspec_vars( metacore, dataset_name = "ADAE") %>%
  set_variable_labels(metacore, dataset_name = "ADAE") %>%
  sort_by_key( metacore, dataset_name = "ADAE") %>%
  order_cols(metacore, dataset_name = "ADAE")

adae %>%
  xportr_type(metacore) %>%
xportr_write(adae, "adam/adae.xpt")
