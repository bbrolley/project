library(haven)
library(admiral)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(metacore)
library(metatools)
library(xportr)


library(chatgpt)

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
              SITEID = SITEID, SITEGR1=SITEID, STUDYID = STUDYID, SUBJID = SUBJID, USUBJID = USUBJID,
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
  derive_var_disposition_status(
    dataset_ds = ds,
    new_var = DCDECOD,
    status_var = DSDECOD,
    filter_ds = DSCAT == "DISPOSITION EVENT"
  )

adsl <- adsl %>%
  derive_var_disposition_status(
    dataset_ds = ds,
    new_var = DCREASD,
    status_var = DSTERM,
    filter_ds = DSCAT == "DISPOSITION EVENT"

  )

adsl <- adsl %>%
  mutate(DISCONFL=if_else(DCREASD !='COMPLETED',"Y",""))

adsl <- adsl %>%
  mutate(DSRAEFL=if_else(DCREASD =='ADVERSE EVENT',"Y",""))


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

adsl <- adsl %>%
  mutate(
    AGEGR1N = as.factor(AGEGR1),

  )

format_trt01an<- function(var_input){
  case_when(
    var_input == "Placebo" ~ 0 ,
    var_input == "Xanomeline Low Dose" ~ 54 ,
    var_input == "Xanomeline High Dose" ~ 81 ,
    TRUE~NA
  )
}

adsl <- adsl %>%
  mutate(
    TRT01AN = format_trt01an(TRT01A),
  )

format_trt01pn<- function(var_input){
  case_when(
    var_input == "Placebo" ~ 0 ,
    var_input == "Xanomeline Low Dose" ~ 54 ,
    var_input == "Xanomeline High Dose" ~ 81 ,
    TRUE~NA
  )
}

adsl <- adsl %>%
  mutate(
    TRT01PN = format_trt01pn(TRT01P),
  )

format_racen<- function(var_input){
  case_when(
    var_input == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 1 ,
    var_input == "ASIAN" ~ 2 ,
    var_input == "BLACK OR AFRICAN AMERICAN" ~ 3 ,
    var_input == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 5 ,
    var_input == "WHITE" ~ 6 ,
    TRUE~NA
  )
}

adsl <- adsl %>%
  mutate(
    RACEN = format_racen(RACE),
  )


adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = vs,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(WEIGHTBL=VSSTRESN),
    filter_add = VSBLFL == "Y" & VSTESTCD == "WEIGHT"
  )

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = vs,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(HEIGHTBL=VSSTRESN),
    filter_add =  VSTESTCD == "HEIGHT"
  )

adsl <- adsl %>%
    mutate(BMIBL= WEIGHTBL/((HEIGHTBL/100)**2))

format_bmigrp<- function(var_input){
  case_when(
    var_input < 25  ~ "<25",
    between(var_input, 25, 30) ~ "25-<30",
    var_input >= 30 ~ ">=30" ,
    TRUE~NA
  )
}

adsl <- adsl %>%
  mutate(
    BMIBLGR1 = format_bmigrp(BMIBL),
  )

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = sv,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(VISIT1DT=SVSTDTC),
    filter_add = VISITNUM == 1
  )

adsl<- adsl %>%
  mutate(VISIT1DT = as.Date(VISIT1DT))


adsl<- adsl %>%
     mutate(ITTFL= if_else(ARMCD != '',"Y","N"))


adsl<- adsl %>%
  mutate(SAFFL= if_else(ITTFL == 'Y' &TRTSDT !='',"Y","N"))


qs_ext <- qs %>%
            filter(QSCAT=="MINI-MENTAL STATE") %>%
            group_by(USUBJID) %>%
            summarize(MMSETOT=sum(as.numeric(QSORRES))) %>%
            select(USUBJID,MMSETOT)

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = qs_ext,
    by_vars = vars(USUBJID),

  )

adsl <- adsl %>%
       derive_vars_dt(
       dtc = RFENDTC,
       new_vars_prefix="RFENDT"
)

mh_ext<- mh %>%
       filter(MHCAT=="PRIMARY DIAGNOSIS") %>%
        derive_vars_dt(
        dtc = MHSTDTC,
        new_vars_prefix="DISONS") %>%
        select(STUDYID,USUBJID,DISONSDT)


adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = mh_ext,
    by_vars = vars(STUDYID,USUBJID),)


adsl <- adsl %>%
        derive_vars_duration(DURDIS,new_var_unit=NULL, DISONSDT,VISIT1DT,in_unit="days",out_unit="months")

format_durgrp<- function(var_input){
  case_when(
    var_input < 12  ~ "<12",
    var_input >= 12 ~ ">=12" ,
    TRUE~NA
  )
}

adsl <- adsl %>%
  mutate(
    DURDSGR1 = format_durgrp(DURDIS),
  )

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = sc,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(EDUCLVL=SCSTRESN),
    filter_add = SCTESTCD=="EDLEVEL"
  )



COMPxxFL <- function(sv, adsl, visitnum, xx){
                    vis_usub <- sv %>% filter(VISITNUM==visitnum) %>%
                    select(USUBJID, SVSTDTC, VISITNUM)
                    out <- full_join(vis_usub, adsl, by ="USUBJID") %>% mutate(SVSTDTC = SVSTDTC %>% ymd, RFENDTC = RFENDTC %>% ymd) %>%
                    mutate(aux = if_else(VISITNUM == visitnum & RFENDTC>=SVSTDTC,"Y","N")) %>%
                    mutate(!!paste0("COMP", xx, "FL") := ifelse(is.na(aux), "N", "Y")) %>%
                    select(USUBJID, !!paste0("COMP", xx, "FL")) %>%
                    arrange(., USUBJID)
                    return(out)
                    }


adsl <- adsl %>%
        left_join(.,COMPxxFL(sv, adsl, 10, 16),by="USUBJID") %>%
        left_join(.,COMPxxFL(sv, adsl, 12, 24),by="USUBJID") %>%
        left_join(.,COMPxxFL(sv, adsl, 8, 8),by="USUBJID")


adsl<- adsl %>%
           mutate(ARMN=case_when(ARM=="Placebo"~0,
                                 ARM== "Xanomeline Low Dose"~54,
                                 TRUE ~81))

adsl<- adsl %>%
       mutate(CUMDOSE=case_when(ARMN %in% c(0,54)~ TRT01PN*TRTDURD,
                                TRUE~999))


# cumdos <- left_join(adsl %>% select(USUBJID,TRTSDT,TRTEDT,TRT01PN,DCDECOD),sv %>% select(USUBJID, VISITNUM, SVSTDTC),by="USUBJID") %>%
#           group_by(USUBJID) %>%
#           summarise(LVIS=last(VISITNUM)) %>%
#           ungroup

adsl <-    qs %>%
           select(USUBJID,QSCAT,VISITNUM) %>%
           filter(QSCAT %in% c("ALZHEIMER'S DISEASE ASSESSMENT SCALE","CLINICIAN'S INTERVIEW-BASED IMPRESSION OF CHANGE (CIBIC+)") & VISITNUM > 3) %>%
           mutate(EFFFL="Y") %>%
           select(USUBJID,EFFFL) %>%
           unique() %>% right_join(.,adsl,by="USUBJID") %>%
           mutate(EFFFL = if_else(EFFFL=="Y","Y","N"))


