# Double-checking and cleaning new datasets
library(dplyr)

# APtools::mayo -----------------------------------------------------------
# Two marker values with event time and censoring status for the subjects
# in Mayo PBC data

mayo <- APtools::mayo %>%
  rename(status = censor)

saveRDS(mayo, here::here("code/data_candidates/mayo.rds"))

# eha::child --------------------------------------------------------------
# Children born in Skellefteå, Sweden, 1850-1884, are followed fifteen years
# or until death or out-migration

child <- eha::child %>%
  select(
    -enter, # = 0 for all obs
    -id, -m.id, # ID vars
    -birthdate, # YYYY-MM-DD variable, reasonable to exclude?
    time = exit, status = event
  )

saveRDS(child, here::here("code/data_candidates/child.rds"))

# frailtyHL::bladder0 -----------------------------------------------------
# Bladder0 is a subset of 410 patients from a full data set with bladder cancer
# from 21 centers that participated in the EORTC trial (Sylvester et al., 2006).
# Time to event is the duration of the disease free interval (DFI), which is
# defined as time from randomization to the date of the first recurrence.

bladder0 <- mlr3misc::load_dataset("bladder0", "frailtyHL") %>%
  rename(time = Surtime, status = Status)

saveRDS(bladder0, here::here("code/data_candidates/bladder0.rds"))

# frailtySurv::hdfail ------------------------------------------------------
# This dataset contains the observed follow-up times and SMART statistics of
# 52k unique hard drives.

hdfail <- frailtySurv::hdfail %>%
  select(-serial) # ID column

saveRDS(hdfail, here::here("code/data_candidates/hdfail.rds"))


# JM::aids.id -------------------------------------------------------------
# A randomized clinical trial in which both longitudinal and survival data were
# collected to compare the efficacy and safety of two antiretroviral drugs in
# treating patients who had failed or were intolerant of zidovudine (AZT) therapy.

# First measurements of JM::aids, therefore obstime == 0 here
aids.id <- JM::aids.id %>%
  rename(time = Time, status = death) %>%
  select(
    -patient, # ID var
    -obstime,  # 0 for all in this version, first measurement,
    -start, -stop, -event # not relevant here
  )

saveRDS(aids.id, here::here("code/data_candidates/aids.id.rds"))


# joineR::liver -----------------------------------------------------------
# Liver cirrhosis drug trial data

# Per AB: joineR::liver: can be used, but only first row per subject (id).
# Column survival is our event time, column time is the time at which liver
# function was measured (feature prothrombin). Keep the value at time = 0.

liver <- joineR::liver %>%
  group_by(id) %>%
  slice_min(time) %>% # First measurement per subject
  ungroup() %>%
  select(
    -id,  # ID var
    -time # Now irrelevant after filtering
  ) %>%
  rename(time = survival, status = cens)

saveRDS(liver, here::here("code/data_candidates/liver.rds"))

# MRsurv::FTR.data --------------------------------------------------------
# Data were extracted from the DIVAT cohort. It corresponds to the reference
# sample constituted by first transplant recipients (FTR).

FTR.data <- mlr3misc::load_dataset("FTR.data", "MRsurv") %>%
  rename(
    time = Tps.Evt,
    status = Evt
  )

saveRDS(FTR.data, here::here("code/data_candidates/FTR.data.rds"))

# MRsurv::STR.data --------------------------------------------------------
# Data were extracted from the DIVAT cohort. It corresponds to the relative
# sample constituted by second transplant recipients (STR).

# "Tattente2cl" = Waiting time between consecutive transplants, ok to include?

STR.data <- mlr3misc::load_dataset("STR.data", "MRsurv") %>%
  rename(
    time = Tps.Evt,
    status = Evt
  )

saveRDS(STR.data, here::here("code/data_candidates/STR.data.rds"))


# parfm::insem ------------------------------------------------------------
# Time to first insemination in dairy heifer cows without time varying covariates

insem <- mlr3misc::load_dataset("insem", "parfm") %>%
  rename(time = Time, status = Status) %>%
  select(-Cowid) # ID var

saveRDS(insem, here::here("code/data_candidates/insem.rds"))


# relsurv::colrec --------------------------------------------------------------
# Survival of patients with colon and rectal cancer diagnosed in 1994-2000.

colrec <- relsurv::colrec %>%
  rename(status = stat)

saveRDS(colrec, here::here("code/data_candidates/colrec.rds"))

