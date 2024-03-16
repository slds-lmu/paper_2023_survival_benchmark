# Double-checking and cleaning new datasets
# See also https://github.com/RaphaelS1/proba_benchmark/issues/28 for discussion on individual datasets
library(dplyr)

save_data <- function(x, path = here::here("code/data/")) {
  xname <- deparse(substitute(x))
  path_file_rds <- paste0(path, xname, ".rds")
  path_file_csv <- paste0(path, xname, ".csv")
  message("Saving ", xname, " at ", path_file_rds, " and ", path_file_csv)

  checkmate::assert_integerish(x[["status"]], lower = 0, upper = 1)
  checkmate::assert_numeric(x[["time"]], lower = 0)

  x_nomiss <- na.omit(x)

  if (nrow(x) - nrow(x_nomiss) > 0) {
    message("Dropping ", nrow(x) - nrow(x_nomiss), " rows due to missings")
  }

  time_zeros <- which(x_nomiss$time == 0)
  if (length(time_zeros) > 0) {
    message("Dropping ", length(time_zeros), " rows due to time = 0")
    x_nomiss <- x_nomiss[-time_zeros, ]
  }

  saveRDS(x_nomiss, file = path_file_rds)
  write.csv(x_nomiss, file = path_file_csv, row.names = FALSE)
}


# dynpred::ova --------------------------------------------------------------------------------

ova <- mlr3misc::load_dataset("ova", "dynpred") |>
  mutate(
    time = tyears,
    status = as.integer(d),
    Broders = factor(Broders),
    FIGOIII = as.integer(FIGO == "III"),
    Ascites = factor(Ascites),
    # Remove > < symbols as they can cause issues
    Diam = stringr::str_replace(Diam, ">", "gt"),
    Diam = stringr::str_replace(Diam, "<", "lt"),
    Diam = factor(Diam)
  ) |>
  select(-tyears, -d, -FIGO, -id)

save_data(ova)

# eha::child --------------------------------------------------------------
# Children born in Skellefteå, Sweden, 1850-1884, are followed fifteen years
# or until death or out-migration

child <- eha::child |>
  select(
    -enter, # = 0 for all obs
    -id, -m.id, # ID vars
    -birthdate, # YYYY-MM-DD variable, reasonable to exclude?
    time = exit, status = event
  )

save_data(child)

# frailtyHL::bladder0 -----------------------------------------------------
# Bladder0 is a subset of 410 patients from a full data set with bladder cancer
# from 21 centers that participated in the EORTC trial (Sylvester et al., 2006).
# Time to event is the duration of the disease free interval (DFI), which is
# defined as time from randomization to the date of the first recurrence.

bladder0 <- mlr3misc::load_dataset("bladder0", "frailtyHL") |>
  rename(time = Surtime, status = Status) |>
  mutate(
    Center = factor(Center) # Group ID was integer
    # time = ifelse(time == 0, 0.001, time)
  )

save_data(bladder0)

# frailtySurv::hdfail ------------------------------------------------------
# This dataset contains the observed follow-up times and SMART statistics of
# 52k unique hard drives.

hdfail <- frailtySurv::hdfail |>
  select(-serial) # ID column

range(hdfail$time)
length(unique(hdfail$time))
summary(hdfail$time)

# Coarsened version to reduce number of unique time points
# Values 0 < t < 1 will be rounded to time = 0, save_data() drops these
hdfail <- hdfail |>
  dplyr::mutate(
    time = floor(time)
  )

range(hdfail$time)
length(unique(hdfail$time))
summary(hdfail$time)

save_data(hdfail)


# KMsurv::kidtran -----------------------------------------------------------------------------

kidtran <- mlr3misc::load_dataset("kidtran", "KMsurv") |>
  mutate(time = as.numeric(time),
         status = delta,
         genderF = as.integer(gender - 1),
         raceBlack = as.integer(race - 1),
         age = as.numeric(age)
  ) |>
  select(-obs, -delta, -gender, -race)

save_data(kidtran)

# KMsurv::std -------------------------------------------------------------
# data from Section 1.12 in Klein and Moeschberger (1997)

std <- mlr3misc::load_dataset("std", "KMsurv") |>
  rename(
    status = rinfct
  ) |>
  mutate(
    condom = factor(condom, labels = c("always", "sometime", "never")),
    iinfct = factor(iinfct, labels = c("gonorrhea", "chlamydia", "both"))
  ) |>
  select(-obs) # Identifier

save_data(std)

# JM::aids.id -------------------------------------------------------------
# A randomized clinical trial in which both longitudinal and survival data were
# collected to compare the efficacy and safety of two antiretroviral drugs in
# treating patients who had failed or were intolerant of zidovudine (AZT) therapy.

# First measurements of JM::aids, therefore obstime == 0 here
aids.id <- JM::aids.id |>
  rename(time = Time, status = death) |>
  select(
    -patient, # ID var
    -obstime,  # 0 for all in this version, first measurement,
    -start, -stop, -event # not relevant here (per AB)
  )

save_data(aids.id)

# joineR::heart.valve -----------------------------------------------------

# Excluded because too few events after baseline filtering

# Per AB: long format with most features collected at follow up.
# Could be used if we only keep baseline covariates (which then are only few however).
# For time-to-event and status variables use last obs. of each subject (num)

# # Making sure to arrange by num/time for filtering
# heart.valve <- joineR::heart.valve |>
#   arrange(num, time)
#
# # time to event: last obs per subject
# heart.valve_tte <- heart.valve |>
#   select(num, time, status) |>
#   group_by(num) |>
#   slice_tail(n = 1) |>
#   ungroup()
#
# # Covariates: baseline, e.g. first per subject
# heart.valve_covars <- heart.valve |>
#   select(-time, -status) |>
#   group_by(num) |>
#   slice_head(n = 1) |>
#   ungroup()
#
# # Double check no subject num was lost for whatever reason
# all(unique(heart.valve_covars$num) == unique(heart.valve_tte$num))
# nrow(heart.valve_covars) == nrow(heart.valve_tte)
#
# # Join by num explicitly just in case
# heart.valve <- left_join(
#   heart.valve_tte,
#   heart.valve_covars,
#   by = "num") |>
#   select(-num)
#
# # Sadly only 54 events afterwards
# table(heart.valve)
#
# save_data(heart.valve)

# joineR::liver -----------------------------------------------------------
# Liver cirrhosis drug trial data

# Per AB: joineR::liver: can be used, but only first row per subject (id).
# Column survival is our event time, column time is the time at which liver
# function was measured (feature prothrombin). Keep the value at time = 0.

liver <- joineR::liver |>
  filter(time == 0) |>
  select(
    -id,  # ID var
    -time # Now irrelevant after filtering
  ) |>
  rename(time = survival, status = cens)

save_data(liver)

# locfit::livmet ----------------------------------------------------------
# Survival times for 622 patients diagnosed with Liver Metastases.
# "Beware, the censoring variable is coded as 1 = uncensored, so use cens=1-z
# in locfit() calls."
# -> so 0 = censored, no recoding needed. Reading is hard.
# https://ftp.uni-bayreuth.de/math/statlib/S/survcart
# Can't find source or real indication for dataset being real

# livmet <- mlr3misc::load_dataset("livmet", "locfit") |>
#   rename(time = t, status = z) |>
#   mutate(
#     # status = 1 - status,
#     # Categorical recoding to factor for tumor TNM
#     tnm = factor(tnm),
#     # (1, 2) -> (0, 1), binary variables
#     sex = sex - 1,
#     pt = pt - 1,
#     lap = lap - 1
#   ) |>
#   filter(time > 0)
#
# save_data(livmet)

# RISCA::dataFTR --------------------------------------------------------
# Data were extracted from the DIVAT cohort. It corresponds to the reference
# sample constituted by first transplant recipients (FTR).

dataFTR <- mlr3misc::load_dataset("dataFTR", "RISCA") |>
  rename(
    time = Tps.Evt,
    status = Evt
  )

save_data(dataFTR)

# RISCA::dataSTR --------------------------------------------------------
# Data were extracted from the DIVAT cohort. It corresponds to the relative
# sample constituted by second transplant recipients (STR).

# "Tattente2cl" = Waiting time between consecutive transplants, ok to include?

dataSTR <- mlr3misc::load_dataset("dataSTR", "RISCA") |>
  rename(
    time = Tps.Evt,
    status = Evt
  )

save_data(dataSTR)

# pec::cost ---------------------------------------------------------------
# This data set contains a subset of the data from the Copenhagen stroke study

# This seems too god to be true almost
cost <- mlr3misc::load_dataset("cost", "pec")

# Check to see if I missed a constant variable maybe?
all(dim(cost) == dim(janitor::remove_constant(cost)))

save_data(cost)

# quantreg::uis -----------------------------------------------------------
# UIS Drug Treatment study data

# Exclude LEN.T and FRAC to avoid optimistic bias due to future information

uis <- mlr3misc::load_dataset("uis", "quantreg") |>
  select(
    -ID, # ID var
    -Y,  # Y: log(TIME)
    -LEN.T, -FRAC # Length of stay, compliance, not known at baseline
  ) |>
  rename(
    time = TIME,
    status = CENSOR
  ) |>
  mutate(
    HC = factor(HC), # Categorical, 4 levels
  )

save_data(uis)

# relsurv::rdata ----------------------------------------------------------
# "Survival data."
# Need to check: Pohar M., Stare J. (2006) "Relative survival analysis in R."
# Computer Methods and Programs in Biomedicine, 81: 272-278.

# year: recode to factor date
# age + agegrp can both stay

# Year column in class 'date' DDmonYY format for year extraction

rdata <- mlr3misc::load_dataset("rdata", "relsurv") |>
  rename(status = cens) |>
  mutate(
    # Extract 2 digit year from date of diagnosis
    year = stringr::str_extract(date:::as.character.date(year), "\\d{2}$"),
    year = factor(year),
    # recode sex from 1,2 to 0,1 for consistency
    sex = sex - 1
  ) |>
  # Redudant variable, grouped version of agre
  select(-agegr)

# Sanity check to ensure year recoding worked
stopifnot(length(unique(rdata$year)) == 5)

save_data(rdata)

# relsurv::colrec --------------------------------------------------------------
# Survival of patients with colon and rectal cancer diagnosed in 1994-2000.

# Removed due to date column by ML
# Re-added with converted date col and further preprocessing
colrec <- relsurv::colrec |>
  mutate(
    # Convert date variable to numeric
    diag = as.numeric(diag),
    stage = ifelse(stage == 99, NA_real_, stage),
    sex = factor(sex, levels = 1:2, labels = c("male", "female"))
  ) |>
  rename(status = stat)

save_data(colrec)

# simPH::CarpenterFdaData -------------------------------------------------
# A data set from Carpenter (2002).
# https://www.jstor.org/stable/3088394 Table 1

# No clear doc for censoring variable but assuming normal coding
# based on usage in simPH example code with {survival} etc.

CarpenterFdaData <- mlr3misc::load_dataset("CarpenterFdaData", "simPH") |>
  rename(
    time = acttime,
    status = censor
  ) |>
  select(
    -caseid, # ID var
    -X_st, # constant 1
    -X_t0, # constant 0
    -X_t, # identical to acttime (time).
    -X_d # identical to censor (status)
  )

save_data(CarpenterFdaData)

# smcure::e1684 -----------------------------------------------------------
# The melanoma data from the Eastern Cooperative Oncology Group (ECOG) phase
# III clinical trial e1684 which is used for modeling semicure PH mixture
# cure model (Kirkwood, et al., 1996)

e1684 <- mlr3misc::load_dataset("e1684", "smcure") |>
  rename(time = FAILTIME, status = FAILCENS)

save_data(e1684)

# survival::lung ------------------------------------------------------------------------------
lung = survival::lung |>
  mutate(
    sex = factor(if_else(sex == 1, "M", "F")),
    ph.ecog = as.integer(ph.ecog),
    inst = factor(inst),
    status = status - 1
  ) |>
  tibble::remove_rownames()

save_data(lung)
