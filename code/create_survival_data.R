# ids
c_real = c("aids2", "ALL", "bmt", "channing", "diabetic", "flchain", "gbsg", "grace",
           "hepatoCellular",
           "kidtran", "lung", "melanoma", "metabric", "mgus", "nafld1", "nki", "nwtco", "ova",
           "patient", "pbc", "pharmacoSmoking", "prostateSurvival", "rats", "support",
           "transplant", "tumor", "udca1", "veteran", "wbc1", "whas")

for (i in c_real) {
  saveRDS(do.call(paste0("create_", i), list()),
          paste0("data/", i, ".rds"))
}

create_diabetic = function(...) {
  survival::diabetic %>%
    filter(eye == "left") %>%
    mutate(argon = as.integer(laser == "argon"),
           risk = factor(risk),
           age = as.numeric(age),
           time = as.numeric(time)) %>%
    select(-eye, -laser, -id) %>%
    drop_na()
}
create_mgus = function(...){
  mgus %>%
    select(-pcdx, -id, -pctime) %>%
    mutate(time = futime,
           status = death) %>%
    select(-futime, -death) %>%
    drop_na()
}
create_lung = function(...){
  df = lung %>%
    drop_na() %>%
    mutate(sex = factor(if_else(sex == 1, "M", "F")),
           ph.ecog = as.factor(as.character(ph.ecog)),
           status = status - 1) %>%
    select(-inst)

  df$ph.ecog = forcats::fct_collapse(
    df$ph.ecog,
    "2+" = c("2", "3")
  )

  return(df)
}
create_flchain = function(...){
  flchain %>%
    mutate(mgus = as.factor(as.character(mgus)),
           time = futime,
           flc.grp = as.factor(as.character(flc.grp)),
           status = death) %>%
    select(-futime, -chapter, -death, -creatinine, -chapter) %>%
    drop_na() %>%
    filter(time > 0)
}
create_nafld1 = function(...){
  nafld1 %>%
    mutate(sex = factor(if_else(male == 0, "F", "M")),
           time = futime) %>%
    select(-male, -case.id, -futime, -id) %>%
    drop_na()
}
create_nwtco = function(...) {
  survival::nwtco %>%
    mutate(status = rel,
           time = as.numeric(edrel),
           age = as.numeric(age),
           histol = as.integer(histol - 1),
           stage = factor(stage)) %>%
    select(-seqno, -study, -rel, -edrel, -in.subcohort, -instit) %>%
    drop_na()
}
create_pbc = function(...){
  pbc %>%
    mutate(status = if_else(status == 2, 2, 0) * 0.5,
           ascites = as.factor(as.character(ascites)),
           hepato = as.factor(as.character(hepato)),
           spiders = as.factor(as.character(spiders)),
           edema = as.factor(as.character(edema)),
           stage = as.factor(as.character(stage)),
           trt = as.factor(as.character(trt)),
           time = as.numeric(time),
           chol = as.numeric(chol),
           copper = as.numeric(copper),
           trig = as.numeric(trig),
           platelet = as.numeric(platelet)
    ) %>%
    select(-id) %>%
    drop_na()
}
create_rats = function(...) {
  survival::rats %>%
    mutate(sexF = as.integer(sex == "f"),
           time = as.numeric(time),
           rx = as.integer(rx)) %>%
    select(-sex) %>%
    drop_na()
}
create_transplant = function(...){
  transplant %>%
    mutate(time = futime,
           status = if_else(event == "death", 1, 0)) %>%
    select(-futime, -event) %>%
    drop_na() %>%
    filter(time > 0)
}
create_udca1 = function(...){
  udca1 %>%
    mutate(trt = as.factor(as.character(trt)),
           time = futime,
           stage = as.factor(as.character(stage))) %>%
    select(-id, -futime) %>%
    drop_na()
}
create_veteran = function(...){
  veteran %>%
    mutate(trt = as.factor(as.character(trt)),
           prior = factor(if_else(prior == 0, "0", "1"))) %>%
    drop_na()
}

pycox = reticulate::import("pycox")
create_gbsg = function(...) {
  pycox$datasets$gbsg$read_df() %>%
    mutate(x0 = as.integer(x0),
           x1 = as.integer(x1),
           x2 = as.integer(x2),
           status = event,
           time = as.numeric(duration)) %>%
    select(-event, -duration) %>%
    drop_na()
}
create_metabric = function(...) {
  pycox$datasets$metabric$read_df() %>%
    mutate(x4 = as.integer(x4),
           x5 = as.integer(x5),
           x6 = as.integer(x6),
           x7 = as.integer(x7),
           status = event,
           time = as.numeric(duration)) %>%
    select(-event, -duration) %>%
    drop_na() %>%
    filter(time > 0)
}
create_support = function(...) {
  pycox$datasets$support$read_df() %>%
    mutate(x1 = as.integer(x1),
           x4 = as.integer(x4),
           x5 = as.integer(x5),
           x6 = as.integer(x6),
           status = event,
           time = as.numeric(duration)) %>%
    select(-event, -duration) %>%
    drop_na()
}

create_grace = function(...) {
  data.frame(mlr3::tsk("grace")$data()) %>%
    drop_na()
}
create_whas = function(...) {
  data.frame(mlr3::tsk("whas")$data()) %>%
    drop_na()
}

create_melanoma = function(...) {
  df = MASS::Melanoma %>%
    mutate(status = if_else(status == 1L, 1L, 0L),
           time = as.numeric(time),
           age = as.numeric(age),
           year = factor(year)) %>%
    drop_na()
  df$year = forcats::fct_collapse(
      df$year,
      "<=1966" = c("1962", "1964", "1965", "1966"),
      ">=1973" = c("1973", "1974", "1977")
    )
  return(df)
}
create_aids2 = function(...) {
  MASS::Aids2 %>%
    mutate(time = as.numeric(death - diag),
           sexF = if_else(sex == "F", 1L, 0L),
           status = if_else(status == "D", 1L, 0L),
           age = as.numeric(age)) %>%
    select(-diag, -death, -sex) %>%
    drop_na() %>%
    filter(time > 0)
}

create_tumor = function(...) {
  pammtools::tumor %>%
    mutate(time = days,
           transfusion = if_else(transfusion == "yes", 1L, 0L),
           complications = if_else(complications == "yes", 1L, 0L),
           metastases = if_else(metastases == "yes", 1L, 0L),
           resection = if_else(resection == "yes", 1L, 0L),
           charlson_score = factor(charlson_score),
           sexF = as.integer(sex == "female"),
           age = as.numeric(age)) %>%
    select(-days, -sex) %>%
    as.data.frame() %>%
    drop_na()
}
create_patient = function(...) {
  pammtools::patient %>%
    mutate(time = Survdays,
           status = as.integer(PatientDied),
           sexF = if_else(Gender == "Female", 1L, 0L),
           age = as.numeric(Age)) %>%
    select(-CombinedicuID, -CombinedID, -Survdays, -PatientDied, -survhosp,
           -Gender, -Age) %>%
    drop_na()
}

create_bmt = function(...){
  data(bmt, package = "KMsurv")
  data = bmt %>%
    mutate(group = factor(group),
           time = as.numeric(t1),
           status = d1,
           z1 = as.numeric(z1),
           z2 = as.numeric(z2),
           tp = as.numeric(tp),
           z7 = as.numeric(z7),
           z9 = factor(z9)
    ) %>%
    select(-t1, -d1, -t2, -d2, -d3) %>%
    drop_na()
  rm(bmt, envir = .GlobalEnv)
  data
}
create_channing = function(...){
  data(channing, package = "KMsurv")
  data = channing %>%
    mutate(time = as.numeric(time),
           status = death,
           genderF = as.integer(gender - 1),
           ageentry = as.numeric(ageentry)
    ) %>%
    select(-obs, -age, -death, -gender) %>%
    drop_na() %>%
    filter(time > 0)
  rm(channing, envir = .GlobalEnv)
  data
}
create_kidtran = function(...){
  data(kidtran, package = "KMsurv")
  data = kidtran %>%
    mutate(time = as.numeric(time),
           status = delta,
           genderF = as.integer(gender - 1),
           raceBlack = as.integer(race - 1),
           age = as.numeric(age)
    ) %>%
    select(-obs, -delta, -gender) %>%
    drop_na()
  rm(kidtran, envir = .GlobalEnv)
  data
}

create_hepatoCellular = function(...) {
  asaur::hepatoCellular %>%
    mutate(status = as.integer(Death),
           time = as.numeric(OS),
           Age = as.numeric(Age),
           Tumordifferentiation = as.integer(Tumordifferentiation - 1),
           Tumormultiplicity = as.integer(Tumormultiplicity - 1),
           Tumorsize = as.integer(Tumorsize - 1),
           TNM = as.integer(TNM - 1),
           BCLC = as.integer(BCLC - 1),
           AFP = as.integer(AFP - 1),
           ALT = as.integer(ALT - 1),
           AST = as.integer(AST - 1)) %>%
    select(-Death, -OS, -Number, -Recurrence, -RFS, -HBsAg) %>%
    drop_na()
}
create_pharmacoSmoking = function(...) {
  asaur::pharmacoSmoking %>%
    mutate(sexF = as.integer(gender == "Female"),
           age = as.numeric(age),
           grpCombination = as.integer(grp == "combination"),
           time = ttr,
           status = relapse,
           race = factor(race),
           employment = factor(employment),
           smokingHeavy = as.integer(levelSmoking == "heavy"),
           yearsSmoking = as.numeric(yearsSmoking),
           longestNoSmoke = as.numeric(longestNoSmoke),
           priorAttempts = as.numeric(priorAttempts)
    ) %>%
    select(-id, -ageGroup2, -ageGroup4, -gender, -grp, -ttr, -relapse, -levelSmoking) %>%
    drop_na() %>%
    filter(time > 0)
}
create_prostateSurvival = function(...) {
  asaur::prostateSurvival %>%
    mutate(
      gradeMode = as.integer(grade == "mode"),
      stage = factor(stage),
      ageGroup = factor(ageGroup),
      time = as.numeric(survTime),
      status = if_else(status == 1, 1L, 0L)
    ) %>%
    select(-grade, -survTime) %>%
    drop_na() %>%
    filter(time > 0)
}

create_ALL = function(...){
  data(ALL, package = "dynpred")
  data = ALL %>%
    mutate(time = as.numeric(srv),
           status = as.integer(srv.s),
           year = factor(year),
           agecl = factor(agecl),
           proph = as.integer(proph == "yes"),
           match = as.integer(match == "gender mismatch")
    ) %>%
    select(-id, -rec, -rec.s, -ae, -ae.s, -recae, -recae.s, -rel, -rel.s,
           -srv, -srv.s) %>%
    drop_na()
  rm(ALL, envir = .GlobalEnv)
  data
}
create_nki = function(...){
  data(nki, package = "dynpred")
  data = nki %>%
    mutate(time = as.numeric(tyears),
           status = as.integer(d),
           diameter = as.numeric(diameter),
           posnodes = as.numeric(posnodes),
           age = as.numeric(age),
           mlratio = as.numeric(mlratio),
           chemotherapy = as.integer(chemotherapy == "Yes"),
           hormonaltherapy = as.integer(hormonaltherapy == "Yes"),
           surgeryExcision = as.integer(typesurgery == "excision"),
           histolgrade = factor(histolgrade),
           vasc.invasion = factor(vasc.invasion),
           PICV = as.numeric(PICV)
    ) %>%
    select(-patnr, -d, -tyears, -typesurgery, -crossval.clin.class) %>%
    drop_na()
  rm(nki, envir = .GlobalEnv)
  data
}
create_ova = function(...){
  data(ova, package = "dynpred")
  data = ova %>%
    mutate(time = tyears,
           status = as.integer(d),
           Broders = factor(Broders),
           FIGOIII = as.integer(FIGO == "III"),
           Ascites = factor(Ascites),
           Diam = factor(Diam)
    ) %>%
    select(-tyears, -d, -FIGO, -id) %>%
    drop_na()
  rm(ova, envir = .GlobalEnv)
  data
}
create_wbc1 = function(...){
  data(wbc1, package = "dynpred")
  data = wbc1 %>%
    mutate(time = as.numeric(tyears),
           status = as.integer(d)
    ) %>%
    select(-tyears, -d, -patnr) %>%
    drop_na()
  rm(wbc1, envir = .GlobalEnv)
  data
}
