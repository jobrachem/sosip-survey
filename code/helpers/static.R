#### German names for GLMM on QRPs ####
int <- "(Intercept)"

ag <- "Alter"
g.1 <- "Geschlecht - Männlich"
g.2 <- "Geschlecht - Divers"
g.3 <- "Geschlecht - Keine Antwort"

sf1 <- "Studienfortschritt"
sf2 <- "SF"
sf.ma <- " - Master"
sf.o <- " - Andere"
sf.n <- " - Studiere nicht (mehr)"

sm <- "Semester"
nproj <- "Anzahl emp. Projekte"
inter <- "Interesse"
imp <- "Wichtigkeit"
info <- "Eindruck von Informiertheit"

osi.uni <- "OSI an der Uni - Ja"
osi1 <- "OSI bekannt - Unsicher"
osi2 <- "OSI bekannt - Ja"

rk1 <- "RK gelehrt - Unsicher"
rk2 <- "RK gelehrt - Ja"

prk <- "Praktik gelehrt - Ja"

proj1 <- "Projekt – Anderes"
proj2 <- "Projekt – Projektarbeit"
proj3 <- "Projekt – Bachelorarbeit"
proj4 <- "Projekt – Masterarbeit"

dem <- c(ag, g.1, g.2, g.3)
sfa <- c(paste0(sf1, sf.ma), paste0(sf1, sf.o))
sfb <- c(paste0(sf2, sf.n), paste0(sf2, sf.ma))
sfc <- c(paste0(sf2, sf.n), paste0(sf2, sf.ma), paste0(sf2, sf.o))
att <- c(inter, imp, info)
osi <- c(osi.uni, osi1, osi2)
rk <- c(rk1, rk2)
projx <- c(proj1, proj2, proj3, proj4)

na1 <- c(int, dem, sfc, nproj, att, osi, rk, prk, projx)
na2 <- c(int, dem, sfc, nproj, att, osi, rk, projx)
na3 <- c(int, sfb, proj3)
na4a <- c(int, dem, sfc, nproj, att, osi, rk, prk, "Projekt – Linear", "Projekt – Quadratisch")
na4b <- c(int, dem, sfc, nproj, att, osi, rk, "Projekt – Linear", "Projekt – Quadratisch")
na5 <- c(int, dem, sfc, nproj, att, osi)
na6 <- c(int, dem, sfc, nproj, inter, imp, osi, rk)
na7 <- c(int, dem, sfc, nproj, inter, info, osi, rk)

ref.sex <- "Weiblich (Geschlecht)"
ref.stage <- "Bachelor (Studienfortschritt)"
ref.osi_at_uni <- "Nein (OSI an der Uni)"
ref.osi <- "Nein (OSI bekannt)"
ref.rc <- "Nein (RK gelehrt)"
ref.practice_taught <- "Nein (Praktik gelehrt)"
ref.qrp <- "Fehlende Stichprobenplanung (QRP)"
ref.proj <- "Expra (Projekt)"

abk.glmer <- "B – Beta-Gewicht, SE – Standard Error [Standardfehler], OR – Odds Ratio [Chancenverhältnis], KI - Konfidenzintervall"
abk.osi <- "OSI - Open Science Initiative"
abk.rk <- "RK - Replikationskrise"
abk.qrp <- "QRP – Questionable Research practice [Fragwürdige Forschungspraktik]"
abk.sb <- "SB – Selektives Berichten"
abk.sf <- "SF - Studienfortschritt"

#### Universities with an open science initative listed in NOSI ####
# Extracted in february 2019
nosi_unis <- c(
  "Ludwig-Maximilians-Universität München",
  "Universität Koblenz-Landau",
  "Fern-Universität in Hagen",
  "Westfälische Wilhelms-Universität Münster",
  "Georg-August-Universität Göttingen",
  "Universität Wien",
  "Humboldt-Universität Berlin",
  "Universität Leipzig",
  "Philipps-Universität Marburg",
  "Universität zu Köln",
  "Universität Zürich",
  "Goethe-Universität Frankfurt",
  "Technische Universität Dresden"
)
#### LABELS ####
lab.stage <- c("Bachelor", "Master", "I don't study (any more)", "Other")
lab.field <- c("Psychology", "Other")
lab.yn1 <- c("Yes", "No")
lab.yn2 <- c("Yes", "No", "Not Sure")
lab.degree <- c("B.Sc.", "M.Sc.", "Diploma", "B.A.", "M.A.")
lab.sex <- c("male", "female", "diverse", "no answer")
lab.rc_lectures <- c("yes", "no", "unsure")
lab.project <- c("emp.intern", "project", "thesis.bsc", "thesis.msc", "other")

#### MISC ####
stats_repl <- c(
  "[Ss][Pp][Ss][Ss]" = "SPSS",
  "^[Rr].*" = "R",
  "_[Rr]$" = "R",
  "G-Power" = "G*Power",
  "[Ss][Tt][Aa][Tt][Aa]" = "STATA",
  "[Jj]amovi" = "Jamovi",
  "SYSTAT" = "Systat",
  "Amos" = "AMOS"
)

qrp_names <- c("SR of Variables", "No Sample Planning", "Flexible Sample Size",
               "SR of Conditions", "Flexible Analysis", "Flexible Exclusion", 
               "SR of Hypotheses", "HARKing", "Rounding p-Values")

orp_names <- c("Power Analysis", "Preregistration")

qrp_ids <- c("1" = "SR of Variables", "2" = "No Sample Planning", 
             "3" = "Flexible Sample Size", "4" = "SR of Conditions", 
             "5" = "Flexible Analysis", "6" = "Flexible Exclusion", 
             "7" = "SR of Hypotheses", "8" = "HARKing", 
             "9" = "Rounding p-Values", "10" = "None", "11" = "Not Sure")

n.cor <- c("Alter", "Semester", "Wichtigkeit", "Subj. Inform.", "Interesse",
           "Anz. Projekte")