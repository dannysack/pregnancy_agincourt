library(tidyverse)
library(Hmisc)
library(lubridate)

# I started by pulling each sheet off the password-protected excel file as csv files

# set directory
setwd("/Users/sackd/Box Sync/Vanderbilt University/PhD/Fogarty/Secondary Analysis/data/")

# read in csvs
births <- read_csv("births.csv")
pregnancies <- read_csv("pregnancies.csv")
individuals <- read_csv("individuals.csv")
resstat <- read_csv("resstat.csv")
ses <- read_csv("ses.csv")
ind_ses <- read_csv("individuals_ses.csv")

# pregnancies is my main file, need to add birthweight from the births file based on "Pregnancy" (Id does not match for some reason)

rawdata <- full_join(pregnancies, births %>% select(-Id), by = c("Pregnancy"))

# now need to add refugee status and birthdate to "data" from "inidivduals", remembering that there 
# are likely multiple pregnacies per person
# am doing with a loop, because why not...
# using "1900-01-01" as starting date so formatting is correct

# set up blank columns
rawdata <- rawdata %>% mutate(Refugee = NA,
                        DoB = as.Date("1900-01-01"),
                        DoBEstimated = NA,
                        ObsYear = year(ObservationDate))

# loop through columns matching by individual ID (doing it a chunk at a time)
for(i in 1:nrow(individuals)){
  rawdata$Refugee[rawdata$Id == individuals$Id[i]] <- individuals$Refugee[i]
  rawdata$DoB[rawdata$Id == individuals$Id[i]] <- individuals$DoB[i]
  rawdata$DoBEstimated[rawdata$Id == individuals$Id[i]] <- individuals$DoBEstimated[i]
}

# now need to add resstat to rawdata by observation year and Id and remove duplicate rows
rawdata <- left_join(rawdata, resstat) %>% distinct()

# check years with ses data
table(ses$Year)
# 2001, 2003, 2005, 2007, 2009, 2011, 2013:2020
# see distribution of absolute ses
hist(ses$SESabs)
# get a sense of missingness of total SES for later
table(round(ses$SESabs, 1), useNA = "always")
# range from 0 to 4.2 with no data missing

# now combine individuals_ses with ses to link absolute SES to individual ID by household ID

# have to, in order
# 1) Id in rawdata match Id in ind_ses
# 2) HouseholdId in ind_ses match HousholdID in ses
# 3) Year in ses match ObsYear in rawdata

# set up blank columns
rawdata <- rawdata %>% mutate(SESabs = NA)

# loop through columns and match (need to figure out edge cases)
for(i in 1:nrow(rawdata)){
  # set Id
  loopId <- rawdata$Id[i]
  # subset ind_ses
  loophousehold <- ind_ses %>% 
    filter(Id == loopId) %>% 
    pull(HouseholdId)
  # find ses for given year
  loopses <- ses %>% 
    filter(HouseholdID %in% loophousehold) %>%
    filter(Year == rawdata$ObsYear[i]) %>% 
    pull(SESabs)
  # set as final
  # if no SES matches, fill as missing (length of vector will be zero)
  # if more than one household with SES matching individual in a given year, take the mean
  rawdata$SESabs[i] <- ifelse(length(loopses) == 0, NA, mean(loopses, na.rm = TRUE))
}

# check how many missing birth dates
table(rawdata$DoB == "1900-01-01")

# check ages (so can get a sense of misclassification)
table(round((rawdata$DeliveryDate - rawdata$DoB) / 365.25, 0))

# check for differences b/t Delivery Date and Observation Date (should be less than a year)
table(round((rawdata$ObservationDate - rawdata$DeliveryDate) / 365.25))

# see which years are funky
table(year(rawdata$ObservationDate))
sort(unique(year(rawdata$ObservationDate)))
table(year(rawdata$DeliveryDate))
sort(unique(year(rawdata$DeliveryDate)))
table(year(rawdata$DoB))
sort(unique(year(rawdata$DoB)))

# how many times Delivery Date == Observation Date | DoB
table(rawdata$DeliveryDate == rawdata$ObservationDate) # 106 instances
table(rawdata$DeliveryDate == rawdata$DoB) # five instances
table(rawdata$ObservationDate == rawdata$DoB) # never


# now need to clean

#AntenatalClinic
table(rawdata$AntenatalClinic, useNA = "always")
rawdata$AntenatalClinic[rawdata$AntenatalClinic == "N"] <- "No"
rawdata$AntenatalClinic[rawdata$AntenatalClinic == "Y" | rawdata$AntenatalClinic == "y"] <- "Yes"
rawdata$AntenatalClinic[rawdata$AntenatalClinic == "X" | rawdata$AntenatalClinic == "Q"] <- NA
rawdata$AntenatalClinic <- factor(rawdata$AntenatalClinic, levels = c("Yes", "No"))

# Education
table(rawdata$Education, useNA = "always")
rawdata$Education[rawdata$Education == "-"] <- NA
rawdata$Education[rawdata$Education == "0"] <- 12
rawdata$Education[rawdata$Education == "1"] <- 3
rawdata$Education[rawdata$Education == "2"] <- 4
rawdata$Education[rawdata$Education == "3"] <- 5
rawdata$Education[rawdata$Education == "4"] <- 6
rawdata$Education[rawdata$Education == "5"] <- 7
rawdata$Education[rawdata$Education == "6"] <- 8
rawdata$Education[rawdata$Education == "7"] <- 9
rawdata$Education[rawdata$Education == "8"] <- 10
rawdata$Education[rawdata$Education == "9"] <- 11
rawdata$Education[rawdata$Education == "A"] <- 1
rawdata$Education[rawdata$Education == "A1"] <- 2 # adult basic education level 1, comparable to grade 1-3
rawdata$Education[rawdata$Education == "A2"] <- 5 # adult basic education level 2, comparable to grade 4-6
rawdata$Education[rawdata$Education == "A3"] <- 7 # adult basic education level 3, comparable to grade 7-8
rawdata$Education[rawdata$Education == "A4"] <- 9 # adult basic education level 4, comparable to grade 9
rawdata$Education[rawdata$Education == "B"] <- 2
rawdata$Education[rawdata$Education == "C"] <- 0 # daycare
rawdata$Education[rawdata$Education == "L"] <- 13 # college #  looks like transcrition error, will do 13
rawdata$Education[rawdata$Education == "L1"] <- 13 # at least one year of collect
rawdata$Education[rawdata$Education == "L2"] <- 13 # incomplete college
rawdata$Education[rawdata$Education == "R"] <- 0.5 # daycare
rawdata$Education[rawdata$Education == "M1"] <- 1 # mozambique years completed
rawdata$Education[rawdata$Education == "M2"] <- 2
rawdata$Education[rawdata$Education == "M3"] <- 3 
rawdata$Education[rawdata$Education == "M4"] <- 4
rawdata$Education[rawdata$Education == "M5"] <- 5
rawdata$Education[rawdata$Education == "M6"] <- 6
rawdata$Education[rawdata$Education == "M7"] <- 7
rawdata$Education[rawdata$Education == "M8"] <- 8
rawdata$Education[rawdata$Education == "M9"] <- 9
rawdata$Education[rawdata$Education == "M10"] <- 10
rawdata$Education[rawdata$Education == "M11"] <- 11
rawdata$Education[rawdata$Education == "M12"] <- 12
rawdata$Education[rawdata$Education == "N"] <- 0 # none
rawdata$Education[rawdata$Education == "N1"] <- 9 # NQF 1 National Qualification Framework Level 1, equivalent to grade 9
rawdata$Education[rawdata$Education == "N2"] <- 10 # NQF 1 National Qualification Framework Level 2, equivalent to grade 10
rawdata$Education[rawdata$Education == "N3"] <- 11 # NQF 1 National Qualification Framework Level 3, equivalent to grade 11
rawdata$Education[rawdata$Education == "N4"] <- 14 # NQF 1 National Qualification Framework Level 4, equivalent to grade 12
rawdata$Education[rawdata$Education == "Q"] <- NA # query
rawdata$Education[rawdata$Education == "T1"] <- 13 # incomplete technical degree
rawdata$Education[rawdata$Education == "T2"] <- 13 # incomplete techical school
rawdata$Education[rawdata$Education == "U"] <- 13 # one year of univesity # looks like transcription error, will do 13
rawdata$Education[rawdata$Education == "U1"] <- 14 # started university
rawdata$Education[rawdata$Education == "U2"] <- 15 # complete university
rawdata$Education[rawdata$Education == "X"] <- NA # unknown
rawdata$Education[rawdata$Education == "H"] <- 14 # depretiated higher education

# make numeric
rawdata$Education <- as.numeric(rawdata$Education)

# Scholar
table(rawdata$Scholar, useNA = "always")
rawdata$Scholar[rawdata$Scholar == "n" | rawdata$Scholar == "N"] <- "No"
rawdata$Scholar[rawdata$Scholar == "y" | rawdata$Scholar == "Y"] <- "Yes"
rawdata$Scholar[rawdata$Scholar == "Q" | rawdata$Scholar == "X"] <- NA
rawdata$Scholar <- factor(rawdata$Scholar, levels <- c("No", "Yes"))

# Back to school
table(rawdata$BackToSchool, useNA = "always")
rawdata$BackToSchool[rawdata$BackToSchool == "-"] <- "Not Applicable"
rawdata$BackToSchool[rawdata$BackToSchool == "N"] <- "No"
rawdata$BackToSchool[rawdata$BackToSchool == "Y"] <- "Yes"
rawdata$BackToSchool[rawdata$BackToSchool == "I"] <- "Intend To"
rawdata$BackToSchool[rawdata$BackToSchool == "X" | rawdata$BackToSchool == "Q"] <- NA
rawdata$BackToSchool <- factor(rawdata$BackToSchool, levels = c("Not Applicable", "Yes", "Intend To", "No"))

# Planned Pregnancy
table(rawdata$PregnancyPlanned, useNA = "always")
rawdata$PregnancyPlanned[rawdata$PregnancyPlanned == "N"] <- "No"
rawdata$PregnancyPlanned[rawdata$PregnancyPlanned == "Y"] <- "Yes"
rawdata$PregnancyPlanned[rawdata$PregnancyPlanned == "O"] <- "Other"
rawdata$PregnancyPlanned[rawdata$PregnancyPlanned == "X" | rawdata$PregnancyPlanned == "Q"] <- NA
rawdata$PregnancyPlanned <- factor(rawdata$PregnancyPlanned, levels = c("No", "Yes", "Other"))

# Contraception Before (will code, but not run because variable missing)
table(rawdata$ContraceBe, useNA = "always")
rawdata$ContraceBe[rawdata$ContraceBe == "A"] <- "Abstinence" 
rawdata$ContraceBe[rawdata$ContraceBe == "C"] <- "Condom"
rawdata$ContraceBe[rawdata$ContraceBe == "I"] <- "Injection"
rawdata$ContraceBe[rawdata$ContraceBe == "L"] <- "Loop"
rawdata$ContraceBe[rawdata$ContraceBe == "M"] <- ">1 Type"
rawdata$ContraceBe[rawdata$ContraceBe == "N"] <- "None"
rawdata$ContraceBe[rawdata$ContraceBe == "P"] <- "Pill"
rawdata$ContraceBe[rawdata$ContraceBe == "S"] <- "Sterilisation"
rawdata$ContraceBe[rawdata$ContraceBe == "T"] <- "Traditional"
rawdata$ContraceBe[rawdata$ContraceBe == "X" | rawdata$ContraceBe == "Q"] <- NA
rawdata$ContraceBe <- factor(rawdata$ContraceBe, levels = c("None", "Injection", "Pill", "Condom", 
                                                             "Sterilisation", ">1 Type",
                                                            "Traditional","Loop", "Abstinence"))


# Contraception After (will code, but not run because variable missing)
table(rawdata$ContraceAf, useNA = "always")
rawdata$ContraceAf[rawdata$ContraceAf == "A"] <- "Abstinence" 
rawdata$ContraceAf[rawdata$ContraceAf == "C"] <- "Condom"
rawdata$ContraceAf[rawdata$ContraceAf == "E"] <- "Emergency Contraceptive" 
rawdata$ContraceAf[rawdata$ContraceAf == "I"] <- "Injection"
rawdata$ContraceAf[rawdata$ContraceAf == "L"] <- "Loop"
rawdata$ContraceAf[rawdata$ContraceAf == "M"] <- ">1 Type"
rawdata$ContraceAf[rawdata$ContraceAf == "N"] <- "None"
rawdata$ContraceAf[rawdata$ContraceAf == "P"] <- "Pill"
rawdata$ContraceAf[rawdata$ContraceAf == "S"] <- "Sterilisation"
rawdata$ContraceAf[rawdata$ContraceAf == "T"] <- "Traditional"
rawdata$ContraceAf[rawdata$ContraceAf == "X" | rawdata$ContraceAf == "Q"] <- NA
rawdata$ContraceAf <- factor(rawdata$ContraceAf, levels = c("None", "Injection", "Pill", "Condom", 
                                                            "Emergency Contraceptive", "Sterilisation", 
                                                            ">1 Type",
                                                            "Traditional","Loop", "Abstinence"))

# Delivery Date (create new cateogry that is just month and year and second that is just year)
rawdata$DeliveryMonth <- floor_date(rawdata$DeliveryDate, unit = "month")
rawdata$DeliveryYear <- year(rawdata$DeliveryDate)

# Delivery Place
table(rawdata$DeliveryPlace, useNA = "always")
rawdata$DeliveryPlace[rawdata$DeliveryPlace == "+"] <- "Hospital"
rawdata$DeliveryPlace[rawdata$DeliveryPlace == "C"] <- "Clinic"
rawdata$DeliveryPlace[rawdata$DeliveryPlace == "H"] <- "Home"
rawdata$DeliveryPlace[rawdata$DeliveryPlace == "N"] <- "Health Centre"
rawdata$DeliveryPlace[rawdata$DeliveryPlace == "O"] <- "Other"
rawdata$DeliveryPlace[rawdata$DeliveryPlace == "Q" | rawdata$DeliveryPlace == "X"] <- NA
rawdata$DeliveryPlace <- factor(rawdata$DeliveryPlace, levels = c("Hospital", "Home",
                                                                  "Clinic", "Health Centre",
                                                                  "Other"))

# Hospital where baby was delivered
table(rawdata$Hospital, useNA = "always")
rawdata$Hospital[rawdata$Hospital == "-"] <- "Not Applicable"
rawdata$Hospital[rawdata$Hospital == "MP"] <- "Mapulaneng"
rawdata$Hospital[rawdata$Hospital == "MT"] <- "Matikwane"
rawdata$Hospital[rawdata$Hospital == "O"] <- "Other"
rawdata$Hospital[rawdata$Hospital == "PM"] <- "Pietersburg"
rawdata$Hospital[rawdata$Hospital == "RF"] <- "Rob Ferreria"
rawdata$Hospital[rawdata$Hospital == "T"] <- "Tinswalo"
rawdata$Hospital[rawdata$Hospital == "Q" | rawdata$Hospital == "X"] <- NA
rawdata$Hospital <- factor(rawdata$Hospital, levels = c("Matikwane", "Not Applicable", "Mapulaneng",
                                                        "Other", "Tinswalo", "Rob Ferreria", 
                                                        "Pietersburgh"))

# Attendant
table(rawdata$Attendant, useNA = "always")
rawdata$Attendant[rawdata$Attendant == "B"] <- "Nobody"
rawdata$Attendant[rawdata$Attendant == "C"] <- "Community Member"
rawdata$Attendant[rawdata$Attendant == "D"] <- "Doctor"
rawdata$Attendant[rawdata$Attendant == "F"] <- "Family Member"
rawdata$Attendant[rawdata$Attendant == "M"] <- "Midwife"
rawdata$Attendant[rawdata$Attendant == "N"] <- "Nurse"
rawdata$Attendant[rawdata$Attendant == "O"] <- "Other"
rawdata$Attendant[rawdata$Attendant == "Q" | rawdata$Attendant == "X"] <- NA
rawdata$Attendant <- factor(rawdata$Attendant, levels = c("Nurse", "Doctor", "Family Member",
                                                          "Nobody", "Community Member", "Other",
                                                          "Midwife"))

# Complication
table(rawdata$Complication, useNA = "always")
rawdata$Complication[rawdata$Complication == "C"] <- "Caesarian"
rawdata$Complication[rawdata$Complication == "N"] <- "None"
rawdata$Complication[rawdata$Complication == "O"] <- "Other"
rawdata$Complication[rawdata$Complication == "Q" | rawdata$Complication == "X"] <- NA
rawdata$Complication <- factor(rawdata$Complication, levels = c("None", "Caesarian", "Other"))

# Outcome
table(rawdata$OutCome, useNA = "always")
rawdata$OutCome[rawdata$OutCome == "A"] <- "Abortion"
rawdata$OutCome[rawdata$OutCome == "L" | rawdata$OutCome == "l"] <- "Single Live Birth"
rawdata$OutCome[rawdata$OutCome == "ML"] <- "Multiple Live Born"
rawdata$OutCome[rawdata$OutCome == "MM" | rawdata$OutCome == "mm"] <- "Multiple Some Stillborn"
rawdata$OutCome[rawdata$OutCome == "MS"] <- "Multiple Stillborn"
rawdata$OutCome[rawdata$OutCome == "S"] <- "Single Stillborn"
rawdata$OutCome[rawdata$OutCome == "Q" | rawdata$OutCome == "X"] <- NA
rawdata$OutCome <- factor(rawdata$OutCome, levels = c("Single Live Birth", "Single Stillborn",
                                                      "Multiple Live Born", "Multiple Some Stillborn",
                                                      "Abortion", "Multiple Stillborn"))

# Refugee
table(rawdata$Refugee, useNA = "always")
rawdata$Refugee[rawdata$Refugee == "M"] <- "Mozambican"
rawdata$Refugee[rawdata$Refugee == "N"] <- "South African"
rawdata$Refugee[rawdata$Refugee == "Y"] <- "Mozambican"
rawdata$Refugee[rawdata$Refugee == "O"] <- "Other"
rawdata$Refugee[rawdata$Refugee == "X" | rawdata$Refugee == "Q"] <- NA
rawdata$Refugee <- factor(rawdata$Refugee, levels = c("South African", "Mozambican", "Other"))

# ResStatus
table(rawdata$ResStatus, useNA = "always")
rawdata$ResStatus[rawdata$ResStatus == "P"] <- "Resident"
rawdata$ResStatus[rawdata$ResStatus == "M"] <- "Migrant"
rawdata$ResStatus[rawdata$ResStatus == "O"] <- "Other"
rawdata$ResStatus[rawdata$ResStatus == "C"] <- "Other"
rawdata$ResStatus[rawdata$ResStatus == "E"] <- "Other"
rawdata$ResStatus[rawdata$ResStatus == "W"] <- "Other"
rawdata$ResStatus[rawdata$ResStatus == "V"] <- "Other"
rawdata$ResStatus[rawdata$ResStatus == "Q" | rawdata$ResStatus == "X"] <- NA
rawdata$ResStatus <- factor(rawdata$ResStatus, levels = c("Resident", "Migrant", "Other"))

# Now add some things of interest

# age (in years)
rawdata$AgeAtDelivery <- as.numeric((rawdata$DeliveryDate - rawdata$DoB)/365.25)

# Time to Observation (in days)
rawdata$ObsToDelivery <- as.numeric(rawdata$ObservationDate - rawdata$DeliveryDate)

# Obseration Date (create new cateogry that is just month and year and second that is just year)
rawdata$ObservationMonth <- floor_date(rawdata$ObservationDate, unit = "month")
rawdata$ObservationYear <- year(rawdata$ObservationDate)

# Pregnancy Number

# first need to sort by delivery date, then add number
rawdata <- rawdata[order(rawdata$DeliveryDate),]
rawdata$PregnancyNumber <- ave(as.numeric(rawdata$DeliveryDate - as.Date("1990-01-01")),
                               rawdata$Id, FUN = seq_along)

# now we are in business

# delete identifying covariates
deidentified <- rawdata %>% select(Id, Pregnancy, DeliveryMonth, DeliveryYear, ObservationMonth, 
                                   ObservationYear, AgeAtDelivery, ObsToDelivery,
                                   PregnancyNumber, OutCome, LiveBorn, StillBorn, Complication, Attendant,
                                   DeliveryPlace, Hospital, ContraceBe, ContraceAf, PregnancyPlanned, 
                                   Scholar, BackToSchool, Education, AntenatalClinic, AntenatalVisits,
                                   Birthweight, Refugee, ResStatus, SESabs)

# add labels to columns
label(deidentified$DeliveryMonth) <- "Delivery Month"
label(deidentified$DeliveryYear) <- "Delivery Year"
label(deidentified$ObservationMonth) <- "Observation Month"
label(deidentified$ObservationYear) <- "Observation Year"
label(deidentified$PregnancyNumber) <- "Recorded Pregnancy"
label(deidentified$ObsToDelivery) <- "Days between Observation and Delivery"
units(deidentified$ObsToDelivery) <- "days"
label(deidentified$AgeAtDelivery) <- "Age"
units(deidentified$AgeAtDelivery) <- "years"
label(deidentified$AntenatalVisits) <- "Antenatal Visits"
label(deidentified$Education) <- "(Equivalent) Years of Education"
units(deidentified$Education) <- "years"
label(deidentified$Scholar) <- "Student During Pregnancy"
label(deidentified$BackToSchool) <- "Back to School Status"
label(deidentified$PregnancyPlanned) <- "Pregnancy Intention"
label(deidentified$ContraceBe) <- "Contracption Use Prior to Pregnancy"
label(deidentified$ContraceAf) <- "Using or Intending to Use Postpartum Contraception"
label(deidentified$DeliveryPlace) <- "Delivery Location"
label(deidentified$Attendant) <- "Birth Attendant"
label(deidentified$Complication) <- "Complication"
label(deidentified$OutCome) <- "Birth Outcome"
label(deidentified$Birthweight) <- "Birthweight"
units(deidentified$Birthweight) <- "kilograms"
label(deidentified$Refugee) <- "Nationality"
label(deidentified$ResStatus) <- "Migrant"
label(deidentified$SESabs) <- "Household Absolute SES"

# save for use in analysis.Rmd
Save(deidentified)