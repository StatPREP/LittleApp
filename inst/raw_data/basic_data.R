#' Create the basic data sets for the LittleApps
#' @details
#' **Format for a LittleApp data set**
#' Each Little App data set is a single .rda file with four named components:
#' - frame the actual data frame
#' - types another data frame giving a short description of each variable. See LA_var_types().
#' - codebook: A list with a component named by each variable. The value is a character
#' string describing the variable.
#' - overall: A character string describing the overall data frame
#'
#' These four components are saved into a single .rda file with the name to be
#' displayed for the data set.
#' Inside the Little App, the .rda will be `load()`ed into an environment called
#' `the_data` so that the individual elements can be referred to by "frame", "types", etc.
#'

data(NHANES, package = "NHANES")
library(littleapp2)

SDSdata::sds_setup()

frame <-
    NHANES %>%
    dplyr::select(
      sex = "Gender",
      age = "Age",
      months = "AgeMonths",
      marital = "MaritalStatus",
      education = "Education",
      race = "Race1",
      income = "HHIncomeMid",
      income_poverty = "Poverty",
      home_rooms = "HomeRooms",
      home_own = "HomeOwn",
      bmi = "BMI",
      bmi_who = "BMI_WHO",
      pulse = "Pulse",
      systolic = "BPSysAve",
      diastoic = "BPDiaAve",
      testosterone = "Testosterone",
      direct_chol = "DirectChol",
      tot_chol = "TotChol",
      weight = "Weight",
      height = "Height",
      length = "Length",
      urine_vol = "UrineFlow2",
      urine_flow = "UrineVol2",
      diabetes = "Diabetes",
      diabetes_age = "DiabetesAge",
      health_general = "HealthGen",
      phys_health_bad_days = "DaysPhysHlthBad",
      mental_health_bad_days = "DaysMentHlthBad",
      little_interest = "LittleInterest",
      depressed = "Depressed",
      n_pregnancies = "nPregnancies",
      n_babies = "nBabies",
      age_first_baby = "Age1stBaby",
      pregant_currently = "PregnantNow",
      sleep_hours = "SleepHrsNight",
      sleep_trouble = "SleepTrouble",
      physically_active = "PhysActive",
      active_days = "PhysActiveDays",
      tv_hours = "TVHrsDay",
      comp_hours = "CompHrsDay",
      alcohol_drinks = "AlcoholDay",
      alcohol_days = "AlcoholYear",
      smoke = "SmokeNow",
      smoke100 = "Smoke100",
      smoke_age = "SmokeAge",
      marijuana = "Marijuana",
      marijuana_age = "AgeFirstMarij",
      marijuana_regular = "RegularMarij",
      age_sex = "SexAge",
      orientation_sex = "SexOrientation") %>%
    mutate(age = ifelse(is.na(months), age, months/12)) %>%
    dplyr::select(- months)
# create some binary variables
frame$home_type <- ifelse(frame$home_own == "Other", NA, as.character(frame$home_own))
frame$days_with_no_interest <- ifelse(frame$little_interest  == "None", "No", "Yes")
frame$ever_married <- ifelse(frame$marital %in% c("LivePartner", "NeverMarried"), "No",  "Yes")
frame$height_adults <- ifelse(frame$age >= 18, frame$height, NA)
frame$weight_adults <- ifelse(frame$age >= 18, frame$weight, NA)
frame$BMI_adults <- ifelse(frame$age >= 18, frame$bmi, NA)
codebook <- list(
  sex = "Sex of study participant	coded as male or female",
  age = "Age in years at screening of study participant. Note: Subjects 80 years or older were recorded as 80.",
  marital = "Marital status of study participant. Reported for participants aged 20 years or older. One of Married, Widowed, Divorced, Separated, NeverMarried, or LivePartner (living with partner).",
  ever_married  = "Was the participant ever married",
  race = "Reported race of study participant: Mexican, Hispanic, White, Black, or Other.",
  education = "Educational level of study participant Reported for participants aged 20 years or older. One of 8thGrade, 9-11thGrade, HighSchool, SomeCollege, or CollegeGrad.",
  income = "Total annual gross income for the household in US dollars. The levels are categorical: the number refers to the middle income in a category.",
  income_poverty = "The ratio of income in dollars to the poverty level of income",
  home_rooms = "How many rooms are in home of study participant (counting kitchen but not bathroom). 13 rooms = 13 or more rooms.",
  home_own = "One of Home, Rent, or Other indicating whether the home of study participant or someone in their family is owned, rented or occupied by some other arrangement",
  home_type =  "Does the participant own or rent their home",
  bmi = "BMI",
  bmi_who = "Body mass index category. Reported for participants aged 2 years or older. One of 12.0_18.4, 18.5_24.9, 25.0_29.9, or 30.0_plus.",
  pulse = "60 second pulse rate",
  systolic = "Systolic blood pressure reading, following the procedure outlined for BPXSAR.",
  diastoic = "Diastolic blood pressure reading",
  testosterone = "Testerone total (ng/dL). Reported for participants aged 6 years or older. ",
  direct_chol = "Direct HDL cholesterol in mmol/L. Reported for participants aged 6 years or older",
  tot_chol = "Total HDL cholesterol in mmol/L. Reported for participants aged 6 years or older.",
  weight = "Weight in kg",
  height = "Height in cm",
  length = "Recumbent length in cm. Reported for participants aged 0 - 3 years.",
  urine_vol = "Urine volume in mL – second test. Reported for participants aged 6 years or older.",
  urine_flow = "Urine flow rate (urine volume/time since last urination) in mL/min – second test. Reported for participants aged 6 years or older.",
  diabetes = "Study participant told by a doctor or health professional that they have diabetes. Reported for participants aged 1 year or older as Yes or No.",
  diabetes_age = "Age of study participant when first told they had diabetes. Reported for participants aged 1 year or older.",
  health_general = "Self-reported rating of participant's health in general Reported for participants aged 12 years or older. One of Excellent, Vgood, Good, Fair, or Poor.",
  phys_health_bad_days = "Self-reported number of days participant's physical health was not good out of the past 30 days. Reported for participants aged 12 years or older.",
  mental_health_bad_days = "Self-reported number of days participant's mental health was not good out of the past 30 days. Reported for participants aged 12 years or older.",
  little_interest = "Self-reported number of days where participant had little interest in doing things. Reported for participants aged 18 years or older. One of None, Several, Majority (more than half the days), or AlmostAll.",
  days_with_no_interest = "Does the participant have more than the occasional day when they have little  interest in doing things.",
  depressed = "Self-reported number of days where participant felt down, depressed or hopeless. Reported for participants aged 18 years or older. One of None, Several, Majority (more than half the days), or AlmostAll.",
  n_pregnancies = "How many times participant has been pregnant. Reported for female participants aged 20 years or older.",
  n_babies = "How many of participants deliveries resulted in live births. ",
  age_first_baby = "Age of participant at time of first live birth. 14 years or under = 14, 45 years or older = 45. Reported for female participants aged 20 years or older.",
  pregant_currently = "Pregnancy status at the time of the health examination was ascertained for females 8-59 years of age. Due to disclosure risks pregnancy status was only released for women 20-44 years of age.",
  sleep_hours = "Self-reported number of hours study participant usually gets at night on weekdays or workdays. Reported for participants aged 16 years and older.",
  sleep_trouble = "Participant has told a doctor or other health professional that they had trouble sleeping. Reported for participants aged 16 years and older. Coded as Yes or No.",
  physically_active = "Participant does moderate or vigorous-intensity sports, fitness or recreational activities (Yes or No). Reported for participants 12 years or older.",
  active_days = "Number of days in a typical week that participant does moderate or vigorous-intensity activity. Reported for participants 12 years or older.",
  tv_hours = "Number of hours per day on average participant watched TV over the past 30 days. Reported for participants 2 years or older. One of 0_to_1hr, 1_hr, 2_hr, 3_hr, 4_hr, More_4_hr.",
  comp_hours = "Number of hours per day on average participant used a computer or gaming device over the past 30 days. Reported for participants 2 years or older. One of 0_hrs, 0_to_1hr, 1_hr, 2_hr, 3_hr, 4_hr, More_4_hr.",
  alcohol_drinks = "Average number of drinks consumed on days that participant drank alcoholic beverages. Reported for participants aged 18 years or older.",
  alcohol_days = "Estimated number of days over the past year that participant drank alcoholic beverages. Reported for participants aged 18 years or older.",
  smoke = "Study participant currently smokes cigarettes regularly. Reported for participants aged 20 years or older as Yes or No, provided they answered Yes to having somked 100 or more cigarettes in their life time. All subjects who have not smoked 100 or more cigarettes are listed as NA here.",
  smoke100 = "Study participant has smoked at least 100 cigarettes in their entire life. Reported for participants aged 20 years or older as Yes or No.",
  smoke_age = "Age study participant first started to smoke cigarettes fairly regularly. Reported for participants aged 20 years or older.",
  marijuana = "Participant has tried marijuana. Reported for participants aged 18 to 59 years as Yes or No.",
  marijuana_age = "Age participant first tried marijuana. Reported for participants aged 18 to 59 years.",
  marijuana_regular = "Participant has been/is a regular marijuana user (used at least once a month for a year). Reported for participants aged 18 to 59 years as Yes or No.",
  age_sex = "Age of participant when had sex for the first time. Reported for participants aged 18 to 69 years.",
  orientation_sex = "Participant's self-described sexual orientation. Reported for participants aged 18 to 59 years. One of Heterosexual, Homosexual, Bisexual."
  )
overall <- "This is survey data collected by the US National Center for Health Statistics (NCHS) which has conducted a series of health and nutrition surveys since the early 1960's. These data are for 2011 and 2012. (See NHANES package.)"
types <- LA_var_types(frame)
NHANES2 <- frame
#save(frame, types, codebook, overall, file = "data/Health.rda")
save(NHANES2, file = "data/NHANES2.rda")
