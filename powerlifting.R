library(tidyverse)
library(naniar)

powerlifting <- read_csv("openpowerlifting-2020-10-16.csv", 
                         col_types = "cfffdfffdfddddddddddddddddfddddffffDffff")

miss_var_summary(powerlifting) %>% as.data.frame() #convert to data.frame to view non-truncated results

pl_filter <- powerlifting %>% select(-Country, -BirthYearClass, -Federation, 
                                     -MeetCountry, -MeetState, -MeetName, -Goodlift, 
                                     -Glossbrenner, -Division, -ParentFederation, -MeetTown)

#check missing data again, this time using both graphical and textual representations
miss_var_summary(pl_filter) %>% as.data.frame()
gg_miss_var(pl_filter)

pl_filter2 <- pl_filter %>% filter(Event == "SBD") %>% filter(is.na(Squat1Kg) == F, is.na(Bench1Kg) == F, is.na(Deadlift1Kg) == F) %>%
  filter(is.na(Squat2Kg) == F, is.na(Bench2Kg) == F, is.na(Deadlift2Kg) == F) %>% 
  filter(is.na(Squat3Kg) == F, is.na(Bench3Kg) == F, is.na(Deadlift3Kg) == F) %>%
  filter(is.na(Best3SquatKg) == F, is.na(Best3BenchKg) == F, is.na(Best3DeadliftKg) == F) %>% 
  filter(Place != "DQ", Place != "DD", Place != "NS") %>% #remove no-shows/disqualified lifters 
  select(-Event, -Place)

nrow(pl_filter2)/nrow(pl_filter) #only 20% of the data remains

summary(pl_filter2)

#remove data categories with too low of a sample size and do some feature engineering
pl_filter3 <- pl_filter2 %>% mutate(Tested = as.factor(ifelse(is.na(Tested) == F, "Yes", "No"))) %>% #NA values in tested means untested
  filter((Equipment != "Straps" & Equipment != "Unlimited"), Sex != "Mx") %>% 
  mutate(Squat2J = (abs(Squat2Kg) - abs(Squat1Kg))) %>% #creating variables to show the jumps from <lift>1 to <lift>2
  mutate(Bench2J = (abs(Bench2Kg) - abs(Bench1Kg))) %>% 
  mutate(Dead2J = (abs(Deadlift2Kg) - abs(Deadlift1Kg))) %>%
  mutate(Squat3J = (abs(Squat3Kg) - abs(Squat2Kg))) %>% #same but with <lift>2 to <lift>3
  mutate(Bench3J = (abs(Bench3Kg) - abs(Bench2Kg))) %>% 
  mutate(Dead3J = (abs(Deadlift3Kg) - abs(Deadlift2Kg))) %>%
  mutate(Squat1SF = ifelse(Squat1Kg > 0, 1, 0), Squat2SF = ifelse(Squat2Kg > 0, 1, 0), Squat3SF = ifelse(Squat3Kg > 0, 1, 0)) %>% #dummy variable for success of lift
  mutate(Bench1SF = ifelse(Bench1Kg > 0, 1, 0), Bench2SF = ifelse(Bench2Kg > 0, 1, 0), Bench3SF = ifelse(Bench3Kg > 0, 1, 0)) %>%
  mutate(Deadlift1SF = ifelse(Deadlift1Kg > 0, 1, 0), Deadlift2SF = ifelse(Deadlift2Kg > 0, 1, 0), Deadlift3SF = ifelse(Deadlift3Kg > 0, 1, 0)) %>%
  mutate_at(c("Squat1Kg", "Squat2Kg", "Squat3Kg", "Bench1Kg", "Bench2Kg", "Bench3Kg", "Deadlift1Kg", "Deadlift2Kg", "Deadlift3Kg"), ~abs(.)) #negative numbers were failed lifts

#refactor Sex, Equipment groups
pl_filter3$Sex <- factor(pl_filter3$Sex)
pl_filter3$Equipment <- factor(pl_filter3$Equipment)

pl_filter3 %>% filter(Age < 16) %>% ggplot(aes(TotalKg)) + geom_histogram(bins = 30) 

pl_filter3$Age[is.na(pl_filter3$Age) == T] <- 0
pl_filter3$Age[pl_filter3$Age < 16] <- 0

pl_filter4 <- pl_filter3 %>% select(-AgeClass, -WeightClassKg) %>%
  filter(!is.na(BodyweightKg) == T, !is.na(TotalKg) == T) %>%
  filter(Squat2J < 100, Bench2J < 100, Dead2J < 100) %>% #highly unlikely that anyone will jump 100kg+ between attempts
  filter(Squat3J < 100, Bench3J < 100, Dead3J < 100) %>%
  mutate(BwRatio = TotalKg / BodyweightKg) %>% #weight lifted:bodyweight ratio
  mutate(WilksRaw = Wilks, BwRRaw = BwRatio, DotsRaw = Dots) %>% mutate_at(c("Wilks", "BwRatio", "Dots"), 
                                                                           ~(scale(.) %>% as.vector)) %>% #scale Wilks/Dots/BwRatio and save the raw values
  mutate(CompWilks = (Wilks + BwRatio)/2, CompDots = (Dots + BwRatio)/2) #create a composite score of scaled values to create a strength index

miss_var_summary(pl_filter4)

#some minor feature engineering left
pl_noage <- pl_filter4 %>% filter(Age == 0)

pl_final <- pl_filter4 %>% filter(Age != 0)