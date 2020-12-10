library(strip) #removes extra baggage from LM models, keeps only predictive information
Squat1Kg_model <- lm(Squat1Kg ~ Squat3Kg + Age + Sex + BodyweightKg + Equipment, data = pl_model)
Squat1Kg_model <- strip(Squat1Kg_model, keep = "predict")

Squat2Kg_model <- lm(Squat2Kg ~ Squat3Kg + Age + Sex + BodyweightKg + Equipment, data = pl_model)
Squat2Kg_model <- strip(Squat2Kg_model, keep = "predict")

Bench1Kg_model <- lm(Bench1Kg ~ Bench3Kg + Age + Sex + BodyweightKg + Equipment, data = pl_model)
Bench1Kg_model <- strip(Bench1Kg_model, keep = "predict")

Bench2Kg_model <- lm(Bench2Kg ~ Bench3Kg + Age + Sex + BodyweightKg + Equipment, data = pl_model)
Bench2Kg_model <- strip(Bench2Kg_model, keep = "predict")

Deadlift1Kg_model <- lm(Deadlift1Kg ~ Deadlift3Kg + Age + Sex + BodyweightKg + Equipment, data = pl_model)
Deadlift1Kg_model <- strip(Deadlift1Kg_model, keep = "predict")

Deadlift2Kg_model <- lm(Deadlift2Kg ~ Deadlift3Kg + Age + Sex + BodyweightKg + Equipment, data = pl_model)
Deadlift2Kg_model <- strip(Deadlift2Kg_model, keep = "predict")

#create initial data.frame with random info
user_info <- setNames(data.frame(20, factor("M"), 70, factor("Single-ply"), 140, 100, 185), 
                      c("Age", "Sex", "BodyweightKg", "Equipment", "Squat3Kg", "Bench3Kg", "Deadlift3Kg"))

#create full data.frame (includes 0 age group) with only relevant lift information
pl_web_clean <- pl_filter4 %>% select(-Best3SquatKg, -Best3BenchKg, -Best3DeadliftKg, -Dots, -Wilks, -Tested, -BwRatio, -WilksRaw, -BwRRaw, -DotsRaw, -CompWilks, -CompDots, -Squat2J, -Bench2J, -Dead2J, -Squat3J, -Bench3J, -Dead3J, -Squat4Kg, -Bench4Kg, -Deadlift4Kg)

#find lifts that only have >100 lift attempts for that weight to find the common lifts; due to weight conversions, sample size may be < 20
squat_sampsize <- pl_web_clean %>% group_by(Squat3Kg) %>% summarize(Sample_Size = n()) %>% filter(Sample_Size > 100)
bench_sampsize <- pl_web_clean %>% group_by(Bench3Kg) %>% summarize(Sample_Size = n()) %>% filter(Sample_Size > 100)
deadlift_sampsize <- pl_web_clean %>% group_by(Deadlift3Kg) %>% summarize(Sample_Size = n()) %>% filter(Sample_Size > 100)

#create a top 1% lifter group
pl_top01 <- pl_final %>% 
  group_by(Sex, Tested, Equipment) %>% 
  slice_max(order_by = CompDots, prop = 0.01) %>% 
  ungroup %>% select(Name, Sex, Equipment, Age, Date, BodyweightKg, Squat4Kg, 
                     Best3SquatKg, Bench4Kg, Best3BenchKg, Deadlift4Kg, 
                     Best3DeadliftKg, TotalKg, DotsRaw, BwRRaw, CompDots, Tested) 

#save this as an .RData file to be loaded into the web app
save(Squat1Kg_model, Squat2Kg_model, Bench1Kg_model, Bench2Kg_model, Deadlift1Kg_model, Deadlift2Kg_model, pl_web_clean, squat_sampsize, bench_sampsize, deadlift_sampsize, user_info, pl_top01, file = "PL_Web.RData")
