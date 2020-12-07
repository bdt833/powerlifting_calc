library(ggplot2)
library(ggpubr)
#find the success rate of each lift for each attempt
lifts_SF <- as_tibble(matrix(c(mean(pl_final$Squat1SF), mean(pl_final$Bench1SF), mean(pl_final$Deadlift1SF), 
                               mean(pl_final$Squat2SF), mean(pl_final$Bench2SF), mean(pl_final$Deadlift2SF),
                               mean(pl_final$Squat3SF), mean(pl_final$Bench3SF), mean(pl_final$Deadlift3SF)),
                             byrow = T, nrow = 3))
lifts_SF <- lifts_SF %>% gather("Lift", "Ratio")
lifts_SF$Lift <- c(rep("Squat", 3), rep("Bench", 3), rep("Deadlift", 3))
lifts_SF$Attempt <- rep(c(1,2,3), 3)

#plot
lifts_SF %>% ggplot(aes(Attempt, Ratio, col = Lift)) + 
  geom_point() + 
  geom_line()

pl_top001 <- pl_final %>% 
  group_by(Sex, Tested, Equipment) %>% 
  slice_max(order_by = CompDots, prop = 0.001) %>% 
  ungroup 

#list of top lifters in raw category
pl_top001 %>% select(Name, CompDots, Dots, BwRatio, Equipment, BodyweightKg) %>% 
  filter(Equipment == "Raw") %>% 
  select(-Equipment) %>% 
  arrange(desc(CompDots)) %>% 
  print(n=30)

pl_top001 %>% ggplot(aes(BwRRaw, DotsRaw, col = BodyweightKg)) + 
  geom_point() + 
  scale_color_gradient(low = "blue", high = "red") + 
  facet_wrap(~ Equipment)

pl_top001 %>% ggplot(aes(BwRRaw, DotsRaw, col = Equipment)) +
  geom_point() +
  scale_color_viridis_d(option = "plasma")

library(corrplot)
#select only independent variables of interest for correlation analysis
pl_corr <- pl_final %>% select(Age, BodyweightKg, Squat1Kg, Squat2Kg, 
                               Squat3Kg, Bench1Kg, Bench2Kg, Bench3Kg, 
                               Deadlift1Kg, Deadlift2Kg, Deadlift3Kg, TotalKg)
pl_cor_matrix <- cor(pl_corr)

corrplot(pl_cor_matrix, type = "lower", tl.col = "black", tl.srt = 45)