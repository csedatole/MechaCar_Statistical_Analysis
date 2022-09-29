library(tidyverse)
# Deliverable 1
library(dplyr)

MechaCar_mpg_df <- read.csv("MechaCar_mpg.csv")
head(MechaCar_mpg_df)

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + 
     AWD,data=MechaCar_mpg_df) 

summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + 
             AWD,data=MechaCar_mpg_df))

# Deliverable 2
Suspension_Coil_df <- read.csv("Suspension_Coil.csv")
head(Suspension_Coil_df)

total_summary <- Suspension_Coil_df %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI),SD_PSI=sd(PSI), .groups= "keep")
head(total_summary)

lot_summary <- Suspension_Coil_df %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI),SD_PSI=sd(PSI), .groups= "keep")
lot_summary

# Deliverable 3
t.test(Suspension_Coil_df$PSI, mu=1500)

lot_1 <- subset(Suspension_Coil_df, Manufacturing_Lot == "Lot1")
lot_2 <- subset(Suspension_Coil_df, Manufacturing_Lot == "Lot2")
lot_3 <- subset(Suspension_Coil_df, Manufacturing_Lot == "Lot3")

t.test(lot_1$PSI, mu=1500)
t.test(lot_2$PSI, mu=1500)
t.test(lot_3$PSI, mu=1500)
