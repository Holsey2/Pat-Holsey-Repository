#MSDS 456: Sports Performance Analytics
#Final Project
#Authors: Halperin, Greg and Holsey, Patrick

#Our final project is to analyze the value of switch hitting in the MLB and to build a decision tree
#whether players should continue to switch hit or consider sticking to one side of the plate

#The dataset acquired is from Fangraphs, filtering switch hitters with 600 plate appearances vs
#left-handed pitchers and right-handed pitchers over the years of 2018-2023

#Load packages
library(tidyverse)
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(randomForest)
library(caTools)
library(janitor)
library(Metrics)
library(knitr)
library(kableExtra)
library(htmltools)

#Load data sets 
LHH_vs_LHP <- read_csv("2018-2023 LHH vs LHP - min200.csv")
LHH_vs_RHP <- read_csv("2018-2023 LHH vs RHP - min500.csv")
RHH_vs_LHP <- read_csv("2018-2023 RHH vs LHP - min250.csv")
RHH_vs_RHP <- read_csv("2018-2023 RHH vs RHP - min500.csv")
SH_vs_LHP <- read_excel("2018-2023 Switch-Hitter vs LHP as RHH.xlsx")
SH_vs_RHP <- read_excel("2018-2023 Switch-Hitter vs RHP as LHH.xlsx")
Mullins_LHH_2020 <- read_csv("2018-2020 Cedric Mullins vs RHP as LHH.csv")
Mullins_RHH_2020 <- read_csv("2018-2020 Cedric Mullins vs LHP as RHH.csv")
Mullins_LHH_LHP_2023 <-read_csv("2021-2023 Cedric Mullins vs LHP as LHH.csv")
Mullins_LHH_RHP_2023 <- read_csv("2021-2023 Cedric Mullins vs RHP as LHH.csv")

#Inner joining to get split data for each hitter
LHH_splits <- inner_join(LHH_vs_LHP, LHH_vs_RHP, by = 'PlayerId')
RHH_splits <- inner_join(RHH_vs_LHP, RHH_vs_RHP, by = 'PlayerId')

#Filter each splits table
LHH_splits <- LHH_splits %>% select(Name.x, PlayerId, `Pitcher Handedness.x`,PA.x, wOBA.x, 
                                    OPS.x, `GB%.x`, `LD%.x`, `Hard%.x`, `BB%.x`, `K%.x`,
                                    `Pitcher Handedness.y`, PA.y, wOBA.y, OPS.y, `GB%.y`,
                                    `LD%.y`, `Hard%.y`, `BB%.y`, `K%.y`)
RHH_splits <- RHH_splits %>% select(Name.x, PlayerId, `Pitcher Handedness.x`,PA.x, wOBA.x, 
                                    OPS.x, `GB%.x`, `LD%.x`, `Hard%.x`, `BB%.x`, `K%.x`,
                                    `Pitcher Handedness.y`, PA.y, wOBA.y, OPS.y, `GB%.y`,
                                    `LD%.y`, `Hard%.y`, `BB%.y`, `K%.y`)

#Rounding all numeric values to 3 decimal places
LHH_splits <- LHH_splits %>% 
  mutate_if(is.numeric, round,3)
RHH_splits <- RHH_splits %>% 
  mutate_if(is.numeric, round,3)

#Adjusting variable names to .L / .R to identify pitcher handedness
LHH_splits <- LHH_splits %>%
  rename(Name = Name.x, `Pitcher_Handedness.L` = `Pitcher Handedness.x`,
         PA.L = PA.x, wOBA.L = wOBA.x, OPS.L = OPS.x, `GB%.L` = `GB%.x`, 
         `LD%.L` = `LD%.x`, `Hard%.L` = `Hard%.x`, `BB%.L` = `BB%.x`, 
         `K%.L` = `K%.x`, `Pitcher_Handedness.R` = `Pitcher Handedness.y`, 
         PA.R = PA.y, wOBA.R = wOBA.y, OPS.R = OPS.y, `GB%.R` = `GB%.y`,
         `LD%.R` = `LD%.y`, `Hard%.R` = `Hard%.y`, `BB%.R` = `BB%.y`, 
         `K%.R` = `K%.y`)
RHH_splits <- RHH_splits %>%
  rename(Name = Name.x, `Pitcher_Handedness.L` = `Pitcher Handedness.x`,
         PA.L = PA.x, wOBA.L = wOBA.x, OPS.L = OPS.x, `GB%.L` = `GB%.x`, 
         `LD%.L` = `LD%.x`, `Hard%.L` = `Hard%.x`, `BB%.L` = `BB%.x`, 
         `K%.L` = `K%.x`, `Pitcher_Handedness.R` = `Pitcher Handedness.y`, 
         PA.R = PA.y, wOBA.R = wOBA.y, OPS.R = OPS.y, `GB%.R` = `GB%.y`,
         `LD%.R` = `LD%.y`, `Hard%.R` = `Hard%.y`, `BB%.R` = `BB%.y`, 
         `K%.R` = `K%.y`)

#cleaning LHH and RHH splits variable names
LHH_splits <- clean_names(LHH_splits)
RHH_splits <-clean_names(RHH_splits)

#renaming LHH and RHH splits percent columns
LHH_splits <- LHH_splits %>% rename(gb_r = gb_percent_r, gb_l = gb_percent_l, 
                                    ld_r = ld_percent_r, ld_l = ld_percent_l,
                                    hard_r = hard_percent_r, hard_l = hard_percent_l,
                                    bb_r = bb_percent_r, bb_l = bb_percent_l,
                                    k_r = k_percent_r, k_l = k_percent_l)
RHH_splits <- RHH_splits %>% rename(gb_r = gb_percent_r, gb_l = gb_percent_l, 
                                    ld_r = ld_percent_r, ld_l = ld_percent_l,
                                    hard_r = hard_percent_r, hard_l = hard_percent_l,
                                    bb_r = bb_percent_r, bb_l = bb_percent_l,
                                    k_r = k_percent_r, k_l = k_percent_l)

#Calculating rounded means of LHH and RHH splits variables vs same handed pitcher
# Used in EDA Plots
LHH_w_oba_mean <- round(mean(LHH_splits$w_oba_l), 3)
LHH_ops_mean <- round(mean(LHH_splits$ops_l), 3)
LHH_gb_mean <- round(mean(LHH_splits$gb_l), 3)
LHH_ld_mean <- round(mean(LHH_splits$ld_l), 3)
LHH_hard_mean <- round(mean(LHH_splits$hard_l), 3)
LHH_bb_mean <- round(mean(LHH_splits$bb_l), 3)
LHH_k_mean <- round(mean(LHH_splits$k_l), 3)
RHH_w_oba_mean <- round(mean(RHH_splits$w_oba_r), 3)
RHH_ops_mean <- round(mean(RHH_splits$ops_r), 3)
RHH_gb_mean <- round(mean(RHH_splits$gb_r), 3)
RHH_ld_mean <- round(mean(RHH_splits$ld_r), 3)
RHH_hard_mean <- round(mean(RHH_splits$hard_r), 3)
RHH_bb_mean <- round(mean(RHH_splits$bb_r), 3)
RHH_k_mean <- round(mean(RHH_splits$k_r), 3)

#combining switch hitter data sets
SH_full <- left_join(SH_vs_LHP,SH_vs_RHP, by = 'PlayerId')

#Filtering Switch hitting variables
SH_filt <- SH_full %>% select(Name.x, PlayerId, `Pitcher Handedness.x`,PA.x, wOBA.x, 
                              OPS.x, `GB%.x`, `LD%.x`, `Hard%.x`, `BB%.x`, `K%.x`,
                              `Pitcher Handedness.y`, PA.y, wOBA.y, OPS.y, `GB%.y`,
                              `LD%.y`, `Hard%.y`, `BB%.y`, `K%.y`)

#Rounding all numeric values to 3 decimal places
SH_filt <- SH_filt %>% 
  mutate_if(is.numeric, round,3)

#Adjusting variable names to .L / .R to identify opposing pitcher handedness
SH_filt <- SH_filt %>%
  rename(Name = Name.x, `Pitcher_Handedness.L` = `Pitcher Handedness.x`,
         PA.L = PA.x, wOBA.L = wOBA.x, OPS.L = OPS.x, `GB%.L` = `GB%.x`, 
         `LD%.L` = `LD%.x`, `Hard%.L` = `Hard%.x`, `BB%.L` = `BB%.x`, 
         `K%.L` = `K%.x`, `Pitcher_Handedness.R` = `Pitcher Handedness.y`, 
         PA.R = PA.y, wOBA.R = wOBA.y, OPS.R = OPS.y, `GB%.R` = `GB%.y`,
         `LD%.R` = `LD%.y`, `Hard%.R` = `Hard%.y`, `BB%.R` = `BB%.y`, 
         `K%.R` = `K%.y`)

#Cleaning the switch-hitter data frame variables so it can be read by randomForest
SH_filt <- clean_names(SH_filt)

#renaming switch-hitter percent columns
SH_filt <- SH_filt %>% rename(gb_r = gb_percent_r, gb_l = gb_percent_l, 
                              ld_r = ld_percent_r, ld_l = ld_percent_l,
                              hard_r = hard_percent_r, hard_l = hard_percent_l,
                              bb_r = bb_percent_r, bb_l = bb_percent_l,
                              k_r = k_percent_r, k_l = k_percent_l)

#Add new columns with default value 'Same as Average'
SH_filt$Perf_w_oba <- "Same as Average"
SH_filt$Perf_ops <- "Same as Average"
SH_filt$Perf_gb <- "Same as Average"
SH_filt$Perf_ld <- "Same as Average"
SH_filt$Perf_hard <- "Same as Average"
SH_filt$Perf_bb <- "Same as Average"
SH_filt$Perf_k <- "Same as Average"

#Set new column values to appropriate Perfs for LHH vs RHH splits. Reversing comparison for GB and K
SH_filt$Perf_w_oba[SH_filt$w_oba_l > LHH_w_oba_mean & SH_filt$w_oba_r > RHH_w_oba_mean] = "Above vs Both"
SH_filt$Perf_w_oba[SH_filt$w_oba_l < LHH_w_oba_mean & SH_filt$w_oba_r < RHH_w_oba_mean] = "Below vs Both"
SH_filt$Perf_w_oba[SH_filt$w_oba_l > LHH_w_oba_mean & SH_filt$w_oba_r < RHH_w_oba_mean] = "Above vs LHP, Below vs RHP"
SH_filt$Perf_w_oba[SH_filt$w_oba_l < LHH_w_oba_mean & SH_filt$w_oba_r > RHH_w_oba_mean] = "Below vs LHP, Above vs RHP"
SH_filt$Perf_ops[SH_filt$ops_l > LHH_ops_mean & SH_filt$ops_r > RHH_ops_mean] = "Above vs Both"
SH_filt$Perf_ops[SH_filt$ops_l < LHH_ops_mean & SH_filt$ops_r < RHH_ops_mean] = "Below vs Both"
SH_filt$Perf_ops[SH_filt$ops_l > LHH_ops_mean & SH_filt$ops_r < RHH_ops_mean] = "Above vs LHP, Below vs RHP"
SH_filt$Perf_ops[SH_filt$ops_l < LHH_ops_mean & SH_filt$ops_r > RHH_ops_mean] = "Below vs LHP, Above vs RHP"
SH_filt$Perf_ops[SH_filt$ops_l > LHH_ops_mean & SH_filt$ops_r == RHH_ops_mean] = "Above vs LHP, Avg vs RHP"
SH_filt$Perf_ops[SH_filt$ops_l == LHH_ops_mean & SH_filt$ops_r > RHH_ops_mean] = "Avg vs LHP, Above vs RHP"
SH_filt$Perf_gb[SH_filt$gb_l < LHH_gb_mean & SH_filt$gb_r < RHH_gb_mean] = "Above vs Both"
SH_filt$Perf_gb[SH_filt$gb_l > LHH_gb_mean & SH_filt$gb_r > RHH_gb_mean] = "Below vs Both"
SH_filt$Perf_gb[SH_filt$gb_l > LHH_gb_mean & SH_filt$gb_r < RHH_gb_mean] = "Below vs LHP, Above vs RHP"
SH_filt$Perf_gb[SH_filt$gb_l < LHH_gb_mean & SH_filt$gb_r > RHH_gb_mean] = "Above vs LHP, Below vs RHP"
SH_filt$Perf_ld[SH_filt$ld_l > LHH_ld_mean & SH_filt$ld_r > RHH_ld_mean] = "Above vs Both"
SH_filt$Perf_ld[SH_filt$ld_l < LHH_ld_mean & SH_filt$ld_r < RHH_ld_mean] = "Below vs Both"
SH_filt$Perf_ld[SH_filt$ld_l > LHH_ld_mean & SH_filt$ld_r < RHH_ld_mean] = "Above vs LHP, Below vs RHP"
SH_filt$Perf_ld[SH_filt$ld_l < LHH_ld_mean & SH_filt$ld_r > RHH_ld_mean] = "Below vs LHP, Above vs RHP"
SH_filt$Perf_hard[SH_filt$hard_l > LHH_hard_mean & SH_filt$hard_r > RHH_hard_mean] = "Above vs Both"
SH_filt$Perf_hard[SH_filt$hard_l < LHH_hard_mean & SH_filt$hard_r < RHH_hard_mean] = "Below vs Both"
SH_filt$Perf_hard[SH_filt$hard_l > LHH_hard_mean & SH_filt$hard_r < RHH_hard_mean] = "Above vs LHP, Below vs RHP"
SH_filt$Perf_hard[SH_filt$hard_l < LHH_hard_mean & SH_filt$hard_r > RHH_hard_mean] = "Below vs LHP, Above vs RHP"
SH_filt$Perf_hard[SH_filt$hard_l > LHH_hard_mean & SH_filt$hard_r == RHH_hard_mean] = "Above vs LHP, Avg vs RHP"
SH_filt$Perf_hard[SH_filt$hard_l == LHH_hard_mean & SH_filt$hard_r > RHH_hard_mean] = "Avg vs LHP, Above vs RHP"
SH_filt$Perf_bb[SH_filt$bb_l > LHH_bb_mean & SH_filt$bb_r > RHH_bb_mean] = "Above vs Both"
SH_filt$Perf_bb[SH_filt$bb_l < LHH_bb_mean & SH_filt$bb_r < RHH_bb_mean] = "Below vs Both"
SH_filt$Perf_bb[SH_filt$bb_l > LHH_bb_mean & SH_filt$bb_r < RHH_bb_mean] = "Above vs LHP, Below vs RHP"
SH_filt$Perf_bb[SH_filt$bb_l < LHH_bb_mean & SH_filt$bb_r > RHH_bb_mean] = "Below vs LHP, Above vs RHP"
SH_filt$Perf_k[SH_filt$k_l < LHH_k_mean & SH_filt$k_r < RHH_k_mean] = "Above vs Both"
SH_filt$Perf_k[SH_filt$k_l > LHH_k_mean & SH_filt$k_r > RHH_k_mean] = "Below vs Both"
SH_filt$Perf_k[SH_filt$k_l > LHH_k_mean & SH_filt$k_r < RHH_k_mean] = "Below vs LHP, Above vs RHP"
SH_filt$Perf_k[SH_filt$k_l < LHH_k_mean & SH_filt$k_r > RHH_k_mean] = "Above vs LHP, Below vs RHP"

#EDA scatterplot for wOBA
EDA_wOBA <- ggplot(data = SH_filt, aes(x = w_oba_r, y = w_oba_l, color = Perf_w_oba)) +
  geom_vline(xintercept = RHH_w_oba_mean) +
  geom_hline(yintercept = LHH_w_oba_mean) +
  geom_point() +
  geom_text_repel(aes(label = name), size = 3, color = "black") +
  labs(title = "wOBA Splits Compared to Same-Sided League Averages", x = "wOBA vs R", y = "wOBA vs L",
       color = "Performance vs League Average") +
  theme(plot.title=element_text(hjust=0.4, size = 15))

#EDA scatterplot for OPS
EDA_OPS <- ggplot(data = SH_filt, aes(x = ops_r, y = ops_l, color = Perf_ops)) +
  geom_vline(xintercept = RHH_ops_mean) +
  geom_hline(yintercept = LHH_ops_mean) +
  geom_point() +
  geom_text_repel(aes(label = name), size = 3, color = "black") +
  labs(title = "OPS Splits Compared to Same-Sided League Averages", x = "OPS vs R", y = "OPS vs L",
       color = "Performance vs League Average") +
  theme(plot.title=element_text(hjust=0.4, size = 15))

#EDA scatterplot for GB%
EDA_GB <- ggplot(data = SH_filt, aes(x = gb_r, y = gb_l, color = Perf_gb)) +
  geom_vline(xintercept = RHH_gb_mean) +
  geom_hline(yintercept = LHH_gb_mean) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_point() +
  geom_text_repel(aes(label = name), size = 3, color = "black") +
  labs(title = "Ground Ball% Splits Compared to Same-Sided League Averages", x = "GB% vs R", y = "GB% vs L",
       color = "Performance vs League Average") +
  theme(plot.title=element_text(hjust=0.4, size = 15))

#EDA scatterplot for LD%
EDA_LD <- ggplot(data = SH_filt, aes(x = ld_r, y = ld_l, color = Perf_ld)) +
  geom_vline(xintercept = RHH_ld_mean) +
  geom_hline(yintercept = LHH_ld_mean) +
  geom_point() +
  geom_text_repel(aes(label = name), size = 3, color = "black") +
  labs(title = "Line Drive% Splits Compared to Same-Sided League Averages", x = "LD vs R", y = "LD vs L",
       color = "Performance vs League Average") +
  theme(plot.title=element_text(hjust=0.4, size = 15))

#EDA scatterplot for Hard%
EDA_HARD <- ggplot(data = SH_filt, aes(x = hard_r, y = hard_l, color = Perf_hard)) +
  geom_vline(xintercept = RHH_hard_mean) +
  geom_hline(yintercept = LHH_hard_mean) +
  geom_point() +
  geom_text_repel(aes(label = name), size = 3, color = "black") +
  labs(title = "Hard Hit% Splits Compared to Same-Sided League Averages", x = "Hard vs R", y = "Hard vs L",
       color = "Performance vs League Average") +
  theme(plot.title=element_text(hjust=0.4, size = 15))

#EDA scatterplot for BB%
EDA_BB <- ggplot(data = SH_filt, aes(x = bb_r, y = bb_l, color = Perf_bb)) +
  geom_vline(xintercept = RHH_bb_mean) +
  geom_hline(yintercept = LHH_bb_mean) +
  geom_point() +
  geom_text_repel(aes(label = name), size = 3, color = "black") +
  labs(title = "Walk% Splits Compared to Same-Sided League Averages", x = "BB vs R", y = "BB vs L",
       color = "Performance vs League Average") +
  theme(plot.title=element_text(hjust=0.4, size = 15))

#EDA scatterplot for K%
EDA_K <- ggplot(data = SH_filt, aes(x = k_r, y = k_l, color = Perf_k)) +
  geom_vline(xintercept = RHH_k_mean) +
  geom_hline(yintercept = LHH_k_mean) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_point() +
  geom_text_repel(aes(label = name), size = 3, color = "black") +
  labs(title = "Strikeout% Splits Compared to Same-Sided League Averages", x = "K vs R", y = "K vs L",
       color = "Performance vs League Average") + 
  theme(plot.title=element_text(hjust=0.4, size = 15))

#Creating the model
set.seed(111)

#Splitting the LHH_splits data set into training and testing data
LHH.index <- sample(2, nrow(LHH_splits), replace = TRUE, prob = c(0.8, 0.2))
LHH.train <- LHH_splits[LHH.index==1,]
LHH.test <- LHH_splits[LHH.index==2, ]

#Creating random forest for predicting woba against LHP using information against RHP for LHH
LHH.rf <- randomForest(ops_l ~ w_oba_r + ops_r + gb_r + ld_r + hard_r + bb_r + k_r, 
                       data = LHH.train, importance = TRUE)
print(LHH.rf)

#Assigning LHH root mean square error to object
lhh_rmse <- round(0.005468201, 3)

#Splitting the RHH_splits data set into training and testing data
RHH.index <- sample(2, nrow(RHH_splits), replace = TRUE, prob = c(0.8, 0.2))
RHH.train <- RHH_splits[RHH.index==1,]
RHH.test <- RHH_splits[RHH.index==2, ]

#Creating ranomd forest for RHH against RHP using information against LHP
RHH.rf <- randomForest(ops_r ~ w_oba_l + ops_l + gb_l + ld_l + hard_l + bb_l + k_l, 
                       data = RHH.train, importance = TRUE)
print(RHH.rf)

#Assigning RHH root mean square error to object
rhh_rmse <- round(0.003724938, 3)

#Making predictions for Switch hitters only hitting lefty against LHP
pred_l <- predict(LHH.rf, newdata = SH_filt)
print(pred_l)

#Making predictions for Switch hitters only hitting righty against RHP
pred_r <- predict(RHH.rf, newdata = SH_filt)
print(pred_r)

#Add the predicted OPS' to SH_filt dataframe
SH_filt$pred_ops_l <- round(pred_l,3)
SH_filt$pred_ops_r <- round(pred_r,3)

#Filtering Switch-Hitter data frame to display actual vs predicted OPS
SH_filt2 <- SH_filt %>% select(name, ops_l, pred_ops_l, ops_r, pred_ops_r)

# Creating a variable that will decide whether a hitter should look to stop switch hitting
SH_filt2$`Change?` <- case_when(SH_filt2$pred_ops_l > SH_filt2$ops_l + lhh_rmse & 
                                  SH_filt2$pred_ops_r < SH_filt2$ops_r - rhh_rmse ~ "Yes",
                                SH_filt2$pred_ops_l < SH_filt2$ops_l - lhh_rmse & 
                                  SH_filt2$pred_ops_r > SH_filt2$ops_r + rhh_rmse ~ "Yes",
                                SH_filt2$pred_ops_l < SH_filt2$ops_l - lhh_rmse &
                                  SH_filt2$pred_ops_r < SH_filt2$ops_r - rhh_rmse ~ "No",
                                SH_filt2$pred_ops_l < SH_filt2$ops_l - lhh_rmse &
                                  SH_filt2$pred_ops_r > SH_filt2$ops_r - rhh_rmse & 
                                  SH_filt2$pred_ops_r < SH_filt2$ops_r + rhh_rmse ~ "Maybe",
                                SH_filt2$pred_ops_r < SH_filt2$ops_r - rhh_rmse & 
                                  SH_filt2$pred_ops_l > SH_filt2$ops_l - lhh_rmse & 
                                  SH_filt2$pred_ops_l < SH_filt2$ops_l + lhh_rmse ~ "Maybe")

#Creating conditional table to display results
SH_results <- kable(SH_filt2, caption = "Switch Hitter Predicted OPS Chart", 
                    col.names = c("Name", "Actual OPS v L",
                                  "Predicted OPS v L", "Actual OPS v R",
                                  "Predicted OPS v R", "Change?")) %>%
  kable_styling() %>%
  #Applying conditions to the Predicted OPS v L column
  column_spec(3, color = "white", bold = TRUE,
              background = case_when(
                SH_filt2$pred_ops_l > SH_filt2$ops_l + lhh_rmse ~ "red",
                SH_filt2$pred_ops_l < SH_filt2$ops_l - lhh_rmse ~ "green",
                SH_filt2$pred_ops_l > SH_filt2$ops_l - lhh_rmse & 
                  SH_filt2$pred_ops_l < SH_filt2$ops_l + lhh_rmse ~ "goldenrod3"
              )) %>%
  #Applying conditions to the Predicted OPS v R column
  column_spec(5, color = "white", bold = TRUE,
              background = case_when(
                SH_filt2$pred_ops_r > SH_filt2$ops_r + rhh_rmse ~ "red",
                SH_filt2$pred_ops_r < SH_filt2$ops_r - rhh_rmse ~ "green",
                SH_filt2$pred_ops_r > SH_filt2$ops_r - rhh_rmse & 
                  SH_filt2$pred_ops_r < SH_filt2$ops_r + rhh_rmse~ "goldenrod3"
              ))  %>%
  #Applying conditions to the Change column
  column_spec(6, bold = TRUE,
              color = case_when(SH_filt2$`Change?` == "Yes" ~ "red",
                                SH_filt2$`Change?` == "No" ~ "black",
                                SH_filt2$`Change?` == "Maybe" ~ "goldenrod3"))

SH_results

#Filter each Mullins table
Mullins_LHH_2020 <- Mullins_LHH_2020 %>% select(Name, PlayerId, `Pitcher Handedness`,PA, wOBA, 
                                                OPS, `GB%`, `LD%`, `Hard%`, `BB%`, `K%`)
Mullins_RHH_2020 <- Mullins_RHH_2020 %>% select(Name, PlayerId, `Pitcher Handedness`,PA, wOBA, 
                                                OPS, `GB%`, `LD%`, `Hard%`, `BB%`, `K%`)
Mullins_LHH_RHP_2023 <- Mullins_LHH_RHP_2023 %>% select(Name, PlayerId, `Pitcher Handedness`,PA, wOBA, 
                                                        OPS, `GB%`, `LD%`, `Hard%`, `BB%`, `K%`)
Mullins_LHH_LHP_2023 <- Mullins_LHH_LHP_2023 %>% select(Name, PlayerId, `Pitcher Handedness`,PA, wOBA, 
                                                        OPS, `GB%`, `LD%`, `Hard%`, `BB%`, `K%`)

#combining Mullins datasets pre-2020 and post-2020
Mullins_2020 <- left_join(Mullins_LHH_2020, Mullins_RHH_2020, by = 'PlayerId')
Mullins_2023 <- left_join(Mullins_LHH_LHP_2023, Mullins_LHH_RHP_2023, by = 'PlayerId')

#Rounding all Mullins numeric values to 3 decimal places
Mullins_2020 <- Mullins_2020 %>% 
  mutate_if(is.numeric, round,3)
Mullins_2023<- Mullins_2023 %>%
  mutate_if(is.numeric, round,3)

#Filtering Mullins data frame variables
Mullins_2020<- Mullins_2020 %>% select(Name.x, PlayerId, `Pitcher Handedness.x`,PA.x, wOBA.x, 
                                       OPS.x, `GB%.x`, `LD%.x`, `Hard%.x`, `BB%.x`, `K%.x`,
                                       `Pitcher Handedness.y`, PA.y, wOBA.y, OPS.y, `GB%.y`,
                                       `LD%.y`, `Hard%.y`, `BB%.y`, `K%.y`)

Mullins_2023<- Mullins_2023 %>% select(Name.x, PlayerId, `Pitcher Handedness.x`,PA.x, wOBA.x, 
                                       OPS.x, `GB%.x`, `LD%.x`, `Hard%.x`, `BB%.x`, `K%.x`,
                                       `Pitcher Handedness.y`, PA.y, wOBA.y, OPS.y, `GB%.y`,
                                       `LD%.y`, `Hard%.y`, `BB%.y`, `K%.y`)

#Adjusting Mullins filtered variable names to .L / .R
Mullins_2020 <- Mullins_2020 %>%
  rename(Name = Name.x, `Pitcher_Handedness.R` = `Pitcher Handedness.x`,
         PA.R = PA.x, wOBA.R = wOBA.x, OPS.R = OPS.x, `GB%.R` = `GB%.x`, 
         `LD%.R` = `LD%.x`, `Hard%.R` = `Hard%.x`, `BB%.R` = `BB%.x`, 
         `K%.R` = `K%.x`, `Pitcher_Handedness.L` = `Pitcher Handedness.y`, 
         PA.L = PA.y, wOBA.L = wOBA.y, OPS.L = OPS.y, `GB%.L` = `GB%.y`,
         `LD%.L` = `LD%.y`, `Hard%.L` = `Hard%.y`, `BB%.L` = `BB%.y`, 
         `K%.L` = `K%.y`)

Mullins_2023 <- Mullins_2023 %>%
  rename(Name = Name.x, `Pitcher_Handedness.L` = `Pitcher Handedness.x`,
         PA.L = PA.x, wOBA.L = wOBA.x, OPS.L = OPS.x, `GB%.L` = `GB%.x`, 
         `LD%.L` = `LD%.x`, `Hard%.L` = `Hard%.x`, `BB%.L` = `BB%.x`, 
         `K%.L` = `K%.x`, `Pitcher_Handedness.R` = `Pitcher Handedness.y`, 
         PA.R = PA.y, wOBA.R = wOBA.y, OPS.R = OPS.y, `GB%.R` = `GB%.y`,
         `LD%.R` = `LD%.y`, `Hard%.R` = `Hard%.y`, `BB%.R` = `BB%.y`, 
         `K%.R` = `K%.y`)

#Cleaning the Mullins data frame variables
Mullins_2020 <- clean_names(Mullins_2020)
Mullins_2023 <- clean_names(Mullins_2023)

#renaming switch-hitter percent columns
Mullins_2020 <- Mullins_2020 %>% rename(gb_r = gb_percent_r, gb_l = gb_percent_l, 
                                        ld_r = ld_percent_r, ld_l = ld_percent_l,
                                        hard_r = hard_percent_r, hard_l = hard_percent_l,
                                        bb_r = bb_percent_r, bb_l = bb_percent_l,
                                        k_r = k_percent_r, k_l = k_percent_l)

Mullins_2023 <- Mullins_2023 %>% rename(gb_r = gb_percent_r, gb_l = gb_percent_l, 
                                        ld_r = ld_percent_r, ld_l = ld_percent_l,
                                        hard_r = hard_percent_r, hard_l = hard_percent_l,
                                        bb_r = bb_percent_r, bb_l = bb_percent_l,
                                        k_r = k_percent_r, k_l = k_percent_l)

#Making OPS predictions for Mullins prior to abandoning switch-hitting
pred_Mullins_l <- predict(LHH.rf, newdata = Mullins_2020)
print(pred_Mullins_l)

pred_Mullins_r <- predict(RHH.rf, newdata = Mullins_2020)
print(pred_Mullins_r)

#Create Mullins data frame with predicted, pre-, and post-switch hitting
Mullins_full <- rbind(Mullins_2020, Mullins_2023)

#Add the predicted OPS' to Mullins_full dataframe
Mullins_2020$pred_ops_l <- pred_Mullins_l
Mullins_2020$pred_ops_r <- pred_Mullins_r
Mullins_full$pred_ops_l <- pred_Mullins_l
Mullins_full$pred_ops_r <- pred_Mullins_r

#Filtering Mullins Actual OPS and Predicted OPS'
Mullins_ops_pred <- Mullins_2020 %>%
  select(name, ops_l, pred_ops_l, ops_r, pred_ops_r)

#Creating the variable to decide whether a change should occur
Mullins_ops_pred$`Change?` <- case_when(Mullins_ops_pred$pred_ops_l > Mullins_ops_pred$ops_l + lhh_rmse & 
                                          Mullins_ops_pred$pred_ops_r < Mullins_ops_pred$ops_r - rhh_rmse ~ "Yes",
                                        Mullins_ops_pred$pred_ops_l < Mullins_ops_pred$ops_l - lhh_rmse & 
                                          Mullins_ops_pred$pred_ops_r > Mullins_ops_pred$ops_r + rhh_rmse ~ "Yes",
                                        Mullins_ops_pred$pred_ops_l < Mullins_ops_pred$ops_l - lhh_rmse &
                                          Mullins_ops_pred$pred_ops_r < Mullins_ops_pred$ops_r - rhh_rmse ~ "No",
                                        Mullins_ops_pred$pred_ops_l < Mullins_ops_pred$ops_l - lhh_rmse &
                                          Mullins_ops_pred$pred_ops_r > Mullins_ops_pred$ops_r - rhh_rmse & 
                                          Mullins_ops_pred$pred_ops_r < Mullins_ops_pred$ops_r + rhh_rmse ~ "Maybe",
                                        Mullins_ops_pred$pred_ops_r < Mullins_ops_pred$ops_r - rhh_rmse & 
                                          Mullins_ops_pred$pred_ops_l > Mullins_ops_pred$ops_l - lhh_rmse & 
                                          Mullins_ops_pred$pred_ops_l < Mullins_ops_pred$ops_l + lhh_rmse ~ "Maybe")

Mullins_results <- kable(Mullins_ops_pred, caption = "Cedric Mullins Predicted OPS Chart",
                         col.names = c("Name", "Actual OPS v L",
                                       "Predicted OPS v L", "Actual OPS v R",
                                       "Predicted OPS v R", "Change?")) %>%
  kable_styling() %>%
  # Apply conditional formatting to the Predicted OPS v L column
  column_spec(3, color = "white", bold = TRUE,
              background = case_when(
                Mullins_ops_pred$pred_ops_l > Mullins_ops_pred$ops_l + lhh_rmse ~ "red",
                Mullins_ops_pred$pred_ops_l < Mullins_ops_pred$ops_l - rhh_rmse ~ "green",
                Mullins_ops_pred$pred_ops_l > Mullins_ops_pred$ops_l - lhh_rmse & 
                  Mullins_ops_pred$pred_ops_l < Mullins_ops_pred$ops_l + lhh_rmse~ "goldenrod3"
              )) %>%
  # Apply conditional formatting to the Predicted OPS v R column
  column_spec(5, color = "white", bold = TRUE,
              background = case_when(
                Mullins_ops_pred$pred_ops_r > Mullins_ops_pred$ops_r + rhh_rmse ~ "red",
                Mullins_ops_pred$pred_ops_r < Mullins_ops_pred$ops_r - rhh_rmse ~ "green",
                Mullins_ops_pred$pred_ops_r > Mullins_ops_pred$ops_r - rhh_rmse & 
                  Mullins_ops_pred$pred_ops_r < Mullins_ops_pred$ops_r + rhh_rmse~ "goldenrod3"
              )) %>%
# Apply conditional formatting to the Change column
  column_spec(6, bold = TRUE,
              color = case_when(Mullins_ops_pred$`Change?` == "Yes" ~ "red",
                                Mullins_ops_pred$`Change?` == "No" ~ "black",
                                Mullins_ops_pred$`Change?` == "Maybe" ~ "goldenrod3"))
Mullins_results