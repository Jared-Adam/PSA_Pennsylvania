# sent prey Pennsylvania 
# goals: 
  # 1: run models individually for each year on growth stage and time of predation/ total predation
  # 2: run models of total predation by growth stage and year 
  # create plots for each year and then facet wrap them all together with growth stage being wrapped


# packages ####
library(tidyverse)
library(lme4)

# data ####
sent <- PSA_PA_Sent_prey
as_tibble(sent)

# extract year from the date
colnames(sent)
sent_years <- sent %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  dplyr::select(-location, -date) %>% 
  relocate(year, growth_stage, plot_id, block, treatment, row, sample, n.absent, n.partial, n.predated, 
          d.absent, d.partial, d.predated, to.predated, n.weather, d.weather)%>% 
  mutate(n.predated = as.double(n.predated),
         d.predated = as.double(d.predated),
         to.predated = as.double(to.predated))

# subset by year and then growth stage 
sent_21 <- subset(sent_years, year == '2021')
sent_22 <- subset(sent_years, year == '2022')
sent_23 <- subset(sent_years, year == '2023')

# 2021 ####
sent_21

#subset by growth stage
sent_21_v3_loop <- subset(sent_21, growth_stage == 'V3')
sent_21_v5_loop <- subset(sent_21, growth_stage == 'V5')
sent_21_r3_loop <- subset(sent_21, growth_stage == 'R3')


# V3
# predation timing is the thing I want to loop over
# so, step 1, create a list of those timings
# step 2, created an empty list for the outputs
v3_time_list <- c("to.predated", "n.predated", "d.predated")
v3_21_summary_list <- list()
for(i in 1:length(v3_time_list)){ # for each iteration across the length of the list I made
  print(i) # print each iteration
  col <- v3_time_list[i] #place the iterations into an object named col
  print(col) # print them to make sure it works
  sent_21_v3 <- subset(sent_21_v3_loop, select = c("plot_id", "row", "treatment", col)) # subset what I want to use in the model plus the new col I made
  colnames(sent_21_v3) <- c("plot_id", "row", "treatment", "col") # add this as a column name for the model
  #print sent_21_V3 to make sure it works
  sent_21_v3_model <- glmer(col ~ as.factor(treatment) +
                             (1|plot_id/row), data = sent_21_v3,
                           family = binomial)
  summary_v3_sent <- summary(sent_21_v3_model)
  v3_21_summary_list[[i]] <- summary_v3_sent
}
v3_21_summary_list
hist(residuals(v3_21_summary_list[[1]]))
hist(residuals(v3_21_summary_list[[2]]))
hist(residuals(v3_21_summary_list[[3]]))


# V5
v5_time_list <- c('to.predated', 'n.predated', 'd.predated')
v5_21_summary_list <- list()
for(i in 1:length(v5_time_list)){
  print(i)
  col <- v5_time_list[i]
  print(col)
  sent_21_v5 <- subset(sent_21_v5_loop, select = c("plot_id", "row", "treatment", col))
  colnames(sent_21_v5) <- c('plot_id', 'row', 'treatment', 'col')
  sent_21_v5_model <- glmer(col ~ as.factor(treatment)+
                              (1|plot_id/row), data = sent_21_v5, 
                            family = binomial)
  summary_v5_sent <- summary(sent_21_v5_model)
  v5_21_summary_list[[i]] <- summary_v5_sent
}
v5_21_summary_list
hist(residuals(v5_21_summary_list[[1]]))
hist(residuals(v5_21_summary_list[[2]]))
hist(residuals(v5_21_summary_list[[3]]))


# R3
r3_time_list <- c('to.predated', 'n.predated', 'd.predated')
r3_21_summary_list <- list()
for(i in 1:length(r3_time_list)){
  print(i)
  col <- r3_time_list[i]
  print(col)
  sent_21_r3 <- subset(sent_21_r3_loop, select = c("plot_id", "row", "treatment", col))
  colnames(sent_21_r3) <- c('plot_id', 'row', 'treatment', 'col')
  sent_21_r3_model <- glmer(col ~ as.factor(treatment)+
                              (1|plot_id/row), data = sent_21_r3, 
                            family = binomial)
  summary_r3_sent <- summary(sent_21_r3_model)
  r3_21_summary_list[[i]] <- summary_r3_sent
}
r3_21_summary_list
#error in d.predated: IDK why

hist(residuals(r3_21_summary_list[[1]]))
hist(residuals(r3_21_summary_list[[2]]))
#hist(residuals(r3_21_summary_list[[3]]))














# 2022 ####
sent_22

# 2023 ####
sent_23
