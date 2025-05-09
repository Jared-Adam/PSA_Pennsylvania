# sent prey Pennsylvania 
# goals: 
  # 1: run models individually for each year on growth stage and time of predation/ total predation
  # 2: run models of total predation by growth stage and year 
  # create plots for each year and then facet wrap them all together with growth stage being wrapped


# packages ####
library(tidyverse)
library(lme4)
library(performance)
library(see)

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
         to.predated = as.double(to.predated)) %>% 
  print(n = Inf)

pred_tot <- sent_years %>% 
  select(-n.absent, -n.partial, -d.absent, -d.partial, -d.predated)
  
 
  
sent_prop <- sent %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  dplyr::select(-location, -date) %>% 
  group_by(year, block, treatment, growth_stage, plot_id) %>% 
  summarise(pred_tot = sum(to.predated)) %>% 
  mutate(total = 6) %>% 
  mutate(prop_pred = (pred_tot/total)) %>% 
  print(n= Inf)


# subset by year and then growth stage 
sent_21 <- subset(sent_years, year == '2021')
sent_22 <- subset(sent_years, year == '2022')
sent_23 <- subset(sent_years, year == '2023')

# 2021 ####
sent_21

# perofrmaance test 
test_model <- glmer(prop_pred ~ as.factor(treatment) +
                            (1|block), data = sent_prop, 
                    weights = total,
                    family = binomial)
r2_nakagawa(test_model)
result<-binned_residuals(test_model)
plot(result)
summary(test_model)




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
r2_list <- list()
binned_residuals <- list()
for(i in 1:length(v3_time_list)){ # for each iteration across the length of the list I made
  print(i) # print each iteration
  col <- v3_time_list[i] #place the iterations into an object named col
  print(col) # print them to make sure it works
  sent_21_v3 <- subset(sent_21_v3_loop, select = c("plot_id", "row", "treatment",'block', col)) # subset what I want to use in the model plus the new col I made
  colnames(sent_21_v3) <- c("plot_id", "row", "treatment", 'block', "col") # add this as a column name for the model
  #print sent_21_V3 to make sure it works
  sent_21_v3_model <- glmer(col ~ as.factor(treatment) +
                             (1|block), data = sent_21_v3,
                           family = binomial)
  summary_v3_sent <- summary(sent_21_v3_model)
  v3_21_summary_list[[i]] <- summary_v3_sent
  r2_list_final <- r2_nakagawa(sent_21_v3_model)
  r2_list[[i]] <- r2_list_final
  residual_list <- binned_residuals(sent_21_v3_model)
  binned_residuals[[i]] <- residual_list
}
v3_21_summary_list
r2_list
plot(binned_residuals[[1]])

hist(residuals(v3_21_summary_list[[1]]))
hist(residuals(v3_21_summary_list[[2]]))
hist(residuals(v3_21_summary_list[[3]]))

# V5
v5_time_list <- c('to.predated', 'n.predated', 'd.predated')
v5_21_summary_list <- list()
r2_list_v5 <- list()
binned_residuals_v5 <- list()
for(i in 1:length(v5_time_list)){
  print(i)
  col <- v5_time_list[i]
  print(col)
  sent_21_v5 <- subset(sent_21_v5_loop, select = c("plot_id", "row", "treatment",'block', col))
  colnames(sent_21_v5) <- c('plot_id', 'row', 'treatment','block', 'col')
  sent_21_v5_model <- glmer(col ~ as.factor(treatment)+
                              (1|block), data = sent_21_v5, 
                            family = binomial)
  summary_v5_sent <- summary(sent_21_v5_model)
  v5_21_summary_list[[i]] <- summary_v5_sent
  r2_list_final <- r2_nakagawa(sent_21_v5_model)
  r2_list_v5[[i]] <- r2_list_final
  residual_list <- binned_residuals(sent_21_v5_model)
  binned_residuals_v5[[i]] <- residual_list
}
v5_21_summary_list
r2_list_v5
plot(binned_residuals_v5[[1]])

hist(residuals(v5_21_summary_list[[1]]))
hist(residuals(v5_21_summary_list[[2]]))
hist(residuals(v5_21_summary_list[[3]]))


# R3
r3_time_list <- c('to.predated', 'n.predated', 'd.predated')
r3_21_summary_list <- list()
r2_list_r3 <- list()
binned_residuals_r3 <- list()
for(i in 1:length(r3_time_list)){
  print(i)
  col <- r3_time_list[i]
  print(col)
  sent_21_r3 <- subset(sent_21_r3_loop, select = c("plot_id", "row", "treatment", "block", col))
  colnames(sent_21_r3) <- c('plot_id', 'row', 'treatment', 'block', 'col')
  sent_21_r3_model <- glmer(col ~ as.factor(treatment)+
                              (1|block), data = sent_21_r3, # nesting year in the future
                            family = binomial)
  summary_r3_sent <- summary(sent_21_r3_model)
  r3_21_summary_list[[i]] <- summary_r3_sent
  r2_list_final <- r2_nakagawa(sent_21_r3_model)
  r2_list_r3[[i]] <- r2_list_final
#   residual_list <- binned_residuals(sent_21_r3_model)
#   binned_residuals_r3[[i]] <- residual_list
 }
r3_21_summary_list
r2_list_r3
binned_residuals_r3
#Singularity error: Is this becuase my values are so close to 1 that there is no variation? 


hist(residuals(r3_21_summary_list[[1]]))
hist(residuals(r3_21_summary_list[[2]]))
#hist(residuals(r3_21_summary_list[[3]]))

# binary data is ok with plot becuase each is a data point. Count or biomass does not work becuase it is pseudo rep. 
# in that case, we take the average. Wallace said this, I do not agree and changed it to a prop

###
##
#

# 2022 ####
sent_22
unique(sent_22$growth_stage)

#subset by growth stage
# sent_22_v3_loop <- subset(sent_22, growth_stage == 'V3')
# sent_22_v5_loop <- subset(sent_22, growth_stage == 'V5')
# sent_22_r3_loop <- subset(sent_22, growth_stage == 'R3')



time_list <- c("to.predated", "n.predated", "d.predated")
growth_list <- c('V3', 'V5', 'R3')
v3_22_summary_list <- list()
r2_list_22_v3 <- list()
binned_residuals <- list()

for(i in 1:length(time_list)){
  for(j in 1: length(growth_list)){# for each iteration across the length of the list I made
  print(i) # print each iteration
  print(j)
  col <- v3_time_list[i] #place the iterations into an object named col
  growth <- growth_list[j]
  print(col) # print them to make sure it works
  print(growth)
  sent_22_whole_md <- subset(sent_22, select = c("plot_id", "row", "treatment",'block', col)) # subset what I want to use in the model plus the new col I made
  colnames(sent_22_whole_md) <- c("plot_id", "row", "treatment", 'block', "col") # add this as a column name for the model
  #print sent_21_V3 to make sure it works
  sent_22_v3_model <- glmer(col ~ as.factor(treatment) +
                              (1|block), data = sent_22_whole_md,
                            family = binomial)
  summary_22_v3_sent <- summary(sent_22_v3_model)
  v3_22_summary_list[[i]] <- summary_22_v3_sent
  r2_list_22_v3_final <- r2_nakagawa(sent_22_v3_model)
  r2_list_22_v3[[i]] <- r2_list_22_v3_final
  }
}

v3_22_summary_list
r2_list_22_v3[[1]]
# plot(binned_residuals())

# did this work? 
# I got a bunch of models outputs, but cannot seem to parse out 

# 1/21/2023 the output above seems to be only the summary of the first three, which are to.predated from each growth stage 
# the test below output matches that of the above, confirming my theory. 
# do i need day and night? likely not... but ok for now. 
# a lot of singularity stuff.. something to look at while 

# 2022 test ####
colnames(sent_22)

sent_22_loop <- sent_22
#time_list <- c("to.predated", "n.predated", "d.predated")
growth_list <- c('V3', 'V5', 'R3')
summary_list_22 <- list()
r2_list_22 <- list()
binned_residuals <- list()

  for(j in 1: length(growth_list)){# for each iteration across the length of the list I made
    #print(i) # print each iteration
    print(j)
    #col <- v3_time_list[i] #place the iterations into an object named col
    growth <- growth_list[j]
    # print(col) # print them to make sure it works
    print(growth)
    sent_22_mdf <- subset(sent_22_loop, select = c("plot_id", "treatment",'block', 'prop_pred', 'total')) # subset what I want to use in the model plus the new col I made
    colnames(sent_22_v3) <- c("plot_id", "treatment", 'block', 'prop_pred', 'total') # add this as a column name for the model
    #print sent_21_V3 to make sure it works
    sent_22_model <- glmer(prop_pred ~ as.factor(treatment) +
                                (1|block), data = sent_22_mdf,
                           weights = total,
                              family = binomial)
    summary_22_v3_sent <- summary(sent_22_model)
    summary_list_22[[j]] <- summary_22_v3_sent
    r2_list_22_final <- r2_nakagawa(sent_22_model)
    r2_list_22[[j]] <- r2_list_22_final
  }

test_summary_list <- v3_22_summary_list
r2_list_22_v3[[3]] # no conditional r2 becuase singular
# plot(binned_residuals())



###
##
#

# 2023 ####
sent_23

sent_23_loop <- sent_23
#time_list <- c("to.predated", "n.predated", "d.predated")
growth_list <- c('V3', 'V5', 'R3')
v3_23_summary_list <- list()
r2_list_23_v3 <- list()
binned_residuals <- list()

  for(j in 1: length(growth_list)){# for each iteration across the length of the list I made
    #print(i) # print each iteration
    print(j)
    #col <- v3_time_list[i] #place the iterations into an object named col
    growth <- growth_list[j]
    # print(col) # print them to make sure it works
    print(growth)
    sent_23_mdf <- subset(sent_23_loop, select = c("plot_id", "row", "treatment",'block', 'to.predated')) # subset what I want to use in the model plus the new col I made
    colnames(sent_23_mdf) <- c("plot_id", "row", "treatment", 'block', 'to.predated') # add this as a column name for the model
    #print sent_21_V3 to make sure it works
    sent_23_v3_model <- glmer(to.predated ~ as.factor(treatment) +
                                (1|block), data = sent_23_mdf,
                              family = binomial)
    summary_23_v3_sent <- summary(sent_23_v3_model)
    v3_23_summary_list[[j]] <- summary_23_v3_sent
    r2_list_23_v3_final <- r2_nakagawa(sent_23_v3_model)
    r2_list_23_v3[[j]] <- r2_list_23_v3_final
  }

test_summary_list <- v3_23_summary_list
r2_list_23_v3[[3]]

# 2023 prop test 
test_loop <- sent_23

sent_23_test_loop$growth_stage <- as.factor(sent_23_test_loop$growth_stage)

growth_list <- c('V3', 'V5', 'R3')
test_summary_list <- list()
r2_test <- list()
binned_residuals <- list()
for (i in 1:length(unique(test_loop$growth_stage))) {
  for(j in 1:length(unique(growth_list))){# for each iteration across the length of the list I made
  #print(i) # print each iteration
  print(j)
  #col <- v3_time_list[i] #place the iterations into an object named col
  growth <- growth_list[j]
  # print(col) # print them to make sure it works
  print(growth)
  test_mdf <- subset(test_loop, select = c("plot_id", "treatment",'block', 'prop_pred', 'total')) # subset what I want to use in the model plus the new col I made
  colnames(test_mdf) <- c("plot_id", "treatment", 'block', 'prop_pred', 'total') # add this as a column name for the model
  #print sent_21_V3 to make sure it works
  test_model <- glmer(prop_pred ~ as.factor(treatment) +
                           (1|block), data = test_mdf,
                         weights = total,
                         family = binomial)
  summary_ <- summary(test_model)
  test_summary_list[[j]] <- summary_
  r2_test_final <- r2_nakagawa(test_model)
  r2_test[[j]] <- r2_test_final
}
}
test_summary_list
r2_test

summary_list <- list()
model <- list()
growthlist <- c('V3', 'V5', 'R3')
for (x in seq_along(unique(test_loop$growth_stage))) {
  print(x)
  val <- unique(test_loop$growth_stage[x])
  # data = subset(test_loop, growth_stage == x)
  model <- glmer(prop_pred ~ as.factor(treatment) +
                   (1|block), data = data, 
                 weights = total, 
                 family = binomial)
  summary_here <- summary(summary(model))
  summary_list[[x]] <- summary_here
}
summary_list






library(broom)

broom_test <- test_loop %>% 
  ungroup() %>% 
  mutate(growth_stage = as.factor(growth_stage)) %>% 
  nest(growth_stage) %>% 
  mutate(model = map(glmer(prop_pred ~ as.factor(treatment) +
                                      (1|block), data = test_loop, 
                                    weights = total, 
                                    family = binomial))) %>% 
  unnest(out)

# 1/22/2023 : done with this shit for a while 


# all years for fuck sake ####
sent_years
pred_tot
sent_prop
sent_years$growth_stage <- as.factor(sent_years$growth_stage)
test <- glmer(to.predated ~ as.factor(treatment)*growth_stage +
                            (1|year/block/plot_id), data = sent_years,
                          family = binomial)

summary(test)
r2_nakagawa(test)
result <- binned_residuals(test)
plot(result)

null <- glmer(to.predated ~ as.factor(treatment) +
                (1|year), data = sent_years,
              family = binomial)
summary(null)
r2_nakagawa(null)
result_null <- binned_residuals(null)
plot(result_null)

block_md <- glmer(to.predated ~ as.factor(treatment) +
                (1|year/block), data = sent_years,
              family = binomial)
summary(block_md)
r2_nakagawa(block_md)
result_block <- binned_residuals(block_md)
plot(result_block)





# plots ####
sent_prop





ggplot(sent_prop, aes(x = treatment, y = prop_pred, fill = as.factor(treatment)))+
  geom_boxplot()+
  coord_flip()+
  facet_grid(~year)
