# sent prey Pennsylvania 
# goals: 
  # 1: run models individually for each year on growth stage and time of predation/ total predation
  # 2: run models of total predation by growth stage and year 
  # create plots for each year and then facet wrap them all together with growth stage being wrapped


# packages ####
library(tidyverse)
library(MASS)
library(performance)
library(lme4)
library(emmeans)
library(lmtest)
library(nlme)
library(multcomp)

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
  dplyr::select(-n.absent, -n.partial, -d.absent, -d.partial, -d.predated)
  
 
  
sent_prop <- sent %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  dplyr::select(-location, -date) %>% 
  group_by(growth_stage, treatment) %>% 
  summarise(prop = mean(to.predated),
            sd = sd(to.predated),
            n = n(),
            se = sd/sqrt(n)) %>% 
  mutate_at(vars(1:2), factor) %>% 
  print(n= Inf)


# subset by year and then growth stage 
sent_21 <- subset(sent_years, year == '2021')
sent_22 <- subset(sent_years, year == '2022')
sent_23 <- subset(sent_years, year == '2023')


# all years  ####
sent_years
pred_tot
sent_prop
sent_years <- sent_years %>% 
  mutate_at(vars(1:6), as.factor)

# model I am using for now 
m0 <- glmer(to.predated ~ 
              (1|year/block/plot_id/growth_stage),
            data = sent_years, 
            family = binomial)

m1 <- glmer(to.predated ~ treatment +
              (1|year/block/plot_id/growth_stage),
            data = sent_years, 
            family = binomial)

m2 <- glmer(to.predated ~ treatment+growth_stage +
              (1|year/block/plot_id/growth_stage),
            data = sent_years, 
            family = binomial)

m3 <- glmer(to.predated ~ treatment*growth_stage +
              (1|year/block/plot_id/growth_stage),
            data = sent_years, 
            family = binomial)
check_model(m3)
anova(m0, m1, m2, m3)
summary(m3)
binned_residuals(m3)
r2_nakagawa(m3)
#   Conditional R2: 0.555
#   Marginal R2: 0.101
m_emm <- emmeans(m3, ~ treatment, type = 'pairwise')
pairs(m_emm, simple = "each")
pwpm(m_emm)
cld(m_emm, Letters = letters)

mg_emm <- emmeans(m3, ~ growth_stage, type = 'pairwise')
pairs(mg_emm, simple = "each")
pwpm(mg_emm)
cld(mg_emm, Letters = letters)


# plots ####

ggplot(sent_prop, aes(x = treatment, y =  prop))+
  geom_point(aes(size = 5, color = treatment))+
  facet_wrap(~factor(growth_stage, level = c("V3", "V5", "R3")))+
  geom_errorbar(aes(x = treatment,ymin = prop - se, ymax = prop + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1)+
  scale_x_discrete(labels=c("No CC", "14-21 DPP", "3-7 DPP", "1-3 DAP"),
                   limits = c("1", "2", "4", "3"))+ 
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  labs(
    title = "Corn: Mean predation",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Mean proportion predated (x/1)"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 20),
        plot.subtitle = element_text(size = 18),
        plot.title = element_text(size = 24),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        strip.text.x = element_text(size = 20), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())
  


trt_prop <- sent %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  dplyr::select(-location, -date) %>% 
  group_by(treatment) %>% 
  summarise(prop = mean(to.predated),
            sd = sd(to.predated),
            n = n(),
            se = sd/sqrt(n)) %>% 
  mutate_at(vars(1), factor) %>% 
  print(n= Inf)

ggplot(trt_prop, aes(x = treatment, y =  prop))+
  geom_point(aes(color = treatment), size = 10)+
  geom_errorbar(aes(x = treatment,ymin = prop - se, ymax = prop + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1)+
  scale_x_discrete(labels=c("No CC", "14-21 DPP", "3-7 DPP", "1-3 DAP"),
                   limits = c("1", "2", "4", "3"))+ 
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  labs(
    title = "Corn: Mean predation x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Mean proportion predated ( x / 1 )"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())+
  annotate("text", x = 1, y = .71, label = "a", size = 10)+
  annotate("text", x = 2, y = .77, label = "ab", size = 10)+
  annotate("text", x = 4, y = .82, label = "b", size = 10)+
  annotate("text", x = 3, y = .84, label = "ab", size = 10)
  


gs_prop <- sent %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  dplyr::select(-location, -date) %>% 
  group_by(growth_stage) %>% 
  summarise(prop = mean(to.predated),
            sd = sd(to.predated),
            n = n(),
            se = sd/sqrt(n)) %>% 
  mutate_at(vars(1), factor) %>% 
  print(n= Inf)

ggplot(gs_prop, aes(x = growth_stage, y =  prop))+
  geom_point(aes(color = growth_stage), size = 10)+
  geom_errorbar(aes(x = growth_stage,ymin = prop - se, ymax = prop + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1)+
  scale_x_discrete(limits = c("V3", "V5", "R3"))+ 
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77"))+
  labs(
    title = "Corn: Mean predation x Growth Stage",
    subtitle = "Years: 2021-2023",
    x = "Growth Stage",
    y = "Mean proportion predated ( x / 1 )"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())+
  annotate("text", x = 1, y = 0.68, label = "a", size = 10)+
  annotate("text", x = 2, y = 0.825, label = "b", size = 10)+
  annotate("text", x = 3, y = 0.84, label = "b", size = 10)
  


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
  sent_21_v3 <- subset(sent_21_v3_loop, dplyr::select = c("plot_id", "row", "treatment",'block', col)) # subset what I want to use in the model plus the new col I made
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
  sent_21_v5 <- subset(sent_21_v5_loop, dplyr::select = c("plot_id", "row", "treatment",'block', col))
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
  sent_21_r3 <- subset(sent_21_r3_loop, dplyr::select = c("plot_id", "row", "treatment", "block", col))
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
  sent_22_whole_md <- subset(sent_22, dplyr::select = c("plot_id", "row", "treatment",'block', col)) # subset what I want to use in the model plus the new col I made
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
    sent_22_mdf <- subset(sent_22_loop, dplyr::select = c("plot_id", "treatment",'block', 'prop_pred', 'total')) # subset what I want to use in the model plus the new col I made
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
    sent_23_mdf <- subset(sent_23_loop, dplyr::select = c("plot_id", "row", "treatment",'block', 'to.predated')) # subset what I want to use in the model plus the new col I made
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
  test_mdf <- subset(test_loop, dplyr::select = c("plot_id", "treatment",'block', 'prop_pred', 'total')) # subset what I want to use in the model plus the new col I made
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


# jw method? ####
install.packages("nlme")
library(nlme)
help(lme)
help(corCAR1)

# this is a repeated measures model, and thus, plot id is included 
m1 <- nlme::lme(to.predated ~ as.factor(treatment)*growth_stage, random = ~1|year/block/plot_id, 
          correlation = nlme::corCAR1(form=~1|year/block/plot_id), data = sent_years)
summary(m1)
r2_nakagawa(m1)
binned_residuals(m1, residuals = "response")

m2 <- nlme::lme(to.predated ~ as.factor(treatment)*growth_stage, random = ~1|block/plot_id, 
                correlation = nlme::corCAR1(form=~1|block/plot_id), data = sent_years)
summary(m2)
r2_nakagawa(m2)
binned_residuals(m2, residuals = "response")


m3 <- nlme::lme(to.predated ~ as.factor(treatment)*growth_stage, random = ~1|plot_id, 
                correlation = nlme::corCAR1(form=~1|plot_id), data = sent_years)
summary(m3)

