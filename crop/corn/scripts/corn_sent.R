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
library(car)
library(glmmTMB)
library(ggpubr)

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
  
 
proportion_df <- sent %>%
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  group_by(plot_id, block, growth_stage, treatment, year) %>% 
  summarise(prop = mean(to.predated)) %>% 
  mutate_at(1:5, as.factor) %>% 
  print(n = 10)

ad_proportion_df <- proportion_df %>% 
  mutate(ad_prop = case_when(prop == 0 ~ 0.01,
                                   prop == 1 ~ .99,
                                   .default = as.numeric(prop))) %>% 
  print(n = 10)

ad_proportion_df %>% 
  ggplot(aes(y = ad_prop, x = treatment))+
  facet_wrap(~growth_stage)+
  geom_point()



?case_when

?mutate_at
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



# models with beta distribution ####
proportion_df

m0 <- glmmTMB(ad_prop ~ (1|year/block/plot_id), family = beta_family(link = "logit"),  data = ad_proportion_df)
m1 <- glmmTMB(ad_prop ~ treatment + (1|year/block/plot_id), family = beta_family(link = "logit"), data = ad_proportion_df)
m2 <- glmmTMB(ad_prop ~ growth_stage + (1|year/block/plot_id), family = beta_family(link = "logit"), data = ad_proportion_df)
m3 <- glmmTMB(ad_prop ~ treatment*growth_stage + (1|year/block/plot_id), family = beta_family(link = "logit"), data = ad_proportion_df)
anova(m0,m1,m2,m3)
Anova(m3)
summary(m2)
qqnorm(resid(m3))
hist(resid(m3))

# all years  ####
sent_years
pred_tot
sent_prop
sent_years <- sent_years %>% 
  mutate_at(vars(1:6), as.factor)

# messed with this on 6/18/24
# removed growth stage from the random effects term
# model I am using for now 
m0 <- glmer(to.predated ~ 
              (1|year/block/plot_id),
            data = sent_years, 
            family = binomial)

m1 <- glmer(to.predated ~ treatment +
              (1|year/block/plot_id),
            data = sent_years, 
            family = binomial)

m2 <- glmer(to.predated ~ treatment+growth_stage +
              (1|year/block/plot_id),
            data = sent_years, 
            family = binomial)
 
m3 <- glmer(to.predated ~ treatment*growth_stage +
              (1|year/block/plot_id),
            data = sent_years, 
            family = binomial)

isSingular(m3)
rePCA(m3)
Anova(m3)

isSingular(m3)
check_model(m3)
hist(residuals(m3))
anova(m0, m1, m2, m3)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# m0    4 1079.5 1099.5 -535.77   1071.5                          
# m1    7 1076.9 1111.8 -531.47   1062.9  8.6093  3    0.03496 *  
# m2    9 1041.8 1086.7 -511.93   1023.9 39.0870  2  3.254e-09 ***
# m3   15 1051.5 1126.2 -510.73   1021.5  2.3972  6    0.87979    

Anova(m3)

summary(m3)
r2_nakagawa(m3)
# Conditional R2: 0.350
# Marginal R2: 0.076

cld(emmeans(m3, ~treatment, type = 'response'), Letters = letters)
cld(emmeans(m3, ~ growth_stage, type = 'response'), Letters = letters)
cld(emmeans(m3, ~treatment|growth_stage, type = 'response'), Letters = letters)


# plots ####

# pub plots: to combine with beans ##

#4.21.25
cld(emmeans(m1, ~treatment, type = 'response'), Letters = letters)

gs_beta_plot <- cld(emmeans(m2, ~growth_stage, type = 'response'), Letters = letters)

corn_sent_gs.p <- gs_beta_plot %>% 
  ggplot(aes(x = growth_stage, y = response))+
  geom_point(size = 5)+
  geom_errorbar(aes(x = growth_stage, ymin = response - SE, ymax = response + SE, width = .5), data = gs_beta_plot)+
  ylim(0,1)+
  scale_x_discrete(limits = c('V3', 'V5', 'R3'))+
  labs(title = "Corn",
       x = 'Growth stage')+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=22),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        strip.text = element_text(size = 24),
        axis.ticks = element_blank())+
  geom_text(data = gs_beta_plot, aes(y = 1, label = trimws(.group)), size = 8)




corn_sent_gs.p
bean_sent_trt.p
bean_sent_gs.p

c_b_beta_fig <- ggarrange(bean_sent_gs.p + rremove("ylab") + rremove("xlab"), 
          bean_sent_trt.p+ rremove("ylab"),
          corn_sent_gs.p + rremove("ylab"), labels = c("1", "2", "3"), font.label = list(size = 20, color = 'cornsilk4'))

annotate_figure(c_b_beta_fig,
                left = text_grob("Probability of predation (x/6)", size = 32, rot = 90))

###


sent_trt <- cld(emmeans(m3, ~treatment, type = 'response'), Letters = letters)

corn_trt_plot <- sent_trt %>% 
  ggplot(aes(x = treatment, y = prob))+
  geom_point(size = 5)+
  geom_errorbar(aes(x = treatment, ymin = prob - SE, ymax = prob + SE, width = .5), data = sent_trt)+
  ylim(0,1)+
  scale_x_discrete(limits = c('1', '2', '4', '3'), 
                   labels = c('No CC', 'Early', 'Late', 'Green'))+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=22),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        strip.text = element_text(size = 24),
        axis.ticks = element_blank())+
  geom_text(data = sent_trt, aes(y = 1, label = trimws(.group)), size = 8)



sent_gs <- cld(emmeans(m3, ~ growth_stage, type = 'response'), Letters = letters)

corn_sent_plot <- sent_gs %>% 
  ggplot(aes(x = growth_stage, y = prob))+
  geom_point(size = 5)+
  geom_errorbar(aes(x = growth_stage, ymin = prob - SE, ymax = prob + SE, width = .5), data = sent_gs)+
  ylim(0,1)+
  scale_x_discrete(limits = c('V3', 'V5', 'R3'))+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        strip.text = element_text(size = 24),
        axis.ticks = element_blank())+
  geom_text(data = sent_gs, aes(y = 1, label = trimws(.group)), size = 8)

# Bringing in the bean plots too to arrange. Need to run this code separate 

ggarrange(corn_sent_plot + rremove("ylab") + rremove("xlab")  + rremove("x.text") , 
          corn_trt_plot+ rremove("ylab") + rremove("xlab")  + rremove("x.text")+ rremove("y.text"),
          bean_sent_plot + rremove("ylab") ,
          bean_trt_plot+ rremove("ylab")+ rremove("y.text"))



##





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
  scale_x_discrete(labels=c("No CC", "Early", "Late", "Green"),
                   limits = c("1", "2", "4", "3"))+ 
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  labs(
    title = "Corn: Mean predation x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment termination",
    y = "Mean proportion predated ( x / 1 )"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 26), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())+
  annotate("text", x = 1, y = .84, label = "a", size = 10)+
  annotate("text", x = 2, y = .84, label = "ab", size = 10)+
  annotate("text", x = 4, y = .84, label = "ab", size = 10)+
  annotate("text", x = 3, y = .84, label = "b", size = 10)


trt_new <- sent %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  dplyr::select(-location, -date) %>% 
  group_by(treatment, plot_id) %>% 
  summarise(prop = mean(to.predated),
            sd = sd(to.predated),
            n = n(),
            se = sd/sqrt(n)) %>% 
  mutate_at(vars(1), factor) %>% 
  print(n= Inf)
# treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 1          0.936 0.608 Inf   -0.2549      2.13  a    
# 2          1.291 0.611 Inf    0.0941      2.49  ab   
# 3          1.585 0.612 Inf    0.3848      2.79  ab   
# 4          1.904 0.621 Inf    0.6864      3.12   b 
ggplot(trt_new, aes(x = treatment, y = prop, fill = treatment))+
  geom_boxplot(alpha = 0.7)+
  scale_x_discrete(labels=c("No CC", "Early", "Late", "Green"),
                   limits = c("1", "2", "4", "3"))+ 
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  labs(
    title = "Corn: Mean predation x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment termination",
    y = "Mean proportion predated ( x / 1 )"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 26), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())+
  annotate("text", x = 1, y = .95, label = "a", size = 10)+
  annotate("text", x = 2, y = .95, label = "ab", size = 10)+
  annotate("text", x = 4, y = .95, label = "ab", size = 10)+
  annotate("text", x = 3, y = .95, label = "b", size = 10)+
  scale_y_continuous(limits = c(0,1))
  






gs_prop <- sent %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  dplyr::select(-location, -date) %>% 
  group_by(growth_stage,plot_id) %>% 
  summarise(prop = mean(to.predated),
            sd = sd(to.predated),
            n = n(),
            se = sd/sqrt(n)) %>% 
  mutate_at(vars(1), factor) %>% 
  print(n= Inf)
# growth_stage emmean    SE  df asymp.LCL asymp.UCL .group
# V3            0.738 0.586 Inf    -0.410      1.89  a    
# V5            1.726 0.593 Inf     0.564      2.89   b   
# R3            1.823 0.593 Inf     0.660      2.99   b  
ggplot(gs_prop, aes(x = growth_stage, y =  prop, fill = growth_stage))+
  geom_boxplot(alpha = 0.7)+
  scale_x_discrete(limits = c("V3", "V5", "R3"))+ 
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77"))+
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
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 26), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())+
  annotate("text", x = 1, y = 1, label = "a", size = 10)+
  annotate("text", x = 2, y = 1, label = "b", size = 10)+
  annotate("text", x = 3, y = 1, label = "b", size = 10)+
  scale_y_continuous(limits = c(0,1))
  
# pub plots ####

ggplot(trt_prop, aes(x = treatment, y =  prop))+
  geom_point(size = 10)+
  geom_errorbar(aes(x = treatment,ymin = prop - se, ymax = prop + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1)+
  scale_x_discrete(labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"),
                   limits = c("1", "2", "4", "3"))+ 
  labs(
    title = "Corn: Mean predation x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Mean proportion predated ( x / 1 )",
    caption = "DPP: Days pre plant
DAP: Days after plant"
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
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  annotate("text", x = 1, y = .71, label = "a", size = 10)+
  annotate("text", x = 2, y = .77, label = "ab", size = 10)+
  annotate("text", x = 4, y = .82, label = "ab", size = 10)+
  annotate("text", x = 3, y = .84, label = "b", size = 10)


ggplot(gs_prop, aes(x = growth_stage, y =  prop))+
  geom_point(size = 10)+
  geom_errorbar(aes(x = growth_stage,ymin = prop - se, ymax = prop + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1)+
  scale_x_discrete(limits = c("V3", "V5", "R3"))+ 
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


