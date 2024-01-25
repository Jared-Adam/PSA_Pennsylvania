# Jared Adam 
# sent prey for beans 
# on a plain

# packages ####
library(tidyverse)
library(MASS)
library(performance)
library(lme4)
library(emmeans)
library(lmtest)

# data ####
beans_sent <- sent_prey_beans_all

colnames(beans_sent)
sent_years <- beans_sent %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  dplyr::select(-location, -date) %>% 
  rename(plot_id = ploitid) %>% 
  relocate(year, growth_stage, plot_id, block, treatment, row, sample, pm.absent, pm.partial, n.pred, 
          am.absent, am.partial, d.pred, to.predated, pm.weather, am.weather)%>%
  mutate(n.pred = as.double(n.pred),
         d.pred = as.double(d.pred),
         to.predated = as.double(to.predated)) %>% 
  print(n = Inf)

pred_tot <- sent_years %>% 
  dplyr::select(-pm.absent, -pm.partial, -am.absent, -am.partial, -d.pred, -n.pred)
  
 
  
sent_prop <- beans_sent %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  dplyr::select(-location, -date) %>% 
  rename(plot_id = ploitid) %>% 
  group_by(year, block, treatment, growth_stage, plot_id) %>% 
  summarise(pred_tot = sum(to.predated)) %>% 
  mutate(total = 6) %>% 
  mutate(prop_pred = (pred_tot/total)) %>% 
  print(n= Inf)


# subset by year and then growth stage 
sent_22 <- subset(sent_years, year == '2022')
sent_23 <- subset(sent_years, year == '2023')

# all ####
sent_years

sent_years$growth_stage <- as.factor(sent_years$growth_stage)
sent_years$block <- as.factor(sent_years$block)
test <- glmer(to.predated ~ as.factor(treatment)*growth_stage +
                            (1|year/block/plot_id), data = sent_years,
                          family = binomial)

summary(test)
r2_nakagawa(test)
result <- binned_residuals(test)
plot(result)

#not a good fit 
block_md <- glmer(to.predated ~ as.factor(treatment) +
                (1|year/block), data = sent_years,
              family = binomial)
check_model(block_md)
summary(block_md)
r2_nakagawa(block_md)
result_block <- binned_residuals(block_md)
plot(result_block)

#singular fit
plot_md <- glmer(to.predated ~ as.factor(treatment) +
                   (1|year/block/plot_id) , data = sent_years, 
                 family = binomial)
check_model(plot_md)
summary(plot_md)
this <- binned_residuals(plot_md)
plot(this)
r2_nakagawa(plot_md)


growth_md <-  glmer(to.predated ~ as.factor(treatment)*growth_stage +
                      (1|year/block/plot_id) , data = sent_years, 
                    family = binomial)
check_model(growth_md)
summary(growth_md)
that <- binned_residuals(growth_md)
plot(that)
r2_nakagawa(growth_md)

# cannot get a binned residual off this model
m1 <- glmer(to.predated ~ as.factor(treatment) +
              (1|year/growth_stage/block), 
            data = sent_years, 
            family = binomial)
check_model(m1)
summary(m1)
binned_residuals(m1)
r2_nakagawa(m1)
?emmeans
m1_emm <- emmeans(m1, pairwise ~ as.factor(treatment), type = 'response')
m1_plot <- as.data.frame(m1_emm$emmeans)

# plots ####
m1_plot
ggplot(m1_plot, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 3,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0, linewidth = 1.5)+
  geom_errorbar(aes(x = treatment,ymin = asymp.LCL, ymax = asymp.UCL), 
                alpha = .6, width = 0, linewidth = 1)+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.text.y = element_text(size = 12))+
  annotate("text", x = 2, y = 1.01, label = "t2:p = 0.00107**")+
  annotate("text", x = 3, y = 1.01, label = "t3:p = 2.04e-05***")+
  annotate("text", x = 4, y = 1.01, label = "t4:p = 0.03882*")+
  labs(
    title = "Beans sent prey both years",
    y = "Mean predation",
    x = "Treatment"
  )+
  theme(legend.position = 'none')
