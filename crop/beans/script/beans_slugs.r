# Jared Adam 
# beans slugs = 2022 and 2023
# started on the plane 
# adding here slug populations models and figs
# slug x predator regressions and plots 

# packages ####
library(tidyverse)
library(emmeans)
library(lme4)
library(lmtest)
library(MASS)
library(nlme)
library(plotly)
library(multcomp)
library(ggpubr)
library(corrplot)
library(ggcorrplot)
library(performance)


# data ####
slugs <- slugs_beans_all %>% 
  mutate(slug_count = as.numeric(slug_count)) %>% 
  rename(precip = '7_day_precip_in') %>% 
  mutate(temp = as.numeric(temp)) %>% 
  mutate(treatment = case_when(plot %in% c(101,203,304,401,503) ~ 1,
                               plot %in% c(103,204,302,403,501) ~ 2,
                               plot %in% c(102,201,303,402,502) ~ 3, 
                               plot %in% c(104,202,301,404,504) ~ 4)) %>% 
  mutate(block = case_when(plot %in% c(101,102,103,104) ~ 1,
                           plot %in% c(201,202,203,204) ~ 2,
                           plot %in% c(301,302,303,304) ~ 3,
                           plot %in% c(401,402,403,404) ~ 4,
                           plot %in% c(501,502,503,504) ~ 5)) %>% 
  mutate(block = as.factor(block)) %>%
  dplyr::select(-location, -shingle_id, -time, -temp, -row) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y'))  %>% 
  # dplyr::select(-date) %>% 
  mutate(year = as.factor(year), 
       treatment = as.factor(treatment))%>% 
  rename(season = seaon) %>% 
  mutate(season = case_when(season == "fall" ~ "Fall",
                            season == "spring" ~ "Spring"))%>% 
  group_by(season, year, month, plot, treatment, block, date) %>% 
  summarise(total_slug =  sum(slug_count))%>% 
  print(n = Inf)
slugs <- slugs[1:400,]
slugs <- slugs %>% 
  replace(is.na(.),0) %>% 
  print(n = Inf)
unique(slugs$treatment)
unique(slugs$season)

# getting precip 
slug_precip <- slugs_beans_all %>% 
  mutate(slug_count = as.numeric(slug_count)) %>% 
  rename(precip = '7_day_precip_in') %>% 
  mutate(temp = as.numeric(temp)) %>% 
  mutate(treatment = case_when(plot %in% c(101,203,304,401,503) ~ 1,
                               plot %in% c(103,204,302,403,501) ~ 2,
                               plot %in% c(102,201,303,402,502) ~ 3, 
                               plot %in% c(104,202,301,404,504) ~ 4)) %>% 
  mutate(block = case_when(plot %in% c(101,102,103,104) ~ 1,
                           plot %in% c(201,202,203,204) ~ 2,
                           plot %in% c(301,302,303,304) ~ 3,
                           plot %in% c(401,402,403,404) ~ 4,
                           plot %in% c(501,502,503,504) ~ 5)) %>% 
  mutate(block = as.factor(block)) %>%
  dplyr::select(-location, -shingle_id, -time, -temp, -row) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y'))  %>% 
  # dplyr::select(-date) %>% 
  mutate(year = as.factor(year), 
         treatment = as.factor(treatment))%>% 
  rename(season = seaon) %>% 
  mutate(season = case_when(season == "fall" ~ "Fall",
                            season == "spring" ~ "Spring"))%>% 
  group_by(year, season, month, plot, date) %>% 
  summarise(sum_pre = sum(precip)) %>% 
  print(n = Inf)
slug_precip <- slug_precip[1:400,]
slug_precip <- slug_precip %>% 
  replace(is.na(.),0) %>%
  ungroup() %>% 
  dplyr::select(sum_pre) %>% 
  print(n = Inf)
  
final_slug <- cbind(slug_precip, slugs) 

final_slug <- final_slug %>% 
  mutate_at(vars(2:7), as.factor) %>% 
  relocate(date, season, year, month, plot, treatment, block) %>% 
  mutate(date = as.character(date),
         date = as.factor(date)) %>% 
  as_tibble() %>% 
  print(n = 10)
unique(final_slug$season)


# bs_22 <- subset(slugs, year == "2022")
# bs_23 <- subset(slugs, year == "2023")

# explore the data ####
final_slug
resp <- names(final_slug[9])
resp <- set_names(resp)

cat_pred <- names(final_slug[1:7])
cat_pred <- set_names(cat_pred)

box_plots <- function(x,y) {
  ggplot(final_slug, aes(x = .data[[x]], y = .data[[y]]))+
    geom_boxplot()+
    theme_bw()
}

cat_plots <- map(cat_pred, ~box_plots(.x,'total_slug'))
ggarrange(plotlist = cat_plots)

con_pred <- names(final_slug[8])
con_pred <- set_names(con_pred)

scatter_p <- function(x,y) {
  ggplot(final_slug, aes(x = .data[[x]], y = .data[[y]]))+
    geom_point()+
    stat_smooth(method = 'loess', se = FALSE, color = 'grey75')+
    theme_bw()
}

con_plot <- map(con_pred, ~scatter_p(.x, 'total_slug'))
con_plot



# model choice? ####

# look at overdispersion: variance > mean?
#spring
dispersion_stats <- spring_slugs %>% 
  group_by(treatment) %>%
  summarise(
    mean = mean(total_slug, na.rm=TRUE),
    variances = var(total_slug, na.rm=TRUE),
    ratio = variances/mean) 
if(dispersion_stats$mean[1] > dispersion_stats$variances[1] & 
   dispersion_stats$mean[2] > dispersion_stats$variances[2] &
   dispersion_stats$mean[3] > dispersion_stats$variances[3] &
   dispersion_stats$mean[4] > dispersion_stats$variances[4]){
  print("run a poisson, probs")
  } else {
    print("these jawns overdispersed")
  }

#fall
dispersion_stats <- fall_slugs %>% 
  group_by(treatment) %>%
  summarise(
    mean = mean(total_slug, na.rm=TRUE),
    variances = var(total_slug, na.rm=TRUE),
    ratio = variances/mean) 
if(dispersion_stats$mean[1] > dispersion_stats$variances[1] & 
   dispersion_stats$mean[2] > dispersion_stats$variances[2] &
   dispersion_stats$mean[3] > dispersion_stats$variances[3] &
   dispersion_stats$mean[4] > dispersion_stats$variances[4]){
  print("run a poisson, probs")
} else {
  print("these jawns overdispersed")
}

# model selection ####
fall_slugs <- subset(slugs, season == "Fall")

poisson_model <- glmer(total_slug ~ treatment*year + 
                         (1|date), 
                       data = fall_slugs, 
                       family = poisson)

nb_model_trt <- glmer.nb(total_slug ~ treatment*year + 
                           (1|date), 
                         data = fall_slugs) 

lrtest(poisson_model,nb_model_trt)

# fall
f0 <- glmer.nb(total_slug ~  + 
                 (1|date), 
               data = fall_slugs) 
f1 <- glmer.nb(total_slug ~ treatment + 
                 (1|date), 
               data = fall_slugs) 
f2 <- glmer.nb(total_slug ~ treatment+year + 
                 (1|date), 
               data = fall_slugs) 
f3 <- glmer.nb(total_slug ~ treatment*year + 
                 (1|date), 
               data = fall_slugs) 

isSingular(f3)
anova(f0, f1, f2, f3)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# f0    3 590.64 600.54 -292.32   584.64                          
# f1    6 570.21 590.00 -279.11   558.21 26.4304  3   7.75e-06 ***
#   f2    7 572.06 595.15 -279.03   558.06  0.1551  1     0.6937    
# f3   10 577.90 610.89 -278.95   557.90  0.1556  3     0.9844 
summary(f3)
hist(residuals(f3))
res <- residuals(f3)
qqnorm(res)
plot(fitted(f3), res)
r2_nakagawa(f3)
binned_residuals(f3)
check_model(f3)

cld(emmeans(f3, ~treatment), Letters = letters)
# treatment  emmean    SE  df asymp.LCL asymp.UCL .group
# 4         -0.6361 0.393 Inf    -1.406     0.134  a    
# 3         -0.4107 0.386 Inf    -1.167     0.346  a    
# 2         -0.0566 0.371 Inf    -0.784     0.671  ab   
# 1          0.3447 0.361 Inf    -0.363     1.052   b 

cld(emmeans(f3, ~treatment|year), Letters = letters)
# year = 2022:
#   treatment  emmean    SE  df asymp.LCL asymp.UCL .group
# 4         -0.5566 0.418 Inf    -1.377     0.264  a    
# 3         -0.2209 0.409 Inf    -1.022     0.580  a    
# 2          0.0687 0.401 Inf    -0.718     0.855  ab   
# 1          0.4857 0.393 Inf    -0.285     1.256   b   
# 
# year = 2023:
#   treatment  emmean    SE  df asymp.LCL asymp.UCL .group
# 4         -0.7156 0.665 Inf    -2.019     0.587  a    
# 3         -0.6004 0.654 Inf    -1.883     0.682  a    
# 2         -0.1819 0.624 Inf    -1.405     1.041  a    
# 1          0.2037 0.605 Inf    -0.983     1.390  a

##

# spring
spring_slugs <- subset(slugs, season == "Spring")
# let's see which is better, poisson or nb? 
# run one of each where the only difference is the family 
poisson_model <- glmer(total_slug ~ treatment*year + 
                         (1|date), 
                       data = spring_slugs, 
                       family = poisson)

nb_model_trt <- glmer.nb(total_slug ~ treatment*year + 
                           (1|block/date), 
                         data = spring_slugs) 

lrtest(poisson_model,nb_model_trt)
# the negative binomial has the higher likelihood score, so we will use that

# spring
m0 <- glmer.nb(total_slug ~  + 
           (1|block/date), 
         data = spring_slugs) 
m1 <- glmer.nb(total_slug ~ treatment + 
           (1|block/date), 
         data = spring_slugs) 
m2 <- glmer.nb(total_slug ~ treatment+year + 
           (1|block/date), 
         data = spring_slugs) 
m3 <- glmer.nb(total_slug ~ treatment*year + 
           (1|block/date), 
         data = spring_slugs) 

isSingular(m3)
anova(m0, m1, m2, m3)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# m0    4 830.18 843.37 -411.09   822.18                          
# m1    7 835.11 858.20 -410.56   821.11  1.0623  3     0.7862    
# m2    8 739.71 766.09 -361.85   723.71 97.4088  1     <2e-16 ***
# m3   11 745.26 781.54 -361.63   723.26  0.4508  3     0.9296  


summary(m3)
hist(residuals(m3))
res <- residuals(m3)
qqnorm(res)
plot(fitted(m3), res)
r2_nakagawa(m3)
binned_residuals(m3)
check_model(m3)
cld(emmeans(m3, ~treatment|year), Letters = letters)
# year = 2022:
#   treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 3          -3.23 1.025 Inf     -5.24     -1.22  a    
# 1          -3.23 1.025 Inf     -5.24     -1.22  a    
# 2          -3.21 1.024 Inf     -5.22     -1.21  a    
# 4          -2.53 0.742 Inf     -3.99     -1.08  a    
# 
# year = 2023:
#   treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 3           1.60 0.179 Inf      1.25      1.95  a    
# 2           1.75 0.177 Inf      1.40      2.10  a    
# 4           1.76 0.176 Inf      1.42      2.11  a    
# 1           1.76 0.176 Inf      1.42      2.11  a  
# 

cld(emmeans(m3, ~year), Letters = letters)
# year emmean    SE  df asymp.LCL asymp.UCL .group
# 2022  -3.05 0.507 Inf     -4.04     -2.06  a    
# 2023   1.72 0.141 Inf      1.44      1.99   b   


# bsl.table <- as.data.frame(summary(m1)$coefficients)
# #CI <- confint(m1)
# bsl.table <-cbind(row.names(bsl.table), bsl.table)
# names(bsl.table) <- c("Term", "B", "SE", "t", "p")
# nice_table(bsl.table, highlight = TRUE)





# plots for slug populations  ####
slug_plot <- final_slug %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(total_slug), 
            sd = sd(total_slug), 
            n = n(), 
            se = sd/sqrt(n))

ggplot(slug_plot, aes(x = treatment, y = mean, fill = treatment))+
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7)+
  geom_errorbar(aes(x = treatment,ymin = mean - se, ymax = mean + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1)+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+
  labs( x = 'Treatment termination',
        y = 'Average slug counts / trap', 
        title = "Soybean: Average Slug Counts / Trap x Treatment",
        subtitle = " Years: 2022-2023")+
#         caption = "DPP: Days pre plant
# DAP : Days after plant")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))


fall_plot <- fall_slugs %>% 
  mutate(group = case_when(
    year == '2022' & treatment %in% c('4','3') ~ 'a', 
    year == '2022' & treatment == '2' ~ 'ab',
    year == '2022' & treatment == '1' ~ 'b', 
    year == '2023' ~ 'a'
  ))

ggplot(fall_plot, aes(x = treatment, y = total_slug, fill = treatment))+
  geom_boxplot(alpha = 0.7)+
  geom_point(size = 1.5)+
  facet_wrap(~year)+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+
  labs( x = 'Treatment termination',
        y = 'Average slug counts / trap', 
        title = "Soy: Fall Average Slug Counts / Trap x Treatment",
        subtitle = " Years: 2022-2023")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 26),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  geom_text(aes(label = group, y = 16), size = 10)



spring_plot <- spring_slugs %>% 
  mutate(year_lab = case_when(
    year == '2022' ~ '2022 a',
    year == '2023' ~ '2023 b'
  ))

ggplot(spring_plot, aes(x = treatment, y = total_slug, fill = treatment))+
  geom_boxplot(alpha = 0.7)+
  geom_point(size = 1.5)+
  facet_wrap(~year_lab)+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+
  labs( x = 'Treatment termination',
        y = 'Average slug counts / trap', 
        title = "Soy: Spring Average Slug Counts / Trap x Treatment",
        subtitle = " Years: 2022-2023")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 26),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))









# add sig values in ppt: confusing with two factor facets
# slugs$szn <- factor(slugs$season, levels = c("Spring", "Fall"))
# ggplot(slugs, aes(x = as.character(treatment), y = total_slug, fill = treatment))+
#   geom_boxplot(alpha = 0.7)+
#   facet_wrap(~year + szn, scales = "free_y")+
#   scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
#   scale_x_discrete(limits = c("1", "2", "4", "3"),
#                    labels=c("No CC", "Early", "Late", "Green"))+
#   labs( x = 'Treatment termination',
#         y = 'Total Slug Counts', 
#         title = "Soybean: Total Slugs by Treatment",
#         subtitle = " Years: 2022-2023")+
#   theme(legend.position = "none",
#         axis.text.x = element_text(size=18),
#         axis.text.y = element_text(size = 18),
#         strip.text = element_text(size = 16),
#         axis.title = element_text(size = 20),
#         plot.title = element_text(size = 20),
#         plot.subtitle = element_text(size = 16), 
#         panel.grid.major.y = element_line(color = "darkgrey"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank())



# pub plots ####

ggplot(slug_plot, aes(x = treatment, y = mean))+
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7)+
  geom_errorbar(aes(x = treatment,ymin = mean - se, ymax = mean + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  labs( x = 'Treatment',
        y = 'Total Slug Counts', 
        title = "Soybean: Average Slug Counts / Trap x Treatment",
        subtitle = " Years: 2022-2023",
        caption = "DPP: Days pre plant
DAP : Days after plant")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))























