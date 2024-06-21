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

# model selection (as seen in corn thesis models) ####
fall_slugs <- subset(slugs, season == "Fall") %>% 
  mutate(plot = as.factor(plot))
fs <- fall_slugs %>% 
  group_by(year, treatment, block) %>% 
  summarise(average = mean(total_slug))

poisson_model <- glmer(average ~ treatment*year + 
                         (1|block), 
                       data = fs, 
                       family = poisson)

nb_model_trt <- glmer.nb(average ~ treatment*year + 
                           (1|block),
                         data = fs) 

gaus_model_trt <- lmer(average ~ treatment*year + 
                         (1|block), 
                         data = fs) 


anova(poisson_model, nb_model_trt, gaus_model_trt)

                  
# fall
f0 <- glmer.nb(average ~ 
                 (1|block), 
               data = fs) 
f1 <- glmer.nb(average ~ treatment + 
                 (1|block), 
               data = fs) 
f2 <- glmer.nb(average ~ treatment+year + 
                 (1|block), 
               data = fs) 
f3 <- glmer.nb(average ~ treatment*year + 
                 (1|block), 
               data = fs) 

isSingular(f3)
anova(f0, f1, f2, f3)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# f0    3 113.17 118.23 -53.583  107.166                       
# f1    6 111.76 121.89 -49.879   99.758 7.4081  3    0.05997 .
# f2    7 107.73 119.56 -46.866   93.733 6.0249  1    0.01411 *
# f3   10 113.69 130.58 -46.846   93.691 0.0418  3    0.99776  

summary(f3)
hist(residuals(f3))
res <- residuals(f3)
qqnorm(res)
plot(fitted(f3), res)
r2_nakagawa(f3)
binned_residuals(f3)
check_model(f3)

cld(emmeans(f3, ~year), Letters = letters)
# year emmean    SE  df asymp.LCL asymp.UCL .group
# 2023 -0.230 0.258 Inf    -0.735     0.275  a
# 2022  0.469 0.182 Inf     0.113     0.825   b

cld(emmeans(f3, ~treatment|year), Letters = letters)
# year = 2022:
#   treatment    emmean    SE  df asymp.LCL asymp.UCL .group
# 4         -4.50e-06 0.445 Inf   -0.8720     0.872  a    
# 3          2.73e-01 0.379 Inf   -0.4702     1.017  a    
# 2          5.72e-01 0.330 Inf   -0.0757     1.219  a    
# 1          1.03e+00 0.256 Inf    0.5270     1.532  a    
# 
# year = 2023:
#   treatment    emmean    SE  df asymp.LCL asymp.UCL .group
# 4         -6.29e-01 0.614 Inf   -1.8317     0.575  a    
# 3         -5.11e-01 0.566 Inf   -1.6211     0.599  a    
# 2         -6.90e-02 0.457 Inf   -0.9643     0.826  a    
# 1          2.88e-01 0.379 Inf   -0.4548     1.030  a 

##

# spring
spring_slugs <- subset(slugs, season == "Spring")%>% 
  mutate(plot = as.factor(plot))

ss <- spring_slugs %>% 
  group_by(year, treatment, block) %>% 
  summarise(average = mean(total_slug))

poisson_model <- glmer(average ~ treatment*year + 
                         (1|block), 
                       data = ss, 
                       family = poisson)

nb_model_trt <- glmer.nb(average ~ treatment*year + 
                           (1|block),
                         data = ss) 

gaus_model_trt <- lmer(average ~ treatment*year + 
                         (1|block), 
                       data = ss) 


anova(poisson_model, nb_model_trt, gaus_model_trt)


# spring
s0 <- glmer.nb(average ~ 
                 (1|block), 
               data = ss) 
s1 <- glmer.nb(average ~ treatment + 
                 (1|block), 
               data = ss) 
s2 <- glmer.nb(average ~ treatment+year + 
                 (1|block), 
               data = ss) 
s3 <- glmer.nb(average ~ treatment*year + 
                 (1|block), 
               data = ss) 


isSingular(s3)
anova(s0, s1, s2, s3)
# npar    AIC    BIC   logLik deviance    Chisq Df Pr(>Chisq)    
# s0    3 287.80 292.86 -140.899  281.798                           
# s1    6 292.60 302.73 -140.300  280.600   1.1982  3     0.7534    
# s2    7 113.59 125.42  -49.797   99.594 181.0055  1     <2e-16 ***
# s3   10 119.50 136.39  -49.751   99.502   0.0922  3     0.9928 


summary(s3)
hist(residuals(s3))
res <- residuals(s3)
qqnorm(res)
plot(fitted(s3), res)
r2_nakagawa(s3)
binned_residuals(s3)
check_model(s3)
cld(emmeans(s3, ~treatment|year), Letters = letters)
# year = 2022:
#   treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 2          -3.04 1.683 Inf     -6.34    0.2593  a    
# 3          -3.04 1.558 Inf     -6.09    0.0142  a    
# 1          -3.04 1.190 Inf     -5.37   -0.7079  a    
# 4          -2.35 1.253 Inf     -4.80    0.1090  a    
# 
# year = 2023:
#   treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 3           1.75 0.226 Inf      1.30    2.1915  a    
# 2           1.92 0.214 Inf      1.50    2.3408  a    
# 1           1.93 0.214 Inf      1.51    2.3441  a    
# 4           2.01 0.209 Inf      1.60    2.4154  a    
# 
# 

cld(emmeans(s3, ~year), Letters = letters)
# year emmean    SE  df asymp.LCL asymp.UCL .group
# 2022  -2.87 0.884 Inf     -4.60     -1.13  a    
# 2023   1.90 0.159 Inf      1.59      2.21   b  


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


fall_labs <- c('2022 a', '2023 b')
names(fall_labs) <- c('2022', '2023')
ggplot(fall_plot, aes(x = treatment, y = total_slug, fill = treatment))+
  geom_boxplot(alpha = 0.7)+
  geom_point(size = 1.5)+
  facet_wrap(~year, labeller = labeller(year = fall_labs))+
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
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))


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























