# Jared Adam
# first date unknown
#revisit : 2/14/2024

# slugs baby lettuce goooo 

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

# data ####
slugs <- PSA_PA_slugs

# test ####
  
colnames(slugs)
test_slug <- slugs[1:200,]
test_slug$month <- gsub(" ", "", test_slug$month)
test_slug %>% 
  dplyr::select(-location, -shingle_id, -time, -temp, -row) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  rename(precip = "7day_precip") %>% 
  dplyr::select(-date) %>% 
  group_by(season, year, month, plot_id, treatment, block) %>% 
  summarise(total_slug =  sum(slug_count)) %>%  
  print(n = Inf)

# # precip 
# test_slug %>% 
#   dplyr::select(-location, -shingle_id, -time, -temp, -row) %>% 
#   mutate(date = as.Date(date, "%m/%d/%Y"),
#          year = format(date, '%Y')) %>% 
#   rename(precip = "7day_precip") %>% 
#   group_by(season, year, month, block) %>% 
#   summarise(total_precip = sum(precip))



# whole data set ####

slug_clean <- slugs %>% 
  dplyr::select(-location, -shingle_id, -time, -row) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  rename(precip = "7day_precip") %>% 
  # dplyr::select(date) %>% 
  mutate(year = as.factor(year), 
       treatment = as.factor(treatment))%>%
  mutate(season = case_when(season == "fall" ~ "Fall", 
                            season == "spring" ~ "Spring")) %>% 
  group_by(season, year, month, plot_id, treatment, block, precip, temp, date) %>% 
  summarise(total_slug =  sum(slug_count))%>% 
  replace(is.na(.),0) %>% 
  arrange(date, plot_id) %>% 
  print(n = Inf)

#subset by season


# cs_21 <- subset(slug_clean, year == "2021")
# cs_22 <- subset(slug_clean, year == "2022")
# cs_23 <- subset(slug_clean, year == "2023")
# explore the data ####

#explore
unique(slug_clean$date)
slug_clean <- slug_clean %>% 
  mutate_at(vars(1:6), as_factor) %>% 
  mutate(date = as.character(date)) %>% 
  mutate(date = as_factor(date))

cat_resp <- names(slug_clean[10])
cat_resp <- set_names(cat_resp)

cat_exp <- names(slug_clean[1:6])
cat_exp <- set_names(cat_exp)


box_plots <- function(x,y) {
  ggplot(slug_clean, aes(x = .data[[x]], y = .data[[y]]))+
    geom_boxplot()+
    theme_bw()
}

cat_plots <- map(cat_exp, ~box_plots(.x, 'total_slug'))
cat_plots
ggarrange(plotlist = cat_plots)



con_resp <- names(slug_clean[10])
con_resp <- set_names(con_resp)

con_exp <- names(slug_clean[7:8])
con_exp <- set_names(con_exp)

scatter_plots <- function(x,y){
  ggplot(slug_clean, aes(x = .data[[x]], y = .data[[y]]))+
    geom_point()+
    stat_smooth(method = 'loess', se = FALSE, color = 'grey75')+
    theme_bw()
}

con_plots <- map(con_exp, ~scatter_plots(.x, 'total_slug'))
ggarrange(plotlist = con_plots)


ggplot(slug_clean, aes(x = season, y = precip))+
  geom_boxplot()+
  theme_bw()
ggplot(slug_clean, aes(x = season, y = temp))+
  geom_boxplot()+
  theme_bw()


# correlation test 
slug_cor <- slug_clean %>% 
  ungroup() %>% 
  dplyr::select(total_slug, block, plot_id, season, treatment, year, date)

model.matrix(~0+., data = slug_cor) %>% 
  cor(use = 'pairwise.complete.obs') %>% 
  ggcorrplot(show.diag = FALSE, type = 'lower', lab = TRUE, lab_size = 2)

# model choice? ####

# look at overdispersion: variance > mean?
dispersion_stats <- slug_clean %>% 
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
# SPRING
spring_slugs <- subset(slug_clean, season == "Spring") %>% 
  mutate_at(vars(1:6), as.factor)
spoisson_model <- glmer(total_slug ~ treatment*year + 
                          (1|block), 
                        data = spring_slugs, 
                        family = poisson)

snb_model_trt <- glmer.nb(total_slug ~ treatment*year + 
                            (1|block), 
                          data = spring_slugs) 

gaus_model <- lmer(total_slug ~ treatment*year + 
                      (1|block), 
                    data = spring_slugs)

lrtest(spoisson_model, snb_model_trt, gaus_model)
# the negative binomial has the higher likelihood score, so we will use that

# FALL
fall_slugs <- subset(slug_clean, season == "Fall")%>% 
  mutate_at(vars(1:6), as.factor)

# let's see which is better, poisson or nb? 
# run one of each where the only difference is the family 
fpoisson_model <- glmer(total_slug ~ treatment*year + 
                         (1|block), 
                       data = fall_slugs, 
                       family = poisson)

fnb_model_trt <- glmer.nb(total_slug ~ treatment*year + 
                           (1|block), 
                         data = fall_slugs) 

lrtest(fpoisson_model, fnb_model_trt)
# the negative binomial has the higher likelihood score, so we will use that



# models ####

# Fall

f0 <- glmer.nb(total_slug ~ +
                 (1|block/plot_id),
               data = fall_slugs)

f1 <- glmer.nb(total_slug ~ treatment+
                 (1|block/plot_id), data = fall_slugs) 

f2 <- glmer.nb(total_slug ~ treatment+year +
                 (1|block/plot_id), data = fall_slugs) 

f3 <- glmer.nb(total_slug ~ treatment*year +
                 (1|block/plot_id), data = fall_slugs) 

# p3 <- glmer.nb(total_slug ~ precip +
#                  (1|year/block), data = fall_slugs) 
anova(f3, p3)

rePCA(f3)

anova(f0, f1, f2, f3)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# f0    4 1997.6 2012.9 -994.79   1989.6                          
# f1    7 1984.1 2010.9 -985.06   1970.1  19.452  3  0.0002204 ***
# f2    9 1833.4 1867.8 -907.69   1815.4 154.733  2  < 2.2e-16 ***
# f3   15 1826.8 1884.2 -898.38   1796.8  18.617  6  0.0048620 ** 

# ?waldtest
# waldtest(f1, test = 'F')

check_model(f3)
summary(f3)
hist(residuals(f3))
res <- residuals(f3)
qqnorm(res)
plot(fitted(f3), res)
check_singularity(f3)
r2_nakagawa(f3) 

cld(emmeans(f3, ~treatment), Letters = letters)
# treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 3           1.12 0.208 Inf     0.718      1.53  a    
# 4           1.27 0.206 Inf     0.862      1.67  a    
# 2           1.31 0.207 Inf     0.901      1.71  a    
# 1           1.64 0.204 Inf     1.237      2.04   b   



# sl.table <- as.data.frame(summary(m1)$coefficients)
# #CI <- confint(m1)
# sl.table <-cbind(row.names(sl.table), sl.table)
# names(sl.table) <- c("Term", "B", "SE", "t", "p")
# nice_table(sl.table, highlight = TRUE)


# Spring

s0 <- glmer.nb(total_slug ~ +
                 (1|block/plot_id),
               data = spring_slugs)

s1 <- glmer.nb(total_slug ~ treatment +
                 (1|block/plot_id), data = spring_slugs) 

s2 <- glmer.nb(total_slug ~ treatment + year +
                 (1|block/plot_id), data = spring_slugs) 

s3 <- glmer.nb(total_slug ~ treatment*year +
                 (1|block/plot_id), data = spring_slugs)
rePCA(s3)
isSingular(s3)

anova(s0, s1, s2, s3)
# npar    AIC    BIC  logLik deviance    Chisq Df Pr(>Chisq)    
# s0    3 1609.7 1621.2 -801.85   1603.7                           
# s1    6 1611.3 1634.2 -799.64   1599.3   4.4230  3     0.2193    
# s2    8 1510.3 1540.9 -747.14   1494.3 104.9994  2     <2e-16 ***
# s3   14 1518.6 1572.2 -745.29   1490.6   3.6926  6     0.7182    

check_model(s3)
summary(s3)
hist(residuals(s3))
check_singularity(s3)
r2_nakagawa(s3) 
cld(emmeans(s3, ~treatment|year),Letters = letters)
# year = 2021:
#   treatment  emmean    SE  df asymp.LCL asymp.UCL .group
# 1          1.1587 0.597 Inf    -0.011    2.3284  a    
# 4          1.4228 0.594 Inf     0.259    2.5862  ab   
# 2          1.5517 0.593 Inf     0.390    2.7136  ab   
# 3          1.8102 0.592 Inf     0.650    2.9704   b   
# 
# year = 2022:
#   treatment  emmean    SE  df asymp.LCL asymp.UCL .group
# 1         -1.9395 0.942 Inf    -3.785   -0.0940  a    
# 2         -1.6942 0.913 Inf    -3.484    0.0952  a    
# 4         -1.2619 0.871 Inf    -2.970    0.4459  a    
# 3         -1.0525 0.860 Inf    -2.738    0.6334  a    
# 
# year = 2023:
#   treatment  emmean    SE  df asymp.LCL asymp.UCL .group
# 3         -0.2238 0.697 Inf    -1.590    1.1420  a    
# 1         -0.1949 0.696 Inf    -1.560    1.1700  a    
# 2         -0.1114 0.696 Inf    -1.475    1.2518  a    
# 4          0.0581 0.694 Inf    -1.302    1.4180  a    

cld(emmeans(s3, ~treatment), Letters = letters)
# treatment  emmean    SE  df asymp.LCL asymp.UCL .group
# 1         -0.3252 0.439 Inf    -1.186     0.536  a    
# 2         -0.0846 0.432 Inf    -0.931     0.762  a    
# 4          0.0730 0.422 Inf    -0.754     0.900  a    
# 3          0.1780 0.419 Inf    -0.644     1.000  a 

cld(emmeans(s3, ~year), Letters = letters)
# year emmean    SE  df asymp.LCL asymp.UCL .group
# 2022 -1.487 0.807 Inf    -3.069    0.0951  a    
# 2023 -0.118 0.675 Inf    -1.441    1.2050  ab   
# 2021  1.486 0.580 Inf     0.349    2.6223   b 


# plots corn slugs ####
# overall

slug_plot <- slug_clean %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(total_slug), 
            sd =  sd(total_slug),
            n= n(), 
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
        title = "Corn: Average Slug Counts / Trap x Treatment",
        subtitle = " Years: 2021-2023")+
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

# fall
fall_plot <- fall_slugs %>% 
  mutate(group = case_when(
    year == '2021' & treatment == '3' ~ 'a',
    year == '2021' & treatment == '4' ~ 'ab', 
    year == '2021' & treatment == '2' ~ 'ab', 
    year == '2021' & treatment == '1' ~ 'b', 
    year == '2022' & treatment == '3' ~ 'a', 
    year == '2022' & treatment == '4' ~ 'a', 
    year == '2022' & treatment == '2' ~ 'b', 
    year == '2022' & treatment == '1' ~ 'b', 
    year == '2023' & treatment == '1' ~ 'a', 
    year == '2023' & treatment == '2' ~ 'a', 
    year == '2023' & treatment == '3' ~ 'a', 
    year == '2023' & treatment == '4' ~ 'a'
  ))

f.labs <- c('2021 a', '2022 b', '2023 b')
names(f.labs) <- c('2021', '2022', '2023')
ggplot(fall_plot, aes(x = treatment, y = total_slug, fill = treatment))+
  geom_boxplot(alpha = 0.7)+
  geom_point(size = 1.5)+
  facet_wrap(~year, labeller = labeller(year = f.labs))+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+
  labs( x = 'Treatment termination',
        y = 'Average slug counts / trap', 
        title = "Corn: Fall Average Slug Counts / Trap x Treatment",
        subtitle = " Years: 2021-2023")+
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
  geom_text(aes(label = group, y = 44), size = 10)


# spring 
spring_plot <- spring_slugs %>% 
  mutate(group = case_when(
    year == '2021' & treatment == '1' ~ 'a',
    year == '2021' & treatment == '4' ~ 'ab',
    year == '2021' & treatment == '2' ~ 'ab',
    year == '2021' & treatment == '3' ~ 'b',
    year == '2022' & treatment == '1' ~ 'a',
    year == '2022' & treatment == '2' ~ 'a',
    year == '2022' & treatment == '3' ~ 'a',
    year == '2022' & treatment == '4' ~ 'a',
    year == '2023' & treatment == '3' ~ 'a',
    year == '2023' & treatment == '1' ~ 'a',
    year == '2023' & treatment == '2' ~ 'a',
    year == '2023' & treatment == '4' ~ 'a'
  ))
# year emmean    SE  df asymp.LCL asymp.UCL .group
# 2022 -1.487 0.807 Inf    -3.069    0.0951  a    
# 2023 -0.118 0.675 Inf    -1.441    1.2050  ab   
# 2021  1.486 0.580 Inf     0.349    2.6223   b 
s.labs <- c('2021 a', '2022 b', '2023 ab')
names(s.labs) <- c('2021', '2022', '2023')
ggplot(spring_plot, aes(x = treatment, y = total_slug, fill = treatment))+
  geom_boxplot(alpha = 0.7)+
  geom_point(size = 1.5)+
  facet_wrap(~year, labeller = labeller(year = s.labs))+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+
  labs( x = 'Treatment termination',
        y = 'Average slug counts / trap', 
        title = "Corn: Spring Average Slug Counts / Trap x Treatment",
        subtitle = " Years: 2021-2023")+
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
  geom_text(aes(label = group, y = 44), size = 10)


# 
# ggplot(slug_clean, aes(x = treatment, y = total_slug, fill = season))+
#   geom_boxplot()+
#   facet_wrap(~year)+
#   scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+
#   labs( x = 'Treatment',
#         y = 'Total Slug Counts', 
#         title = "Total Spring Slugs by Treatment")+
#   theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1),
#         axis.text.y = element_text(size = 12))
# 
# ggplot(fall_slugs, aes(x = treatment, y = total_slug, fill = year))+
#   geom_boxplot()+
#   facet_wrap(.~year)+
#   ggtitle("Total Slugs by Treatment")+
#   scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+
#   ylab("Total slug counts")+
#   xlab("")+
#   theme(axis.text.x = element_text(size=12),
#         axis.text.y = element_text(size = 12))
# 
# # final figure by season and year
# slug_clean$szn <- factor(slug_clean$season, levels = c("Spring", "Fall"))
# ggplot(slug_clean, aes(x = treatment, y = total_slug, fill = treatment))+
#   geom_boxplot(alpha = 0.7)+
#   facet_wrap(year~szn, scales = "free_y", ncol = 2)+
#   scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
#   scale_x_discrete(limits = c("1", "2", "4", "3"),
#                    labels=c("No CC", "Early", "Late", "Green"))+
#   labs( x = 'Treatment termination',
#         y = 'Total Slug Counts', 
#         title = "Corn: Total Slugs by Treatment",
#         subtitle = " Years: 2021-2023")+
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
# 
# 
# 
# 
# 
# ggplotly(test)

# pub plot ####

ggplot(slug_plot, aes(x = treatment, y = mean))+
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7)+
  geom_errorbar(aes(x = treatment,ymin = mean - se, ymax = mean + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  labs( x = 'Treatment',
        y = 'Total Slug Counts', 
        title = "Corn: Average Slug Counts / Trap x Treatment",
        subtitle = " Years: 2021-2023",
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

# slugs and precip ####
precip_slug <- slug_clean %>% 
  group_by(season, year, treatment) %>% 
  summarise(precip_tot = sum(precip),
            slug_tot = sum(total_slug)) 

precip <- kruskal.test(precip_tot ~ year, data = precip_slug)
precip
pairwise.wilcox.test(precip_slug$precip_tot, precip_slug$year)
hist(residuals(precip))

ggplot(precip_slug, aes(x = precip_tot, y = slug_tot))+
  geom_point()+
  facet_wrap(~year + season)

ggplot(precip_slug, aes(x = year, y = precip_tot, fill = year))+
  geom_bar(position = "dodge", stat = "identity")+
  facet_wrap(~season)


