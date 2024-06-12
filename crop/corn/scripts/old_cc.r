# old cc code and data
cc <- PSA_PAcc_biomass
bcc <- beans_cc_biomasss

# corn cover crop data and stats ####
cc_clean <- cc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_biomass_g = as.numeric(cc_biomass_g)) %>% 
  group_by(year, trt) %>% 
  summarise(cc_mean = mean(cc_biomass_g),
            cc_sd = sd(cc_biomass_g),
            cc_se = cc_sd/sqrt(n())) %>%
  arrange(year, factor(trt, c("check", "green", "brown", "gr-br")))

# over all bar
cc_mg_plot <- cc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_biomass_g = as.numeric(cc_biomass_g))%>% 
  group_by(year, trt, plot) %>% 
  summarise(mean_cc = mean(cc_biomass_g)) %>% 
  mutate(mg_ha = mean_cc*0.04) %>% 
  group_by(trt) %>% 
  summarise(mean_mg = mean(mg_ha),
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))

# bar by year 
cc_year_plot <- cc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_biomass_g = as.numeric(cc_biomass_g))%>% 
  group_by(year, trt, plot) %>% 
  summarise(mean_cc = mean(cc_biomass_g)) %>% 
  mutate(mg_ha = mean_cc*0.04) %>% 
  group_by(trt, year) %>% 
  summarise(mean_mg = mean(mg_ha),
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))

# model and boxplot
cc_mg_model <- cc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_biomass_g = as.numeric(cc_biomass_g)) %>% 
  group_by(year, trt, plot,block) %>% 
  summarise(mean_cc = mean(cc_biomass_g)) %>% 
  mutate(mg_ha = mean_cc*0.04) %>% 
  filter(trt != "check") %>% 
  print(n = Inf)

cc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_biomass_g = as.numeric(cc_biomass_g)) %>% 
  mutate(mg_ha = cc_biomass_g*0.04) %>% 
  group_by(year, trt, plot,block) %>% 
  summarise(mean_cc = mean(mg_ha)) %>% 
  filter(trt != "check") %>% 
  print(n = Inf)








cc1 <- lm(mg_ha ~ trt + year,
          data = cc_mg_model)
summary(cc1)
hist(residuals(cc1))
cc_em <- emmeans(cc1, ~trt + year)
pwpm(cc_em)
cld(cc_em, Letters = letters)
# trt   year emmean    SE df lower.CL upper.CL .group  
# brown 2023 -0.563 0.424 40   -1.419    0.293  a      
# brown 2022  1.044 0.424 40    0.188    1.900   b     
# gr-br 2023  1.246 0.424 40    0.390    2.102   b     
# gr-br 2022  2.853 0.424 40    1.997    3.709    c    
# green 2023  3.913 0.424 40    3.057    4.769    cd   
# brown 2021  5.408 0.424 40    4.552    6.264     de  
# green 2022  5.519 0.424 40    4.663    6.375      ef 
# gr-br 2021  7.217 0.424 40    6.361    8.073       f 
# green 2021  9.883 0.424 40    9.027   10.739        g

cc_mg_model %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mg_ha), 
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))

ca <- aov(mg_ha ~ year, data = cc_mg_model)
TukeyHSD(ca)

# $year
# diff       lwr        upr     p adj
# 2022-2021 -4.36412 -6.378883 -2.3493571 0.0000133
# 2023-2021 -5.97080 -7.985563 -3.9560371 0.0000000
# 2023-2022 -1.60668 -3.621443  0.4080829 0.1408407
year_corn <- cc_mg_model %>% 
  group_by(year) %>% 
  summarise(mean = mean(mg_ha), 
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))
names(year_corn) <- c("Year", "Mean", "Sd", "n", "SE")
year_corn_cc <- flextable(year_corn)
theme_zebra(year_corn_cc) %>% 
  save_as_docx(path = 'corn_annual_cc.docx')


# 2021
c21 <- filter(cc_mg_model, year == "2021")
cc21 <- lmer(mg_ha ~ trt +
               (1|block),
             data = c21)
summary(cc21)

cc21_em <- emmeans(cc21, ~trt)
pwpm(cc21_em)
cld(cc21_em, Letters = letters)
# trt   emmean    SE df lower.CL upper.CL .group
# brown   4.08 0.485 12     3.03     5.14  a    
# gr-br   6.81 0.485 12     5.75     7.87   b   
# green  11.61 0.485 12    10.55    12.67    c  

trt_ord <- c("14-28 DPP", "3-7 DPP", "1-3 DAP")
corn_21_mean <- c21 %>% 
  ungroup() %>% 
  mutate(trt = case_when(trt == 'brown' ~ '14-28 DPP',
                         trt == 'green' ~ '1-3 DAP',
                         trt == 'gr-br' ~ '3-7 DPP',
                         .default = as.character(trt))) %>%
  mutate(trt = factor(trt, levels = trt_ord)) %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mg_ha), 
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n)) %>% 
  mutate(Year = c(2021, 2021,2021)) %>% 
  relocate(Year, trt) %>% 
  mutate_at(vars(1:2), as.factor)


# 2022
c22 <- filter(cc_mg_model, year == "2022")
cc22 <- lmer(mg_ha ~ trt +
               (1|block),
             data = c22)
summary(c22)

cc22_em <- emmeans(cc22, ~trt)
pwpm(cc22_em)
cld(cc22_em, Letters = letters)
# trt   emmean    SE   df lower.CL upper.CL .group
# brown   1.17 0.163 11.2    0.807     1.52  a    
# gr-br   2.76 0.163 11.2    2.399     3.12   b   
# green   5.49 0.163 11.2    5.133     5.85    c  

trt_ord <- c("14-28 DPP", "3-7 DPP", "1-3 DAP")
corn_22_mean <- c22 %>%  
  ungroup() %>% 
  mutate(trt = case_when(trt == 'brown' ~ '14-28 DPP',
                         trt == 'green' ~ '1-3 DAP',
                         trt == 'gr-br' ~ '3-7 DPP',
                         .default = as.character(trt))) %>% 
  mutate(trt = factor(trt, levels = trt_ord)) %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mg_ha), 
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))%>% 
  mutate(Year = c(2022, 2022,2022)) %>% 
  relocate(Year, trt) %>% 
  mutate_at(vars(1:2), as.factor)

# 2023
c23 <- filter(cc_mg_model, year == "2023")
cc23 <- lmer(mg_ha ~ trt +
               (1|block),
             data = c23)
summary(cc23)

cc23_em <- emmeans(cc23, ~trt)
pwpm(cc23_em)
cld(cc23_em, Letters = letters)
# trt   emmean    SE   df lower.CL upper.CL .group
# brown  0.639 0.145 11.5    0.322    0.956  a    
# gr-br  1.745 0.145 11.5    1.428    2.062   b   
# green  2.212 0.145 11.5    1.895    2.529   b  

trt_ord <- c("14-28 DPP", "3-7 DPP", "1-3 DAP")
corn_23_mean <- c23 %>%   
  ungroup() %>% 
  mutate(trt = case_when(trt == 'brown' ~ '14-28 DPP',
                         trt == 'green' ~ '1-3 DAP',
                         trt == 'gr-br' ~ '3-7 DPP',
                         .default = as.character(trt))) %>% 
  mutate(trt = factor(trt, levels = trt_ord)) %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mg_ha), 
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))%>% 
  mutate(Year = c(2023, 2023, 2023)) %>% 
  relocate(Year, trt) %>% 
  mutate_at(vars(1:2), as.factor)

corn_cc_table <- rbind(corn_21_mean, corn_22_mean, corn_23_mean)
names(corn_cc_table) <- c("Year", "Treatment", "Mean", "Sd", "n", "SE")

test <- flextable(corn_cc_table)
test <- autofit(test) 
test <- add_header_lines(test,
                         values = 'Corn: Cover crop biomass values')
theme_zebra(test) %>% 
  save_as_docx(path = 'corn.cc.table.docx')

# bean cover crop data and stats ####

bcc
bcc_start <- bcc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  group_by(year, trt) %>% 
  summarise(cc_mean = mean(cc_g),
            cc_sd = sd(cc_g),
            cc_se = cc_sd/sqrt(n())) 

# new_bcc <- rbind(as.data.frame(cc_start), new_checks)
# 
# new_bcc <- as_tibble(new_cc)
# 
# bcc_clean <- new_cc %>% 
#   mutate_at(vars(1:2), as.factor)%>%
#   mutate_at(vars(3:5), as.numeric) %>% 
#   arrange(year, factor(trt, c("check", "gr", "br", "grbr")))

# all data
bcc_mg_plot <- bcc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_g = as.numeric(cc_g))%>% 
  group_by(year, trt, plot) %>% 
  summarise(mean_cc = mean(cc_g)) %>% 
  mutate(mg_ha = mean_cc*0.04) %>% 
  group_by(trt) %>% 
  summarise(mean_mg = mean(mg_ha),
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))

# bar by year 
bcc_year_plot <- bcc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_g = as.numeric(cc_g))%>% 
  group_by(year, trt, plot) %>% 
  summarise(mean_cc = mean(cc_g)) %>% 
  mutate(mg_ha = mean_cc*0.04) %>% 
  group_by(trt, year) %>% 
  summarise(mean_mg = mean(mg_ha),
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))

# model and boxplot
bcc_mg_model <- bcc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_g = as.numeric(cc_g))%>% 
  group_by(year, trt, plot) %>% 
  summarise(mean_cc = mean(cc_g)) %>% 
  mutate(mg_ha = mean_cc*0.04) %>% 
  mutate(block = case_when(plot %in% c("101", '102', '103','104') ~ 1,
                           plot %in% c('201', '202', '203' ,'204') ~ 2, 
                           plot %in% c('301', '302', '303', '304') ~ 3,
                           plot %in% c('401', '402', '403', '404') ~ 4, 
                           plot %in% c('501', '502', '503', '504') ~ 5)) %>%
  mutate(block = as.factor(block)) %>% 
  print( n = Inf)
unique(bcc_mg_model$block)

bc0 <- lmer(mg_ha~
              (1|year/block),
            data = bcc_mg_model)
summary(bc0)

bc1 <- lm(mg_ha ~ trt + year,
          data = bcc_mg_model)
summary(bc1)

cld(emmeans(bc1, ~trt + year), Letters = letters)


# trt  emmean    SE   df lower.CL upper.CL .group
# br    0.634 0.136 7.36    0.316    0.951  a    
# grbr  1.779 0.136 7.36    1.461    2.097   b   
# gr    2.153 0.136 7.36    1.835    2.471   b   

bcc_mg_model %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mg_ha), 
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))
# <fct> <dbl> <dbl> <int>  <dbl>
#   1 br    0.634 0.233    10 0.0737
# 2 gr    2.15  0.561    10 0.178 
# 3 grbr  1.78  0.428    10 0.135

ba <- aov(mg_ha ~ year, data = bcc_mg_model)
TukeyHSD(ba)

# $year
# diff        lwr       upr    p adj
# 2023-2022 0.04613333 -0.5450744 0.6373411 0.874153

bean_year_cc <- bcc_mg_model %>% 
  group_by(year) %>% 
  summarise(mean = mean(mg_ha), 
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))
# year   mean    sd     n    se
# <fct> <dbl> <dbl> <int> <dbl>
#   1 2022   1.50 0.976    15 0.252
# 2 2023   1.54 0.544    15 0.140
names(bean_year_cc) <- c("Year", "Mean", "Sd", "n", "SE")
bean_year_cc <- flextable(bean_year_cc)
theme_zebra(bean_year_cc) %>% 
  save_as_docx(path = 'bean_annual_cc.docx')


# 2022
b22 <- filter(bcc_mg_model, year == "2022")
bcc22 <- lmer(mg_ha ~ trt +
                (1|block),
              data = b22)
summary(bcc22)

bcc22_em <- emmeans(bcc22, ~trt)
pwpm(bcc22_em)
cld(bcc22_em, Letters = letters)
# trt  emmean    SE df lower.CL upper.CL .group
# br    0.419 0.179 12   0.0284    0.809  a    
# grbr  1.522 0.179 12   1.1312    1.912   b   
# gr    2.556 0.179 12   2.1656    2.947    c   

trt_ord <- c("14-28 DPP", "3-7 DPP", "1-3 DAP")
b22_table <- b22 %>% 
  ungroup() %>% 
  mutate(trt = case_when(trt == 'br' ~ '14-28 DPP',
                         trt == 'gr' ~ '1-3 DAP',
                         trt == 'grbr' ~ '3-7 DPP',
                         .default = as.character(trt))) %>% 
  mutate(trt = factor(trt, levels = trt_ord)) %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mg_ha), 
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))   %>% 
  mutate(Year = c(2022, 2022, 2022)) %>% 
  relocate(Year, trt) %>% 
  mutate_at(vars(1:2), as.factor)

# trt    mean     sd     n     se
# <fct> <dbl>  <dbl> <int>  <dbl>
#   1 br    0.419 0.0668     5 0.0299
# 2 gr    2.56  0.531      5 0.237 
# 3 grbr  1.52  0.442      5 0.198 


# 2023
b23 <- filter(bcc_mg_model, year == "2023")
bcc23 <- lmer(mg_ha ~ trt +
                (1|block),
              data = b23)
summary(bcc23)

bcc23_em <- emmeans(bcc23, ~trt)
pwpm(bcc23_em)
cld(bcc23_em, Letters = letters)
# trt  emmean     SE df lower.CL upper.CL .group
# br    0.848 0.0704 12    0.695     1.00  a    
# gr    1.750 0.0704 12    1.597     1.90   b   
# grbr  2.037 0.0704 12    1.883     2.19    c    

b23_table <- b23 %>%
  ungroup() %>% 
  mutate(trt = case_when(trt == 'br' ~ '14-28 DPP',
                         trt == 'gr' ~ '1-3 DAP',
                         trt == 'grbr' ~ '3-7 DPP',
                         .default = as.character(trt))) %>% 
  mutate(trt = factor(trt, levels = trt_ord)) %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mg_ha), 
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))   %>% 
  mutate(Year = c(2023, 2023, 2023)) %>% 
  relocate(Year, trt) %>% 
  mutate_at(vars(1:2), as.factor)

b_cc_table <- rbind(b22_table, b23_table)

names(b_cc_table) <- c("Year", "Treatment", "Mean", "Sd", "n", "SE")
b_cc_table <- flextable(b_cc_table)
b_cc_table <- autofit(b_cc_table) 
b_cc_table <- add_header_lines(b_cc_table,
                               values = 'Soybean: Cover crop biomass values')
theme_zebra(b_cc_table) %>% 
  save_as_docx(path = 'bean.cc.table.docx')




# corn - soy: cover crops ####
# 21 corn - 22 soybeans
cc_mg_model # corn
bcc_mg_model # soybeans

ccc_21 <- cc_mg_model %>% 
  filter(year == "2021") %>% 
  arrange(plot) %>% 
  mutate(crop = 'corn')

bcc_22 <- bcc_mg_model %>% 
  filter(year == '2022') %>% 
  arrange(plot) %>% 
  mutate(crop = 'beans')

cs2122 <- rbind(ccc_21, bcc_22) %>%
  mutate( trt = case_when(trt == 'gr' ~ 'green',
                          trt == 'br' ~ 'brown',
                          trt == 'grbr' ~ 'gr-br',
                          .default = as.factor(trt))) %>% 
  mutate(crop = as.factor(crop)) %>% 
  print(n = Inf)

clm1 <- glmer(mg_ha ~ trt +
                (1|crop), data = cs2122)
hist(residuals(clm1))
cclm_em <- emmeans(clm1, ~ trt)
cld(cclm_em, Letters = letters)
# all differ 
check_model(clm1)
r2_nakagawa(clm1)

# corn cover crop plots ####

cc_clean
ggplot(filter(cc_clean, trt != "check"), aes(x = trt, y = cc_mean, fill = trt))+
  facet_wrap(~year)+
  scale_x_discrete(labels = c("14-21 DPP", "3-7 DPP", "1-3 DPP"))+
  scale_fill_manual(values = c("#D95F02",  "#7570B3","#1B9E77"))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.75)+
  geom_errorbar( aes(x=trt, ymin=cc_mean-cc_se, ymax=cc_mean+cc_se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  labs(title = "Corn: Average cover crop biomass by treatment",
       subtitle = "Years: 2021-2023",
       x = "Treatment",
       y = "Mean cover crop (g/m2)")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#over all bar
ggplot(filter(cc_mg_plot, trt != "check"), aes(x = trt, y = mean_mg, fill = trt))+
  scale_x_discrete(labels = c("14-21 DPP", "3-7 DPP", "1-3 DAP"))+
  scale_fill_manual(values = c("#D95F02",  "#7570B3","#1B9E77"))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7)+
  geom_errorbar( aes(x=trt, ymin=mean_mg-se, ymax=mean_mg+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  labs(title = "Corn: Mean Cover Crop Biomass x Treatment",
       subtitle = "Years: 2021-2023",
       x = "Treatment",
       caption = "DPP: Days pre plant
DAP: Days after plant")+
  ylab(bquote("Mean cover crop" (Mg / ha ^-1)))+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))+
  annotate("text", x = 1, y = 2.8, label = "a", size = 10)+
  annotate("text", x = 2, y = 4.9, label = "b", size = 10)+
  annotate("text", x = 3, y = 7.9, label = "c", size = 10)

# bar by year 
ggplot(filter(cc_year_plot, trt != "check"), aes(x = trt, y = mean_mg, fill = trt))+
  scale_x_discrete(labels = c("14-21 DPP", "3-7 DPP", "1-3 DAP"))+
  scale_fill_manual(values = c("#D95F02",  "#7570B3","#1B9E77"))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7)+
  geom_errorbar( aes(x=trt, ymin=mean_mg-se, ymax=mean_mg+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  facet_wrap(~year)+
  labs(title = "Corn: Cover Crop Biomass x Treatment",
       subtitle = "Years: 2021-2023",
       x = "Treatment",
       caption = "DPP: Days pre plant
DAP: Days after plant")+
  ylab(bquote("Biomass" (Mg / ha ^-1)))+
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
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))

# overall box
ggplot(filter(cc_mg_model, trt != "check"), aes(x = trt, y = mg_ha, fill = trt))+
  scale_x_discrete(labels = c("Early", "Late", "Green"))+
  scale_fill_manual(values = c("#D95F02",  "#7570B3","#1B9E77"))+
  geom_boxplot(alpha = 0.7)+
  geom_point(size = 2)+
  labs(title = "Corn: Cover Crop Biomass x Treatment",
       subtitle = "Years: 2021-2023",
       x = "Treatment termination")+
  #        caption = "DPP: Days pre plant
  # DAP: Days after plant")+
  ylab(bquote("Biomass" (Mg / ha ^-1)))+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))+
  annotate("text", x = 1, y = 14, label = "a", size = 10)+
  annotate("text", x = 2, y = 14, label = "b", size = 10)+
  annotate("text", x = 3, y = 14, label = "c", size = 10)





###

# bean cover crop plots ####

#over all bar
ggplot(filter(bcc_mg_plot, trt != "check"), aes(x = trt, y = mean_mg, fill = trt))+
  scale_x_discrete(limits = c('br', 'grbr', 'gr'),
                   labels = c("14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  scale_fill_manual(values = c("#D95F02","#1B9E77" , "#7570B3"))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7)+
  geom_errorbar( aes(x=trt, ymin=mean_mg-se, ymax=mean_mg+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  labs(title = "Soybean: Mean Cover Crop Biomass x Treatment",
       subtitle = "Years: 2022-2023",
       x = "Treatment",
       caption = "DPP: Days pre plant
DAP: Days after plant")+
  ylab(bquote("Mean cover crop" (Mg / ha ^-1)))+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))+
  annotate("text", x = 1, y = 1.5, label = "a", size = 10)+
  annotate("text", x = 2, y = 2.2, label = "b", size = 10)+
  annotate("text", x = 3, y = 3, label = "c", size = 10)

# bar by year 
ggplot(filter(bcc_year_plot, trt != "check"), aes(x = trt, y = mean_mg, fill = trt))+
  scale_x_discrete(limits = c('br', 'grbr', 'gr'),
                   labels = c("14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  scale_fill_manual(values = c("#D95F02","#1B9E77", "#7570B3"))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7)+
  geom_errorbar( aes(x=trt, ymin=mean_mg-se, ymax=mean_mg+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  facet_wrap(~year)+
  labs(title = "Soybean: Cover Crop Biomass x Treatment",
       # subtitle = "Years: 2022-2023",
       x = "Treatment",
       caption = "DPP: Days pre plant
DAP: Days after plant")+
  ylab(bquote("Mean" (Mg / ha ^-1)))+
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
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))

# overall box
ggplot(filter(bcc_mg_model, trt != "check"), aes(x = trt, y = mg_ha, fill = trt))+
  scale_x_discrete(limits = c("br", "grbr", "gr"),
                   labels = c("Early", "Late", "Green"))+
  scale_fill_manual(values = c("#D95F02", "#1B9E77", "#7570B3"))+
  geom_boxplot(alpha = 0.7)+
  geom_point(size = 2)+
  labs(title = "Soybean: Cover Crop Biomass x Treatment",
       subtitle = "Years: 2022-2023",
       x = "Treatment termination")+
  #        caption = "DPP: Days pre plant
  # DAP: Days after plant")+
  ylab(bquote("Biomass" (Mg / ha ^-1)))+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))+
  annotate("text", x = 1, y = 3.5, label = "a", size = 10)+
  annotate("text", x = 2, y = 3.5, label = "b", size = 10)+
  annotate("text", x = 3, y = 3.5, label = "b", size = 10)


# Conditional R2: 0.913
# Marginal R2: 0.170

# sum stats 
cs2122 %>% 
  group_by(trt) %>% 
  summarise(
    mean = mean(mg_ha),
    sd = sd(mg_ha),
    n = n(), 
    se = sd/sqrt(n)
  )
# trt    mean    sd     n    se
# 1 brown  2.25  2.01    10 0.634
# 2 gr-br  4.17  2.89    10 0.915
# 3 green  7.08  4.87    10 1.54 



# 22 corn - 23 soybeans
ccc_22 <- cc_mg_model %>% 
  filter(year == '2022') %>% 
  arrange(plot) %>% 
  mutate(crop = 'corn')

bcc_23 <- cc_mg_model %>% 
  filter(year == '2023') %>% 
  arrange(plot) %>% 
  mutate(crop = 'beans')

cs2223 <- rbind(ccc_22, bcc_23) %>% 
  mutate( trt = case_when(trt == 'gr' ~ 'green',
                          trt == 'br' ~ 'brown',
                          trt == 'grbr' ~ 'gr-br',
                          .default = as.factor(trt))) %>% 
  mutate(crop = as.factor(crop)) %>% 
  print(n = Inf)

#sum stats 
cs2223 %>% 
  group_by(trt) %>% 
  summarise(
    mean = mean(mg_ha),
    sd = sd(mg_ha),
    n = n(), 
    se = sd/sqrt(n)
  )
# trt    mean    sd     n     se
# 1 brown 0.902 0.307    10 0.0970
# 2 gr-br 2.25  0.612    10 0.193 
# 3 green 3.85  1.79     10 0.566 

clm2 <- glmer(mg_ha ~ trt +
                (1|crop),
              data = cs2223)
hist(residuals(clm2))
cclm2_em <- emmeans(clm2, ~trt)
cld(cclm2_em, Letters = letters)
# all differ 

check_model(clm2)
r2_nakagawa(clm2)
# Conditional R2: 0.840
# Marginal R2: 0.458

# corn - soy: cover crop plots ####
ggplot(cs2122, aes(x = trt, y = mg_ha, fill = trt))+
  geom_boxplot()+
  annotate('text', x=1, y = 5, label = 'a', size = 10)+
  annotate('text', x=2, y = 5, label = 'b', size = 10)+
  annotate('text', x=3, y = 5, label = 'c', size = 10)

ggplot(cs2223, aes(trt, mg_ha, fill = trt))+
  geom_boxplot()+
  annotate('text', x=1, y = 5, label = 'a', size = 10)+
  annotate('text', x=2, y = 5, label = 'b', size = 10)+
  annotate('text', x=3, y = 5, label = 'c', size = 10)

# yield ####
# corn all 

g <- lmer(yieldbuac ~ cc +
            (1|year/block), 
          data = corn)
hist(residuals(g))
g_em <- emmeans(g, ~cc)
cld(c_em, Letters = letters)

cm0 <- lmer(yieldbuac ~ 
              (1|year/block),
            data = corn)
cm1 <- lmer(yieldbuac ~ cc +
              (1|year/block), 
            data = corn)
hist(residuals(cm1))
anova(cm0, cm1)
c_em <- emmeans(cm1, ~cc)
cld(c_em, Letters = letters)

cm <- aov(yieldbuac ~ year, corn)
TukeyHSD(cm)
hist(residuals(cm))


# 2021
corn_21 <- filter(corn, year == "2021")
cm21 <- lmer(yieldbuac ~ cc +
               (1|block), 
             data = corn_21)
c21_em <- emmeans(cm21, ~cc)
cld(c21_em, Letters = letters)
corn_21 %>% 
  summarise(mean = mean(yieldbuac), 
            sd = sd(yieldbuac), 
            n = n(), 
            se = sd / sqrt(n))

# 2022
corn_22 <- filter(corn, year == "2022")
cm22 <- lmer(yieldbuac ~ cc +
               (1|block), 
             data = corn_22)
c22_em <- emmeans(cm22, ~cc)
cld(c22_em, Letters = letters)
corn_22 %>% 
  summarise(mean = mean(yieldbuac), 
            sd = sd(yieldbuac), 
            n = n(), 
            se = sd / sqrt(n))


# 2023
corn_23 <- filter(corn, year == "2023")
cm23 <- lmer(yieldbuac ~ cc +
               (1|block), 
             data = corn_23)
c23_em <- emmeans(cm23, ~cc)
cld(c23_em, Letters = letters)

cavg_paper <- corn_23 %>%
  group_by(cc) %>% 
  summarise(mean = mean(yieldbuac), 
            sd = sd(yieldbuac), 
            n = n(), 
            se = sd / sqrt(n))

names(cavg_paper) <- c("Treatment","Mean","Sd", "n", "SE")
cavg23_table <- flextable(cavg_paper)
cavg23_table <- autofit(cavg23_table)
theme_zebra(cavg23_table) %>% 
  save_as_docx(path = 'cavg23_table.docx')

# beans all 

bg <- lmer(yieldbuac ~ cc +
             (1|year/block), 
           data = beans)
hist(residuals(bg))
bg_em <- emmeans(bg, ~cc)
cld(bg_em, Letters = letters)

bm0 <- lmer(yieldbuac ~ 
              (1|year/block),
            data = beans)
bm1 <- lmer(yieldbuac ~ cc +
              (1|year/block), 
            data = beans)
anova(bm0, bm1)
b_em <- emmeans(bm1, ~cc)
cld(b_em, Letters = letters)

ba <- aov(yieldbuac ~ year, data = beans)
hist(residuals(ba))
TukeyHSD(ba)

# $year
# diff      lwr      upr     p adj
# 2023-2022 8.05 1.133493 14.96651 0.0237249

# 2022
bean_22 <- filter(beans, year == "2022")
bm22 <- lmer(yieldbuac ~ cc +
               (1|block), 
             data = bean_22)
b22_em <- emmeans(bm22, ~cc)
cld(b22_em, Letters = letters)

bean_22 %>%
  summarise(mean = mean(yieldbuac), 
            sd = sd(yieldbuac), 
            n = n(), 
            se = sd / sqrt(n))

# 2023
bean_23 <- filter(beans, year == "2023")
bm23 <- lmer(yieldbuac ~ cc +
               (1|block), 
             data = bean_23)
b23_em <- emmeans(bm23, ~cc)
cld(b23_em, Letters = letters)

bean_23 %>%
  summarise(mean = mean(yieldbuac), 
            sd = sd(yieldbuac), 
            n = n(), 
            se = sd / sqrt(n))



