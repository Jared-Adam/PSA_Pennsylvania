# yield and cc with wallace df 
# time to check and see if our data / outputs match

# packages ####
library(tidyverse)
library(lme4)
library(emmeans)
library(lmtest)
library(multcomp)

# data ####
wield <- wallace_yield_cb
colnames(wield)
cield <- wield %>% 
  dplyr::select(Year, YieldBuac, Plot, Block, Crop, CC) %>% 
  mutate(YieldBuac = case_when(YieldBuac == "na" ~ NA, 
                               .default = as.character(YieldBuac))) %>% 
  na.omit() %>% 
  print(n = Inf)
unique(cield$Crop)
unique(cield$Year)
beans <- filter(cield, Crop == "soybean") %>% 
  arrange(Year, Plot, Block) %>% 
  rename(year = Year, 
         yieldbuac = YieldBuac, 
         plot = Plot, 
         block = Block, 
         crop = Crop, 
         cc = CC) %>% 
  mutate(yieldbuac = as.numeric(yieldbuac)) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate(cc = case_when(cc == "14-21 DPP" ~ "14-28 DPP", 
                        .default = as.factor(cc))) %>% 
  mutate_at(vars(3:6),as.factor) %>%
  print(n = Inf)

beans_avg <- beans %>% 
  group_by(year, cc) %>% 
  summarise(mean = mean(yieldbuac), 
            sd = sd(yieldbuac), 
            n = n(), 
            se = sd / sqrt(n))

ggplot(beans_avg, aes(x = cc, y = mean, fill = cc))+
  geom_bar(stat = 'identity', position = 'dodge')+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se))+
  scale_x_discrete(limits = c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  facet_wrap(~year)

corn <- filter(cield, Crop == "corn") %>% 
  arrange(Year, Plot, Block) %>% 
  rename(year = Year, 
         yieldbuac = YieldBuac, 
         plot = Plot, 
         block = Block, 
         crop = Crop,
         cc = CC) %>% 
  mutate(yieldbuac = as.numeric(yieldbuac)) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate(cc = case_when(cc == "14-21 DPP" ~ "14-28 DPP", 
                        .default = as.factor(cc))) %>% 
  mutate_at(vars(3:6),as.factor) %>%
  print(n = Inf)

corn_avg <- corn %>% 
  group_by(year, cc) %>% 
  summarise(mean = mean(yieldbuac), 
            sd = sd(yieldbuac), 
            n = n(), 
            se = sd / sqrt(n))

ggplot(corn_avg, aes(x = cc, y = mean, fill = cc))+
  geom_bar(stat = 'identity', position = 'dodge')+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se))+
  scale_x_discrete(limits = c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  facet_wrap(~year)

#  stats ####

# corn all 
p <- glmer(yieldbuac ~ 
               (1|year/block), 
             family = poisson, 
             data = corn)
nb <- glmer.nb(yieldbuac ~
                  (1|year/block), 
                data = corn)
lrtest(p, nb)

cm0 <- glmer.nb(yieldbuac ~ 
                  (1|year/block),
                data = corn)
cm1 <- glmer.nb(yieldbuac ~ cc +
                  (1|year/block), 
                data = corn)
anova(cm0, cm1)
c_em <- emmeans(cm1, ~cc)
cld(c_em, Letters = letters)

cm <- aov(yieldbuac ~ year, corn)
TukeyHSD(cm)


# 2021
corn_21 <- filter(corn, year == "2021")
cm21 <- glmer.nb(yieldbuac ~ cc +
                  (1|block), 
                data = corn_21)
c21_em <- emmeans(cm21, ~cc)
cld(c21_em, Letters = letters)

# 2022
corn_22 <- filter(corn, year == "2022")
cm22 <- glmer.nb(yieldbuac ~ cc +
                   (1|block), 
                 data = corn_22)
c22_em <- emmeans(cm22, ~cc)
cld(c22_em, Letters = letters)

# 2023
corn_23 <- filter(corn, year == "2023")
cm23 <- glmer.nb(yieldbuac ~ cc +
                   (1|block), 
                 data = corn_23)
c23_em <- emmeans(cm23, ~cc)
cld(c23_em, Letters = letters)


# beans all
bp <- glmer(yieldbuac ~ 
             (1|year/block), 
           family = poisson, 
           data = beans)
bnb <- glmer.nb(yieldbuac ~
                 (1|year/block), 
               data = beans)
lrtest(bp, bnb)

bm0 <- glmer.nb(yieldbuac ~ 
                  (1|year/block),
                data = beans)
bm1 <- glmer.nb(yieldbuac ~ cc +
                  (1|year/block), 
                data = beans)
anova(bm0, bm1)
b_em <- emmeans(bm1, ~cc)
cld(b_em, Letters = letters)

# 2022
bean_22 <- filter(beans, year == "2022")
bm22 <- glmer.nb(yieldbuac ~ cc +
                   (1|block), 
                 data = bean_22)
b22_em <- emmeans(bm22, ~cc)
cld(b22_em, Letters = letters)

# 2023
bean_23 <- filter(beans, year == "2023")
bm23 <- glmer.nb(yieldbuac ~ cc +
                   (1|block), 
                 data = bean_23)
b23_em <- emmeans(bm23, ~cc)
cld(b23_em, Letters = letters)





# plots ####
# corn 
corn_plot <- corn %>% 
  group_by(cc) %>%
  summarise(mean = mean(yieldbuac),
            sd = sd(yieldbuac),
            n = n(), 
            se = sd/sqrt(n))


ggplot(corn_plot, aes(x= cc, y = mean, fill = cc))+
  geom_bar(position = 'dodge' , stat = 'identity', alpha = .7)+
  scale_x_discrete(limits = c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  scale_fill_manual(values = c("#1B9E77","#D95F02","#7570B3","#E7298A"))+
  geom_errorbar( aes(x=cc, ymin=mean-se, ymax=mean+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  ylab(bquote("Mean"(bu / ac ^-1)))+
  labs(x = 'Treatment',
       title = 'Corn: Yield x treatment',
       subtitle = "Years: 2021-2023",
       caption = "DPP: Days pre plant
DAP : Days after plant")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))

bean_plot <- beans %>% 
  group_by(cc) %>%
  summarise(mean = mean(yieldbuac),
            sd = sd(yieldbuac),
            n = n(), 
            se = sd/sqrt(n))


ggplot(bean_plot, aes(x= cc, y = mean, fill = cc))+
  geom_bar(position = 'dodge' , stat = 'identity', alpha = .7)+
  scale_x_discrete(limits = c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  scale_fill_manual(values = c("#1B9E77","#D95F02","#7570B3","#E7298A"))+
  geom_errorbar( aes(x=cc, ymin=mean-se, ymax=mean+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  ylab(bquote("Mean"(bu / ac ^-1)))+
  labs(x = 'Treatment',
       title = 'Soybean: Yield x treatment',
       subtitle = "Years: 2022-2023",
       caption = "DPP: Days pre plant
DAP : Days after plant")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))
