# Jared Adam
# 1/25/2024
# Plain script
# going to work through as much bean stuff as I can
# this script will be a bit unorganized 


# packages ####
library(tidyverse)
library(MASS)
library(performance)
library(lme4)
library(emmeans)

# data ####

# bean scripts 
one <- read.csv("https://raw.githubusercontent.com/Jared-Adam-PSU/535/main/SentintelPrey.ALLtimes.2022.csv")
two <- read.csv("https://raw.githubusercontent.com/Jared-Adam-PSU/535/main/SentPreyandPF.Beans.2022.csv")
three <- read.csv("https://raw.githubusercontent.com/Jared-Adam-PSU/535/main/Pitfalls.ALLtimings.2022.csv")

sent_beans <- one

pf <- two

pf_raw <- three

