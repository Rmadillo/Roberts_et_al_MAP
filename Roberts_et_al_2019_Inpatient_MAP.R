############################################################################## #
#
# Code to reproduce analysis in Roberts et al. in review. 
# Age-based norms of measured mean arterial pressure in hospitalized 
# pediatric patients
# 
# Code by: Andy Cooper, Dwight Barry, Steve Smith, Amber Yun, Leeann Choi
# Date: 19 May 2019
# Version: 1.0
#
# URL: https://github.com/Rmadillo/Roberts_et_al_MAP
# Contact: dwight [dot] barry {at} seattlechildrens [dot] org
# PRs, suggestions, improvements, and corrections welcome!
#
# License: This code is released under GNU GENERAL PUBLIC LICENSE Version 3
#
# This code is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# It is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this code. If not, see <https://www.gnu.org/licenses/>.
#
############################################################################## #



############################################################################## #
# A. SET UP                                                                 ####
############################################################################## #


# Load libraries
library(kableExtra)
library(tidyverse)
library(ggridges)
library(cowplot)



# For export files naming
output_date = as.character(Sys.Date())


# Bootstrap replication runs per age group
reps = 10



############################################################################## #
# B. GET DATA                                                               ####
############################################################################## #


# Acquire data
temp = tempfile()
download.file("https://github.com/Rmadillo/Roberts_et_al_MAP/blob/master/Roberts_et_al_2019_Inpatient_MAP.csv.zip?raw=true", temp, mode = "wb")
unzip(temp, "Roberts_et_al_2019_Inpatient_MAP.csv")
map_query_results = read_csv("Roberts_et_al_2019_Inpatient_MAP.csv")
unlink(temp)


#### __Table 1 ####
# Data to calculate race/ethnicity demographics in Table 1 are not included here
# Calculating patient counts by age group is included in code for Table S3

# Count patients by birth sex
map_query_results %>%
    select(PT_ID, SEX) %>%
    distinct() %>%
    group_by(SEX) %>%
    count()


# Cleaning
map_all = map_query_results %>%
    mutate(AGE_GROUP_ORDERED = ordered(AGE_GROUP, levels = c(
                "37 weeks PMA-30 Days",
                "1-3 Months",
                "3-6 Months",
                "6-12 Months",
                "12-18 Months",
                "18-24 Months",
                "2-3 Years",
                "3-4 Years",
                "4-5 Years",
                "5-6 Years",
                "6-7 Years",
                "7-8 Years",
                "8-9 Years",
                "9-10 Years",
                "10-11 Years",
                "11-12 Years",
                "12-13 Years",
                "13-14 Years",
                "14-15 Years",
                "15-16 Years",
                "16-17 Years",
                "17-18 Years")))


# Remove likely invalid MAP readings
map = map_all %>%
    filter(between(MAP, 12, 200))


# Remove likely invalid SBP & DBP readings and calculate estimated MAP
map_calc = map_all %>%
    filter(between(SBP, 17, 200) & 
           between(DBP, 8, 160)) %>%
    mutate(CALC_MAP = round(((2 * DBP) + SBP) / 3))


# Filter for behavior-based analysis
map_behavior = map %>%
    filter(UNIT_GROUP == "Acute") 


# Calm behavior MAP
map_behavior_calm = map_behavior %>%
    filter(BEHAVIOR_GROUP == "Quiet State") 


# Fussy behavior MAP
map_behavior_fussy = map_behavior %>%
    filter(BEHAVIOR_GROUP == "Disturbed State")


# Female MAP
map_f = map %>%
    filter(SEX == "F")


# Male MAP
map_m = map %>%
    filter(SEX == "M")



############################################################################## #
#### C. ANALYSIS: MAP Distributions                                         ####
############################################################################## #



## IMPORTANT NOTE: no seeds are set, so expect small differences from the 
## paper's results. Repeated runs showed high stability (0-3 mmHg max).



#### _Distributions for Figure 1 ####

age_groups = with(map, unique(AGE_GROUP))
map_output_distribution = list()
counter = 1

for(i in age_groups){ 
    
    for(j in 1:reps){
 
        map_output_distribution[[counter]] = map %>% 
            filter(AGE_GROUP == i) %>% 
            group_by(PT_ID) %>% 
            sample_n(size = 1, replace = FALSE) %>%
            ungroup() %>% 
            mutate(agegroup = i) %>%
            sample_n(size = length(unique(PT_ID)), replace = TRUE) %>% 
            select(agegroup, MAP) %>%
            group_by(agegroup) 
 
        counter = counter + 1
        
    }
}
 

MAP_Output_Distribution = do.call(rbind, map_output_distribution) 


MAP_Output_Distribution = MAP_Output_Distribution %>%
    mutate(AGE_GROUP_ORDERED = ordered(agegroup, levels = c(
                "37 weeks PMA-30 Days",
                "1-3 Months",
                "3-6 Months",
                "6-12 Months",
                "12-18 Months",
                "18-24 Months",
                "2-3 Years",
                "3-4 Years",
                "4-5 Years",
                "5-6 Years",
                "6-7 Years",
                "7-8 Years",
                "8-9 Years",
                "9-10 Years",
                "10-11 Years",
                "11-12 Years",
                "12-13 Years",
                "13-14 Years",
                "14-15 Years",
                "15-16 Years",
                "16-17 Years",
                "17-18 Years")),
           AGE_GROUP_ORDERED_CHR = ordered(
                case_when(as.character(AGE_GROUP_ORDERED) 
                    == '37 weeks PMA-30 Days' ~ '37 weeks PMA-\n30 Days', 
                TRUE ~ as.character(AGE_GROUP_ORDERED)), 
                levels = c(
                        "37 weeks PMA-\n30 Days",
                        "1-3 Months",
                        "3-6 Months",
                        "6-12 Months",
                        "12-18 Months",
                        "18-24 Months",
                        "2-3 Years",
                        "3-4 Years",
                        "4-5 Years",
                        "5-6 Years",
                        "6-7 Years",
                        "7-8 Years",
                        "8-9 Years",
                        "9-10 Years",
                        "10-11 Years",
                        "11-12 Years",
                        "12-13 Years",
                        "13-14 Years",
                        "14-15 Years",
                        "15-16 Years",
                        "16-17 Years",
                        "17-18 Years")))


#### __Figure 1 ####
ggplot(MAP_Output_Distribution, aes(x = MAP, y = AGE_GROUP_ORDERED_CHR, 
            fill = factor(..quantile..))) + 
    stat_density_ridges(rel_min_height = 0.01, 
            scale = 1.5, size = 0.25, bandwidth = 2, 
            geom = "density_ridges_gradient",
            quantiles = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
            calc_ecdf = TRUE, quantile_lines = T) +
    scale_x_continuous(expand = c(0.02, 0.02), limits = c(20, 120), 
            breaks = c(20, 40, 60, 80, 100, 120),
            minor_breaks = c(30, 50, 70, 90, 110)) +
    scale_y_discrete(expand = c(0, 1.5)) +
    scale_fill_manual(values = c("#000050BF", "#2C7BB6BF", "#ABD9E9BF", "#A0A0A0BF", 
                                 "#A0A0A0BF", "#FDAE61BF", "#FF0000BF", "#500000BF")) + 
    labs(y = "", x = "Mean Arterial Pressure (mmHg)") +
    coord_flip() + 
    theme_ridges(center_axis_labels = T, font_size = 10) +
    theme(legend.position = 'none', 
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, 
                                     size = 8, color = "gray20"),
          axis.text.y = element_text(vjust = 0.5, size = 8, color = "gray20"),
          axis.ticks.y.left = element_line(color = "gray20"),
          axis.ticks.x.bottom = element_line(color = "gray20"),
          panel.grid.major = element_line(size = 0.5),
          plot.margin = unit(c(1, 1, 1, 1), "mm")) +
    panel_border(colour = "gray20", size = 0.5, linetype = 1, remove = FALSE)
    
ggsave(paste0("Roberts_et_al_MAP_Figure_1_", output_date,".pdf"), width = 6.5, height = 5)



#### _Distributions for Figure 2 ####


map_output = list()
counter = 1

for(i in age_groups){ 
    
    for(j in 1:reps){
 
        map_output[[counter]] = map %>% 
            filter(AGE_GROUP == i) %>% 
            group_by(PT_ID) %>% 
            sample_n(size = 1, replace = FALSE) %>%
            ungroup() %>% 
            mutate(agegroup = i) %>%
            sample_n(size = length(unique(PT_ID)), replace = TRUE) %>% 
            group_by(agegroup) %>%
            summarize(MAP_Qs = list(enframe(
                quantile(MAP, probs = seq(0.02, 0.98, by = 0.01))))) %>% 
            unnest() %>% 
            as.data.frame() 
 
        counter = counter + 1
        
    }
}
 

MAP_Output = do.call(rbind, map_output) %>% 
    group_by(agegroup, name) %>% 
    summarize(QUANTILES = mean(value)) %>%
    mutate(GROUPING = "Measured MAP")


map_calc_output = list()
counter = 1

for(i in age_groups){ 
    
    for(j in 1:reps){
 
        map_calc_output[[counter]] = map_calc %>% 
            filter(AGE_GROUP == i) %>% 
            group_by(PT_ID) %>% 
            sample_n(size = 1, replace = FALSE) %>%
            ungroup() %>% 
            mutate(agegroup = i) %>%
            sample_n(size = length(unique(PT_ID)), replace = TRUE) %>% 
            group_by(agegroup) %>%
            summarize(MAP_Qs = list(enframe(
                quantile(CALC_MAP, probs = seq(0.02, 0.98, by = 0.01))))) %>% 
            unnest() %>% 
            as.data.frame() 
 
        counter = counter + 1
        
    }
}
 

MAP_CALC_Output = do.call(rbind, map_calc_output) %>% 
    group_by(agegroup, name) %>% 
    summarize(QUANTILES = mean(value)) %>%
    mutate(GROUPING = "Calculated MAP")


# Union measured and calculated MAP
MAP_FINAL = rbind(MAP_Output, MAP_CALC_Output)


MAP_FINAL_MAIN = MAP_FINAL %>%
    filter(name %in% c("5%", "10%", "25%", "50%", "75%", "90%", "95%")) %>%
    mutate(NAMED_Q = ordered(name, levels = c("5%", "10%", "25%", "50%", "75%", "90%", "95%")),
        QUANTILES = round(QUANTILES, 0),
        AGE_GROUP_ORDERED = ordered(agegroup, levels = c(
                "37 weeks PMA-30 Days",
                "1-3 Months",
                "3-6 Months",
                "6-12 Months",
                "12-18 Months",
                "18-24 Months",
                "2-3 Years",
                "3-4 Years",
                "4-5 Years",
                "5-6 Years",
                "6-7 Years",
                "7-8 Years",
                "8-9 Years",
                "9-10 Years",
                "10-11 Years",
                "11-12 Years",
                "12-13 Years",
                "13-14 Years",
                "14-15 Years",
                "15-16 Years",
                "16-17 Years",
                "17-18 Years")),
           AGE_GROUP_ORDERED_CHR = ordered(
                case_when(as.character(AGE_GROUP_ORDERED) 
                    == '37 weeks PMA-30 Days' ~ '37 weeks PMA-\n30 Days', 
                TRUE ~ as.character(AGE_GROUP_ORDERED)), 
                levels = c(
                        "37 weeks PMA-\n30 Days",
                        "1-3 Months",
                        "3-6 Months",
                        "6-12 Months",
                        "12-18 Months",
                        "18-24 Months",
                        "2-3 Years",
                        "3-4 Years",
                        "4-5 Years",
                        "5-6 Years",
                        "6-7 Years",
                        "7-8 Years",
                        "8-9 Years",
                        "9-10 Years",
                        "10-11 Years",
                        "11-12 Years",
                        "12-13 Years",
                        "13-14 Years",
                        "14-15 Years",
                        "15-16 Years",
                        "16-17 Years",
                        "17-18 Years")))


MAP_FINAL_MAIN_Q = MAP_FINAL_MAIN %>%
    filter(NAMED_Q %in% c("5%", "10%", "25%", "50%", "75%", "90%", "95%"))


#### __Figure 2 ####
ggplot(MAP_FINAL_MAIN_Q, aes(AGE_GROUP_ORDERED_CHR, QUANTILES, 
        group = as.factor(NAMED_Q), 
        color = as.factor(NAMED_Q), 
        shape = as.factor(NAMED_Q))) +
    geom_point(data = filter(MAP_FINAL_MAIN_Q, GROUPING == "Measured MAP")) +
    geom_line(data = filter(MAP_FINAL_MAIN_Q, GROUPING == "Measured MAP"), 
        linetype = "solid") + 
    geom_point(data = filter(MAP_FINAL_MAIN_Q, GROUPING == "Calculated MAP")) +
    geom_line(data = filter(MAP_FINAL_MAIN_Q, GROUPING == "Calculated MAP"),
        linetype = "dashed") +
    theme_bw(base_size = 10) +
    scale_shape_manual(name = "Quantile", 
        values = 1:nlevels(as.factor(MAP_FINAL_MAIN_Q$NAMED_Q))) +
    scale_color_manual(values = c("#4575B4", "#74ADD1", "#ABD9E9", "darkgray",
                                  "#FEE090", "#FDAE61", "#F46D43", "#D73027"),
                       name = "Quantile") +
    scale_y_continuous(limits = c(39, 103)) +
    labs(x = "",  
         y = "Mean Arterial Pressure (mmHg)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
         plot.caption = element_text(hjust = 0.5))

ggsave(paste0("Roberts_et_al_MAP_Figure_2_", output_date, ".pdf"), width = 6.5, height = 6)



############################################################################## #
#### D. ANALYSIS: Patterns in behavior at blood pressure reading            ####
############################################################################## #


#### _Distributions for Figure 3 ####

map_behavior_calm_distribution = list()
counter = 1

for(i in age_groups){ 
    
    for(j in 1:reps){
 
        map_behavior_calm_distribution[[counter]] = map_behavior_calm %>% 
            filter(AGE_GROUP_ORDERED == i) %>% 
            group_by(PT_ID) %>% 
            sample_n(size = 1, replace = FALSE) %>%
            ungroup() %>% 
            mutate(agegroup = i) %>%
            sample_n(size = length(unique(PT_ID)), replace = TRUE) %>% 
            select(agegroup, MAP) %>%
            group_by(agegroup) 
 
        counter = counter + 1
        
    }
}


MAP_Behavior_Calm = do.call(rbind, map_behavior_calm_distribution) %>%    
    mutate(BEHAVIOR_GROUP = "Quiet State") 


map_behavior_fussy_distribution = list()

counter = 1

for(i in age_groups){ 
    
    for(j in 1:reps){
 
        map_behavior_fussy_distribution[[counter]] = map_behavior_fussy %>% 
            filter(AGE_GROUP_ORDERED == i) %>% 
            group_by(PT_ID) %>% 
            sample_n(size = 1, replace = FALSE) %>%
            ungroup() %>% 
            mutate(agegroup = i) %>%
            sample_n(size = length(unique(PT_ID)), replace = TRUE) %>% 
            select(agegroup, MAP) %>%
            group_by(agegroup) 
 
        counter = counter + 1
        
    }
}

MAP_Behavior_Fussy = do.call(rbind, map_behavior_fussy_distribution) %>%
    mutate(BEHAVIOR_GROUP = "Disturbed State") 


MAP_Behavior_FINAL = rbind(MAP_Behavior_Calm, MAP_Behavior_Fussy) %>%
    mutate(AGE_GROUP_ORDERED = ordered(agegroup, levels = c(
                "37 weeks PMA-30 Days",
                "1-3 Months",
                "3-6 Months",
                "6-12 Months",
                "12-18 Months",
                "18-24 Months",
                "2-3 Years",
                "3-4 Years",
                "4-5 Years",
                "5-6 Years",
                "6-7 Years",
                "7-8 Years",
                "8-9 Years",
                "9-10 Years",
                "10-11 Years",
                "11-12 Years",
                "12-13 Years",
                "13-14 Years",
                "14-15 Years",
                "15-16 Years",
                "16-17 Years",
                "17-18 Years")),
           AGE_GROUP_ORDERED_CHR = ordered(
                case_when(as.character(AGE_GROUP_ORDERED) 
                    == "37 weeks PMA-30 Days" ~ "37 weeks PMA-\n30 Days", 
                TRUE ~ as.character(AGE_GROUP_ORDERED)), 
                levels = c(
                        "37 weeks PMA-\n30 Days",
                        "1-3 Months",
                        "3-6 Months",
                        "6-12 Months",
                        "12-18 Months",
                        "18-24 Months",
                        "2-3 Years",
                        "3-4 Years",
                        "4-5 Years",
                        "5-6 Years",
                        "6-7 Years",
                        "7-8 Years",
                        "8-9 Years",
                        "9-10 Years",
                        "10-11 Years",
                        "11-12 Years",
                        "12-13 Years",
                        "13-14 Years",
                        "14-15 Years",
                        "15-16 Years",
                        "16-17 Years",
                        "17-18 Years")))


MAP_Behavior_ALL = MAP_Output_Distribution %>%
    droplevels() %>%
    mutate(BEHAVIOR_GROUP = "All Inpatient") %>%
    select(agegroup, MAP, BEHAVIOR_GROUP, AGE_GROUP_ORDERED, AGE_GROUP_ORDERED_CHR) 


MAP_Behavior_ALL_Q = rbind(MAP_Behavior_ALL, MAP_Behavior_FINAL) %>%
    ungroup() %>%
    select(AGE_GROUP_ORDERED_CHR, BEHAVIOR_GROUP, MAP) %>%
    group_by(AGE_GROUP_ORDERED_CHR, BEHAVIOR_GROUP) %>%
    summarise(QUANTILES = list(enframe(quantile(MAP, 
            probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))))) %>% 
    unnest() %>%
    mutate(NAMED_Q = ordered(name, 
                levels = c("5%", "10%", "25%", "50%", "75%", "90%", "95%")),
           QUANTILES = round(value, 0))



#### __Figure 3 ####

hyper = ggplot(MAP_Behavior_ALL_Q, aes(AGE_GROUP_ORDERED_CHR, QUANTILES, 
        group = as.factor(NAMED_Q), 
        color = as.factor(BEHAVIOR_GROUP), 
        shape = as.factor(NAMED_Q))) +
    geom_point(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "All Inpatient" & NAMED_Q %in% c("90%", "95%"))) +
    geom_line(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "All Inpatient" & NAMED_Q %in% c("90%", "95%")), 
        linetype = "solid") + 
    geom_point(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "Disturbed State" & NAMED_Q %in% c("90%", "95%"))) +
    geom_line(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "Disturbed State" & NAMED_Q %in% c("90%", "95%")),
        linetype = "dashed") +
    geom_point(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "Quiet State" & NAMED_Q %in% c("90%", "95%"))) +
    geom_line(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "Quiet State" & NAMED_Q %in% c("90%", "95%")),
        linetype = "dotted") +
    theme_bw(base_size = 10) +
    scale_shape_manual(name = "Quantile", 
        values = c(6,7)) +
    scale_color_manual(values = c("gray20", "#FF0000", "#2C7BB6")) +
    guides(color = guide_legend(reverse = T), shape = guide_legend(reverse = T)) +
    labs(x = "", # x = "Age in months relative to 37 weeks PMA", 
         y = "Hypertension\n") +
    theme(legend.position = "none", legend.box = "vertical",
        axis.text.x = element_blank(),        
        axis.title.x = element_blank(),
                axis.title.y = element_text(face = "italic"),
                plot.margin = unit(c(4, 1, 0, 1), "mm"))


middle = ggplot(MAP_Behavior_ALL_Q, aes(AGE_GROUP_ORDERED_CHR, QUANTILES, 
        group = as.factor(NAMED_Q), 
        color = as.factor(BEHAVIOR_GROUP), 
        shape = as.factor(NAMED_Q))) +
    geom_point(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "All Inpatient" & NAMED_Q %in% c("50%"))) +
    geom_line(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "All Inpatient" & NAMED_Q %in% c("50%")), 
        linetype = "solid") + 
    geom_point(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "Disturbed State" & NAMED_Q %in% c("50%"))) +
    geom_line(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "Disturbed State" & NAMED_Q %in% c("50%")),
        linetype = "dashed") +
    geom_point(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "Quiet State" & NAMED_Q %in% c("50%"))) +
    geom_line(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "Quiet State" & NAMED_Q %in% c("50%")),
        linetype = "dotted") +
    theme_bw(base_size = 10) +
    scale_shape_manual(name = "Quantile", values = 4) +
    scale_color_manual(values = c("gray20", "#FF0000", "#2C7BB6")) +
    guides(color = guide_legend(reverse = T), shape = guide_legend(reverse = T)) +
    labs(x = "", 
         y = expression(atop(italic("Median"), "Mean Arterial Pressure (mmHg)"))) +
    theme(legend.position = "none", legend.box = "vertical",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0, 1, 0, 1), "mm"))


hypo = ggplot(MAP_Behavior_ALL_Q, aes(AGE_GROUP_ORDERED_CHR, QUANTILES, 
        group = as.factor(NAMED_Q), 
        color = as.factor(BEHAVIOR_GROUP), 
        shape = as.factor(NAMED_Q))) +
    geom_point(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "All Inpatient" & NAMED_Q %in% c("5%", "10%"))) +
    geom_line(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "All Inpatient" & NAMED_Q %in% c("5%", "10%")), 
        linetype = "solid") + 
    geom_point(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "Disturbed State" & NAMED_Q %in% c("5%", "10%"))) +
    geom_line(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "Disturbed State" & NAMED_Q %in% c("5%", "10%")),
        linetype = "dashed") +
    geom_point(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "Quiet State" & NAMED_Q %in% c("5%", "10%"))) +
    geom_line(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "Quiet State" & NAMED_Q %in% c("5%", "10%")),
        linetype = "dotted") +
    theme_bw(base_size = 10) +
    scale_shape_manual(name = "Quantile", values = c(1, 2)) +
    scale_color_manual(values = c("gray20", "#FF0000", "#2C7BB6")) +
    scale_y_continuous(breaks = c(40, 50, 60)) +
    labs(x = "", 
         y = "Hypotension\n") +
    theme(legend.position = "none", legend.box = "vertical",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(face = "italic"),
        plot.caption = element_text(hjust = 0.5),
        plot.margin = unit(c(0, 1, 4, 1), "mm"))
    

lege = ggplot(MAP_Behavior_ALL_Q, aes(AGE_GROUP_ORDERED_CHR, QUANTILES, 
        group = as.factor(NAMED_Q), 
        color = as.factor(BEHAVIOR_GROUP), 
        shape = as.factor(NAMED_Q))) +
    geom_point(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "All Inpatient" & NAMED_Q %in% c("5%", "10%", "50%", "90%", "95%"))) +
    geom_line(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "All Inpatient" & NAMED_Q %in% c("5%", "10%", "50%", "90%", "95%")), 
        linetype = "solid") + 
    geom_point(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "Disturbed State" & NAMED_Q %in% c("5%", "10%", "50%", "90%", "95%"))) +
    geom_line(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "Disturbed State" & NAMED_Q %in% c("5%", "10%", "50%", "90%", "95%")),
        linetype = "dashed") +
    geom_point(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "Quiet State" & NAMED_Q %in% c("5%", "10%", "50%", "90%", "95%"))) +
    geom_line(data = filter(MAP_Behavior_ALL_Q, BEHAVIOR_GROUP == "Quiet State" & NAMED_Q %in% c("5%", "10%", "50%", "90%", "95%")),
        linetype = "dotted") +
    labs(color = "Behavior") +
    theme_bw(base_size = 10) +
    scale_shape_manual(name = "Quantile", values = c(1, 2, 4, 6, 7)) +
    scale_color_manual(values = c("gray20", "#FF0000", "#2C7BB6")) +
    scale_y_continuous(breaks = c(40, 50, 60)) +
    guides(color = guide_legend(reverse = T), shape = guide_legend(reverse = T)) 

legend_plot = get_legend(lege)

prow = plot_grid(hyper, middle, hypo, ncol = 1, align = "v", 
                 rel_heights = c(0.32, 0.28, 0.4))

p = plot_grid( prow, legend_plot, rel_widths = c(.8, .2))
p

ggsave(paste0("Roberts_et_al_MAP_Figure_3_", output_date, ".pdf"), p, width = 6.5, height = 8)



############################################################################## #
# E. ANALYSIS: Birth sex based MAP distributions                           #####
############################################################################## #


#### _Distributions for birth sex ####

map_f_distribution = list()
counter = 1

for(i in age_groups){ 
    
    for(j in 1:reps){
 
        map_f_distribution[[counter]] = map_f %>% 
            filter(AGE_GROUP == i) %>% 
            group_by(PT_ID) %>% 
            sample_n(size = 1, replace = FALSE) %>%
            ungroup() %>% 
            mutate(agegroup = i) %>%
            sample_n(size = length(unique(PT_ID)), replace = TRUE) %>% 
            select(agegroup, MAP) %>%
            group_by(agegroup) 
 
        counter = counter + 1
        
    }
}

MAP_F = do.call(rbind, map_f_distribution) %>%    
    mutate(SEX = "F") 


map_m_distribution = list()
counter = 1

for(i in age_groups){ 
    
    for(j in 1:reps){
 
        map_m_distribution[[counter]] = map_m %>% 
            filter(AGE_GROUP == i) %>% 
            group_by(PT_ID) %>% 
            sample_n(size = 1, replace = FALSE) %>%
            ungroup() %>% 
            mutate(agegroup = i) %>%
            sample_n(size = length(unique(PT_ID)), replace = TRUE) %>% 
            select(agegroup, MAP) %>%
            group_by(agegroup) 
 
        counter = counter + 1
        
    }
}

MAP_M = do.call(rbind, map_m_distribution) %>%    
    mutate(SEX = "M") 


MAP_FM_FINAL = rbind(MAP_F, MAP_M) %>%
    mutate(AGE_GROUP_ORDERED = ordered(agegroup, levels = c(
                "37 weeks PMA-30 Days",
                "1-3 Months",
                "3-6 Months",
                "6-12 Months",
                "12-18 Months",
                "18-24 Months",
                "2-3 Years",
                "3-4 Years",
                "4-5 Years",
                "5-6 Years",
                "6-7 Years",
                "7-8 Years",
                "8-9 Years",
                "9-10 Years",
                "10-11 Years",
                "11-12 Years",
                "12-13 Years",
                "13-14 Years",
                "14-15 Years",
                "15-16 Years",
                "16-17 Years",
                "17-18 Years")),
           AGE_GROUP_ORDERED_CHR = ordered(
                case_when(as.character(AGE_GROUP_ORDERED) 
                    == "37 weeks PMA-30 Days" ~ "37 weeks PMA-\n30 Days", 
                TRUE ~ as.character(AGE_GROUP_ORDERED)), 
                levels = c(
                        "37 weeks PMA-\n30 Days",
                        "1-3 Months",
                        "3-6 Months",
                        "6-12 Months",
                        "12-18 Months",
                        "18-24 Months",
                        "2-3 Years",
                        "3-4 Years",
                        "4-5 Years",
                        "5-6 Years",
                        "6-7 Years",
                        "7-8 Years",
                        "8-9 Years",
                        "9-10 Years",
                        "10-11 Years",
                        "11-12 Years",
                        "12-13 Years",
                        "13-14 Years",
                        "14-15 Years",
                        "15-16 Years",
                        "16-17 Years",
                        "17-18 Years")))


MAP_FM_ALL_Q = MAP_FM_FINAL %>%
    ungroup() %>%
    select(AGE_GROUP_ORDERED_CHR, SEX, MAP) %>%
    group_by(AGE_GROUP_ORDERED_CHR, SEX) %>%
    summarise(QUANTILES = list(enframe(quantile(MAP, 
            probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))))) %>% 
    unnest() %>%
    mutate(NAMED_Q = ordered(name, 
                levels = c("5%", "10%", "25%", "50%", "75%", "90%", "95%")),
           QUANTILES = round(value, 0))


#### __Figure 4 ####
ggplot(MAP_FM_ALL_Q, aes(AGE_GROUP_ORDERED_CHR, QUANTILES, 
        group = as.factor(NAMED_Q), 
        color = as.factor(SEX), 
        shape = as.factor(NAMED_Q))) +
    geom_point(data = filter(MAP_FM_ALL_Q, SEX == "M")) +
    geom_line(data = filter(MAP_FM_ALL_Q, SEX == "M"),
        linetype = "dashed") +
    geom_point(data = filter(MAP_FM_ALL_Q, SEX == "F")) +
    geom_line(data = filter(MAP_FM_ALL_Q, SEX == "F"),
        linetype = "solid") +
    theme_bw(base_size = 10) +
    scale_shape_manual(name = "Quantile", 
        values = 1:nlevels(as.factor(MAP_FM_ALL_Q$QUANTILES))) +
    scale_color_manual(values = c("#2C7BB6", "#595959")) +
    scale_y_continuous(limits = c(39, 106)) +
    guides(shape = guide_legend(reverse = T)) +
    labs(x = "", y = "Mean Arterial Pressure (mmHg)", color = "Sex") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
         plot.caption = element_text(hjust = 0.5))
    
ggsave(paste0("Roberts_et_al_MAP_Figure_4_", output_date, ".pdf"), width = 6.5, height = 5)





############################################################################## #
# F. ANALYSIS: Thresholds comparisons                                       ####
############################################################################## #


## To match the paper's results, use this code chunk:
MAP_5th = read_csv("https://raw.githubusercontent.com/Rmadillo/Roberts_et_al_MAP/master/MAP_Lookup_Table_2019-04-29.csv")


# ## To use the random results from above, use this code chunk:
# MAP_5th = MAP_FINAL_MAIN %>%
#     ungroup() %>%
#     filter(GROUPING == "Measured MAP",
#            name == "5%") %>%
#     droplevels() %>%
#     select(AGE_GROUP_ORDERED, QUANTILES) %>%
#     arrange(AGE_GROUP_ORDERED)


MAP_5th = MAP_5th %>%
    filter(GROUPING == "Measured MAP") %>%
    mutate(AGE_GROUP_ORDERED = ordered(AGE_GROUP_ORDERED, levels = c(
                "37 weeks PMA-30 Days",
                "1-3 Months",
                "3-6 Months",
                "6-12 Months",
                "12-18 Months",
                "18-24 Months",
                "2-3 Years",
                "3-4 Years",
                "4-5 Years",
                "5-6 Years",
                "6-7 Years",
                "7-8 Years",
                "8-9 Years",
                "9-10 Years",
                "10-11 Years",
                "11-12 Years",
                "12-13 Years",
                "13-14 Years",
                "14-15 Years",
                "15-16 Years",
                "16-17 Years",
                "17-18 Years"))) %>%
    select(AGE_GROUP_ORDERED, MAP_5th = `5%`)


map_thresholds = map_all %>%
    filter(between(SBP, 17, 200) & between(DBP, 8, 160) & between(MAP, 20, 200)) %>%
    left_join(MAP_5th, by = "AGE_GROUP_ORDERED") %>%
    mutate(SCH_MAP_HYPOTENSION = case_when(
        MAP < MAP_5th ~ "SCH Hypotensive",
        MAP >= MAP_5th ~ "SCH Normal",
        TRUE ~ NA_character_))


map_thresholds = map_thresholds %>%
    mutate(
        ### PALS SBP 5th percentile (PALS hypotension)
        # https://www.acls-pals-bls.com/algorithms/pals/
        # https://chemm.nlm.nih.gov/pals.htm
            PALS_SBP_5th = case_when(
                AGE_GROUP_ORDERED == "37 weeks PMA-30 Days" ~ 60,    
                AGE_GROUP_ORDERED == "1-3 Months" ~ 70, 
                AGE_GROUP_ORDERED == "3-6 Months" ~ 70,    
                AGE_GROUP_ORDERED == "6-12 Months" ~ 70, 
                AGE_GROUP_ORDERED == "12-18 Months" ~ 70 + (1 * 2),
                AGE_GROUP_ORDERED == "18-24 Months" ~ 70 + (1 * 2), 
                AGE_GROUP_ORDERED == "2-3 Years" ~ 70 + (2 * 2), 
                AGE_GROUP_ORDERED == "3-4 Years" ~ 70 + (3 * 2), 
                AGE_GROUP_ORDERED == "4-5 Years" ~ 70 + (4 * 2), 
                AGE_GROUP_ORDERED == "5-6 Years" ~ 70 + (5 * 2), 
                AGE_GROUP_ORDERED == "6-7 Years" ~ 70 + (6 * 2), 
                AGE_GROUP_ORDERED == "7-8 Years" ~ 70 + (7 * 2), 
                AGE_GROUP_ORDERED == "8-9 Years" ~ 70 + (8 * 2), 
                AGE_GROUP_ORDERED == "9-10 Years" ~ 70 + (9 * 2), 
                AGE_GROUP_ORDERED == "10-11 Years" ~ 70 + (10 * 2), 
                AGE_GROUP_ORDERED == "11-12 Years" ~ 90, 
                AGE_GROUP_ORDERED == "12-13 Years"~ 90, 
                AGE_GROUP_ORDERED == "13-14 Years"~ 90, 
                AGE_GROUP_ORDERED == "14-15 Years"~ 90, 
                AGE_GROUP_ORDERED == "15-16 Years"~ 90, 
                AGE_GROUP_ORDERED == "16-17 Years"~ 90, 
                AGE_GROUP_ORDERED == "17-18 Years"~ 90,
                TRUE ~ NA_real_),
            PALS_SBP_HYPOTENSION = case_when(
                SBP < PALS_SBP_5th ~ "PALS Hypotensive",
                SBP >= PALS_SBP_5th ~ "PALS Normal",
                TRUE ~ NA_character_),
    ### APLS 6th edition
    # https://books.google.com/books?id=pauaCwAAQBAJ&lpg=PP1&dq=advanced%20pediatric%20life%20support%206th%20edition&pg=PT97#v=onepage&q=table%204.1&f=false
            APLS_SBP_5th = case_when(
                AGE_GROUP_ORDERED == "37 weeks PMA-30 Days" ~ 65,    
                AGE_GROUP_ORDERED == "1-3 Months" ~ 65, 
                AGE_GROUP_ORDERED == "3-6 Months" ~ 65,    
                AGE_GROUP_ORDERED == "6-12 Months" ~ 70, 
                AGE_GROUP_ORDERED == "12-18 Months" ~ 70,
                AGE_GROUP_ORDERED == "18-24 Months" ~ 70, 
                AGE_GROUP_ORDERED == "2-3 Years" ~ 70, 
                AGE_GROUP_ORDERED == "3-4 Years" ~ 70, 
                AGE_GROUP_ORDERED == "4-5 Years" ~ 70, 
                AGE_GROUP_ORDERED == "5-6 Years" ~ 80, 
                AGE_GROUP_ORDERED == "6-7 Years" ~ 80, 
                AGE_GROUP_ORDERED == "7-8 Years" ~ 80, 
                AGE_GROUP_ORDERED == "8-9 Years" ~ 80, 
                AGE_GROUP_ORDERED == "9-10 Years" ~ 80, 
                AGE_GROUP_ORDERED == "10-11 Years" ~ 80, 
                AGE_GROUP_ORDERED == "11-12 Years" ~ 80, 
                AGE_GROUP_ORDERED == "12-13 Years"~ 90, 
                AGE_GROUP_ORDERED == "13-14 Years"~ 90, 
                AGE_GROUP_ORDERED == "14-15 Years"~ 90, 
                AGE_GROUP_ORDERED == "15-16 Years"~ 90, 
                AGE_GROUP_ORDERED == "16-17 Years"~ 90, 
                AGE_GROUP_ORDERED == "17-18 Years"~ 90,
                TRUE ~ NA_real_),
        APLS_SBP_HYPOTENSION = factor(case_when(
                SBP < APLS_SBP_5th ~ "APLS Hypotensive",
                SBP >= APLS_SBP_5th ~ "APLS Normal",
                TRUE ~ NA_character_)),
        ### IPSCC 2005
        # Table 3 in https://journals.lww.com/pccmjournal/Citation/2005/07000/Reply__Values_for_Systolic_Blood_Pressure.27.aspx
        # Table 1 in https://www.ncbi.nlm.nih.gov/pubmed/17273118
        IPSCC_SBP_5th = case_when(
                AGE_GROUP_ORDERED == "37 weeks PMA-30 Days" ~ 59, 
                    # Note: 59 for 0-1 week; 79 for 1 week to 1 month 
                AGE_GROUP_ORDERED == "1-3 Months" ~ 75, 
                AGE_GROUP_ORDERED == "3-6 Months" ~ 75,    
                AGE_GROUP_ORDERED == "6-12 Months" ~ 75, 
                AGE_GROUP_ORDERED == "12-18 Months" ~ 74,
                AGE_GROUP_ORDERED == "18-24 Months" ~ 74, 
                AGE_GROUP_ORDERED == "2-3 Years" ~ 74, 
                AGE_GROUP_ORDERED == "3-4 Years" ~ 74, 
                AGE_GROUP_ORDERED == "4-5 Years" ~ 74, 
                AGE_GROUP_ORDERED == "5-6 Years" ~ 74, 
                AGE_GROUP_ORDERED == "6-7 Years" ~ 83, 
                AGE_GROUP_ORDERED == "7-8 Years" ~ 83, 
                AGE_GROUP_ORDERED == "8-9 Years" ~ 83, 
                AGE_GROUP_ORDERED == "9-10 Years" ~ 83, 
                AGE_GROUP_ORDERED == "10-11 Years" ~ 83, 
                AGE_GROUP_ORDERED == "11-12 Years" ~ 83, 
                AGE_GROUP_ORDERED == "12-13 Years"~ 83, 
                AGE_GROUP_ORDERED == "13-14 Years"~ 90, 
                AGE_GROUP_ORDERED == "14-15 Years"~ 90, 
                AGE_GROUP_ORDERED == "15-16 Years"~ 90, 
                AGE_GROUP_ORDERED == "16-17 Years"~ 90, 
                AGE_GROUP_ORDERED == "17-18 Years"~ 90,
                TRUE ~ NA_real_),
        IPSCC_SBP_HYPOTENSION = case_when(
                SBP < IPSCC_SBP_5th ~ "IPSCC Hypotensive",
                SBP >= IPSCC_SBP_5th ~ "IPSCC Normal",
                TRUE ~ NA_character_))
    


### Counts and crosstabs to compare SCH MAP hypotension to other thresholds, save to csv

# PALS

PALS = map_thresholds %>%
    group_by(AGE_GROUP_ORDERED, SCH_MAP_HYPOTENSION, PALS_SBP_HYPOTENSION) %>%
    count() %>%
    set_names(c("AGE_GROUP_ORDERED", "SCH", "COMPARISON", "MAP_COUNT")) %>%
    as_tibble() %>%
    mutate(STANDARD = "PALS")

SCH_PALS = ftable(addmargins(xtabs(~ AGE_GROUP_ORDERED + PALS_SBP_HYPOTENSION + 
                SCH_MAP_HYPOTENSION, data = map_thresholds)))

SCH_PALS_OUT = stats:::format.ftable(SCH_PALS, quote = F)
write.table(SCH_PALS_OUT, paste0("SCH_PALS_", output_date, ".csv"), quote = F, 
            row.names = F, col.names = F, sep = ",")


# APLS

APLS = map_thresholds %>%
    group_by(AGE_GROUP_ORDERED, SCH_MAP_HYPOTENSION, APLS_SBP_HYPOTENSION) %>%
    count() %>%
    set_names(c("AGE_GROUP_ORDERED", "SCH", "COMPARISON", "MAP_COUNT")) %>%
    as_tibble() %>%
    mutate(STANDARD = "APLS")

SCH_APLS = ftable(addmargins(xtabs(~ AGE_GROUP_ORDERED + APLS_SBP_HYPOTENSION + 
                SCH_MAP_HYPOTENSION, data = map_thresholds)))

SCH_APLS_OUT = stats:::format.ftable(SCH_APLS, quote = F)
write.table(SCH_APLS_OUT, paste0("SCH_APLS_", output_date, ".csv"), quote = F, 
            row.names = F, col.names = F, sep = ",")


# IPSCC

IPSCC = map_thresholds %>%
    group_by(AGE_GROUP_ORDERED, SCH_MAP_HYPOTENSION, IPSCC_SBP_HYPOTENSION) %>%
    count() %>%
    set_names(c("AGE_GROUP_ORDERED", "SCH", "COMPARISON", "MAP_COUNT")) %>%
    as_tibble() %>%
    mutate(STANDARD = "IPSCC")

SCH_IPSCC = ftable(addmargins(xtabs(~ AGE_GROUP_ORDERED + IPSCC_SBP_HYPOTENSION + 
                SCH_MAP_HYPOTENSION, data = map_thresholds)))

SCH_IPSCC_OUT = stats:::format.ftable(SCH_IPSCC, quote = F)
write.table(SCH_IPSCC_OUT, paste0("SCH_IPSCC_", output_date, ".csv"), quote = F, 
            row.names = F, col.names = F, sep = ",")


# All Joined

THRESHOLDS = bind_rows(PALS, APLS, IPSCC) %>%
    mutate(GROUPING = case_when(
                str_detect(SCH, "Normal") & str_detect(COMPARISON, "Normal") ~ 
                    "Normotensive values both thresholds",
                str_detect(SCH, "Hypotensive") & str_detect(COMPARISON, "Normal") ~ 
                    "Hypotensive values only MAP, not comparison",
                str_detect(SCH, "Normal") & str_detect(COMPARISON, "Hypotensive") ~ 
                    "Hypotensive values only comparison, not MAP",
                str_detect(SCH, "Hypotensive") & str_detect(COMPARISON, "Hypotensive") ~ 
                    "Hypotensive values both thresholds",
                TRUE ~ NA_character_),
        GROUPING_ORDERED = ordered(GROUPING, levels = c(
            "Hypotensive values only MAP, not comparison",
            "Hypotensive values only comparison, not MAP",
            "Hypotensive values both thresholds",
            "Normotensive values both thresholds")),
        AGE_GROUP_ORDERED_CHR = ordered(
                case_when(as.character(AGE_GROUP_ORDERED) 
                    == '37 weeks PMA-30 Days' ~ '37 weeks PMA-\n30 Days', 
                TRUE ~ as.character(AGE_GROUP_ORDERED)), 
                levels = c(
                        "37 weeks PMA-\n30 Days",
                        "1-3 Months",
                        "3-6 Months",
                        "6-12 Months",
                        "12-18 Months",
                        "18-24 Months",
                        "2-3 Years",
                        "3-4 Years",
                        "4-5 Years",
                        "5-6 Years",
                        "6-7 Years",
                        "7-8 Years",
                        "8-9 Years",
                        "9-10 Years",
                        "10-11 Years",
                        "11-12 Years",
                        "12-13 Years",
                        "13-14 Years",
                        "14-15 Years",
                        "15-16 Years",
                        "16-17 Years",
                        "17-18 Years")))


#### __Figure 5 ####
ggplot(THRESHOLDS, aes(AGE_GROUP_ORDERED, MAP_COUNT, fill = GROUPING_ORDERED)) +
    geom_bar(position = "fill", stat = "identity") +
    facet_wrap(~ STANDARD, nrow = 2, scales = "free_x") +
    scale_y_continuous(breaks = c(0.74, 0.8, 0.85, 0.9, 0.95, 1), 
                       labels = c("5%", "80%", "85%", "90%", "95%", "100%"),
                       minor_breaks = NULL) +
    
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.755, ymax = 0.775), fill = "white") + 
              
    annotate("point", size = 3, x = seq(0.85, 21.85, 1), y = 0.755, color = "transparent", 
             fill = "white", shape = 24) +
    annotate("point", size = 3, x = seq(1.15, 22.15, 1), y = 0.775, color = "transparent", 
             fill = "white", shape = 25) +
    
    annotate("segment", x = c(1:22), xend = c(1:22), y = 0.7525, yend = 0.7775, 
             colour = "gray92", size = 0.25) +
    
    coord_flip(ylim = c(0.74, 1.00)) +
    scale_fill_manual(values = c("#FF0000BF", "#2C7BB6BF", "#A0A0A0BF", "#595959BF")) +
    labs(x = "", y = "", fill = ""
         #,
         #title = "SCH MAP 5th vs Comparison 5th"
         ) +
    theme_bw(base_size = 10) +
    guides(fill = guide_legend(#nrow = 2, byrow = TRUE, 
        reverse = T)) +
    theme(legend.position = c(0.75, 0.25),
          axis.text.y = element_text(size = 8)) 
 
ggsave(paste0("Roberts_et_al_MAP_Figure_5_", output_date, ".pdf"), width = 8.5, height = 5.5)


#### __Table 2 ####


thresholds_sum = THRESHOLDS %>% 
    group_by(STANDARD, SCH, COMPARISON) %>% 
    summarise(MAP_SUM = sum(MAP_COUNT)) %>%
    group_by(STANDARD) %>% 
    mutate(FREQ = round(MAP_SUM/sum(MAP_SUM), 3) * 100)

write_csv(thresholds_sum, paste0("Roberts_et_al_MAP_Table_2_", output_date, ".csv"))



############################################################################## #
#### G. SUPPLEMENTAL MATERIAL                                               ####
############################################################################## #


#### _Figure S1 ####

# Selection of SBP reading cutoff values (to remove data entry error, etc.)

SBP_dist_mid = ggplot(map_all, aes(SBP)) +
    geom_density(fill = "darkblue", alpha = 0.4) +
    labs(x = "SBP (mmHG)", y = "", title = "") +
    theme_bw(base_size = 10)

SBP_dist_left = ggplot(map_all, aes(SBP)) +
    geom_vline(xintercept = 18, linetype = "dashed") +
    geom_density(fill = "darkblue", alpha = 0.4) +
    xlim(0, 40) +
    labs(x = "", y = "density", title = "") +
    theme_bw(base_size = 10)

SBP_dist_right = ggplot(map_all, aes(SBP)) +
    geom_vline(xintercept = 200, linetype = "dashed") +
    geom_density(fill = "darkblue", alpha = 0.4) +
    xlim(170, 220) +
    labs(x = "", y = "", title = "") +
    theme_bw(base_size = 10)

valid_SBP = plot_grid(SBP_dist_left, SBP_dist_mid, SBP_dist_right, nrow = 1, labels = c("A", "", ""))

# Selection of MAP reading cutoff values (to remove data entry error, etc.)

map_dist_mid = ggplot(map_all, aes(MAP)) +
    geom_density(fill = "darkblue", alpha = 0.4) +
    labs(x = "MAP (mmHG)", y = "", title = "") +
    theme_bw(base_size = 10)

map_dist_left = ggplot(map_all, aes(MAP)) +
    geom_vline(xintercept = 12, linetype = "dashed") +
    geom_density(fill = "darkblue", alpha = 0.4) +
    xlim(0, 30) +
    labs(x = "", y = "density", title = "") +
    theme_bw(base_size = 10)

map_dist_right = ggplot(map_all, aes(MAP)) +
    geom_vline(xintercept = 200, linetype = "dashed") +
    geom_density(fill = "darkblue", alpha = 0.4) +
    xlim(150, 220) +
    labs(x = "", y = "", title = "") +
    theme_bw(base_size = 10)

valid_MAP = plot_grid(map_dist_left, map_dist_mid, map_dist_right, nrow = 1, labels = c("B", "", ""))

# Selection of DBP reading cutoff values (to remove data entry error, etc.)

DBP_dist_mid = ggplot(map_all, aes(DBP)) +
    geom_density(fill = "darkblue", alpha = 0.4) +
    labs(x = "DBP (mmHG)", y = "", title = "") +
    theme_bw(base_size = 10)

DBP_dist_left = ggplot(map_all, aes(DBP)) +
    geom_vline(xintercept = 8, linetype = "dashed") +
    geom_density(fill = "darkblue", alpha = 0.4) +
    xlim(0, 20) +
    labs(x = "", y = "density", title = "") +
    theme_bw(base_size = 10)

DBP_dist_right = ggplot(map_all, aes(DBP)) +
    geom_vline(xintercept = 160, linetype = "dashed") +
    geom_density(fill = "darkblue", alpha = 0.4) +
    xlim(140, 180) +
    labs(x = "", y = "", title = "") +
    theme_bw(base_size = 10)

valid_DBP = plot_grid(DBP_dist_left, DBP_dist_mid, DBP_dist_right, nrow = 1, labels = c("C", "", ""))

valid_BPs = plot_grid(valid_SBP, valid_MAP, valid_DBP, ncol = 1)

ggsave(paste0("Roberts_et_al_MAP_Suppl_Figure_S1_", output_date, ".pdf"), width = 6.5, height = 7.5)



#### _Table S1 ####

# Counts of behavior stats recorded with measured MAP values in acute care

MAP_ACUTE_BEHAVIOR = ftable(addmargins(xtabs(~ AGE_GROUP_ORDERED + BEHAVIOR_GROUP, data = map_behavior)))
    
# CSV table of MAP counts by behavior
MAP_ACUTE_BEHAVIOR_OUT = stats:::format.ftable(MAP_ACUTE_BEHAVIOR, quote = F)
write.table(MAP_ACUTE_BEHAVIOR_OUT, paste0("Roberts_et_al_MAP_Suppl_Table_S1_",
        output_date, ".csv"), quote = F, row.names = F, col.names = F, sep = ",")



#### _Table S2 ####
# HTML table for behavior MAP lookup
MAP_BEHV_HTML = reshape2::dcast(MAP_Behavior_ALL_Q, AGE_GROUP_ORDERED_CHR + BEHAVIOR_GROUP ~ NAMED_Q, value.var = "QUANTILES") 

kable(MAP_BEHV_HTML[ , -1], 
        format = "html",
        #format = "latex",
        col.names = c("Age Group & MAP Type \u00A0\u00A0", 
            names(MAP_BEHV_HTML[-c(1:2)]))
      ) %>%
    kable_styling("striped", full_width = F) %>%
    add_header_above(c(" ", "Quantile" = 7)) %>%
    kableExtra::group_rows(index = c("37 weeks PMA-30 Days" = 3, "1-3 Months" = 3, 
        "3-6 Months" = 3, "6-12 Months" = 3, "12-18 Months" = 3, 
        "18-24 Months" = 3, "2-3 Years" = 3, "3-4 Years" = 3, 
        "4-5 Years" = 3, "5-6 Years" = 3, "6-7 Years" = 3, 
        "7-8 Years" = 3, "8-9 Years" = 3, "9-10 Years" = 3, 
        "10-11 Years" = 3, "11-12 Years" = 3, "12-13 Years" = 3, 
        "13-14 Years" = 3, "14-15 Years" = 3, "15-16 Years" = 3, 
        "16-17 Years" = 3, "17-18 Years" = 3)) 

# CSV table for behavior MAP lookup
write_csv(MAP_BEHV_HTML, paste0("Roberts_et_al_MAP_Suppl_Table_S2_", output_date, ".csv"))



#### _Figure S2 ####
map_behavior %>%
    filter(BEHAVIOR_GROUP %in% c("Quiet State", "Disturbed State")) %>%
    ggplot(aes(x = MAP, y = AGE_GROUP_ORDERED, 
                    fill = BEHAVIOR_GROUP)) + 
        geom_density_ridges(alpha = 0.4, rel_min_height = 0.02, scale = 0.75) +
        scale_x_continuous(expand = c(0.05, 0.05), limits = c(20, 120),
                       breaks = c(20, 40, 60, 80, 100, 120),
                       minor_breaks = c(30, 50, 70, 90, 110)) +
        scale_y_discrete(expand = c(0.05, 0)) +
        scale_fill_manual(values = c("darkred", "darkblue"), 
                          guide = guide_legend(reverse=TRUE)) +
        theme_bw(base_size = 10) + 
        coord_flip() +
        theme(legend.position = "top",
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              plot.caption = element_text(hjust = 0.5)) +
        labs(y = "", x = "Mean Arterial Pressure (mmHg)", color = "Behavior: ", 
             fill = "Behavior: ", title = "")

ggsave(paste0("Roberts_et_al_MAP_Suppl_Figure_S2_", output_date, ".pdf"), width = 6.5, height = 4.5)



#### _Table S3 ####

# Count total MAP readings and patients
map_all %>%         
    group_by(AGE_GROUP_ORDERED) %>%         
    summarise(MAP_Counts = n(),
            Patient_Counts = n_distinct(PT_ID)) %>%
    write_csv(paste0("Roberts_et_al_MAP_Suppl_Table_S3_", output_date, ".csv")) %>%
    print(n = Inf)



#### _Table S4 ####
# Lookup table for measured and calculated MAP quantiles
# corresponds with Figure 2
# NOTE: version of this table in submitted paper is located at:
# https://github.com/Rmadillo/Roberts_et_al_MAP/blob/master/MAP_Lookup_Table_2019-04-29.csv

MAP_FINAL_TABLE = MAP_FINAL %>%
    ungroup() %>%
    filter(name %in% c("5%", "10%", "20%", "30%", "40%", "50%", 
                "60%", "70%", "80%", "90%", "95%")) %>%
    mutate(NAMED_Q = ordered(name, levels = c("5%", "10%", "20%", "30%", "40%", 
                "50%", "60%", "70%", "80%", "90%", "95%")),
        GROUPING = ordered(GROUPING, levels = c("Measured MAP", "Calculated MAP")),
        QUANTILES = round(QUANTILES, 0),
        AGE_GROUP_ORDERED = ordered(agegroup, levels = c(
                "37 weeks PMA-30 Days",
                "1-3 Months",
                "3-6 Months",
                "6-12 Months",
                "12-18 Months",
                "18-24 Months",
                "2-3 Years",
                "3-4 Years",
                "4-5 Years",
                "5-6 Years",
                "6-7 Years",
                "7-8 Years",
                "8-9 Years",
                "9-10 Years",
                "10-11 Years",
                "11-12 Years",
                "12-13 Years",
                "13-14 Years",
                "14-15 Years",
                "15-16 Years",
                "16-17 Years",
                "17-18 Years"))) %>%
    select(-agegroup, -name) %>%
    arrange(AGE_GROUP_ORDERED, GROUPING, NAMED_Q)

MAP_HTML = reshape2::dcast(MAP_FINAL_TABLE, AGE_GROUP_ORDERED + GROUPING ~ NAMED_Q, 
                     value.var = "QUANTILES") 

# HTML table for measured and calculated MAP lookup
kable(MAP_HTML[ , -1], 
        format = "html",
        col.names = c("Age Group & MAP Type \u00A0\u00A0", 
            names(MAP_HTML[-c(1:2)]))) %>%
    kable_styling("striped", full_width = F) %>%
    add_header_above(c(" ", "Quantile" = 11)) %>%
    kableExtra::group_rows(index = c("37 weeks PMA-30 Days" = 2, "1-3 Months" = 2, 
        "3-6 Months" = 2, "6-12 Months" = 2, "12-18 Months" = 2, 
        "18-24 Months" = 2, "2-3 Years" = 2, "3-4 Years" = 2, 
        "4-5 Years" = 2, "5-6 Years" = 2, "6-7 Years" = 2, 
        "7-8 Years" = 2, "8-9 Years" = 2, "9-10 Years" = 2, 
        "10-11 Years" = 2, "11-12 Years" = 2, "12-13 Years" = 2, 
        "13-14 Years" = 2, "14-15 Years" = 2, "15-16 Years" = 2, 
        "16-17 Years" = 2, "17-18 Years" = 2)) 

# CSV table for measured and calculated MAP lookup
write_csv(MAP_HTML, paste0("Roberts_et_al_MAP_Suppl_Table_S4_", output_date, ".csv"))



#### _Table S5 ####
# Table of bootstrapped measured MAP quantiles by birth sex and age group 

MAP_FM_TABLE = MAP_FM_ALL_Q %>%
    arrange(AGE_GROUP_ORDERED_CHR, SEX, NAMED_Q)

MAP_HTML_FM = reshape2::dcast(MAP_FM_TABLE, AGE_GROUP_ORDERED_CHR + SEX ~ NAMED_Q, value.var = "QUANTILES")

# HTML table for MAP lookup by birth sex and age group
kable(MAP_HTML_FM[ , -1],
        format = "html",
        #format = "latex",
        col.names = c("Age Group & Sex \u00A0\u00A0",
            names(MAP_HTML_FM[-c(1:2)]))) %>%
    kable_styling("striped", full_width = F) %>%
    add_header_above(c(" ", "Quantile" = 7)) %>%
    kableExtra::group_rows(index = c("37 weeks PMA-30 Days" = 2, "1-3 Months" = 2,
        "4-6 Months" = 2, "6-12 Months" = 2, "12-18 Months" = 2,
        "18-24 Months" = 2, "2-3 Years" = 2, "3-4 Years" = 2,
        "4-5 Years" = 2, "5-6 Years" = 2, "6-7 Years" = 2,
        "7-8 Years" = 2, "8-9 Years" = 2, "9-10 Years" = 2,
        "10-11 Years" = 2, "11-12 Years" = 2, "12-13 Years" = 2,
        "13-14 Years" = 2, "14-15 Years" = 2, "15-16 Years" = 2,
        "16-17 Years" = 2, "17-18 Years" = 2)) 

# CSV table for MAP lookup by birth sex and age group
write_csv(MAP_HTML_FM, paste0("Roberts_et_al_MAP_Suppl_Table_S5_", output_date, ".csv"))



############################################################################## #
# END OF FILE ------------------------------------------------------------ #####
############################################################################## #
