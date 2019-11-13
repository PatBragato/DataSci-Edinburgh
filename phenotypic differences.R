# APB Prac 3
# Pat Bragato

#Libraries ----
library(dplyr)
library(tidyverse)
library(GAD)
library(gridExtra)
library(ggplot2)

# Hypo Length ----
Hlength <- read.csv("hypo_data.csv")

# Converting to long form
data_long <- gather(data= Hlength, key = "group", 
                    value = "length", c(3:10))

data_long$length <- parse_number(data_long$length) 

data_long <- filter(data_long, is.finite(length))

data_long <- rename(data_long, treatment = ï..Treatment)

averages <- data_long %>% 
  group_by(treatment, Mutant) %>% 
  summarise(average = mean(length), error = sd(length))

str(data_long)
Treat <- as.fixed(data_long$treatment)
Mut <- as.random(data_long$Mutant)

# Box Plot
(boxplot <- ggplot(data_long, aes (x = treatment, length)) +
   geom_boxplot(aes(fill = Mutant)) +
   theme_classic() +
   labs(x = "\nTreatment", y = "Hypocotyl Length (mm)\n"))

ggsave(filename = "Hypo_boxplot.png", device = "png", 
       width = 8, height = 5)
# Biomass ----
biomass <- read.csv("biomass.csv")

# Converting to long form
biomass_long <- gather(data = biomass, key = "group", 
                    value = "length", c(3:10))

biomass_long$length <- parse_number(biomass_long$length) 

biomass_long <- filter(biomass_long, is.finite(length))

biomass_long <- rename(biomass_long, treatment = ï..Treatment)

# Graphing
(biomass_box <- ggplot(biomass_long, aes (x = treatment, length)) +
    geom_boxplot(aes(fill = Mutant)) +
    theme_classic() +
    labs(title = "Biomass of plants",
         x = "\nTreatment", y = "Plant biomass (g)\n"))

ggsave(filename = "biomass_boxplot.png", device = "png", 
       width = 8, height = 5)


# Leafmass ----
leafmass <- read.csv("leafmass.csv")

# Converting to long form
leafmass_long <- gather(data = leafmass, key = "group", 
                       value = "length", c(3:7))

leafmass_long$length <- parse_number(leafmass_long$length) 

leafmass_long <- filter(leafmass_long, is.finite(length))

leafmass_long <- rename(leafmass_long, treatment = ï..Treatment)

# Graphing
(leafmass_box <- ggplot(leafmass_long, aes (x = treatment, length)) +
    geom_boxplot(aes(fill = Mutant)) +
    theme_classic() +
    labs(title = "Mass of 3rd leaf",
         x = "\nTreatment", y = "Plant leafmass (g)\n"))

ggsave(filename = "leafmass_boxplot.png", device = "png", 
       width = 8, height = 5)


# Leaf Area ----
leaf_area <- read.csv("leafarea.csv")

# Converting to long form
leaf_area_long <- gather(data = leaf_area, key = "group", 
                       value = "length", c(3:8))

leaf_area_long$length <- parse_number(leaf_area_long$length) 

leaf_area_long <- filter(leaf_area_long, is.finite(length))

leaf_area_long <- rename(leaf_area_long, treatment = ï..Treatment)

# Graphing
(leaf_area_box <- ggplot(leaf_area_long, aes (x = treatment, length)) +
    geom_boxplot(aes(fill = Mutant)) +
    theme_classic() +
    labs(title = "Leaf Area of plants",
         x = "\nTreatment", y = "Leaf area (mm^2) \n"))

ggsave(filename = "leaf_area_boxplot.png", device = "png", 
       width = 8, height = 5)

# Leaf ratio ----
leaf_ratio <- read.csv("leafratio.csv")

# Converting to long form
leaf_ratio_long <- gather(data = leaf_ratio, key = "group", 
                       value = "length", c(3:8))

leaf_ratio_long$length <- parse_number(leaf_ratio_long$length) 

leaf_ratio_long <- filter(leaf_ratio_long, is.finite(length))

leaf_ratio_long <- rename(leaf_ratio_long, treatment = ï..Treatment)

# Graphing
(leaf_ratio_box <- ggplot(leaf_ratio_long, aes (x = treatment, length)) +
    geom_boxplot(aes(fill = Mutant)) +
    theme_classic() +
    labs(title = "Length to width ratio of 3rd leaf",
         x = "\nTreatment", y = "Length:Width\n"))

ggsave(filename = "leaf_ratio_boxplot.png", device = "png", 
       width = 8, height = 5)


# Petiole Length ----
petiole_length <- read.csv("petiolelength.csv")

# Converting to long form
petiole_length_long <- gather(data = petiole_length, key = "group", 
                       value = "length", c(3:8))

petiole_length_long$length <- parse_number(petiole_length_long$length) 

petiole_length_long <- filter(petiole_length_long, is.finite(length))

petiole_length_long <- rename(petiole_length_long, treatment = ï..Treatment)

# Graphing
(petiole_length_box <- ggplot(petiole_length_long, aes (x = treatment, length)) +
    geom_boxplot(aes(fill = Mutant)) +
    theme_classic() +
    labs(title = "Petiole length of 3rd Leaf",
         x = "\nTreatment", y = "Petiole Length (mm)\n"))

ggsave(filename = "petiole_length_boxplot.png", device = "png", 
       width = 8, height = 5)


# Creating a Panel
panel <- grid.arrange(biomass_box, leaf_area_box, leaf_ratio_box, leafmass_box, 
                      petiole_length_box, boxplot)
ggsave(panel, filename = "panel.png", device = "png", width = 16, height = 16)
