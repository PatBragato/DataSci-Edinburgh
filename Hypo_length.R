#APB Prac 3
# Pat Bragato

#Libraries ----
library(dplyr)
library(tidyverse)
library(GAD)

# Data Wrangling ----

Hlength <- read.csv("data3.csv")

# Converting to long form
data_long <- gather(data= Hlength, key = "group", 
                   value = "length", c(3:9))

data_long$length <- parse_number(data_long$length) 

data_long <- filter(data_long, is.finite(length))

data_long <- rename(data_long, treatment = ï..Treatment)

averages <- data_long %>% 
  group_by(treatment, Mutant) %>% 
  summarise(average = mean(length), error = sd(length))

str(data_long)
Treat <- as.fixed(data_long$treatment)
Mut <- as.random(data_long$Mutant)

# Graphing ----
(barplot <- ggplot(averages, aes(x = treatment, y = average, fill = Mutant)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = average - error, ymax = average + error), width =.2,
                 position = position_dodge(.9)) +
  theme_classic() +
  labs(x = "\nTreatment", y = "Average Hypocotyl Length (mm)\n") +
   scale_fill_brewer(palette="Set1")
   )
ggsave(filename = "Hypo_length.pdf", device = "pdf", 
       width = 8, height = 5)

# Analysis ----
hypo_model <- lm(length ~ Treat + Mut + Mut:Treat, data = data_long)
C.test(hypo_model)

gad(hypo_model)
