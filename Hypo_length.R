#APB Prac 3
# Pat Bragato

#Libraries ----
library(dplyr)
library(tidyverse)
library(GAD)

# Data Wrangling ----

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

# Box Plot ----
(boxplot <- ggplot(data_long, aes (x = treatment, length)) +
   geom_boxplot(aes(fill = Mutant)) +
   theme_classic() +
   labs(x = "\nTreatment", y = "Hypocotyl Length (mm)\n"))

ggsave(filename = "Hypo_boxplot.png", device = "png", 
       width = 8, height = 5)

# Bar Plot ----
averages <- data_long %>% 
  group_by(treatment, Mutant) %>% 
  summarise(average = mean(length), error = sd(length))


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

# Mutant A ----
MA <- filter(data_long, Mutant == "a")
TreatA <- as.fixed(MA$treatment)


a_model <- lm(length ~ TreatA, data = MA)
C.test(a_model)

gad(a_model)


# Mutant B ----
MB <- filter(data_long, Mutant == "b")
TreatB <- as.fixed(MB$treatment)


b_model <- lm(length ~ TreatB, data = MB)
C.test(b_model)

gad(b_model)

