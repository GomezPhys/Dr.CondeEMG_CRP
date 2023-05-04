### Required packages
library(readxl) ## To load excel sheet
library(rlang) 

library(kableExtra) #tables

library(ggplot2) #data visualization
library(ggpubr)#data visualization
library(ggprism)##makes plots look like graphad


###
library(readxl)
EMG_CRP <- read_excel("~/Ergonomics Lab (PERROS 69)/EMG_CRP.xlsx", sheet = "EMG")

View(EMG_CRP)







###Convert from character to factor data
EMG_CRP$Site <- as.factor(EMG_CRP$Site)
EMG_CRP$Condition <- as.factor(EMG_CRP$Condition)
EMG_CRP$Day <- as.factor(EMG_CRP$Day)

###Order Day
EMG_CRP$Day <- ordered(EMG_CRP$Day, levels = c("1", "2", "3", "4", "5"))

###Order Condtion
EMG_CRP$Condition <- ordered(EMG_CRP$Condition, levels = c("Beginning", "End"))

###Order Site
EMG_CRP$Site <- ordered(EMG_CRP$Site, levels = c("ECRL", "Shoulder"))

###
###### Linear Mixed models Voltage
lmModel = lmer(Voltage ~ Condition + Site + (1|Subject),
               data=EMG_CRP, REML=FALSE)
summary(lmModel)






# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- EMG_CRP %>%
  pairwise_t_test(Voltage ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")
pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
EMG_CRP %>% cohens_d(Voltage ~ Condition,
                paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

##plots
Concentration_plot <- ggboxplot(EMG_CRP, x = "Site", y = "Voltage", 
                                color = "Day", fill = "Condition", palette =get_palette("Set1", 4), 
                                ylab = "Voltage") + stat_pvalue_manual(pwc, size = 4.5, hide.ns = TRUE) 
+ theme_prism()

ggboxplot(Accurate, x = "Day", y = "Condition",
          color = "Day", palette = get_palette("Set1",4)
          ylab = "Condition") + facet_grid(~Condition)

library(ggpubr)

# Convert from character to factor data
EMG_CRP$Site <- as.factor(EMG_CRP$Site)
EMG_CRP$Condition <- as.factor(EMG_CRP$Condition)
EMG_CRP$Day <- as.factor(EMG_CRP$Day)

# Order Day
EMG_CRP$Day <- ordered(EMG_CRP$Day, levels = c("1", "2", "3", "4", "5"))

# Order Condtion
EMG_CRP$Condition <- ordered(EMG_CRP$Condition, levels = c("Beginning", "End"))

# Order Site
EMG_CRP$Site <- ordered(EMG_CRP$Site, levels = c("ECRL", "Shoulder"))

library(ggpubr)

install.packages("ggpubr")
library(ggpubr)
ggboxplot(EMG_CRP)
print(EMG_CRP)

class(EMG_CRP$Voltage)


# Create boxplot
ggboxplot(EMG_CRP, x = "Site", y = "Voltage")

Concentration_plot <- ggboxplot(EMG_CRP, x = "Site", y = "Voltage(mV)", 
                                color = "Day", fill = "Condition", palette =get_palette("Set1", 4), 
                                ylab = "Voltage") + 
  theme_prism() +
  stat_pvalue_manual(pwc, size = 4.5, hide.ns = TRUE)


# Display plot
Concentration_plot


# Print plot
Concentration_plot

library(ggplot2)

ggplot(EMG_CRP, aes(x = Site, y = Voltage, fill = Site)) +
  geom_boxplot() +
  labs(x = "Day", y = "Voltage") +
  facet_wrap(~ Day)

EMG_CRP$Condition <- factor(EMG_CRP$Condition, levels = c("Beginning", "End"))

##sm
ggplot(EMG_CRP, aes(x = Site, y = Voltage, fill = Site)) +
  geom_boxplot() +
  labs(x = "Day", y = "Voltage") +
  facet_wrap(~ Day)


Concentration_plot <- ggplot(EMG_CRP, aes(x = Site, y = Voltage, fill = Condition)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_errorbar(aes(ymin = Voltage - sd, ymax = Voltage + sd), width = 0.2, position = "dodge") +
  facet_grid(rows = vars(Day), scales = "free") +
  labs(x = "Site", y = "Voltage", fill = "Condition") +
  ggtitle("EMG CRP Concentration by Site and Condition") +
  theme_prism()
Concentration_plot


which(!is.numeric(EMG_CRP$Voltage))

library(dplyr)

EMG_CRP_summary <- EMG_CRP %>%
  group_by(Site, Condition, Day) %>%
  summarise(sd = sd(Voltage))

Concentration_plot <- ggplot(EMG_CRP, aes(x = Site, y = Voltage, fill = Condition)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_errorbar(aes(ymin = Voltage - sd, ymax = Voltage + sd), width = 0.2, position = "dodge") +
  facet_grid(rows = vars(Day), scales = "free") +
  labs(x = "Site", y = "Voltage", fill = "Condition") +
  ggtitle("EMG CRP Concentration by Site and Condition") +
  theme_prism()
Concentration_plot

