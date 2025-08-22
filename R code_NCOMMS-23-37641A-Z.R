# --------------------- Install and load packages ------------------

packages <- c("dplyr", "tableone", "ggplot2", "tidyr", "readr", "tibble",
              
              "stringr", "lubridate", "caret", "tidyverse", "ggplot",
              
              "shiny", "rmarkdown", "knitr", "magrittr",
              
              "data.table", "Rcpp", "devtools", "R6",
              
              "shinydashboard", "leaflet", "readxl", "psych", "ggpubr", "lattice", "plotly", "plyr", "dplyr", "gridExtra", "ggtext")



# Install packages if not already installed

install.packages(setdiff(packages, rownames(installed.packages())))

install.packages("gglot")


# Load installed packages

lapply(packages, library, character.only = TRUE)


#---------------------------Upload ex vivo TBR data-----------------------

alldata <- read_excel("~/Desktop/PhD data/Guided by Light hoofdproject/TBR all values.xlsx") # data is available in "Data Source File, tab TBR ex vivo (bread loaves)"
View(alldata)

alldata <- alldata %>% mutate(dose = as.factor(Dose))
attach(alldata)

tapply(alldata$`Gewogen TBR lamellen`, alldata$Dose, summary)
tapply(alldata$`TBR vers`, alldata$Dose, summary)

kruskal.test(`Gewogen TBR lamellen`~dose, data = alldata)
kruskal.test(`TBR vers`~dose, data = alldata)


#--------------------------- Figure 4 (weighed TBR) --------------------------


figTBRgewogen <- ggplot(data = alldata, aes(y = `Gewogen TBR lamellen`, x = Dose, fill = Dose)) + 
  # Zwarte foutbalken (whiskers)
  stat_boxplot(geom = "errorbar", linewidth = 0.75, width = 0.4, color = "black") +
  
  # Zwarte omranding van boxplots
  geom_boxplot(fatten = 1.5, linewidth = 0.75, color = "black", 
               coef = 1.5, outlier.shape = NA) +
  
  theme_classic() +
  labs(x = "Dose (mg/kg)", y = "Tumor-to-Background Ratio") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none",
    axis.text = element_text(face = "bold", colour = "black", size = 11),
    axis.title = element_text(face = "bold", colour = "black"),
    axis.ticks = element_line(colour = "black", linewidth = 1),
    axis.line = element_line(linewidth = 1, colour = "black")
  ) +
  scale_fill_manual(values = rep("#e5e5fe", length(unique(alldata$Dose)))) +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(2, 10), expand = expansion(mult = c(0, 0))) +
  scale_x_discrete(expand = expansion(mult = c(0.25, 0.25)))

figTBRgewogen


# Some minor changes to figure aesthetics

figTBRgewogen <- ggplot(data = alldata, aes(y = `Gewogen TBR lamellen`, x = Dose, fill = Dose)) + 
  # Zwarte foutbalken (whiskers)
  stat_boxplot(geom = "errorbar", linewidth = 0.75, width = 0.4, color = "black") +
  
  # Zwarte omranding van boxplots
  geom_boxplot(fatten = 1.5, linewidth = 0.75, color = "black", 
               coef = 1.5, outlier.shape = NA) +
  
  theme_classic() +
  labs(x = "Dose (mg/kg)", y = "Tumor-to-Background Ratio") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none",
    axis.text = element_text(face = "bold", colour = "black", size = 11),
    axis.title = element_text(face = "bold", colour = "black"),
    axis.ticks = element_line(colour = "black", linewidth = 1),
    axis.line = element_line(linewidth = 1, colour = "black")
  ) +
  scale_fill_manual(values = rep("#cce5ff", length(unique(alldata$Dose)))) +  # Aangepaste lichtblauwe kleur
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 10), expand = expansion(mult = c(0, 0))) +
  scale_x_discrete(expand = expansion(mult = c(0.25, 0.25)))

figTBRgewogen


#---------------------------Differences in tumor and background MFI-------------------------
install.packages("dplyr")

# Load required libraries
library(readxl)
library(dplyr)

# Path to your Excel file
file_path <- "~/Desktop/PhD data/Guided by Light hoofdproject/mfi.xlsx"  # data is available in "Data Source File, tab MFI ex vivo (bread loaves)" 

# Read the Excel file
data <- read_excel(file_path)

# Convert 'mfi gewogen' to numeric (replace comma by dot first)
data$`mfi gewogen` <- as.numeric(gsub(",", ".", data$`mfi gewogen`))

# Check data structure
str(data)

# Calculate summary stats by group
summary_stats <- data %>%
  group_by(type) %>%
  summarise(
    mean_mfi = mean(`mfi gewogen`, na.rm = TRUE),
    sd_mfi = sd(`mfi gewogen`, na.rm = TRUE),
    n = n()
  )

print(summary_stats)

# Filter rijen waar 'mfi gewogen' niet NA is
data_clean <- data %>% filter(!is.na(`mfi gewogen`))

# T-toets uitvoeren
t_test_result <- t.test(`mfi gewogen` ~ type, data = data_clean)

# Resultaat printen
print(t_test_result)


# nu enkel voor de 0.025 dosering

library(readxl)
library(dplyr)

# Load your data
data <- read_excel("~/Desktop/PhD data/Guided by Light hoofdproject/mfi.xlsx")

# Fix decimal commas
data$`mfi gewogen` <- as.numeric(gsub(",", ".", data$`mfi gewogen`))

# Filter for dose = 0.025 and remove NAs
data_025 <- data %>%
  filter(dose == 0.025, !is.na(`mfi gewogen`))

# Summary stats per group (Tumor vs Background)
summary_stats_025 <- data_025 %>%
  group_by(type) %>%
  summarise(
    mean_mfi = mean(`mfi gewogen`, na.rm = TRUE),
    sd_mfi = sd(`mfi gewogen`, na.rm = TRUE),
    n = dplyr::n()
  )

print(summary_stats_025)

# T-test between Tumor and Background for dose = 0.025
t_test_result_025 <- t.test(`mfi gewogen` ~ type, data = data_025)
print(t_test_result_025)



#--------------------------- Supplementary Figure 3 (Weighed MFI) ------------------------
FigMFIgewogen <- ggplot(data = MFI, aes(x=dose, y=`mfi gewogen`, fill=type), na.rm=T) +
  stat_boxplot(geom = "errorbar", size = 0.8, width=0.4, position=position_dodge(0.7)) +
  geom_boxplot(width=0.7, fatten=NULL, size = 0.8) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.64, size = 0.8, linetype = "solid",position=position_dodge(0.7)) +
  theme_classic() +
  guides(fill=guide_legend(title= "Tissue")) +
  labs(x="Dose (mg/kg)", y = "Mean Fluorescence Intensity (a.u.)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5,face = "bold"),
        legend.title = element_blank(), legend.text = element_text(face="bold")) +
  theme(legend.position = c(0.2, 0.9)) +
  scale_y_continuous(breaks = seq(0,0.5, 0.1), limits = c(-0.005, 0.5), expand = expansion(mult = c(0,0))) +
  scale_x_discrete(expand = expansion(mult = c(0.25,0.25))) +
  theme(axis.text = element_text(face="bold", colour = "black", size = 11), axis.title = element_text(face="bold"),
        axis.ticks = element_line(colour = "black", size = 1), axis.line = element_line(linewidth = 1))


FigMFIgewogen


# Some minor changes to figure aesthetics


FigMFIgewogen <- ggplot(data = MFI, aes(x = dose, y = `mfi gewogen`, fill = type), na.rm = TRUE) +
  stat_boxplot(geom = "errorbar", size = 0.8, width = 0.4, position = position_dodge(0.7)) +
  geom_boxplot(width = 0.7, fatten = NULL, size = 0.8) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.64, size = 0.8, linetype = "solid", position = position_dodge(0.7)) +
  theme_classic() +
  guides(fill = guide_legend(title = "Tissue")) +
  labs(x = "Dose (mg/kg)", y = "MFI (a.u.)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(face = "bold"),
    legend.position = c(0.2, 0.9),
    axis.text = element_text(face = "bold", colour = "black", size = 11),
    axis.title = element_text(face = "bold"),
    axis.ticks = element_line(colour = "black", size = 1),
    axis.line = element_line(linewidth = 1)
  ) +
  scale_y_continuous(breaks = seq(0, 0.5, 0.025), limits = c(0, 0.5), expand = expansion(mult = c(0, 0))) +
  scale_x_discrete(expand = expansion(mult = c(0.25, 0.25)))

FigMFIgewogen


#--------------------------- Load and interpret data Figure 2 (spectroscopy data) ----------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
install.packages("readxl")
install.packages("dplyr")  
install.packages("ggplot2")  
install.packages("ggpubr")  



dom <- read_excel("~/Desktop/PhD data/Guided by Light hoofdproject/Data MDSFR SFF.xlsx", 
                  na = c(NA, "NA", " ", "", "-99", "-99.00", "-96", "-", "nvt", "97.00", -95.00)) # data is available in "Data Source File, tab MDSFR SFF data (in vivo)" 


dom <- dom %>% mutate(Dose = as.factor(Dose),
                      Time = as.factor(Time),
                      Tissue = as.factor(Tissue))

dom <- dom %>% mutate(Intensity = as.numeric(Intensity))

dom$Tissue <- factor(dom$Tissue, levels= c("Tumor", "Background"))


#--------------------------- Figure 2 (spectroscopy data) ---------------------------


# Zorg dat de Time-kolom in df.ratio een factor is in de juiste volgorde
df.ratio$Time <- factor(as.character(df.ratio$Time), levels = c("0.5", "2", "4", "18"))

# Plot
p <- ggplot() +
  geom_boxplot(
    data = dom,
    aes(x = Time, y = Intensity, fill = Tissue),
    width = 0.6,
    size = 0.5,
    position = position_dodge(0.6),
    outlier.shape = NA,
    coef = 1.5
  ) +
  scale_x_discrete(limits = c("0.5", "2", "4", "18")) +
  scale_y_continuous(
    name = "Intrinsic Fluorescence (Q.mfa,x[mm-1])",
    sec.axis = sec_axis(~./sec_range, name = "Tumor-to-Background Ratio", breaks = seq(0, 3, 0.5))
  ) +
  geom_line(
    data = df.ratio,
    aes(x = Time, y = Ratio * sec_range),
    color = "blue",
    group = 1,
    size = 1.1
  ) +
  ggtitle(expression("Temporal " * italic("in vivo") * " MDSFR/SFF spectroscopy")) +
  theme_classic() +
  theme(
    aspect.ratio = 0.7,
    plot.margin = margin(30, 20, 30, 50),
    
    # As-titels
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold", size = 10, margin = margin(0, 10, 0, 0)),
    axis.title.y.right = element_text(face = "bold", colour = "blue", size = 10, margin = margin(0, 10, 0, 0)),
    
    # Titel blijft klein
    plot.title = element_text(face = "bold", size = 10, hjust = 0.5, margin = margin(b = 10)),
    
    # Legenda op gewenste positie (alleen omhoog verplaatst)
    legend.title = element_blank(),
    legend.text = element_text(face = "bold", size = 9),
    legend.position = c(0.08, 0.85),
    legend.justification = c(0.5, 0.5),
    legend.key.size = unit(0.6, "cm"),
    
    # Asopmaak
    axis.text = element_text(face = "bold", colour = "black", size = 11),
    axis.text.y.right = element_text(face = "bold", colour = "blue", size = 11),
    axis.ticks = element_line(colour = "black", size = 1),
    axis.line = element_line(linewidth = 1),
    axis.line.y.right = element_line(colour = "blue", size = 1),
    axis.ticks.y.right = element_line(colour = "blue", size = 1)
  ) +
  facet_wrap(~Dose, labeller = labeller(Dose = function(x) paste(x, "mg/kg"))) +
  labs(x = "Time (h)")

# Opslaan als afbeelding
ggsave("~/Desktop/MDSFR SFF plot.png", plot = p, width = 12, height = 8, dpi = 300)




# Plot including horizontal line of threshold TBR 2.0 


library(ggplot2)
library(grid)  # voor unit()

# Zorg dat Time een factor is met juiste volgorde
dom$Time <- factor(dom$Time, levels = c("0.5", "2", "4", "18"))

# Plot maken
p2 <- ggplot() +
  geom_boxplot(
    data = dom,
    aes(x = Time, y = Intensity, fill = Tissue),
    width = 0.6,
    size = 0.5,
    position = position_dodge(0.6),
    outlier.shape = NA,
    coef = 1.5
  ) +
  scale_x_discrete(limits = c("0.5", "2", "4", "18")) +
  scale_y_continuous(
    name = "Intrinsic Fluorescence (Q.mfa,x[mm-1])",
    sec.axis = sec_axis(~./sec_range, name = "Tumor-to-Background Ratio", breaks = seq(0, 3, 0.5))
  ) +
  geom_line(
    data = df.ratio,
    aes(x = Time, y = Ratio * sec_range),
    color = "blue",
    group = 1,
    size = 1.1
  ) +
  # Lichte horizontale stippellijn op Tumor-to-Background Ratio = 2.0
  geom_hline(
    yintercept = 2.0 * sec_range,
    linetype = "dashed",
    color = "grey70",
    size = 0.3
  ) +
  ggtitle(expression("Temporal " * italic("in vivo") * " MDSFR/SFF spectroscopy")) +
  theme_classic() +
  theme(
    aspect.ratio = 0.7,
    plot.margin = margin(30, 20, 30, 50),
    
    # As-titels
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold", size = 10, margin = margin(0, 10, 0, 0)),
    axis.title.y.right = element_text(face = "bold", colour = "blue", size = 10, margin = margin(0, 10, 0, 0)),
    
    # Titel blijft klein
    plot.title = element_text(face = "bold", size = 10, hjust = 0.5, margin = margin(b = 10)),
    
    # Legenda op gewenste positie (alleen omhoog verplaatst)
    legend.title = element_blank(),
    legend.text = element_text(face = "bold", size = 9),
    legend.position = c(0.08, 0.85),
    legend.justification = c(0.5, 0.5),
    legend.key.size = unit(0.6, "cm"),
    
    # Asopmaak
    axis.text = element_text(face = "bold", colour = "black", size = 11),
    axis.text.y.right = element_text(face = "bold", colour = "blue", size = 11),
    axis.ticks = element_line(colour = "black", size = 1),
    axis.line = element_line(linewidth = 1),
    axis.line.y.right = element_line(colour = "blue", size = 1),
    axis.ticks.y.right = element_line(colour = "blue", size = 1)
  ) +
  facet_wrap(~Dose, labeller = labeller(Dose = function(x) paste(x, "mg/kg"))) +
  labs(x = "Time (h)")

# Opslaan als afbeelding
ggsave("~/Desktop/MDSFR SFF plot definitief.png", plot = p2, width = 12, height = 8, dpi = 300)



# Zorg dat Time een factor is met juiste volgorde
dom$Time <- factor(dom$Time, levels = c("0.5", "2", "4", "18"))
df.ratio$Time <- factor(as.character(df.ratio$Time), levels = c("0.5", "2", "4", "18"))  # <- Belangrijk!

# Plot maken
p2 <- ggplot() +
  geom_boxplot(
    data = dom,
    aes(x = Time, y = Intensity, fill = Tissue),
    width = 0.6,
    size = 0.5,
    position = position_dodge(0.6),
    outlier.shape = NA,
    coef = 1.5
  ) +
  scale_x_discrete(limits = c("0.5", "2", "4", "18")) +
  scale_y_continuous(
    name = "Intrinsic Fluorescence (Q.mfa,x[mm-1])",
    sec.axis = sec_axis(~./sec_range, name = "Tumor-to-Background Ratio", breaks = seq(0, 3, 0.5))
  ) +
  geom_line(
    data = df.ratio,
    aes(x = Time, y = Ratio * sec_range),
    color = "blue",
    group = 1,
    size = 1.1
  ) +
  geom_hline(
    yintercept = 2.0 * sec_range,
    linetype = "dashed",
    color = "grey70",
    size = 0.3
  ) +
  ggtitle(expression("Temporal " * italic("in vivo") * " MDSFR/SFF spectroscopy")) +
  theme_classic() +
  theme(
    aspect.ratio = 0.7,
    plot.margin = margin(30, 20, 30, 50),
    
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold", size = 10, margin = margin(0, 10, 0, 0)),
    axis.title.y.right = element_text(face = "bold", colour = "blue", size = 10, margin = margin(0, 10, 0, 0)),
    
    plot.title = element_text(face = "bold", size = 10, hjust = 0.5, margin = margin(b = 10)),
    
    legend.title = element_blank(),
    legend.text = element_text(face = "bold", size = 9),
    legend.position = c(0.08, 0.85),
    legend.justification = c(0.5, 0.5),
    legend.key.size = unit(0.6, "cm"),
    
    axis.text = element_text(face = "bold", colour = "black", size = 11),
    axis.text.y.right = element_text(face = "bold", colour = "blue", size = 11),
    axis.ticks = element_line(colour = "black", size = 1),
    axis.line = element_line(linewidth = 1),
    axis.line.y.right = element_line(colour = "blue", size = 1),
    axis.ticks.y.right = element_line(colour = "blue", size = 1)
  ) +
  facet_wrap(~Dose, labeller = labeller(Dose = function(x) paste(x, "mg/kg"))) +
  labs(x = "Time (h)")

# Opslaan als afbeelding
ggsave("~/Desktop/MDSFR SFF plot definitief.png", plot = p2, width = 12, height = 8, dpi = 300)

# other option

# Zorg dat Time een factor is met juiste volgorde
dom$Time <- factor(dom$Time, levels = c("0.5", "2", "4", "18"))
df.ratio$Time <- factor(as.character(df.ratio$Time), levels = c("0.5", "2", "4", "18"))

# Plot maken
p2 <- ggplot() +
  geom_boxplot(
    data = dom,
    aes(x = Time, y = Intensity, fill = Tissue),
    width = 0.6,
    size = 0.5,
    position = position_dodge(0.6),
    outlier.shape = NA,
    coef = 1.5
  ) +
  scale_x_discrete(limits = c("0.5", "2", "4", "18")) +
  scale_y_continuous(
    name = "Intrinsic Fluorescence (Q.mfa,x[mm-1])",
    sec.axis = sec_axis(~./sec_range, name = "Tumor-to-Background Ratio", breaks = seq(0, 3, 0.5))
  ) +
  geom_line(
    data = df.ratio,
    aes(x = Time, y = Ratio * sec_range),
    color = "blue",
    group = 1,
    size = 1.1
  ) +
  # Dikkere blauwe stippellijn bij TBR = 2
  geom_hline(
    yintercept = 2.0 * sec_range,
    linetype = "dashed",
    color = "blue",
    size = 0.6
  ) +
  ggtitle(expression("Temporal " * italic("in vivo") * " MDSFR/SFF spectroscopy")) +
  theme_classic() +
  theme(
    aspect.ratio = 0.7,
    plot.margin = margin(30, 20, 30, 50),
    
    # As-titels
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold", size = 10, margin = margin(0, 10, 0, 0)),
    axis.title.y.right = element_text(face = "bold", colour = "blue", size = 10, margin = margin(0, 10, 0, 0)),
    
    # Titel blijft klein
    plot.title = element_text(face = "bold", size = 10, hjust = 0.5, margin = margin(b = 10)),
    
    # Legenda op gewenste positie (alleen omhoog verplaatst)
    legend.title = element_blank(),
    legend.text = element_text(face = "bold", size = 9),
    legend.position = c(0.08, 0.85),
    legend.justification = c(0.5, 0.5),
    legend.key.size = unit(0.6, "cm"),
    
    # Asopmaak
    axis.text = element_text(face = "bold", colour = "black", size = 11),
    axis.text.y.right = element_text(face = "bold", colour = "blue", size = 11),
    axis.ticks = element_line(colour = "black", size = 1),
    axis.line = element_line(linewidth = 1),
    axis.line.y.right = element_line(colour = "blue", size = 1),
    axis.ticks.y.right = element_line(colour = "blue", size = 1)
  ) +
  facet_wrap(~Dose, labeller = labeller(Dose = function(x) paste(x, "mg/kg"))) +
  labs(x = "Time (h)")

# Opslaan als afbeelding
ggsave("~/Desktop/MDSFR SFF plot definitief.png", plot = p2, width = 12, height = 8, dpi = 300)


#-------------- Exact TBRs as mentioned in main text ---------------------

TBR <- read.csv("TBR.csv", sep = ";", dec = ".",
                na.strings = c(NA, "NA", " ", "", "-99", "-99.00", "-96", "-", "nvt", "97.00", -95.00),
                stringsAsFactors = T)

TBR1 <- subset(TBR, Dose == "0.01")
TBR2 <- subset(TBR, Dose == "0.025")
TBR3 <- subset(TBR, Dose == "0.05")

kruskal.test(data = TBR1, TBR~Time)
kruskal.test(data = TBR2, TBR~Time)
kruskal.test(data = TBR3, TBR~Time)

TBR4 <- subset(TBR, Time == "0.5")
TBR5 <- subset(TBR, Time == "2")
TBR6 <- subset(TBR, Time == "4")
TBR7 <- subset(TBR, Time == "18")

kruskal.test(data=TBR4, TBR~Dose)
kruskal.test(data=TBR5, TBR~Dose)
kruskal.test(data=TBR6, TBR~Dose)
kruskal.test(data=TBR7, TBR~Dose)

#----Verschil aantal inadequate marges------

# Verschil inadequate marges obv gehele cohort
# 24 van de 31 patienten hadden intraoperatief inadequate marge, 16 van de 31 patienten hadden postoperatief inadequate marge  
tabel <- matrix(c(24, 7, 16, 15), nrow = 2, byrow = TRUE)

# Fisher's exact test uitvoeren
fisher.test(tabel)

# Verschil inadequate marges obv optimale dosering
# 15 van de 21 patienten hadden intraoperatief inadequate marge, 10 van de 21 patienten hadden postoperatief inadequate marge
tabel2 <- matrix(c(16, 5, 10, 11), nrow = 2, byrow = TRUE)

# Fisher's exact test uitvoeren
fisher.test(tabel2)


#Andere optie: met FLI zijn 25 van de 25 inadequate marges gevonden, met IOA 17 van de 25
install.packages("exact2x2")
library(exact2x2)

tabel <- matrix(c(14, 9, 0, 0), nrow = 2, byrow = TRUE)
mcnemar.exact(tabel)

#Andere optie: met FLI 15 van de 15 inadequate marges gevonden, met IOA 9 van de 15
tabel <- matrix(c(9, 6, 0, 0), nrow = 2, byrow = TRUE)
mcnemar.exact(tabel)


#---------- Mean TBR lamellen---------

# Laad de benodigde packages
library(readxl)
library(dplyr)


# Filter de rijen met dose == 0.025 en bereken mean + SD
resultaat <- alldata %>%
  filter(Dose == 0.025) %>%
  summarise(
    Mean_TBR = mean(`Gewogen TBR lamellen`, na.rm = TRUE),
    SD_TBR = sd(`Gewogen TBR lamellen`, na.rm = TRUE)
  )

# Print het resultaat
print(resultaat)


#----- Sample size sufficient------

# p-waarde
# Geobserveerde gegevens
n <- 21      # Aantal patiënten
x <- 11      # Aantal adequate marges
p0 <- 0.15   # Nulhypothese proportie

# Exacte binomiale test (éénzijdig, alternatief: p > 0.15)
binom.test(x, n, p = p0, alternative = "greater")

install.packages("pwr")
# Poweranalyse (achteraf), met de geobserveerde proportie van 11/21
library(pwr)

# Verwachte effectgrootte volgens H1: p = 0.40
# Effectgrootte berekend als Cohen's h
p1 <- 0.40
h <- ES.h(p1, p0)

# Poweranalyse met huidige steekproefgrootte
pwr.p.test(h = h, n = n, sig.level = 0.05, alternative = "greater")


#------ Power calculations -------

# Geobserveerde data

n <- 14        # totaal aantal patiënten

x <- 7        # aantal successen (adequate marges)

p0 <- 0.15     # nulhypothese: maximaal 15% adequate marges



# Eenzijdige binomiale test (H0: p <= 0.15, H1: p > 0.15)

binom.test(x, n, p = p0, alternative = "greater")


# Parameters

p_true <- 0.60  # veronderstelde werkelijke kans

threshold <- 7  # minimale aantal successen voor "effectief"



# Power = kans op ≥ 7 successen onder p = 0.60

power <- 1 - pbinom(threshold - 1, size = n, prob = p_true)

cat("Power bij p = 0.62:", round(power, 4), "\n")


n <- 15

threshold <- 7

p_H1 <- 0.40



power_H1 <- 1 - pbinom(threshold - 1, size = n, prob = p_H1)

cat("Power bij p = 0.40:", round(power_H1, 4), "\n")

