packages <- c("dplyr", "tableone", "ggplot2", "tidyr", "readr", "tibble",
              
              "stringr", "lubridate", "caret", "tidyverse",
              
              "shiny", "rmarkdown", "knitr", "magrittr", "binom",
              
              "data.table", "Rcpp", "devtools", "R6",
              
              "shinydashboard", "leaflet", "readxl", "psych", "ggpubr", "lattice", "plotly", "plyr", "dplyr", "gridExtra", "ggtext")


# Install packages if not already installed

install.packages(setdiff(packages, rownames(installed.packages())))



# Load installed packages

lapply(packages, library, character.only = TRUE)


#--------------------------- Mean gewogen ex vivo TBR voor alle doseringen -----------------------

alldata <- read_excel("~/Desktop/PhD data/Guided by Light hoofdproject/TBR all values.xlsx")
View(alldata)

alldata <- alldata %>% mutate(dose = as.factor(Dose))
attach(alldata)

# 1. Load required package
install.packages("readxl")     
library(readxl)

# 2. Read Excel file from your path
df <- read_excel("~/Desktop/PhD data/Guided by Light hoofdproject/TBR all values.xlsx")

# 3. Remove rows with missing values in the TBR column
df_clean <- na.omit(df)

# 4. Calculate mean and standard deviation per Dose
summary_stats <- aggregate(`Gewogen TBR lamellen` ~ Dose, data = df_clean, 
                           FUN = function(x) c(mean = mean(x), sd = sd(x)))

# 5. Clean up output
summary_stats <- do.call(data.frame, summary_stats)
colnames(summary_stats) <- c("Dose", "Mean_TBR", "SD_TBR")

# 6. Print result
print(summary_stats)

#--------------------------- Figuur gewogen ex vivo TBR --------------------------

install.packages("ggplot2")
library(ggplot2)

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



#-----------Nieuwe versie figuur TBR ex vivo 31-10-2025-----------

library(ggplot2)
alldata_clean <- alldata[!is.na(alldata$Dose) & !is.na(alldata$`Gewogen TBR lamellen`), ]
alldata_clean$Dose <- as.factor(alldata_clean$Dose)

figTBRgewogen <- ggplot(
  data = alldata_clean,
  aes(y = `Gewogen TBR lamellen`, x = Dose, fill = Dose)
) +
  stat_boxplot(geom = "errorbar", linewidth = 0.75, width = 0.4, color = "black") +
  geom_boxplot(
    fatten = 1.5,
    linewidth = 0.75,
    color = "black",
    coef = 1.5,
    outlier.shape = NA
  ) +
  theme_classic() +
  labs(
    x = "Dose (mg/kg)",
    y = "Tumor-to-Background Ratio",
    title = "Ex vivo fluorescence imaging (in bread loaves)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "italic", size = 12),
    legend.position = "none",
    axis.text = element_text(face = "bold", colour = "black", size = 10),
    axis.title = element_text(face = "bold", colour = "black"),
    axis.ticks = element_line(colour = "black", linewidth = 1),
    axis.line = element_line(linewidth = 1, colour = "black")
  ) +
  scale_fill_manual(values = c("#cce5ff", "#cce5ff", "#cce5ff")) +
  scale_y_continuous(
    breaks = seq(0, 10, 2),
    limits = c(0, 10),
    expand = expansion(mult = c(0, 0))
  )

figTBRgewogen


# ---------- Boxplots ex vivo TBR inclusief statistische test ------------
# Packages 
library(ggplot2)
library(dplyr)
library(ggpubr)

# 1️⃣ Data schoonmaken 
alldata_clean <- alldata %>%
  filter(!is.na(Dose), !is.na(`Gewogen TBR lamellen`)) %>%
  mutate(Dose = as.factor(Dose))

# 2️⃣ Pairwise Wilcoxon-tests 
pairwise_result <- pairwise.wilcox.test(
  x = alldata_clean$`Gewogen TBR lamellen`,
  g = alldata_clean$Dose,
  p.adjust.method = "BH"
)
pairwise_result

# 3️⃣ Plot met ggplot2 en ggpubr 
figTBRgewogen <- ggplot(
  data = alldata_clean,
  aes(y = `Gewogen TBR lamellen`, x = Dose, fill = Dose)
) +
  stat_boxplot(geom = "errorbar", linewidth = 0.75, width = 0.4, color = "black") +
  geom_boxplot(
    fatten = 1.5,
    linewidth = 0.75,
    color = "black",
    coef = 1.5,
    outlier.shape = NA
  ) +
  theme_classic() +
  labs(
    x = "Dose (mg/kg)",
    y = "Tumor-to-Background Ratio"
    # Titel weggelaten
  ) +
  theme(
    legend.position = "none",
    axis.text = element_text(face = "bold", colour = "black", size = 10),
    axis.title = element_text(face = "bold", colour = "black"),
    axis.ticks = element_line(colour = "black", linewidth = 1),
    axis.line = element_line(linewidth = 1, colour = "black")
  ) +
  scale_fill_manual(values = c("#cce5ff", "#cce5ff", "#cce5ff")) +
  scale_y_continuous(
    breaks = seq(0, 14, 2),
    limits = c(0, 14),
    expand = expansion(mult = c(0, 0))
  ) +
  
  # 4️⃣ Alle pairwise vergelijkingen annoteren met specifieke hoogtes
stat_compare_means(
  comparisons = list(
    c("0.01", "0.025"),
    c("0.01", "0.05"),
    c("0.025", "0.05")
  ),
  method = "wilcox.test",
  p.adjust.method = "BH",
  label = "p.signif", 
  label.y = c(11.5, 12.7, 12), # hoogtes per vergelijking
  size = 3 # grootte van de ns-labels verkleind
)

# 5️⃣ Plot weergeven
figTBRgewogen


#---------------------------figuur vers TBR---------------
figTBRvers <- ggplot(data = alldata, aes(y = TBR.vers, x = dose, fill=dose)) + 
  stat_boxplot(geom = "errorbar", size = 0.75, width=0.4) +
  geom_boxplot(fatten=NULL, size = 0.75) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 0.75, linetype = "solid") +
  theme_classic() +
  labs(title="Mean TBR per dose", subtitle = "Fresh specimen (Tumor-to-Mucosa)", x="Dose cRGD-ZW8000-1 (mg/kg)", y = "TBR") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.position="none") +
  scale_fill_manual(values = c("#617E62", "#7FA280", "#9BC69D")) +
  scale_y_continuous(breaks = seq(1,5, 1), limits = c(0.9,5), expand = expansion(mult = c(0,0))) +
  scale_x_discrete(expand = expansion(mult = c(0.25,0.25))) +
  theme(axis.text = element_text(face="bold", colour = "black"), axis.title = element_text(face="bold"),
        axis.ticks = element_line(colour = "black", size = 1), axis.line = element_line(linewidth = 1))

figTBRvers
#---------------------------verschil tumor en background MFI-------------------------
install.packages("dplyr")

# Load required libraries
library(readxl)
library(dplyr)

# Path to your Excel file
file_path <- "~/Desktop/PhD data/Guided by Light hoofdproject/mfi.xlsx"

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



#---------------------------figuur mfi gewogen------------------------
install.packages("readxl")     
library(readxl)

MFI <- read_excel("~/Desktop/PhD data/Guided by Light hoofdproject/mfi.xlsx")


# Zorg dat 'type' factor is en de volgorde omdraaien
MFI$type <- factor(MFI$type, levels = c("Tumor", "Background"))  # gewenste volgorde

# Plot maken zonder extra markers
FigMFIgewogen <- ggplot(MFI, aes(x = dose, y = `mfi gewogen`, fill = type)) +
  stat_boxplot(
    geom = "errorbar",
    linewidth = 0.8,
    width = 0.4,
    position = position_dodge(0.7),
    na.rm = TRUE
  ) +
  geom_boxplot(
    width = 0.7,
    linewidth = 0.8,
    position = position_dodge(0.7),
    na.rm = TRUE
  ) +
  theme_classic() +
  guides(fill = guide_legend(title = "Tissue")) +
  labs(x = "Dose (mg/kg)", y = "Mean Fluorescence Intensity (a.u.)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.position = c(0.2, 0.9),
    axis.text = element_text(face = "bold", colour = "black", size = 11),
    axis.title = element_text(face = "bold"),
    axis.ticks = element_line(colour = "black", linewidth = 1),
    axis.line = element_line(linewidth = 1)
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.5, 0.1),
    limits = c(-0.005, 0.5),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.25, 0.25)))

# Plot tonen
FigMFIgewogen


#------------ figuur mfi gewogen inclusief statistische test-----------
library(readxl)
library(ggplot2)
library(ggpubr)

# Data inlezen
MFI <- read_excel("~/Desktop/PhD data/Guided by Light hoofdproject/mfi.xlsx")

# Zorg dat 'mfi gewogen' numeriek is
MFI$`mfi gewogen` <- as.numeric(MFI$`mfi gewogen`)

# Zet 'type' als factor in de gewenste volgorde
MFI$type <- factor(MFI$type, levels = c("Tumor", "Background"))

# Dodge breedte van de boxplots
dodge_width <- 0.7

# Numerieke x-waarden van doses
dose_levels <- levels(factor(MFI$dose))
dose_x <- setNames(seq_along(dose_levels), dose_levels)

# Dataframe met annotaties voor p-values
pvals <- data.frame(
  xmin = c(dose_x["0.01"] - dodge_width/2,
           dose_x["0.025"] - dodge_width/2,
           dose_x["0.05"] - dodge_width/2),
  xmax = c(dose_x["0.01"] + dodge_width/2,
           dose_x["0.025"] + dodge_width/2,
           dose_x["0.05"] + dodge_width/2),
  y.position = c(0.2, 0.45, 0.55),   # hoogte van de lijnen
  label = c("*", "**", "**"),
  group1 = c("Tumor", "Tumor", "Tumor"),
  group2 = c("Background", "Background", "Background")
)

# Plot maken
FigMFIgewogen <- ggplot(MFI, aes(x = dose, y = `mfi gewogen`, fill = type)) +
  stat_boxplot(
    geom = "errorbar",
    linewidth = 0.8,
    width = 0.4,
    position = position_dodge(dodge_width),
    na.rm = TRUE
  ) +
  geom_boxplot(
    width = 0.7,
    linewidth = 0.8,
    position = position_dodge(dodge_width),
    na.rm = TRUE
  ) +
  # stat_summary verwijderd om horizontale lijn in boxplots weg te halen
  stat_pvalue_manual(
    pvals,
    label = "label",
    xmin = "xmin",
    xmax = "xmax",
    y.position = "y.position",
    tip.length = 0.03,
    bracket.size = 0.8,
    inherit.aes = FALSE
  ) +
  theme_classic() +
  labs(
    x = "Dose (mg/kg)",
    y = "Mean Fluorescence Intensity (a.u.)",
    fill = NULL   # verwijdert titel van de legenda
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.position = c(0.2, 0.9),
    axis.text = element_text(face = "bold", colour = "black", size = 11),
    axis.title = element_text(face = "bold"),
    axis.ticks = element_line(colour = "black", linewidth = 1),
    axis.line = element_line(linewidth = 1)
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.6, 0.1),
    limits = c(-0.005, 0.6),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.25, 0.25)))

# Plot tonen
FigMFIgewogen


# p waarden
doses <- unique(MFI$dose)

# Lege dataframe om p-waarden op te slaan
p_values <- data.frame(Dose = character(), p_value = numeric(), stringsAsFactors = FALSE)

# Loop over doses en bereken Mann-Whitney U test (Wilcoxon rank-sum)
for(d in doses){
  subset_data <- MFI[MFI$dose == d, ]
  test <- wilcox.test(`mfi gewogen` ~ type, data = subset_data, exact = TRUE)
  p_values <- rbind(p_values, data.frame(Dose = d, p_value = test$p.value))
}

p_values


#---------------------------figuur mfi vers--------------------
FigMFIvers <- ggplot(data = MFI, aes(x=dose, y=mfi.vers, fill=type), na.rm=T) +
  stat_boxplot(geom = "errorbar", size = 0.8, width=0.6, position=position_dodge(0.9)) +
  geom_boxplot(width=0.9, fatten=NULL, size = 0.8) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.815, size = 0.8, linetype = "solid",position=position_dodge(0.897)) +
  theme_classic() +
  guides(fill=guide_legend(title= "Tissue")) +
  labs(title="Mean fluorescence intensity per dose", subtitle = "Fresh specimen", 
       x="Dose cRGD-ZW800-1 (mg/kg)", y = "MFI (a.u.)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5,face = "bold"),
        legend.title = element_text(face="bold"), legend.text = element_text(face="bold")) +
  theme(legend.position = c(0.15, 0.95)) +
  scale_y_continuous(breaks = seq(0,0.6, 0.1), limits = c(0, 0.6), expand = expansion(mult = c(0,0))) +
  scale_x_discrete(expand = expansion(mult = c(0.25,0.25))) +
  theme(axis.text = element_text(face="bold", colour = "black"), axis.title = element_text(face="bold"),
        axis.ticks = element_line(colour = "black", size = 1), axis.line = element_line(linewidth = 1))

FigMFIvers



#---------------------------figuren samen--------------------------

f1 <- grid.arrange (FigMFIgewogen, figTBRgewogen, ncol=2)
f2 <- grid.arrange (figTBRvers, FigMFIvers, ncol=2)

#---------------------------figuur Dom data omschrijven----------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
install.packages("readxl")
install.packages("dplyr")  
install.packages("ggplot2")  
install.packages("ggpubr")  



dom <- read_excel("~/Desktop/PhD data/Guided by Light hoofdproject/Data MDSFR SFF.xlsx", 
                  na = c(NA, "NA", " ", "", "-99", "-99.00", "-96", "-", "nvt", "97.00", -95.00))


dom <- dom %>% mutate(Dose = as.factor(Dose),
                      Time = as.factor(Time),
                      Tissue = as.factor(Tissue))

dom <- dom %>% mutate(Intensity = as.numeric(Intensity))

dom$Tissue <- factor(dom$Tissue, levels= c("Tumor", "Background"))
#---------------------------figuur maken---------------------------

df.ratio <- dom %>%
  group_by(Dose, Time) %>%
  summarize(Ratio = mean(Intensity[Tissue == "Tumor"]) / mean(Intensity[Tissue == "Background"]))%>%
  ungroup() %>%
  mutate(Ratio = as.numeric(Ratio))

View(df.ratio)

# Get the max value of the 'Intensity' variable
max_intensity <- max(dom$Intensity, na.rm = TRUE)

# Create a secondary axis range for the 'Ratio' variable
sec_range <- max_intensity / max(df.ratio$Ratio)


ggplot() +
  stat_boxplot(data = dom, aes(x = Time, y = Intensity, fill = Tissue),
               geom = "errorbar", size = 0.1, width = 0.4, position = position_dodge(0.6)) +
  geom_boxplot(data = dom, aes(x = Time, y = Intensity, fill = Tissue),
               width = 0.6, size = 0.5, position = position_dodge(0.6)) +
  scale_y_continuous(name = "Intrinsic Fluorescence (Q.mfa,x[mm-1])", 
                     sec.axis = sec_axis(~./sec_range, name = "Tumor-to-Background Ratio", breaks = seq(0,3,0.5))) +
  geom_line(data = df.ratio, aes(x = Time, y = Ratio * sec_range),
            color = "blue", group = 1, size = 1.1) +
  stat_compare_means(data = dom, aes(x = Time, y = Intensity, fill = Tissue,label = ..p.signif..),
                     method = "wilcox.test", label = "p", 
                     group.by = "Tissue") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.text = element_text(face = "bold"),
        axis.text = element_text(face = "bold", colour = "black", size = 11), 
        axis.text.y.right = element_text(face = "bold", colour = "blue", size = 11), 
        axis.title = element_text(face = "bold"),
        axis.title.y.right = element_text(face = "bold", colour = "blue"),
        axis.ticks = element_line(colour = "black", size = 1), 
        axis.line = element_line(linewidth = 1),
        axis.line.y.right = element_line(colour = "blue", size = 1),
        axis.ticks.y.right = element_line(colour = "blue", size = 1),
        legend.position = c(0.1, 0.87)) +
  facet_wrap(~Dose)


#----- Definitief figuur MDSFR SFF data-----------


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




#--------- Plot inclusief horizontale lijn TBR 2-------------


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




#---------------------------statistiek--------------

print(dom)
install.packages("rstatix")
library(rstatix)

dom_wilcox <- dom %>%
  group_by(Dose, Time) %>%
  wilcox_test(Intensity ~ Tissue)

View(dom_summary)
summary(dom$Intensity)

dom1 <- subset(dom, Dose == "0.01" & Time == "0.5" )
dom2 <- subset(dom, Dose == "0.01" & Time == "2" )
dom3 <- subset(dom, Dose == "0.025" & Time == "0.5" )
dom4 <- subset(dom, Dose == "0.025" & Time == "2" )
dom5 <- subset(dom, Dose == "0.025" & Time == "4" )
dom6 <- subset(dom, Dose == "0.025" & Time == "18" )

View(dom3)
wilcox.test(data=dom1, Intensity~Tissue, paired = FALSE)
wilcox.test(data=dom2, Intensity~Tissue, paired = FALSE)
wilcox.test(data=dom3, Intensity~Tissue, paired = FALSE)
wilcox.test(data=dom3, Intensity~Tissue, paired = FALSE)
wilcox.test(data=dom, Intensity~Tissue, paired = FALSE)
wilcox.test(data=dom5, Intensity~Tissue, paired = FALSE)
wilcox.test(data=dom6, Intensity~Tissue, paired = FALSE)

#toetsen tumor vs gezond per dosis en per tijdspunt
install.packages("tidyr")
library(tidyr)
library(rstatix)

# Create a summary table
dom_summary <- dom %>% 
  group_by(Dose, Tissue, Time) %>% 
  summarize(Mean_Intensity = mean(Intensity),
            SD = sd(Intensity),
            n = n()) %>% 
  pivot_wider(names_from = Tissue, values_from = c(Mean_Intensity, SD, n)) %>% 
  rename_all(~paste0(ifelse(. == "Time", "", .), ifelse(. == "Mean_Intensity", " Mean", ifelse(. == "SD", " SD", " n")))) %>%
  wilcox_test("Mean_Intensity_Tumor n" ~ "Mean_Intensity_Background n")

# Add p-values
dom_pvalues <- dom %>% 
  group_by(Dose, Tissue, Time) %>% 
  summarize(p_value = ifelse(sum(Tissue == "Tumor") < 2 | sum(Tissue == "Healthy") < 2, NA, formatC(wilcox.test(Intensity[Tissue == "Tumor"], Intensity[Tissue == "Healthy"])$p.value, digits = 2)))

dom_summary <- left_join(dom_summary, dom_pvalues, by = "Time")

# Print the summary table with p-values
dom_summary

#TBRs verschillend?


#------------------------------TBRs

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



#----- Sample size voldoende?------

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


#------- In vivo TBR-----

# Load the libraries
library(readxl)
library(dplyr)

# Read the Excel file (use full path and fix spacing if needed)
data <- read_excel("~/Desktop/PhD data/Guided by Light hoofdproject/TBR in vivo.xlsx")

# Convert columns to numeric, coercing non-numeric values to NA
data <- data %>%
  mutate(
    Exp38 = as.numeric(gsub(",", ".", Exp38)),  # handle comma-decimal if needed
    Exp50 = as.numeric(gsub(",", ".", Exp50))
  )

# Calculate median and IQR range (Q1, Q3) per Dose
summary_by_dose <- data %>%
  group_by(Dose) %>%
  summarise(
    Median_Exp38 = median(Exp38, na.rm = TRUE),
    Q1_Exp38 = quantile(Exp38, 0.25, na.rm = TRUE),
    Q3_Exp38 = quantile(Exp38, 0.75, na.rm = TRUE),
    
    Median_Exp50 = median(Exp50, na.rm = TRUE),
    Q1_Exp50 = quantile(Exp50, 0.25, na.rm = TRUE),
    Q3_Exp50 = quantile(Exp50, 0.75, na.rm = TRUE)
  )

# Calculate overall median and IQR range (Q1, Q3)
summary_overall <- data %>%
  summarise(
    Median_Exp38 = median(Exp38, na.rm = TRUE),
    Q1_Exp38 = quantile(Exp38, 0.25, na.rm = TRUE),
    Q3_Exp38 = quantile(Exp38, 0.75, na.rm = TRUE),
    
    Median_Exp50 = median(Exp50, na.rm = TRUE),
    Q1_Exp50 = quantile(Exp50, 0.25, na.rm = TRUE),
    Q3_Exp50 = quantile(Exp50, 0.75, na.rm = TRUE)
  )

# Print the results
print(summary_by_dose)
print(summary_overall)

# Install ggplot2 if you haven't already
install.packages("ggplot2")

# Load the library
library(ggplot2)


# Boxplot van Exp38 per Dose
library(ggplot2)
library(dplyr)

# Bereken boxplot-statistieken handmatig voor caps
box_stats <- data %>%
  group_by(Dose) %>%
  summarise(
    ymin = boxplot.stats(Exp38)$stats[1],  # Lower whisker
    lower = boxplot.stats(Exp38)$stats[2], # Q1
    middle = boxplot.stats(Exp38)$stats[3],# Median
    upper = boxplot.stats(Exp38)$stats[4], # Q3
    ymax = boxplot.stats(Exp38)$stats[5]   # Upper whisker
  ) %>%
  mutate(Dose = as.factor(Dose))

# Plot met boxplot en handmatige whisker caps
ggplot(data, aes(x = as.factor(Dose), y = Exp38)) +
  geom_boxplot(fill = "#cce5ff", color = "black", width = 0.6, outlier.shape = NA) +
  
  # Voeg horizontale streepjes toe aan uiteinden van whiskers
  geom_segment(data = box_stats,
               aes(x = as.numeric(Dose) - 0.15, xend = as.numeric(Dose) + 0.15,
                   y = ymin, yend = ymin), inherit.aes = FALSE, color = "black") +
  geom_segment(data = box_stats,
               aes(x = as.numeric(Dose) - 0.15, xend = as.numeric(Dose) + 0.15,
                   y = ymax, yend = ymax), inherit.aes = FALSE, color = "black") +
  
  labs(x = "Dose", y = "Tumor-to-Background Ratio") +
  theme_classic() +
  theme(
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_blank()
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(0, 10, by = 1)
  )






library(ggplot2)
library(dplyr)

# Bereken boxplot-statistieken handmatig voor caps
box_stats <- data %>%
  group_by(Dose) %>%
  summarise(
    ymin = boxplot.stats(Exp38)$stats[1],  # Lower whisker
    lower = boxplot.stats(Exp38)$stats[2], # Q1
    middle = boxplot.stats(Exp38)$stats[3],# Median
    upper = boxplot.stats(Exp38)$stats[4], # Q3
    ymax = boxplot.stats(Exp38)$stats[5]   # Upper whisker
  ) %>%
  mutate(Dose = as.factor(Dose))

# Plot met boxplot en handmatige whisker caps
ggplot(data, aes(x = as.factor(Dose), y = Exp38)) +
  geom_boxplot(fill = "#cce5ff", color = "black", width = 0.6, outlier.shape = NA) +
  geom_segment(data = box_stats,
               aes(x = as.numeric(Dose) - 0.15, xend = as.numeric(Dose) + 0.15,
                   y = ymin, yend = ymin), inherit.aes = FALSE, color = "black") +
  geom_segment(data = box_stats,
               aes(x = as.numeric(Dose) - 0.15, xend = as.numeric(Dose) + 0.15,
                   y = ymax, yend = ymax), inherit.aes = FALSE, color = "black") +
  labs(x = "Dose (mg/kg)", y = "Tumor-to-Background Ratio") +
  theme_classic() +
  theme(
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_blank(),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(0, 10, by = 1)
  )


#----------- Spectroscopie toename significant per tijdseenheid
# Zet Ratio expliciet om naar numeriek (als dat mogelijk is)
Intensity_over_time$Ratio <- as.numeric(Intensity_over_time$Ratio)

library(readxl)
library(dplyr)
library(tidyr)

# Data inlezen
Intensity_over_time <- read_excel("~/Desktop/PhD data/Guided by Light hoofdproject/Intensity over time.xlsx")

# Filter alleen tijd 0.5 en 18
df_subset <- Intensity_over_time %>%
  filter(Time %in% c(0.5, 18)) %>%
  mutate(Time_char = as.character(Time))  # Zet om naar character voor string checks

# Functie voor veilige t-test
safe_ttest <- function(data) {
  data <- data %>% filter(!is.na(Ratio))
  times_present <- unique(data$Time_char)
  
  if (all(c("0.5", "18") %in% times_present)) {
    # Check of beide groepen minstens 2 observaties hebben (minimum voor t-test)
    n_0_5 <- sum(data$Time_char == "0.5")
    n_18 <- sum(data$Time_char == "18")
    
    if (n_0_5 >= 2 & n_18 >= 2) {
      test <- t.test(Ratio ~ Time_char, data = data)
      return(tibble(
        p_value = test$p.value,
        mean_0_5 = mean(data$Ratio[data$Time_char == "0.5"]),
        mean_18 = mean(data$Ratio[data$Time_char == "18"])
      ))
    } else {
      return(tibble(
        p_value = NA_real_,
        mean_0_5 = mean(data$Ratio[data$Time_char == "0.5"]),
        mean_18 = mean(data$Ratio[data$Time_char == "18"])
      ))
    }
  } else {
    # Als niet beide tijden aanwezig, mean waar mogelijk, p_value = NA
    mean_0_5 <- if ("0.5" %in% times_present) mean(data$Ratio[data$Time_char == "0.5"]) else NA_real_
    mean_18 <- if ("18" %in% times_present) mean(data$Ratio[data$Time_char == "18"]) else NA_real_
    return(tibble(
      p_value = NA_real_,
      mean_0_5 = mean_0_5,
      mean_18 = mean_18
    ))
  }
}

# Per dosis de analyse doen
results_ttest <- df_subset %>%
  group_by(Dose) %>%
  group_modify(~ safe_ttest(.x))

print(results_ttest)


#------power berekening-------

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


#------------------- Diagnostic performance -----------------

------------# Inadequate margin detection: Binomial Clopper-Pearson method-----

TP <- a; FP <- b; FN <- c; TN <- d


calc_diag_metrics <- function(TP, FP, FN, TN, conf.level = 0.95) {

  # Sensitiviteit
  
  sens <- TP / (TP + FN)
  
  sens_ci <- binom.test(x = TP, n = TP + FN, conf.level = conf.level)$conf.int
  
  # Specificiteit
  
  spec <- TN / (TN + FP)
  
  spec_ci <- binom.test(x = TN, n = TN + FP, conf.level = conf.level)$conf.int
  
  # Positieve voorspellende waarde (PPV)
  
  ppv <- TP / (TP + FP)
  
  ppv_ci <- binom.test(x = TP, n = TP + FP, conf.level = conf.level)$conf.int
  
  # Negatieve voorspellende waarde (NPV)
  
  npv <- TN / (TN + FN)
  
  npv_ci <- binom.test(x = TN, n = TN + FN, conf.level = conf.level)$conf.int
  
  # Accuracy
  
  accuracy <- (TP + TN) / (TP + FP + FN + TN)
  
  accuracy_ci <- binom.test(x = TP + TN, n = TP + FP + FN + TN, conf.level = conf.level)$conf.int
  
  # Resultaten als dataframe
  
  results <- data.frame(
    
    Metric = c("Sensitivity", "Specificity", "PPV", "NPV", "Accuracy"),
    
    Estimate = c(sens, spec, ppv, npv, accuracy),
    
    Lower_CI = c(sens_ci[1], spec_ci[1], ppv_ci[1], npv_ci[1], accuracy_ci[1]),
    
    Upper_CI = c(sens_ci[2], spec_ci[2], ppv_ci[2], npv_ci[2], accuracy_ci[2])
    
  )
  
  return(results)
  
}

------------# Tumor detection: ANOVA-type Wilson score---------------

library(binom)

TP <- a; FP <- b; FN <- c; TN <- d

# Sensitiviteit

sens <- binom.confint(x = TP, n = TP + FN, conf.level = 0.95, methods = "wilson")

sens


# Specificiteit

spec <- binom.confint(x = TN, n = TN + FP, conf.level = 0.95, methods = "wilson")

spec


# Positieve voorspellende waarde (PPV)

ppv <- binom.confint(x = TP, n = TP + FP, conf.level = 0.95, methods = "wilson")

ppv


# Negatieve voorspellende waarde (NPV)

npv <- binom.confint(x = TN, n = TN + FN, conf.level = 0.95, methods = "wilson")

npv


# Accuracy

accuracy <- binom.confint(x = TP + TN, n = TP + FP + FN + TN, conf.level = 0.95, methods = "wilson")

accuracy




