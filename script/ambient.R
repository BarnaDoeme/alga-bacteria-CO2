library(ggplot2)
library(readxl)
library(RColorBrewer)
library(scales)
library(forcats)
library(dplyr)

#loading data
data <- read_excel("data/alga_growth_01-26.xlsx")
#rearrange factor levels
data <- data %>% 
  mutate(treatment = fct_relevel(treatment, "control", "Azospirillum", "Herbaspirillum"))


#----optical density----
#plotting
# Select the color palette
palette <- brewer.pal(3, "Set3")

# Use the palette in your plot
ggplot(data, aes(x = as.factor(day), y = OD682, fill = treatment)) +
  geom_jitter(alpha = 0, color = "black", position = position_jitterdodge(0.1)) +
  geom_smooth(aes(x=day, y=OD682), method = 'lm', formula = y~x, se = FALSE, color = "black", linetype = "solid", size = 1.5) +
  geom_smooth(aes(x=day, y=OD682, color = treatment), method = 'lm', formula = y~x, linetype = "solid") +
  geom_boxplot(position = "dodge", outlier.shape = NA) +
  geom_jitter(alpha = 0.5, color = "black", position = position_jitterdodge(0.1)) +
  labs(x = "Days", y = "Optical density at 682 nm") +
  #  ggtitle(expression(paste("Optical density of ", italic("Chlorella vulgaris "), " \n211-11b on 4 consecutive days co-cultured with Plant Growth Promoting Bacteria"))) +
  theme_bw() +
  scale_fill_manual(values = palette, 
                    name = "Treatment", 
                    labels = c("Control", "Azospirillum", "Herbaspirillum")) +
  scale_color_manual(values = palette, 
                     name = "Treatment", 
                     labels = c("Control", "Azospirillum", "Herbaspirillum")) +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16))

ggsave("output/ambient_od.png", 
       width = 30, 
       height = 20, 
       units = "cm", 
       dpi = 500)

#with Hungarian text
# Use the palette in your plot
ggplot(data, aes(x = as.factor(day), y = OD682, fill = treatment)) +
  geom_jitter(alpha = 0, color = "black", position = position_jitterdodge(0.1)) +
  geom_smooth(aes(x=day, y=OD682), method = 'lm', formula = y~x, se = FALSE, color = "black", linetype = "solid", size = 1.5) +
  geom_smooth(aes(x=day, y=OD682, color = treatment), method = 'lm', formula = y~x, linetype = "solid") +
  geom_boxplot(position = "dodge", outlier.shape = NA) +
  geom_jitter(alpha = 0.5, color = "black", position = position_jitterdodge(0.1)) +
labs(x = "Napok", y = "Optikai sűrűség (682 nm)") +
  #  ggtitle(expression(paste("Optical density of ", italic("Chlorella vulgaris "), " \n211-11b on 4 consecutive days co-cultured with Plant Growth Promoting Bacteria"))) +
  theme_bw() +
  scale_fill_manual(values = palette, 
                    name = "Kezelés", 
                    labels = c("Kontroll", "Azospirillum", "Herbaspirillum")) +
  scale_color_manual(values = palette, 
                     name = "Kezelés", 
                     labels = c("Kontroll", "Azospirillum", "Herbaspirillum")) +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16)) +
  scale_y_continuous(labels = format_format(big.mark = ".", decimal.mark = ","))


ggsave("output/ambient_od_hun.png", 
       width = 30, 
       height = 20, 
       units = "cm", 
       dpi = 500)




#----cell count----
#plotting
ggplot(data, aes(x = as.factor(day), y = cells, fill = treatment)) +
  geom_jitter(alpha = 0, color = "black", position = position_jitterdodge(0.1)) +
  geom_smooth(aes(x=day, y=cells), method = 'lm', formula = y~x, se = FALSE, color = "black", linetype = "solid", size = 1.5) +
  geom_smooth(aes(x=day, y=cells, color = treatment), method = 'lm', formula = y~x, linetype = "solid") +
  geom_boxplot(position = "dodge", outlier.shape = NA) +
  geom_jitter(alpha = 0.5, color = "black", position = position_jitterdodge(0.1)) +
labs(x = "Days", y = expression("Cell count (cells mL"^-1*")")) +
  #  ggtitle(expression(paste("Optical density of ", italic("Chlorella vulgaris "), " \n211-11b on 4 consecutive days co-cultured with Plant Growth Promoting Bacteria"))) +
  theme_bw() +
  scale_fill_manual(values = palette, 
                    name = "Treatment", 
                    labels = c("Control", "Azospirillum", "Herbaspirillum")) +
  scale_color_manual(values = palette, 
                     name = "Treatment", 
                     labels = c("Control", "Azospirillum", "Herbaspirillum")) +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16)) +
  scale_y_continuous(labels = format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE))


ggsave("output/ambient_cells.png", 
       width = 30, 
       height = 20, 
       units = "cm", 
       dpi = 500)

#with Hungarian text
ggplot(data, aes(x = as.factor(day), y = cells, fill = treatment)) +
  geom_jitter(alpha = 0, color = "black", position = position_jitterdodge(0.1)) +
  geom_smooth(aes(x=day, y=cells), method = 'lm', formula = y~x, se = FALSE, color = "black", linetype = "solid", size = 1.5) +
  geom_smooth(aes(x=day, y=cells, color = treatment), method = 'lm', formula = y~x, linetype = "solid") +
  geom_boxplot(position = "dodge", outlier.shape = NA) +
  geom_jitter(alpha = 0.5, color = "black", position = position_jitterdodge(0.1)) +
labs(x = "Napok", y = expression("Sejtszám (db mL"^-1*")")) +
  theme_bw() +
  scale_fill_manual(values = palette, 
                    name = "Kezelés", 
                    labels = c("Kontroll", "Azospirillum", "Herbaspirillum")) +
  scale_color_manual(values = palette, 
                     name = "Kezelés", 
                     labels = c("Kontroll", "Azospirillum", "Herbaspirillum")) +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16)) +
  scale_y_continuous(labels = format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE))

ggsave("output/ambient_cells_hun.png", 
       width = 30, 
       height = 20, 
       units = "cm", 
       dpi = 500)









