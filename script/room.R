library(readxl)
library(dplyr)
library(forcats)
library(ggplot2)
library(RColorBrewer)
library(scales)

#loading data
data <- read_excel("data/alga_growth_12-30.xlsx")
#omitting day18, 8 days are enough
#rearrange factor levels for clearer visuals
data <- data %>% 
  filter(day != 18)%>%
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
  theme_bw() +
  scale_fill_manual(values = palette, 
                    name = "Treatment", 
                    labels = c("Control", "Azospirillum", "Herbaspirillum")) +
  scale_color_manual(values = palette, 
                     name = "Treatment", 
                     labels = c("Control", "Azospirillum", "Herbaspirillum")) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),labels = c("", "2", "", "4", "", "6", "", "8", ""), expand = c(0, 0)) +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16))

ggsave("output/room_od.png", 
       width = 30, 
       height = 20, 
       units = "cm", 
       dpi = 500)

#with Hungarian text
ggplot(data, aes(x = as.factor(day), y = OD682, fill = treatment)) +
  geom_jitter(alpha = 0, color = "black", position = position_jitterdodge(0.1)) +
  geom_smooth(aes(x=day, y=OD682), method = 'lm', formula = y~x, se = FALSE, color = "black", linetype = "solid", size = 1.5) +
  geom_smooth(aes(x=day, y=OD682, color = treatment), method = 'lm', formula = y~x, linetype = "solid") +
  geom_boxplot(position = "dodge", outlier.shape = NA) +
  geom_jitter(alpha = 0.5, color = "black", position = position_jitterdodge(0.1)) +
  labs(x = "Napok", y = "Optikai sűrűség (682 nm)") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),labels = c("", "2", "", "4", "", "6", "", "8", ""), expand = c(0, 0)) +
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


ggsave("output/room_od_hun.png", 
       width = 30, 
       height = 20, 
       units = "cm", 
       dpi = 500)




#----cell count----
ggplot(data, aes(x = as.factor(day), y = cells, fill = treatment)) +
  geom_jitter(alpha = 0, color = "black", position = position_jitterdodge(0.1)) +
  geom_smooth(aes(x=day, y=cells), method = 'lm', formula = y~x, se = FALSE, color = "black", linetype = "solid", size = 1.5) +
  geom_smooth(aes(x=day, y=cells, color = treatment), method = 'lm', formula = y~x, linetype = "solid") +
  geom_boxplot(position = "dodge", outlier.shape = NA) +
  geom_jitter(alpha = 0.5, color = "black", position = position_jitterdodge(0.1)) +
  labs(x = "Days", y = expression("Cell count (cells mL"^-1*")")) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),labels = c("", "2", "", "4", "", "6", "", "8", ""), expand = c(0, 0)) +
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


ggsave("output/room_cells.png", 
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
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),labels = c("", "2", "", "4", "", "6", "", "8", ""), expand = c(0, 0)) +
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

ggsave("output/room_cells_hun.png", 
       width = 30, 
       height = 20, 
       units = "cm", 
       dpi = 500)

