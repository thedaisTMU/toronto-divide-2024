library(DaisTheme)
library(ggplot2)
library(data.table)
library(magrittr)
library(tidyverse)

#########################################################
#Load in graph spread sheets
graph.data <- fread("C:/Users/alockhart/Desktop/Old Projects/Toronto Survey/Graphs/Graphs_spreadsheet.csv")




#################################
#Figure 1 onwards


figure_1_data <- fread("C:/Users/alockhart/Desktop/Old Projects/Toronto Survey/Graphs/Figure_1.csv") %>%
  arrange(-year)
figure_1_data$year <- factor(figure_1_data$year, levels=c(2023, 2020))

figure.1 <- plot.column.dais(figure_1_data, stat, year, order.bar="ascending", group.by=description, label=TRUE,
                           plot.fig.num = "Figure 1",
                           plot.title= graph.data[graph.data$Figure_number=="Figure 1",Figure_title],
                           y.axis= graph.data[graph.data$Figure_number=="Figure 1",Y_Axis],
                           caption = graph.data[graph.data$Figure_number=="Figure 1",Caption],
                           label.unit = graph.data[graph.data$Figure_number=="Figure 1",Y_Axis_Ticks],
                           label.adjust = 0.02,
                           label.text.size = 8,
                           export = FALSE,
                           export.name = "F1") + 
  scale_x_discrete(guide = guide_axis(angle = 0))


figure_3_data <- fread("C:/Users/alockhart/Desktop/Old Projects/Toronto Survey/Graphs/Figure_3.csv") %>%
  arrange(-Year)
figure_3_data$description <- factor(figure_3_data$description, levels=unique(figure_3_data$description))
figure_3_data$Year <- factor(figure_3_data$Year, levels=c(2023, 2020))
figure.3 <- ggplot2::ggplot(figure_3_data, aes(fill=Year, x = description, y = stat)) +
  geom_bar(position="dodge2", stat="identity") + 
  labs(title = "Access Locations", y = "% of those without home internet") + 
  scale_fill_manual(values = c("2023"="#eb0072", "2020"="#000000"),
                    limits = c("2023", "2020"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_text(aes(label = paste0(stat, "%")), position = position_dodge(0.85), vjust=-0.2, size=3)+
  labs(title = "Figure 3",subtitle = graph.data[Figure_number=="Figure 3",Figure_title], caption = graph.data[Figure_number=="Figure 3",Caption],
         fill = "")+
  guides(colour="none")+
  scale_y_continuous(expand = expansion(mult = 0),
                     limits=c(0,55),
                     labels = scales::percent_format(scale=1))+
  dais.base.theme() +
  theme(axis.title.x=element_blank())


figure_4_data <- fread("C:/Users/alockhart/Desktop/Old Projects/Toronto Survey/Graphs/Figure_4.csv")
figure.4 <- plot.column.dais(figure_4_data, stat, description, order.bar="descending", label=FALSE,
                             plot.fig.num = "Figure 4",
                             plot.title= graph.data[graph.data$Figure_number=="Figure 4",Figure_title],
                             y.axis= graph.data[graph.data$Figure_number=="Figure 4",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 4",Caption],
                             label.unit = graph.data[graph.data$Figure_number=="Figure 4",Y_Axis_Ticks],
                             label.adjust = 0.025,
                             export = FALSE,
                             export.name = "F4") + 
  geom_text(aes(label = paste0(stat, "%")), position = position_dodge(0.6), vjust=-0.15)


figure_5_data <- fread("C:/Users/alockhart/Desktop/Old Projects/Toronto Survey/Graphs/Figure_5.csv")
figure.5 <- plot.column.dais(figure_5_data, stat, description, order.bar="descending", label=TRUE,
                             plot.fig.num = "Figure 5",
                             plot.title= graph.data[graph.data$Figure_number=="Figure 5",Figure_title],
                             y.axis= graph.data[graph.data$Figure_number=="Figure 5",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 5",Caption],
                             label.unit = graph.data[graph.data$Figure_number=="Figure 5",Y_Axis_Ticks],
                             label.adjust = 0.035,
                             export = FALSE,
                             export.name = "F5") + 
  scale_y_continuous(expand = expansion(mult = 0),
                     limits=c(0,35),
                     labels = scales::percent_format(scale=1))

figure_6_data <- fread("C:/Users/alockhart/Desktop/Old Projects/Toronto Survey/Graphs/Figure_6.csv")
figure_6_data$description <- factor(figure_6_data$description, levels=unique(figure_6_data$description))
figure_6_data$year <- as.character(figure_6_data$year)
figure.6 <- plot.column.dais(figure_6_data, stat, year, order.bar="descending", group.by=description, label=TRUE,
                             plot.fig.num = "Figure 6",
                             plot.title= graph.data[graph.data$Figure_number=="Figure 6",Figure_title],
                             y.axis= graph.data[graph.data$Figure_number=="Figure 6",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 6",Caption],
                             label.unit = graph.data[graph.data$Figure_number=="Figure 6",Y_Axis_Ticks],
                             label.adjust = 0.025,
                             label.text.size = 9,
                             export = FALSE,
                             export.name = "F6",
                             colours=c("#eb0072","#7474c1","#004c9b","#00a9ef","#6bbfae","#ffdc00","#ff7200")) + 
  scale_x_discrete(guide = guide_axis(angle = 0)) 


figure_7_data <- fread("C:/Users/alockhart/Desktop/Old Projects/Toronto Survey/Graphs/Figure_7.csv") %>%
  arrange(-year)
figure_7_data$description <- factor(figure_7_data$description, levels=unique(figure_7_data$description))
figure_7_data$year <- factor(figure_7_data$year, levels=c(2023, 2020))
figure.7 <- plot.column.dais(figure_7_data, stat, year, group.by=description, label=TRUE,
                             plot.fig.num = "Figure 7",
                             plot.title= graph.data[graph.data$Figure_number=="Figure 7",Figure_title],
                             y.axis= graph.data[graph.data$Figure_number=="Figure 7",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 7",Caption],
                             label.unit = graph.data[graph.data$Figure_number=="Figure 7",Y_Axis_Ticks],
                             label.adjust = 0.025,
                             export = FALSE,
                             export.name = "F7",
                             colours=set.colours(5,type = "gradient", gradient.choice = "hot.pink")) + 
  scale_x_discrete(guide = guide_axis(angle = 0))  + 
  scale_y_continuous(expand = expansion(mult = 0),
                     limits=c(0,55),
                     labels = scales::percent_format(scale=1))


figure_8_data <- fread("C:/Users/alockhart/Desktop/Old Projects/Toronto Survey/Graphs/Figure_8.csv")
figure_8_data$description <- factor(figure_8_data$description, levels=unique(figure_8_data$description))
figure_8_data$year <- factor(figure_8_data$year, levels=c(2023, 2020))
figure.8 <- plot.column.dais(figure_8_data, stat, year, group.by=description, label=TRUE,
                             plot.fig.num = "Figure 8",
                             plot.title= graph.data[graph.data$Figure_number=="Figure 8",Figure_title],
                             y.axis= graph.data[graph.data$Figure_number=="Figure 8",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 8",Caption],
                             label.unit = graph.data[graph.data$Figure_number=="Figure 8",Y_Axis_Ticks],
                             label.adjust = 0.025,
                             export = FALSE,
                             export.name = "F8",
                             colours=set.colours(6,type = "gradient", gradient.choice = "hot.pink")) +
  scale_y_continuous(expand = expansion(mult = 0),
                     limits=c(0,50),
                     labels = scales::percent_format(scale=1))

figure_12_data <- fread("C:/Users/alockhart/Desktop/Old Projects/Toronto Survey/Graphs/Figure_12.csv")
figure_12_data$description <- factor(figure_12_data$description, levels=unique(figure_12_data$description))
figure.12 <- plot.column.dais(figure_12_data, stat, description, label=TRUE,
                             plot.fig.num = "Figure 12",
                             plot.title= graph.data[graph.data$Figure_number=="Figure 12",Figure_title],
                             y.axis= graph.data[graph.data$Figure_number=="Figure 12",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 12",Caption],
                             label.unit = graph.data[graph.data$Figure_number=="Figure 12",Y_Axis_Ticks],
                             label.adjust = 0.045,
                             export = FALSE,
                             export.name = "F12") +
  scale_y_continuous(expand = expansion(mult = 0),
                     limits=c(0,39),
                     labels = scales::percent_format(scale=1))

figure_13_data <- fread("C:/Users/alockhart/Desktop/Old Projects/Toronto Survey/Graphs/Figure_13.csv")
figure_13_data$description <- factor(figure_13_data$description, levels=unique(figure_13_data$description))
figure.13 <- plot.column.dais(figure_13_data, stat, year, group.by=description, label=TRUE,
                              plot.fig.num = "Figure 13",
                              plot.title= graph.data[graph.data$Figure_number=="Figure 13",Figure_title],
                              y.axis= graph.data[graph.data$Figure_number=="Figure 13",Y_Axis],
                              caption = graph.data[graph.data$Figure_number=="Figure 13",Caption],
                              label.unit = graph.data[graph.data$Figure_number=="Figure 13",Y_Axis_Ticks],
                              label.text.size = 9,
                              label.adjust = 0.025,
                              export = FALSE,
                              export.name = "F13",
                              colours=set.colours(5,type = "gradient", gradient.choice = "hot.pink"))

figure_15_data <- fread("C:/Users/alockhart/Desktop/Old Projects/Toronto Survey/Graphs/Figure_15.csv")
figure_15_data$description <- factor(figure_15_data$description, levels=unique(figure_15_data$description))
figure.15 <- plot.column.dais(figure_15_data, stat, description, label=TRUE,
                              plot.fig.num = "Figure 15",
                              plot.title= graph.data[graph.data$Figure_number=="Figure 15",Figure_title],
                              y.axis= graph.data[graph.data$Figure_number=="Figure 15",Y_Axis],
                              caption = graph.data[graph.data$Figure_number=="Figure 15",Caption],
                              label.unit = graph.data[graph.data$Figure_number=="Figure 15",Y_Axis_Ticks],
                              label.adjust = 0.035,
                              export = FALSE,
                              export.name = "F15")+ 
  scale_y_continuous(expand = expansion(mult = 0),
                     limits=c(0,65),
                     labels = scales::percent_format(scale=1))

figure_16_data <- fread("C:/Users/alockhart/Desktop/Old Projects/Toronto Survey/Graphs/Figure_16.csv")
figure_16_data$description <- factor(figure_16_data$description, levels=unique(figure_16_data$description))
figure.16 <- plot.column.dais(figure_16_data, stat, description, label=TRUE,
                              plot.fig.num = "Figure 16",
                              plot.title= graph.data[graph.data$Figure_number=="Figure 16",Figure_title],
                              y.axis= graph.data[graph.data$Figure_number=="Figure 16",Y_Axis],
                              caption = graph.data[graph.data$Figure_number=="Figure 16",Caption],
                              label.unit = graph.data[graph.data$Figure_number=="Figure 16",Y_Axis_Ticks],
                              label.adjust = 0.04,
                              export = FALSE,
                              export.name = "F16")+ 
  scale_y_continuous(expand = expansion(mult = 0),
                     limits=c(0,50),
                     labels = scales::percent_format(scale=1))

export.dais.plot("F1", figure.1, p.height = 4.5, p.width = 7.25, type = "pdf")
export.dais.plot("F3", figure.3, p.height = 4.5, p.width = 7.25, type = "pdf")
export.dais.plot("F4", figure.4, p.height = 4.5, p.width = 7.25, type = "pdf")
export.dais.plot("F5", figure.5, p.height = 4.5, p.width = 7.25, type = "pdf")
export.dais.plot("F6", figure.6, p.height = 4.5, p.width = 7.25, type = "pdf")
export.dais.plot("F7", figure.7, p.height = 4.5, p.width = 7.25, type = "pdf")
export.dais.plot("F8", figure.8, p.height = 4.5, p.width = 7.25, type = "pdf")
export.dais.plot("F11", figure.11, p.height = 4.5, p.width = 7.25, type = "pdf")
export.dais.plot("F12", figure.12, p.height = 4.5, p.width = 7.25, type = "pdf")
export.dais.plot("F14", figure.14, p.height = 4.5, p.width = 7.25, type = "pdf")
export.dais.plot("F15", figure.15, p.height = 4.5, p.width = 7.25, type = "pdf")

library(ggplot2)
library(svglite)
ggplot2::ggsave("F1.svg", plot = figure.1, device = "svg", width = 7.25, height = 4.5, pointsize = 12, bg = "transparent")
ggplot2::ggsave("F3.svg", plot = figure.3, device = "svg", width = 7.25, height = 4.5, pointsize = 12, bg = "transparent")
ggplot2::ggsave("F4.svg", plot = figure.4, device = "svg", width = 7.25, height = 4.5, pointsize = 12, bg = "transparent")
ggplot2::ggsave("F5.svg", plot = figure.5, device = "svg", width = 7.25, height = 4.5, pointsize = 12, bg = "transparent")
ggplot2::ggsave("F6.svg", plot = figure.6, device = "svg", width = 7.25, height = 4.5, pointsize = 12, bg = "transparent")
ggplot2::ggsave("F7.svg", plot = figure.7, device = "svg", width = 7.25, height = 4.5, pointsize = 12, bg = "transparent")
ggplot2::ggsave("F8.svg", plot = figure.8, device = "svg", width = 7.25, height = 4.5, pointsize = 12, bg = "transparent")
ggplot2::ggsave("F12.svg", plot = figure.12, device = "svg", width = 7.25, height = 4.5, pointsize = 12, bg = "transparent")
ggplot2::ggsave("F13.svg", plot = figure.13, device = "svg", width = 7.25, height = 4.5, pointsize = 12, bg = "transparent")
ggplot2::ggsave("F15.svg", plot = figure.15, device = "svg", width = 7.25, height = 4.5, pointsize = 12, bg = "transparent")
ggplot2::ggsave("F16.svg", plot = figure.16, device = "svg", width = 7.25, height = 4.5, pointsize = 12, bg = "transparent")
