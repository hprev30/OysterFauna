#####packages-----
library(ggplot2)
library(dplyr)

#####subsetting and getting total mud percentage-----
imagejgtm = imagej[which(imagej$RESERVE == 'GTM'),]
imagejgtm$Percent_Cover_Mud = imagejgtm$Number_Pts_SandMud/25
imagejgtm$Percent_Cover_Mud = imagejgtm$Percent_Cover_Mud*100

combined$ReefName = factor(combined$ReefName, levels= c("Guana North", "Guana Middle", "Guana South"))
colnames(combined)[1]  <- "ReefName"
combined <- within(combined, ReefName[ReefName == 'Guana_North'] <- 'Guana North')
combined <- within(combined, ReefName[ReefName == 'Guana_Mid'] <- 'Guana Middle')
combined <- within(combined, ReefName[ReefName == 'Guana_South'] <- 'Guana South')
combined <- within(combined, Type[Type == 'PC'] <- 'Point Intercept')
combined <- within(combined, Type[Type == 'Image J'] <- 'ImageJ')

shell = subset(combined, TotalType == "Shell")
mud = subset(combined, TotalType == "Mud")
#####box and whiskers-----

shell_bw = ggplot(shell, aes(x=ReefName, y = TotalShell, color = Type)) + 
  geom_boxplot(lwd = 1) +
  ylab('Shell Cover (%)') + ggtitle("Cover Assessment Method Comparisons") +  xlab("Reef") +
  theme(panel.background = element_blank(),
        axis.line.x = element_line(color="black", linewidth =1),
        axis.line.y = element_line(color="black", linewidth =1),
        plot.title = element_text(face = 'bold')) + 
  scale_color_manual(values = type_colors) +  scale_fill_manual(values = type_colors) +
  theme(axis.text = element_text(color = "black"),axis.line = element_line(),
       panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  geom_point(aes(color = Type),
             position = position_jitterdodge(dodge.width = .9, seed = 1),
             shape = 19, stroke = 0.5, size = 3.0, show.legend = FALSE)

shell_bw

mud_bw = ggplot(mud, aes(x=ReefName, y = TotalShell, color = Type)) + 
  geom_boxplot(lwd = 1) +
  ylab('Mud Cover (%)') + ggtitle("Percent Cover Assessment Method Comparisons") +  xlab("Reef") +
  theme(panel.background = element_blank(),
        axis.line.x = element_line(color="black", linewidth =1),
        axis.line.y = element_line(color="black", linewidth =1),
        plot.title = element_text(face = 'bold')) + 
  scale_color_manual(values = type_colors) +  scale_fill_manual(values = type_colors) +
  theme(axis.text = element_text(color = "black"),axis.line = element_line(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  geom_point(aes(color = Type),
             position = position_jitterdodge(dodge.width = .9, seed = 1),
             shape = 19, stroke = 0.5, size = 3.0, show.legend = FALSE)

mud_bw

#####colors-----
type_colors = c(
  "Point Intercept" = "black",
  "ImageJ" = "red2"
)

