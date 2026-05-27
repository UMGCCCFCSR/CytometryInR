#Problem 1
#In this session, we created beeswarm-style boxplot to display our T-cell frequencies on the y-axis, 
#and timepoint on the x-axis. 
# Using the concepts covered this week, swap out “timepoint” for the “Condition” variable. 
# Adjust other layer arguments accordingly until you can return a similar plot at the end of the class. 
# Finally, figure out how to switch around the order the Condition values are displayed on the x-axis.

#Problem 2
#Circle back to the CytoML-ggcyto flowplot, and modify it until happy with the visual appearance. 
# You may use any resource on the internet to assist, but you must document your steps so that 
# we can also repeat them.

#Problem 3
#histogram/density overlay showing the distribution of a variable on the x-axis
#Similar to what we did during class to show values according to a different data column, 
# try to modify the plot to show data on the basis of group (whether condition, ptype, infant_sex, etc.) 
# and overlay plots

problem 1
#get a plot Tcell frequency y axis and Conditions in X axis. 
StorageLocation <- file.path("06_visualizing", "data")
TheCSV <- list.files(StorageLocation, pattern = ".csv",full.names=TRUE)
Data<- read.csv(TheCSV, check.names=FALSE)

#to change postion of axis
aes(x = Condition, y = TcellFrequency)
#Make sure Condition is treated as categorical
Data$Condition <- factor(Data$Condition)
str(Data$Condition)
 Factor w/ 3 levels "Ctrl","PPD","SEB"
Data$Condition <- factor(Data$Condition,
                         levels = c("Ctrl", "PPD", "SEB"))

ggplot(Data) +
  aes(x = Condition, y = TcellFrequency) +
  geom_boxplot() +
  geom_beeswarm(size = 2.5, cex = 2.5,
                aes(shape = infant_sex, fill = infant_sex)) +
  scale_shape_manual(values = shape_sex) +
  scale_fill_manual(values = fill_sex)

plot2<- ggplot(Data) +
  aes(x = Condition, y = TcellFrequency) +
  geom_boxplot() +
  geom_beeswarm(size = 2.5, cex = 2.5,
                aes(shape = infant_sex, fill = infant_sex)) +
  scale_shape_manual(values = shape_sex) +
  scale_fill_manual(values = fill_sex) +
  theme_classic()
plot2

# to change more items inside the plot ?geom_boxplot
geom_boxplot(
  mapping = NULL,
  data = NULL,
  stat = "boxplot",
  position = "dodge2",
  ...,
  outliers = TRUE,
  outlier.colour = NULL,
  outlier.color = NULL,
  outlier.fill = NULL,
  outlier.shape = NULL,
  outlier.size = NULL,
  outlier.stroke = 0.5,
  outlier.alpha = NULL,
  whisker.colour = NULL,
  whisker.color = NULL,
  whisker.linetype = NULL,
  whisker.linewidth = NULL,
  staple.colour = NULL,
  staple.color = NULL,
  staple.linetype = NULL,
  staple.linewidth = NULL,
  median.colour = NULL,
  median.color = NULL,
  median.linetype = NULL,
  median.linewidth = NULL,
  box.colour = NULL,
  box.color = NULL,
  box.linetype = NULL,
  box.linewidth = NULL,
  notch = FALSE,
  notchwidth = 0.5,
  staplewidth = 0,
  varwidth = FALSE,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE)

# for example change  box.color = NULL to  box.color = "blue",

plot2 <- ggplot(Data) +
  aes(x = Condition, y = TcellFrequency) +
  geom_boxplot(
    box.color = "blue",
    median.color = "red",
    whisker.linetype = "dashed",
    outlier.shape = NA  
  ) +
  geom_beeswarm(
    size = 2.5, cex = 2.5,
    aes(shape = infant_sex, fill = infant_sex)
  ) +
  scale_shape_manual(values = shape_sex) +
  scale_fill_manual(values = fill_sex) +
  theme_classic()

plot2

?geom_beeswarm
 geom_beeswarm(
    size = 2.5, cex = 2.5,
    aes(shape = infant_sex, color = infant_sex)
  ) +
  scale_shape_manual(values = c("Female" = 8, "Male" = 16)) +
   # different number for shpes of the pointers
  scale_color_manual(values = c("Female" = "black", "Male" = "black")) +
  theme_classic()
#shapes
| Code  | Shape                         |
| ----- | ----------------------------- |
| 0     | square (outline)              |
| 1     | circle (outline)              |
| 2     | triangle (up)                 |
| 3     | plus                          |
| 4     | cross                         |
| 5     | diamond                       |
| 6     | triangle (down)               |
| 7     | square + cross                |
| 8     | ⭐ star                        |
| 9     | diamond + plus                |
| 10    | circle + plus                 |
| 11    | triangles (up/down)           |
| 12    | square + plus                 |
| 13    | circle + cross                |
| 14    | square + triangle             |
| 15    | ■ filled square               |
| 16    | ● filled circle               |
| 17    | ▲ filled triangle             |
| 18    | ◆ filled diamond              |
| 19    | ● bigger filled circle        |
| 20    | • small dot                   |
| 21–25 | shapes with **fill + border** |

plot2 <- ggplot(Data) +
  aes(x = Condition, y = TcellFrequency) +
  geom_boxplot(
    box.color = "blue",
    median.color = "red",
    whisker.linetype = "dashed",
    outlier.shape = NA  
  ) +
  geom_beeswarm(
    size = 2.5, cex = 2.5,
    aes(shape = infant_sex, color = infant_sex)
  ) +
  scale_shape_manual(values = c("Female" = 8, "Male" = 16)) +
   # different number for shpes of the pointers
  scale_color_manual(values = c("Female" = "black", "Male" = "black")) +
  theme_classic()

plot2
# add title to the plot
# add square on the legend and legend title centered
plot2 <- ggplot(Data) +
  aes(x = Condition, y = TcellFrequency) +
  geom_boxplot(
    box.color = "blue",
    median.color = "red",
    whisker.linetype = "dashed",
    outlier.shape = NA  
  ) +
  geom_beeswarm(
    size = 2.5, cex = 2.5,
    aes(shape = infant_sex, color = infant_sex)
  ) +
  scale_shape_manual(values = c("Female" = 8, "Male" = 16)) +
   # different number for shpes of the pointers
  scale_color_manual(values = c("Female" = "black", "Male" = "black")) +
  theme_classic()+
 labs(
    title = "T Cell Frequency by Condition",
    x = "Condition",
    y = "T Cell Frequency",
    color = "Sex",
    shape = "Sex") +
theme(
    plot.title = element_text(hjust = 0.5),   # center title
    legend.background = element_rect( fill = "lightblue"),
    legend.box.background = element_rect(colour = "black"),
    legend.title = element_text(hjust = 0.5))
plot2
# to change colors
colors()
#there is 657 

# Problem 2
# this was the initial plot
Plot2 <- ggcyto(gs[6], subset="Tcells", aes(x="CD8", y="CD4")) + geom_hex(bins=100)
Plot2
#so put packages in local enviroment
library(CytoML)
library(ggcyto)
# open data from flowjow
FlowJoWsp <- list.files(path = StorageLocation, pattern = ".wsp", full = TRUE)
ws <- open_flowjo_xml(FlowJoWsp)
gs <- flowjo_to_gatingset(ws=ws, name=1, path = StorageLocation, additional.keys = "GROUPNAME")
gs_get_pop_paths(gs)
Plot <- ggcyto(gs[6], subset="Tcells", aes(x="CD8", y="CD4")) + 
  geom_hex(bins=100)
# now modify aestetics and other layers 
#change bin resolution:geom_hex(bins = 50)
#axis transformations: + scale_x_log10() + scale_y_log10() more detailed week 7
#labels: 
+ labs(
  x = "CD8 Expression",
  y = "CD4 Expression",
  title = "T-cell population")
#theme: classic
# add gate information: + geom_gate() + + geom_stats()
# different geometric: pointer: geom_point(alpha = 0.3, size = 0.5)

colnames(exprs(gs[[6]]))

Plot_3 <- ggcyto(gs[6], subset = "Tcells", 
        aes(x = "Comp-BV650-A", y = "Comp-PE-A")) +
  geom_point(alpha = 0.1, size = 0.3)+
  geom_gate()+
  scale_x_logicle() +
  scale_y_logicle() +
  labs(
    x = "CD8 Expression",
    y = "CD4 Expression",
    title = "T-cell Distribution") 

Plot_3

Plot_4 <-   ggcyto(gs[2], subset = "Tcells",
       aes(x = "Comp-BV650-A", y = "Comp-PE-A")) +
  geom_hex(bins = 80) +
  scale_fill_viridis_c() +
  scale_x_logicle() +
  scale_y_logicle() +
  coord_cartesian(xlim = c(20, 450), ylim = c(30, 350)) + # this is to change the range on axis
  theme_bw()+
   labs(
    x = "CD8 Expression",
    y = "CD4 Expression",
    title = "T-cell Distribution")

Plot_4 


#problem 3: histogram
Plot_5.1 <- ggplot(Data, aes(x = TcellFrequency, color = Condition, fill = Condition)) +
  geom_density(alpha = 0.3) +
  theme_bw()
or 
Plot_5.2 <-ggplot(Data, aes(x = TcellFrequency, color = Condition)) +
  geom_density(linewidth = 1.2) +
  theme_classic()+
  labs(
    x = "TcellFrequency",
    y = "Density",
    title = "T-cell by condtions")+
  theme(
  legend.box.background = element_rect(colour = "black",  linewidth = 0.5) )
# no painted under the curve

Plot_5.1 and Plot_5.2

# if you do histogram plot
Plot_6 <-ggplot(Data, aes(x = TcellFrequency, fill = Condition)) +
  geom_histogram(alpha = 0.3, position = "identity", bins = 60) +
  theme_bw()
Plot_6
# so is like a barchart

# for postion of layers
layer(
  geom = "point",
  stat = "identity",
  position = "jitter") # but what does it do?


git add .
git commit -m "finished homeworks"
git push