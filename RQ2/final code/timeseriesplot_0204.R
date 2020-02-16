# Title: panel_fig_0110.R
# Date: 04-Feb-20
# Author:LGE

# clear workspace
rm(list = ls())
graphics.off()

# libraries 
library(tidyverse)
library(ggplot2)
library(gridExtra)
#theme_set(theme_minimal())

# load data
setwd("~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push")
paneldata <- read.csv("data/RQ2_data_covariates_1212.csv", as.is=T)
paneldata$X <- NULL
names(paneldata)[10]<-"super" # change column name

# calculate average of species groups 
x <- aggregate(cbind(nodeDegree, nodeDegree.in,av_degree, n_turnover,clustering,avg_duration,super) ~ year, data = paneldata, mean, na.rm = TRUE)
y <- aggregate(cbind(nodeDegree, nodeDegree.in,av_degree, n_turnover,clustering,avg_duration,super) ~ year+group, data = paneldata, mean, na.rm = TRUE)
head(paneldata)

# new columns for group and exp_iso3
x$group <- 'All groups'

# merge 
z <- rbind(x,y)

# include only average (all), plaice, lobster
a <- subset(z, group == 'All groups') # | group == 'plaice' | group == 'lobster')
c <- a[!(a$avg_duration==0),] # data shortened for duration plot

# line plot 
r1 <- ggplot(a, aes(x=year, y=super, color=group, group=group)) + 
  geom_line(size=1)+theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) + 
  scale_color_manual(values = c("navy", "deepskyblue2", "coral3")) + labs(y=expression('B/B'['MSY'])) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


r2 <- ggplot(a, aes(x=year, y=clustering, color=group, group=group)) + 
  geom_line(size=1) + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) + 
  scale_color_manual(values = c("navy", "deepskyblue2", "coral3")) + labs(y='Clustering') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


r3 <- ggplot(a, aes(x=year, y=av_degree, color=group, group=group)) + 
  geom_line(size=1)+theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) + 
  scale_color_manual(values = c("navy", "deepskyblue2", "coral3")) + labs(y= "Degree") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

r4 <- ggplot(c, aes(x=year, y=avg_duration, color=group, group=group)) + 
  geom_line(size=1, na.rm=TRUE) + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) + 
  scale_color_manual(values = c("navy", "deepskyblue2", "coral3")) + expand_limits(x = 1995) +labs(y= "Trade duration") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

r5 <- ggplot(a, aes(x=year, y=n_turnover, color=group, group=group)) + 
  geom_line(size=1) + scale_color_manual(values = c("navy", "deepskyblue2", "coral3")) + labs(y= "Turnover", x='Year') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



# combine plots to panel (preliminary plot)
r6 <- grid.arrange(r1, r2, r3, r4, r5, ncol = 1)
r6

# single legend 
b <- grid_arrange_shared_legend(r1, r2, r3, r4, r5, ncol = 6, nrow = 1, position='bottom')

lay <- rbind(c(1),
             c(2),
             c(3),
             c(4),
             c(5))

# function for g_legend to work
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# function for shared legend to work
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

mylegend <- g_legend(r1 + theme(legend.position='bottom', legend.title = element_blank()))

b <- grid.arrange(arrangeGrob(r1 + theme(legend.position="none"),
                               r2 + theme(legend.position="none"),
                               r3 + theme(legend.position="none"),
                               r4 + theme(legend.position="none"),
                               r5 + theme(legend.position="none"),
                               nrow=5),
                   mylegend, heights=c(10, 1))


# save plot
ggsave(filename="ts_diagram_new.png", width = 210, height = 297, units = "mm", dpi=500)

