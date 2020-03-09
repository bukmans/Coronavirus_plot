library(ggplot2)
library(png)
library(grid)
library(gridExtra)
library(tidyverse)
library(plotly)


coro <- read.csv("//pediatrics.ucsd.edu/userhome/user_dys/lorobinson/Desktop/coronavirus.csv", stringsAsFactors = F)
coro$Date <- ymd(coro$Date)
new_case <- coro$Deaths[nrow(coro)] - coro$Deaths[nrow(coro)-1]
m <-  readPNG("//pediatrics.ucsd.edu/userhome/user_dys/lorobinson/Desktop/coronavirus_1.png")
w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 0.5), nrow=dim(m)[1]) #0.5 is alpha
p <- ggplot(coro,  aes(x=Date, y=Deaths, group = 1)) +
      scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
     scale_y_continuous(breaks = seq(0, max(coro$Deaths),500)) +
     annotation_custom(xmin=-Inf, ymin=-Inf, xmax=Inf, ymax=Inf,
                      rasterGrob(w,width=unit(1,"npc"), height=unit(1,"npc"))) +
     annotate("text", x = coro$Date[19], y = max(coro$Deaths)/1.6, label = paste0("+",new_case),colour = "gray", size = 70, alpha = 0.4)+
      geom_line(size=1, color= "red") + 
      geom_point(size = 4,color = "red") +
     labs(title= "Total Number of Deaths")+ 
  theme(#axis.text.x = element_text(angle = 45, vjust = 0.5),
        text = element_text(colour = "white"),
        title = element_text(color = "white"),
        line = element_line(color = "white"),
        rect = element_rect(fill = "black", color = "white"),
        axis.ticks = element_line(color = "#969696"),
        axis.title = element_text(color = "white"),
        axis.text = element_text(color = "#eaeaea"),
        axis.line = element_line(color = "#969696", linetype = 1),
        legend.background = element_rect(fill = NULL, color = NULL),
        legend.position = "bottom",
        legend.key = element_rect(fill = NA, color = NA, linetype = 0),
        strip.background = element_rect(fill=NA,colour=NA,size=NA,linetype = NULL),
        strip.text = element_text(color="white",face="bold",vjust=.5,hjust=.5),
        panel.background = element_rect(fill = "black", color = NULL),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color="#404040", size=24, face="bold.italic",hjust = 0.5,family="Times New Roman"),
        plot.margin = unit(c(1,1,1,1), "cm"))

