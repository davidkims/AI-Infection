library(gridExtra)
library(gridGraphics)
require(ggplot2)
library(dplyr)
library(lubridate)

result3<-result2
q1 <- merge(incidence1, result3, by="time", all.y = TRUE, no.dups = TRUE)
calender <- seq(ymd("2021-08-19"), ymd("2021-12-31"), by="day")
for (i in 50:184){
  q1$dates[i] = calender[i-50+1]
}

result4<-result2
q2 <- merge(incidence1, result4, by="time", all.y = TRUE, no.dups = TRUE)
calender <- seq(ymd("2021-08-19"), ymd("2021-12-31"), by="day")
for (i in 50:184){
  q2$dates[i] = calender[i-50+1]
}

result5<-result2
q3 <- merge(incidence1, result5, by="time", all.y = TRUE, no.dups = TRUE)
calender <- seq(ymd("2021-08-19"), ymd("2021-12-31"), by="day")
for (i in 50:184){
  q3$dates[i] = calender[i-50+1]
}

#q2<-round(q1[,4:43])

##Rt 0.39, 0.41, 0.43
plot1<-ggplot(q1) + 
  # geom_point(aes(x = dates, y = I), size = 1.5, color = "darkblue", fill = "white") +
  # geom_line(aes(x = dates, y = I), size = 1, color = "darkblue", fill = "white") +
  geom_line(aes(x = dates, y = D), size = 2, color="red", group = 1) + 
  geom_ribbon(data = q1, aes(x = dates, ymin = D-30, ymax = D+30), alpha = .1) +
  
  geom_line(data=q2, aes(x = dates, y = D), size = 2, color = "#FF9999", fill = "white") +
  geom_ribbon(data = q2, aes(x = dates, ymin = D-30, ymax = D+30), alpha = .1) +

  geom_line(data=q3, aes(x = dates, y = D), size = 2, color = "#CC0033", fill = "white") +
  geom_ribbon(data = q3, aes(x = dates, ymin = D-30, ymax = D+30), alpha = .1) +

  geom_vline(aes(xintercept = as.numeric(dates[c(50)])), linetype = 2, color = 'red') +
  scale_y_continuous(breaks = seq(0,1200,200), minor_breaks = seq(0,1200,200), limits = c(0, 1200)) +
  ylab('Cumulative deaths') +
  xlab('Date') +
  ggtitle('') +
  scale_x_date(limits = as.Date(c("2021-07-01","2021-12-31")), 
               breaks = as.Date(c("2021-07-01","2021-08-01","2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01")),
               labels=c ("07-01", "08-01","09-01", "10-01", "11-01", "12-01"))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.1, color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        legend.key=element_blank(),
        legend.title = element_blank(),
        text = element_text(size=12),
        panel.spacing = unit(2, "lines"))

plot1

##NPI base, 2 weeks, 4 weeks 
plot2<-ggplot(q1) + 
  # geom_point(aes(x = dates, y = I), size = 1.5, color = "darkblue", fill = "white") +
  # geom_line(aes(x = dates, y = I), size = 1, color = "darkblue", fill = "white") +
  geom_line(aes(x = dates, y = D), size = 2, color="red", group = 1) + 
  geom_ribbon(data = q1, aes(x = dates, ymin = D-20, ymax = D+20), alpha = .1) +
  
  geom_line(data=q2, aes(x = dates, y = D), size = 2, color = "#FF9999", fill = "white") +
  geom_ribbon(data = q2, aes(x = dates, ymin = D-20, ymax = D+20), alpha = .1) +
  
  geom_line(data=q3, aes(x = dates, y = D), size = 2, color = "#CC0033", fill = "white") +
  geom_ribbon(data = q3, aes(x = dates, ymin = D-20, ymax = D+20), alpha = .1) +
  
  geom_vline(aes(xintercept = as.numeric(dates[c(50)])), linetype = 2, color = 'red') +
  scale_y_continuous(breaks = seq(0,1200,200), minor_breaks = seq(0,1200,200), limits = c(0, 1200)) +
  ylab('Cumulative deaths') +
  xlab('Date') +
  ggtitle('') +
  scale_x_date(limits = as.Date(c("2021-07-01","2021-12-31")), 
               breaks = as.Date(c("2021-07-01","2021-08-01","2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01")),
               labels=c ("07-01", "08-01","09-01", "10-01", "11-01", "12-01"))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.1,color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        legend.key=element_blank(),
        legend.title = element_blank(),
        text = element_text(size=12),
        panel.spacing = unit(2, "lines"))

plot2

##Vaccine rate 0.7, 0.6, 0.8
plot3<-ggplot(q1) + 
  # geom_point(aes(x = dates, y = I), size = 1.5, color = "darkblue", fill = "white") +
  # geom_line(aes(x = dates, y = I), size = 1, color = "darkblue", fill = "white") +
  geom_line(aes(x = dates, y = D), size = 2, color="red", group = 1) + 
  geom_ribbon(data = q1, aes(x = dates, ymin = D-20, ymax = D+20), alpha = .1) +
  
  geom_line(data=q2, aes(x = dates, y = D), size = 2, color = "#FF9999", fill = "white") +
  geom_ribbon(data = q2, aes(x = dates, ymin = D-20, ymax = D+20), alpha = .1) +
  
  geom_line(data=q3, aes(x = dates, y = D), size = 2, color = "#CC0033", fill = "white") +
  geom_ribbon(data = q3, aes(x = dates, ymin = D-20, ymax = D+20), alpha = .1) +
  
  geom_vline(aes(xintercept = as.numeric(dates[c(50)])), linetype = 2, color = 'red') +
  scale_y_continuous(breaks = seq(0,1200,200), minor_breaks = seq(0,1200,200), limits = c(0, 1200)) +
  ylab('Cumulative deaths') +
  xlab('Date') +
  ggtitle('') +
  scale_x_date(limits = as.Date(c("2021-07-01","2021-12-31")), 
               breaks = as.Date(c("2021-07-01","2021-08-01","2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01")),
               labels=c ("07-01", "08-01","09-01", "10-01", "11-01", "12-01"))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.1,color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        legend.key=element_blank(),
        legend.title = element_blank(),
        text = element_text(size=12),
        panel.spacing = unit(2, "lines"))

plot3

##Test 0.36, 0.38, 0.4
plot4<-ggplot(q1) + 
  # geom_point(aes(x = dates, y = I), size = 1.5, color = "darkblue", fill = "white") +
  # geom_line(aes(x = dates, y = I), size = 1, color = "darkblue", fill = "white") +
  geom_line(aes(x = dates, y = D), size = 2, color="red", group = 1) + 
  geom_ribbon(data = q1, aes(x = dates, ymin = D-20, ymax = D+20), alpha = .1) +
  
  geom_line(data=q2, aes(x = dates, y = D), size = 2, color = "#FF9999", fill = "white") +
  geom_ribbon(data = q2, aes(x = dates, ymin = D-20, ymax = D+20), alpha = .1) +
  
  geom_line(data=q3, aes(x = dates, y = D), size = 2, color = "#CC0033", fill = "white") +
  geom_ribbon(data = q3, aes(x = dates, ymin = D-20, ymax = D+20), alpha = .1) +
  
  # 
  geom_vline(aes(xintercept = as.numeric(dates[c(50)])), linetype = 2, color = 'red') +
  scale_y_continuous(breaks = seq(0,1200,200), minor_breaks = seq(0,1200,200), limits = c(0, 1200)) +
  ylab('Cumulative deaths') +
  xlab('Date') +
  ggtitle('') +
  scale_x_date(limits = as.Date(c("2021-07-01","2021-12-31")), 
               breaks = as.Date(c("2021-07-01","2021-08-01","2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01")),
               labels=c ("07-01", "08-01","09-01", "10-01", "11-01", "12-01"))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.1, color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        legend.key=element_blank(),
        legend.title = element_blank(),
        text = element_text(size=12),
        panel.spacing = unit(2, "lines"))
  

plot4



library(ggpubr)
plot.all<-ggarrange(plot1, plot2, plot3, plot4,
                       ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom", #hjust=-2, #vjust=0.2,
                       labels = c("Rt(variants)", "NPI(0/2/4wks)", "Vaccination(low/base/high)", "Testing(low/base/high)"),
                       font.label = list(size = 12, color = "dark blue"))+  theme(legend.title = element_text(size = 10, face = "bold"))

annotate_figure(plot.all,
                top = text_grob("Epidemic/Intervention scenarios", color = "black", face = "bold", size = 12),
                bottom = text_grob("", color = "black", hjust = 1, x = 1, face = "italic", size = 8),
                left = text_grob("", color = "black", rot = 90, size = 8),
                right = "",
                fig.lab = "Figure 3", fig.lab.face = "bold")

plot(0,type='n',axes=FALSE,ann=FALSE, xpd=T)
legend("top", title="Legend",
       c("low parameter values","high parameter values"), fill=c("#66CCFF", "#0033FF"), horiz=FALSE, cex=1.10, xpd=T)


ggarrange(plot1,plot1,
          ncol = 2, nrow = 1)


l <- get_legend(plot1)
# remove legends
plot1 <- plot1 + theme(legend.position = "none")
plot1 <- plot1 + theme(legend.position = "none")

# Combining
grid.arrange(plot1, plot1, l, ncol = 2,main = "Main title")

library(ggpubr)
figure.all2<-ggarrange(plot1,
                       ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom", hjust=-1.5, #vjust=0.2,
                       labels = c("Prev100", "Prev200"),
                       font.label = list(size = 8, color = "blue"))+  theme(legend.title = element_text(size = 10, face = "bold"))

annotate_figure(figure.all2,
                top = text_grob("Incremetnal progression scenarios", color = "black", face = "bold", size = 12),
                bottom = text_grob("ReMoTe as of March 30 2021", color = "black", hjust = 1, x = 1, face = "italic", size = 8),
                left = text_grob("Number of population", color = "black", rot = 90, size = 12),
                right = "",
                fig.lab = "Figure 1", fig.lab.face = "bold")


#######


