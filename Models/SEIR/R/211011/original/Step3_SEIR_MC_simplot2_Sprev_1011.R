library(gridExtra)
library(gridGraphics)
require(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

result3<-result2
q1 <- merge(incidence1, result3, by="time", all.y = TRUE, no.dups = TRUE)
q1_lhs <- cbind(result.lhs.ci, q1)
calender <- seq(ymd("2021-08-19"), ymd("2022-12-30"), by="day")
for (i in 50:lastdate){
  q1_lhs$dates[i] = calender[i-50+1]
}

# q1$new_vac <- c(5000,diff(q1$V))
# q11<-q1[c(1:500),]
# q12<-q11[c(2,65,66)]
# colnames(q12)[1]<-"date"
# q13<-merge(q12, cum_vac, by=c("date"), all.x=TRUE)
# q13$V_cum1<-q13$V_cum+15391991*0.8
# 
# plot(q13$date,q13$people_vaccinated/50000000,col="green", type="p",ylim=c(0,1))
# lines(q13$date,q13$people_fully_vaccinated/50000000,col="dark green", type="p")
# lines(q13$date,q13$V_cum1/50000000,type="l",col="red")
# plot(q11$V_cum/51000000)
# plot(q11$date,(q11$V_cum+0.3*51000000)/51000000)

result4<-result2
q2 <- merge(incidence1, result4, by="time", all.y = TRUE, no.dups = TRUE)
q2_lhs <- cbind(result.lhs.ci, q2)
calender <- seq(ymd("2021-08-19"), ymd("2022-12-30"), by="day")
for (i in 50:lastdate){
  q2_lhs$dates[i] = calender[i-50+1]
}

result5<-result2
q3 <- merge(incidence1, result5, by="time", all.y = TRUE, no.dups = TRUE)
q3_lhs <- cbind(result.lhs.ci, q3)
calender <- seq(ymd("2021-08-19"), ymd("2022-12-30"), by="day")
for (i in 50:lastdate){
  q3_lhs$dates[i] = calender[i-50+1]
}

#q2<-round(q1[,4:43])

##Rt 0.39, 0.41, 0.43
plot1<-ggplot(q1_lhs) + 
  #geom_rect(data=q1_lhs, aes(xmin=dates[c(96)], xmax=dates[c(126)], ymin=0, ymax=5000), fill="light grey", alpha=0.02)+
  geom_line(aes(x = dates, y = S), size = 2, color="red", group = 1) + 
#  geom_ribbon(data = q1_lhs, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .1) +
  geom_line(data=q2_lhs, aes(x = dates, y = S), size = 2, color = "#FF9999") +
#  geom_ribbon(data = q2_lhs, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .05) +

  geom_line(data=q3_lhs, aes(x = dates, y = S), size = 2, color = "#CC0033") +
#  geom_ribbon(data = q3_lhs, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .15) +

  geom_vline(aes(xintercept = as.numeric(dates[c(96)])), linetype = 2, color = 'red') +
  scale_y_continuous(breaks = seq(0,1000,100), minor_breaks = seq(0,1000,100), limits = c(0, 1000), expand = c(0, 0)) + ## scale_y_continuous(label=comma)
  ylab('Prevalence') +
  xlab('Date') +
  ggtitle('') +
  scale_x_date( limits = as.Date(c("2021-07-01", "2022-12-30")), breaks = as.Date( c( "2021-07-01",  "2021-08-01","2021-09-01", "2021-10-01",  "2021-11-01","2021-12-01" ,
                                                                                      "2022-01-01",  "2022-02-01","2022-03-01", "2022-04-01",  "2022-05-01","2022-06-01",  
                                                                                      "2022-07-01",  "2022-08-01","2022-09-01", "2022-10-01",  "2022-11-01","2022-12-01" ) ),
                labels = c ("07", "08", "09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),expand = c(0, 0)) +
  #theme_classic()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.1, color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        # legend.key=element_blank(),
        # legend.title = element_blank(),
        text = element_text(size=12),
        panel.spacing = unit(2, "lines"))+
  scale_color_manual(values = c("#FF9999","red", "#CC0033"))

plot1 

##NPI base, 2 weeks, 4 weeks 
plot2<-ggplot(q1_lhs) + 

  geom_line(aes(x = dates, y = S), size = 2, color="red", group = 1) + 
#  geom_ribbon(data = q1_lhs, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .1) +
  
  geom_line(data=q2_lhs, aes(x = dates, y = S), size = 2, color = "#FF9999") +
#  geom_ribbon(data = q2_lhs, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .05) +
  
  geom_line(data=q3_lhs, aes(x = dates, y = S), size = 2, color = "#CC0033") +
#  geom_ribbon(data = q3_lhs, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .15) +
  
  geom_vline(aes(xintercept = as.numeric(dates[c(96)])), linetype = 2, color = 'red') +
  scale_y_continuous(breaks = seq(0,1000,100), minor_breaks = seq(0,1000,100), limits = c(0, 1000), expand = c(0, 0)) + ## scale_y_continuous(label=comma)
  ylab('Prevalence') +
  xlab('Date') +
  ggtitle('') +
  scale_x_date( limits = as.Date(c("2021-07-01", "2022-12-30")), breaks = as.Date( c( "2021-07-01",  "2021-09-01",  "2021-11-01",
                                                                                      "2022-01-01",  "2022-03-01",  "2022-05-01",
                                                                                      "2022-07-01",  "2022-09-01", "2022-11-01") ),
                labels = c ("07", "09",  "11",  "01", "03", "05", "07",  "09", "11"),expand = c(0, 0)) +
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
plot3<-ggplot(q1_lhs) + 
  #geom_rect(data=q1_lhs, aes(xmin=dates[c(96)], xmax=dates[c(126)], ymin=0, ymax=5000), fill="light grey", alpha=0.02)+
  geom_line(aes(x = dates, y = S), size = 2, color="red", group = 1) + 
#  geom_ribbon(data = q1_lhs, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .1) +
  
  geom_line(data=q2_lhs, aes(x = dates, y = S), size = 2, color = "#FF9999", fill = "white") +
#  geom_ribbon(data = q2_lhs, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .05) +
  
  geom_line(data=q3_lhs, aes(x = dates, y = S), size = 2, color = "#CC0033", fill = "white") +
#  geom_ribbon(data = q3_lhs, aes(x = dates, ymin = low, ymax = high),fill="red", alpha = .15) +
  
  geom_vline(aes(xintercept = as.numeric(dates[c(96)])), linetype = 2, color = 'red') +
  scale_y_continuous(breaks = seq(0,1000,100), minor_breaks = seq(0,1000,100), limits = c(0, 1000), expand = c(0, 0)) + ## scale_y_continuous(label=comma)
  ylab('Prevalence') +
  xlab('Date') +
  ggtitle('') +
  scale_x_date( limits = as.Date(c("2021-07-01", "2022-12-30")), breaks = as.Date( c( "2021-07-01",  "2021-09-01",  "2021-11-01",
                                                                                      "2022-01-01",  "2022-03-01",  "2022-05-01",
                                                                                      "2022-07-01",  "2022-09-01", "2022-11-01") ),
                labels = c ("07", "09",  "11",  "01", "03", "05", "07",  "09", "11"),expand = c(0, 0)) +
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
plot4<-ggplot(q1_lhs) + 

  geom_line(aes(x = dates, y = S), size = 2, color="red", group = 1) + 
#  geom_ribbon(data = q1_lhs, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .1) +
  
  geom_line(data=q2_lhs, aes(x = dates, y = S), size = 2, color = "#FF9999") +
#  geom_ribbon(data = q2_lhs, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .05) +
  
  geom_line(data=q3_lhs, aes(x = dates, y = S), size = 2, color = "#CC0033") +
#  geom_ribbon(data = q3_lhs, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .15) +
  
  geom_vline(aes(xintercept = as.numeric(dates[c(96)])), linetype = 2, color = 'red') +
  scale_y_continuous(breaks = seq(0,1000,100), minor_breaks = seq(0,1000,100), limits = c(0, 1000), expand = c(0, 0)) + ## scale_y_continuous(label=comma)
  ylab('Prevalence') +
  xlab('Date') +
  ggtitle('') +
  scale_x_date( limits = as.Date(c("2021-07-01", "2022-12-30")), breaks = as.Date( c( "2021-07-01",  "2021-09-01",  "2021-11-01",
                                                                                      "2022-01-01",  "2022-03-01",  "2022-05-01",
                                                                                      "2022-07-01",  "2022-09-01", "2022-11-01") ),
                labels = c ("07", "09",  "11",  "01", "03", "05", "07",  "09", "11"),expand = c(0, 0)) +
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
                       labels = c("Rt(variants)", "NPI(0/2/4wks)",  "Testing(low/base/high)","Vaccination(low/base/high)"),
                       font.label = list(size = 12, color = "dark blue"))+  theme(legend.title = element_text(size = 10, face = "bold"))

annotate_figure(plot.all,
                top = text_grob("Epidemic/Intervention scenarios", color = "black", face = "bold", size = 12),
                bottom = text_grob("", color = "black", hjust = 1, x = 1, face = "italic", size = 8),
                left = text_grob("", color = "black", rot = 90, size = 8),
                right = "",
                fig.lab = "Figure 1", fig.lab.face = "bold")

plot(0,type='n',axes=FALSE,ann=FALSE, xpd=T)
legend("top", title="Legend",
       c( "low testing value","testing base case","high testing value","confirmed case"), col=c("#FF9999","red", "#CC0033","dark blue"), 
       horiz=FALSE, cex=1.10, xpd=T, lwd=3, pch=c(NA, NA, NA, 16) )

legend("top", title="Legend",
       c( "low value","base case","high value"), col=c("#FF9999","red", "#CC0033"), 
       horiz=FALSE, cex=1.10, xpd=T, lwd=3, pch=c(NA, NA, NA) )



ggarrange(plot1,plot1,
          ncol = 2, nrow = 1)


