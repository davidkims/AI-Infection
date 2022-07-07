library(gridExtra)
library(gridGraphics)
require(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

calender <- seq(ymd("2022-03-12"), ymd("2023-12-30"), by="day")

result3<-result2
q1 <- merge(incidence1, result3, by="time", all.y = TRUE, no.dups = TRUE)
#q1 <- cbind(result.lhs.ci, q1)
for (i in 50:lastdate){
  q1$dates[i] = calender[i-50+1]
}

result4<-result2
q2 <- merge(incidence1, result4, by="time", all.y = TRUE, no.dups = TRUE)
#q2 <- cbind(result.lhs.ci, q2)
for (i in 50:lastdate){
  q2$dates[i] = calender[i-50+1]
}

result5<-result2
q3 <- merge(incidence1, result5, by="time", all.y = TRUE, no.dups = TRUE)
#q3 <- cbind(result.lhs.ci, q3)
for (i in 50:lastdate){
  q3$dates[i] = calender[i-50+1]
}

#q2<-round(q1[,4:43])

res_parametric_si <- estimate_R(q2$TP.incidence, 
                                method="parametric_si",
                                config = make_config(list(
                                  mean_si = 6, 
                                  std_si = 2))
)

Rt_sim<-as.data.frame(res_parametric_si$R)

plot(q1$dates, q1$U)
plot(q1$dates, q1$R)
plot(q1$dates, q1$V)
plot(q1$dates, q1$E)
plot(q1$dates, q1$A)
plot(q1$dates, q1$TP)
plot(q1$dates, q1$S)



##Rt 0.39, 0.41, 0.43
plot1<-ggplot(q1) + 
  #geom_rect(data=q1, aes(xmin=dates[c(96)], xmax=dates[c(126)], ymin=0, ymax=6000), fill="light grey", alpha=0.02)+
  geom_point(aes(x = dates, y = I/1000), size = 1, color = "darkblue") +
  geom_line(aes(x = dates, y = I/1000), size = 0.5, color = "darkblue") +
  geom_line(aes(x = dates, y = TP.incidence/1000), size = 2, color="red", group = 1) + 
  #geom_ribbon(data = q1, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .1) +
  #geom_line(data=q2, aes(x = dates, y = TP.incidence/1000), size = 2, color = "#FF9999") +
  # geom_ribbon(data = q2, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .05) +
  #geom_line(data=q3, aes(x = dates, y = TP.incidence/1000), size = 2, color = "#CC0033") +
  #geom_ribbon(data = q3, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .15) + 
  
  geom_vline(aes(xintercept = as.numeric(dates[c(119)])), linetype = 2, color = 'black') +
  #geom_vline(aes(xintercept = as.numeric(dates[c(255)])), linetype = 2, color = 'red') +
  scale_y_continuous(breaks = seq(0,800,100), minor_breaks = seq(0,800,100), limits = c(0,800), expand = c(0, 0)) + ## scale_y_continuous(label=comma)
  ylab('Incidence of COVID-19 cases (in thousand)') +
  xlab('') +
  ggtitle('') +
  scale_x_date( limits = as.Date(c("2022-01-01", "2022-12-31")), breaks = as.Date( c( "2022-01-01",  "2022-04-01", "2022-07-01", "2022-10-01"
                                                                                      ) ),
                labels = c ("Jan-2022", "Apr-2022", "Jul-2022", "Oct-2022"),expand = c(0, 0)) +
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
plot2<-ggplot(q1) + 
#  geom_rect(data=q1, aes(xmin=dates[c(96)], xmax=dates[c(300)], ymin=0, ymax=20000), fill="light grey", alpha=0.02)+
#  geom_rect(data=q1, aes(xmin=dates[c(96)], xmax=dates[c(300+30)], ymin=0, ymax=20000), fill="light grey", alpha=0.008)+
#  geom_rect(data=q1, aes(xmin=dates[c(96)], xmax=dates[c(300-30)], ymin=0, ymax=20000), fill="grey", alpha=0.5)+
  geom_point(aes(x = dates, y = I), size = 1.5, color = "darkblue") +
  geom_line(aes(x = dates, y = I), size = 1, color = "darkblue") +
  geom_line(aes(x = dates, y = TP.incidence), size = 2, color="blue", group = 1) + 
#  geom_ribbon(data = q1, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .1) +
  
  geom_line(data=q2, aes(x = dates, y = TP.incidence), size = 2, color = "light blue") +
#  geom_ribbon(data = q2, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .05) +
  
  geom_line(data=q3, aes(x = dates, y = TP.incidence), size = 2, color = "dark blue") +
  
#  geom_ribbon(data = q3, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .15) +
  
  geom_vline(aes(xintercept = as.numeric(dates[c(142)])), linetype = 2, color = 'red') +
  geom_vline(aes(xintercept = as.numeric(dates[c(240)])), linetype = 2, color = 'red') +
  scale_y_continuous(breaks = seq(0,16000,2000), minor_breaks = seq(0,16000,2000), limits = c(0, 16000), expand = c(0, 0)) + ## scale_y_continuous(label=comma)
  ylab('Incidence') +
  xlab('') +
  ggtitle('') +
  scale_x_date( limits = as.Date(c("2021-07-01", "2022-12-30")), breaks = as.Date( c( "2021-07-01",  "2021-10-01", 
                                                                                      "2022-01-01",  "2022-04-01",   
                                                                                      "2022-07-01",  "2022-10-01" ) ),
                labels = c ("Jul-2021",  "Oct-2021",   "Jan-2022",  "Apr-2022", "Jul-2022", "Oct-2022"),expand = c(0, 0)) +
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

library(zoo)
q2<-q1%>%select("dates", "TP.incidence")
q2$Yearmonth<-as.yearmon(q2$dates, "%Y-%m")

library(dplyr)
q3<-q2 %>% 
  group_by(Yearmonth) %>% 
  summarise(TP.incidence = sum(TP.incidence))

##Vaccine rate 0.7, 0.6, 0.8
plot3<-ggplot(q1) + 
  #  geom_rect(data=q1, aes(xmin=dates[c(96)], xmax=dates[c(126)], ymin=0, ymax=6000), fill="light grey", alpha=0.02)+
  geom_point(aes(x = dates, y = I), size = 1, color = "darkblue", fill = "white") +
  geom_line(aes(x = dates, y = I), size = 0.5, color = "darkblue", fill = "white") +
  #  geom_ribbon(data = q1, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .1) +
  
  geom_line(data=q2, aes(x = dates, y = TP.incidence), size = 2, color = "light blue", fill = "white") +
  geom_line(aes(x = dates, y = TP.incidence), size = 2, color="blue", group = 1) + 
  #  geom_ribbon(data = q2, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .05) +
  
  geom_line(data=q3, aes(x = dates, y = TP.incidence), size = 2, color = "dark blue", fill = "white") +
  geom_line(data=q4, aes(x = dates, y = TP.incidence), size = 2, color = "black") +
  #  geom_ribbon(data = q3, aes(x = dates, ymin = low, ymax = high),fill="red", alpha = .15) +
  
  geom_vline(aes(xintercept = as.numeric(dates[c(142)])), linetype = 2, color = 'black') +
  geom_vline(aes(xintercept = as.numeric(dates[c(240-30)])), linetype = 2, color = 'red') +
  geom_vline(aes(xintercept = as.numeric(dates[c(240)])), linetype = 2, color = 'red') +
  geom_vline(aes(xintercept = as.numeric(dates[c(240+30)])), linetype = 2, color = 'red') +
  scale_y_continuous(breaks = seq(0,16000,2000), minor_breaks = seq(0,16000,2000), limits = c(0, 16000), expand = c(0, 0)) + ## scale_y_continuous(label=comma)
  ylab('Incidence') +
  xlab('') +
  ggtitle('') +
  scale_x_date( limits = as.Date(c("2021-07-01", "2022-12-30")), breaks = as.Date( c( "2021-07-01",  "2021-10-01", 
                                                                                      "2022-01-01",  "2022-04-01",   
                                                                                      "2022-07-01",  "2022-10-01" ) ),
                labels = c ("Jul-2021",  "Oct-2021",   "Jan-2022",  "Apr-2022", "Jul-2022", "Oct-2022"),expand = c(0, 0)) +
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





##Vaccine rate 0.7, 0.6, 0.8
plot3<-ggplot(q1) + 
#  geom_rect(data=q1, aes(xmin=dates[c(96)], xmax=dates[c(126)], ymin=0, ymax=6000), fill="light grey", alpha=0.02)+
  geom_point(aes(x = dates, y = I), size = 1, color = "darkblue", fill = "white") +
  geom_line(aes(x = dates, y = I), size = 0.5, color = "darkblue", fill = "white") +
#  geom_ribbon(data = q1, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .1) +
  geom_line(aes(x = dates, y = TP.incidence), size = 2, color="red", group = 1) +   
  geom_line(data=q2, aes(x = dates, y = TP.incidence), size = 2, color = "#FF9999", fill = "white") +
  
#  geom_ribbon(data = q2, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .05) +
  
  geom_line(data=q3, aes(x = dates, y = TP.incidence), size = 2, color = "#CC0033", fill = "white") +
#  geom_line(data=q4, aes(x = dates, y = TP.incidence), size = 2, color = "black") +
#  geom_ribbon(data = q3, aes(x = dates, ymin = low, ymax = high),fill="red", alpha = .15) +
  
  geom_vline(aes(xintercept = as.numeric(dates[c(142)])), linetype = 2, color = 'black') +
  geom_vline(aes(xintercept = as.numeric(dates[c(240-30)])), linetype = 2, color = 'red') +
  geom_vline(aes(xintercept = as.numeric(dates[c(240)])), linetype = 2, color = 'red') +
  geom_vline(aes(xintercept = as.numeric(dates[c(240+30)])), linetype = 2, color = 'red') +
  scale_y_continuous(breaks = seq(0,16000,2000), minor_breaks = seq(0,16000,2000), limits = c(0, 16000), expand = c(0, 0)) + ## scale_y_continuous(label=comma)
  ylab('Incidence') +
  xlab('') +
  ggtitle('') +
  scale_x_date( limits = as.Date(c("2021-07-01", "2022-12-30")), breaks = as.Date( c( "2021-07-01",  "2021-10-01", 
                                                                                      "2022-01-01",  "2022-04-01",   
                                                                                      "2022-07-01",  "2022-10-01" ) ),
                labels = c ("Jul-2021",  "Oct-2021",   "Jan-2022",  "Apr-2022", "Jul-2022", "Oct-2022"),expand = c(0, 0)) +
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
  #geom_rect(data=q1, aes(xmin=dates[c(96)], xmax=dates[c(156)], ymin=0, ymax=6000), fill="light grey", alpha=0.02)+
  geom_point(aes(x = dates, y = I), size = 1.5, color = "darkblue") +
  geom_line(aes(x = dates, y = I), size = 1, color = "darkblue") +
  geom_line(aes(x = dates, y = TP.incidence), size = 2, color="red", group = 1) + 
#  geom_ribbon(data = q1, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .1) +
  
  geom_line(data=q2, aes(x = dates, y = TP.incidence), size = 2, color = "#FF9999") +
#  geom_ribbon(data = q2, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .05) +
  
  geom_line(data=q3, aes(x = dates, y = TP.incidence), size = 2, color = "#CC0033") +
#  geom_ribbon(data = q3, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .15) +
  
  geom_vline(aes(xintercept = as.numeric(dates[c(142)])), linetype = 2, color = 'red') +
  geom_vline(aes(xintercept = as.numeric(dates[c(240)])), linetype = 2, color = 'red') +
  scale_y_continuous(breaks = seq(0,20000,5000), minor_breaks = seq(0,20000,5000), limits = c(0, 20000), expand = c(0, 0)) + ## scale_y_continuous(label=comma)
  ylab('Incidence') +
  xlab('Date') +
  ggtitle('') +
  scale_x_date( limits = as.Date(c("2021-07-01", "2022-12-30")), breaks = as.Date( c( "2021-07-01",  "2021-10-01", 
                                                                                      "2022-01-01",  "2022-04-01",   
                                                                                      "2022-07-01",  "2022-10-01" ) ),
                labels = c ("Jul-2021",  "Oct-2021",   "Jan-2022",  "Apr-2022", "Jul-2022", "Oct-2022"),expand = c(0, 0)) +
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
                    labels = c("Rt(2.8/3.2/3.7) after April 2022", "NPI by Mar/Apr/May 2022", "Testing(-/+20%)", "Vaccination(68/72/75% by 2023)"),
                    font.label = list(size = 12, color = "dark blue"))+  theme(legend.title = element_text(size = 10, face = "bold"))

annotate_figure(plot.all,
                top = text_grob("Epidemic/Intervention scenarios", color = "black", face = "bold", size = 12),
                bottom = text_grob("", color = "black", hjust = 1, x = 1, face = "italic", size = 8),
                left = text_grob("", color = "black", rot = 90, size = 8),
                right = "",
                fig.lab = "Figure", fig.lab.face = "bold")


# plot.all<-ggarrange(plot1, plot2, plot3, plot4,
#                        ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom", #hjust=-2, #vjust=0.2,
#                        labels = c("Rt(1.13)/1m NPI/Testing(low/high)", "Rt(1.13)/2m NPI/Testing(low/high)", "Rt(1.2)/1m NPI/Testing(low/high)", "Rt(1.2)/2m NPI/Testing(low/high)"),
#                        font.label = list(size = 12, color = "dark blue"))+  theme(legend.title = element_text(size = 10, face = "bold"))
# 
# annotate_figure(plot.all,
#                 top = text_grob("Epidemic/Intervention scenarios", color = "black", face = "bold", size = 12),
#                 bottom = text_grob("", color = "black", hjust = 1, x = 1, face = "italic", size = 8),
#                 left = text_grob("", color = "black", rot = 90, size = 8),
#                 right = "",
#                 fig.lab = "Figure", fig.lab.face = "bold")

plot(0,type='n',axes=FALSE,ann=FALSE, xpd=T)
legend("top", title="Legend",
       c( "low testing value","testing base case","high testing value","confirmed case"), col=c("#FF9999","red", "#CC0033","dark blue"), 
       horiz=FALSE, cex=1.10, xpd=T, lwd=3, pch=c(NA, NA, NA, 16) )

plot(0,type='n',axes=FALSE,ann=FALSE, xpd=T)
legend("top", title="Age group",
       c( "Age 0-19","Age 20-39", "Age 40-59", "Age 60+"), col=c("dark green","blue", "red", "dark grey"), 
       horiz=FALSE, cex=1.10, xpd=T, lwd=3, pch=c(NA, NA, NA) )

legend(as.numeric(q1$dates[c(100)]),0.4,c("age 0-19","age 20-39", "age 40-59", "age 60+"), lwd=c(2,2), col=c("dark green","blue", "red", "dark grey"), y.intersp=1)


ggarrange(plot1,plot1,
          ncol = 2, nrow = 1)

plot(0,type='n',axes=FALSE,ann=FALSE, xpd=T)
legend("top", title="Legend",
       c( "Projected incidence (base case)", "Projected incidence (scenarios)","Confirmed case"), col=c("#FF9999","red", "dark blue"), 
       horiz=FALSE, cex=1.10, xpd=T, lwd=3, pch=c(NA, NA, NA, 16) )


plot(0,type='n',axes=FALSE,ann=FALSE, xpd=T)
legend("top", title="Legend",
       c( "Projected prevalence (base case)", "Projected prevalence (scenarios)","ICU capacity"), col=c("#FF9999","red", "light blue"), 
       horiz=FALSE, cex=1.10, xpd=T, lwd=3, pch=c(NA, NA, NA, 16) )



plot(0,type='n',axes=FALSE,ann=FALSE, xpd=T)
legend("top", title="Legend",
       c( "Number of hospitalized patients (status quo)", "Number of hospitalized patients (eff 30% with molnupiravir)","Number of hospitalized patients (eff 87% with nirmatrelvir/ritonavir )", 
       "Number of ICU patients (status quo)", "Number of ICU patients  (eff 30% with molnupiravir)","Number of ICU patients (eff 87% with nirmatrelvir/ritonavir)", "hospital capacity", "ICU capacity"), 
       col=c("red", "purple","blue","red", "purple","blue", "light blue", "blue"), 
       horiz=FALSE, cex=1.10, xpd=T, lwd=3, lty=c(1,1,1,3,3,3,2,2), pch=c(NA, NA, NA,NA, NA, NA, NA, NA))

plot(0,type='n',axes=FALSE,ann=FALSE, xpd=T)
legend("top", title="Legend",
       c( "U (1000)", "U (2000)","U (3000)","Confirmed case"), col=c("red","#FF9999","#CC0033", "dark blue"), 
       horiz=FALSE, cex=1.10, xpd=T, lwd=3, pch=c(NA, NA, NA, 16) )

plot(0,type='n',axes=FALSE,ann=FALSE, xpd=T)
legend("top", title="Legend",
       c( "E(10); A(1)", "E(50); A(5)","E(100); A(10)","Confirmed case"), col=c("red","#FF9999","#CC0033", "dark blue"), 
       horiz=FALSE, cex=1.10, xpd=T, lwd=3, pch=c(NA, NA, NA, 16) )

plot(0,type='n',axes=FALSE,ann=FALSE, xpd=T)
legend("top", title="Legend",
       c( "testing (base:0.05)", "testing (0.01)","testing (0.1)","Confirmed case"), col=c("red","#FF9999","#CC0033", "dark blue"), 
       horiz=FALSE, cex=1.10, xpd=T, lwd=3, pch=c(NA, NA, NA, 16) )


plot(0,type='n',axes=FALSE,ann=FALSE, xpd=T)
legend("top", title="Legend",
       c( "force of infection (base)", "force of infection (x0.5)","force of infection (x1.5)","Confirmed case"), col=c("red","#FF9999","#CC0033", "dark blue"), 
       horiz=FALSE, cex=1.10, xpd=T, lwd=3, pch=c(NA, NA, NA, 16) )

plot(0,type='n',axes=FALSE,ann=FALSE, xpd=T)
legend("top", title="Legend",
       c( "Testing (7/6, base)", "Testing (6/22, 8th day)","Testing (6/17, 3rd day)","Confirmed case"), col=c("red","#FF9999","#CC0033", "dark blue"), 
       horiz=FALSE, cex=1.10, xpd=T, lwd=3, pch=c(NA, NA, NA, 16) )

plot(0,type='n',axes=FALSE,ann=FALSE, xpd=T)
legend("top", title="Legend",
       c( "Testing (7/6, base)", "Testing (6/30, 1 wk before)","Testing (6/24, 2 wks before)","Confirmed case"), col=c("red","#FF9999","#CC0033", "dark blue"), 
       horiz=FALSE, cex=1.10, xpd=T, lwd=3, pch=c(NA, NA, NA, 16) )




plot(0,type='n',axes=FALSE,ann=FALSE, xpd=T)
legend("top", title="Legend",
       c( "force of infection (base)", "force of infection (x0.5)","force of infection (x1.5)"), col=c("red","#FF9999","#CC0033"), 
       horiz=FALSE, cex=1.10, xpd=T, lwd=3, pch=c(NA, NA, NA) )

plot(0,type='n',axes=FALSE,ann=FALSE, xpd=T)
legend("top", title="Legend",
       c( "E(10); A(1)", "E(50); A(5)","E(100); A(10)"), col=c("red","#FF9999","#CC0033"), 
       horiz=FALSE, cex=1.10, xpd=T, lwd=3, pch=c(NA, NA, NA) )

plot(0,type='n',axes=FALSE,ann=FALSE, xpd=T)
legend("top", title="Legend",
       c( "U (1000)", "U (2000)","U (3000)"), col=c("red","#FF9999","#CC0033"), 
       horiz=FALSE, cex=1.10, xpd=T, lwd=3, pch=c(NA, NA, NA) )

plot(0,type='n',axes=FALSE,ann=FALSE, xpd=T)
legend("top", title="Legend",
       c( "Testing(basex0.8)", "Testing(base)","Testing(basex1.2)"), col=c("#FF9999","red","#CC0033"), 
       horiz=FALSE, cex=1.10, xpd=T, lwd=3, pch=c(NA, NA, NA) )
D
