###########################################
#Heatmaps
###########################################

hm.base1 <-mapply(ts.sir.hm, Rt=1, npi1=96, npi2 =1, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.95, 1, 1.2), times=3), SIMPLIFY=FALSE)
hm.base2 <-mapply(ts.sir.hm, Rt=1, npi1=126, npi2 =0.85, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.95, 1, 1.2), times=3),  SIMPLIFY=FALSE)
hm.base3 <-mapply(ts.sir.hm, Rt=1, npi1=156, npi2 =0.85, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.95, 1, 1.2), times=3),  SIMPLIFY=FALSE)
#hm.base4 <-mapply(ts.sir.hm, Rt=1.04, npi1=96, npi2 =1, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.95, 1, 1.2), times=3),  SIMPLIFY=FALSE)
#hm.base5 <-mapply(ts.sir.hm, Rt=1.04, npi1=126, npi2 =0.85, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.95, 1, 1.2), times=3), SIMPLIFY=FALSE)
#hm.base6 <-mapply(ts.sir.hm, Rt=1.04, npi1=156, npi2 =0.85, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.95, 1, 1.2), times=3),  SIMPLIFY=FALSE)
hm.base7 <-mapply(ts.sir.hm, Rt=1.1, npi1=96, npi2 =1, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.95, 1, 1.2), times=3),  SIMPLIFY=FALSE)
hm.base8 <-mapply(ts.sir.hm, Rt=1.1, npi1=126, npi2 =0.85, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.95, 1, 1.2), times=3),  SIMPLIFY=FALSE)
hm.base9 <-mapply(ts.sir.hm, Rt=1.1, npi1=156, npi2 =0.85, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.95, 1, 1.2), times=3),  SIMPLIFY=FALSE)

hm.df1 <- as.data.frame(do.call("rbind", hm.base1))
hm.df2 <- as.data.frame(do.call("rbind", hm.base2))
hm.df3 <- as.data.frame(do.call("rbind", hm.base3))
# hm.df4 <- as.data.frame(do.call("rbind", hm.base4))
# hm.df5 <- as.data.frame(do.call("rbind", hm.base5))
# hm.df6 <- as.data.frame(do.call("rbind", hm.base6))
hm.df7 <- as.data.frame(do.call("rbind", hm.base7))
hm.df8 <- as.data.frame(do.call("rbind", hm.base8))
hm.df9 <- as.data.frame(do.call("rbind", hm.base9))

df_hm <- read.csv("heatmap_kr1.csv")

df_hm$npi <- factor(df_hm$npi, levels = c("1", "2", "3"),
                  labels = c("No NPI", "4wk NPI",  "8wk NPI"))

df_hm$test <- factor(df_hm$test, levels = c("0.8",  "1",  "1.2"),
                        labels = c("0.95", "1",  "1.2"))

df_hm$vac <- factor(df_hm$vac, levels = c("0.8","1",   "1.2"),
                        labels = c("0.8" , "1", "1.2"))

df_hm$age <- factor(df_hm$age, levels = c("1", "2", "3",  "4"),
                    labels = c("0-19", "20-39", "40-59",  "60+"))

# names(df1)[names(df1) == 'test_eff'] <- "Effectiveness"
# names(df1)[names(df1) == 'frequency'] <- "Freq per week"

df_hm$Rt<-1.1
df_hm_Rt1<-df_hm
df_hm_Rt1$cum_inc<-c(hm.df1$TP1.incidence,  hm.df1$TP2.incidence,  hm.df1$TP3.incidence,  hm.df1$TP4.incidence,
                     hm.df2$TP1.incidence,  hm.df2$TP2.incidence,  hm.df2$TP3.incidence,  hm.df2$TP4.incidence,
                     hm.df3$TP1.incidence,  hm.df3$TP2.incidence,  hm.df3$TP3.incidence,  hm.df3$TP4.incidence
)/1000

# df_hm$Rt<-1.16
# df_hm_Rt2<-df_hm
# df_hm_Rt2$cum_inc<-c(hm.df4$TP1.incidence,  hm.df4$TP2.incidence,  hm.df4$TP3.incidence,  hm.df4$TP4.incidence,
#                      hm.df5$TP1.incidence,  hm.df5$TP2.incidence,  hm.df5$TP3.incidence,  hm.df5$TP4.incidence,
#                      hm.df6$TP1.incidence,  hm.df6$TP2.incidence,  hm.df6$TP3.incidence,  hm.df6$TP4.incidence
# )/1000

df_hm$Rt<-1.2
df_hm_Rt3<-df_hm
df_hm_Rt3$cum_inc<-c(hm.df7$TP1.incidence,  hm.df7$TP2.incidence,  hm.df7$TP3.incidence,  hm.df7$TP4.incidence,
                     hm.df8$TP1.incidence,  hm.df8$TP2.incidence,  hm.df8$TP3.incidence,  hm.df8$TP4.incidence,
                     hm.df9$TP1.incidence,  hm.df9$TP2.incidence,  hm.df9$TP3.incidence,  hm.df9$TP4.incidence
)/1000

df_hm_Rt_all<-rbind(df_hm_Rt1, df_hm_Rt3)

#install.packages("ggplot")
library(ggplot2)
heatmap.all <- ggplot(data = df_hm_Rt_all, 
                   mapping = aes(x = `vac`,
                                 y = `test`,
                                 fill = cum_inc)) +
  geom_tile() +
  xlab(label = "vaccination") +
  ylab(label = "testing") +
  facet_grid(Rt~ age ~  npi, scales = "free_y", labeller = label_both) +
  scale_fill_distiller(palette=8, trans="reverse", name = "cum_inc (in thousand)") +
  theme(strip.placement = "outside") +
  theme_classic() +
  geom_text(aes(label = paste0(round(cum_inc), "")), size=2.5) +
  theme(strip.text.y = element_text(size =8, colour = "black", angle = 270))
heatmap.all 
ggsave(heatmap.all, filename = "heatmap_all.png", width = 7, height = 5)
graphics.off()


heatmap1 <- ggplot(data = df_hm_Rt1, 
                   mapping = aes(x = `test`,
                                 y = `vac`,
                                 fill = cum_inc)) +
  geom_tile() +
  xlab(label = "testing") +
  ylab(label = "vaccination") +
  facet_grid(age ~  npi, scales = "free_y", labeller = label_both) +
  scale_fill_distiller(palette=8, trans="reverse", name = "cum_inc (in thousand)") +
  theme(strip.placement = "outside") +
  theme_classic() +
  geom_text(aes(label = paste0(round(cum_inc), "")), size=2.5) +
  theme(strip.text.y = element_text(size = 6, colour = "black", angle = 270))
heatmap1 
ggsave(heatmap1, filename = "heatmap_Rt1.png", width = 7, height = 5)
graphics.off()

heatmap2 <- ggplot(data = df_hm_Rt2, 
                   mapping = aes(x = `test`,
                                 y = `vac`,
                                 fill = cum_inc)) +
  geom_tile() +
  xlab(label = "testing") +
  ylab(label = "vaccination") +
  facet_grid(age ~  npi, scales = "free_y", labeller = label_both) +
  scale_fill_distiller(palette=8, trans="reverse", name = "cum_inc") +
  theme(strip.placement = "outside") +
  theme_classic() +
  geom_text(aes(label = paste0(round(cum_inc), "")), size=2.5) +
  theme(strip.text.y = element_text(size = 6, colour = "black", angle = 270))
heatmap2 
ggsave(heatmap2, filename = "heatmap_Rt2.png", width = 7, height = 5)
graphics.off()

heatmap3 <- ggplot(data = df_hm_Rt3, 
                   mapping = aes(x = `test`,
                                 y = `vac`,
                                 fill = cum_inc)) +
  geom_tile() +
  xlab(label = "testing") +
  ylab(label = "vaccination") +
  facet_grid(age ~  npi, scales = "free_y", labeller = label_both) +
  scale_fill_distiller(palette=8, trans="reverse", name = "cum_inc") +
  theme(strip.placement = "outside") +
  theme_classic() +
  geom_text(aes(label = paste0(round(cum_inc), "")), size=2.5) +
  theme(strip.text.y = element_text(size = 6, colour = "black", angle = 270))
heatmap3 

ggsave(heatmap3, filename = "heatmap_Rt3.png", width = 7, height = 5)
graphics.off()

###########################################
#Time series
###########################################

##Graphs: total 81 graphs : 9 graphs per vac.test.base x 9 vac.test.base = 81
timeseries.base1 <-mapply(ts.sir.timeseries, Rt=1, npi1=96, npi2 =1, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.95, 1, 1.2), times=3), SIMPLIFY=FALSE)
timeseries.base2 <-mapply(ts.sir.timeseries, Rt=1, npi1=126, npi2 =0.85, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.95, 1, 1.2), times=3), SIMPLIFY=FALSE)
timeseries.base3 <-mapply(ts.sir.timeseries, Rt=1, npi1=156, npi2 =0.85, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.95, 1, 1.2), times=3), SIMPLIFY=FALSE)
timeseries.base4 <-mapply(ts.sir.timeseries, Rt=1.05, npi1=96, npi2 =1, vaccinating=rep(c(0.5, 1, 1.5), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
timeseries.base5 <-mapply(ts.sir.timeseries, Rt=1.05, npi1=126, npi2 =0.85, vaccinating=rep(c(0.5, 1, 1.5), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
timeseries.base6 <-mapply(ts.sir.timeseries, Rt=1.05, npi1=156, npi2 =0.85, vaccinating=rep(c(0.5, 1, 1.5), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
timeseries.base7 <-mapply(ts.sir.timeseries, Rt=1.1, npi1=96, npi2 =1, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.95, 1, 1.2), times=3), SIMPLIFY=FALSE)
timeseries.base8 <-mapply(ts.sir.timeseries, Rt=1.1, npi1=126, npi2 =0.85, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.95, 1, 1.2), times=3), SIMPLIFY=FALSE)
timeseries.base9 <-mapply(ts.sir.timeseries, Rt=1.1, npi1=156, npi2 =0.85, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.95, 1, 1.2), times=3), SIMPLIFY=FALSE)
#  
# lapply(timeseries.base1)
# 
# 
out_plot1 <- SEIR_smplot(result3 = timeseries.base1[[1]], #Rt=1, npi1=49, npi2 =1, vaccinating=0.5, testing=0.8
                         result4 = timeseries.base1[[2]],  #Rt=1, npi1=49, npi2 =1, vaccinating=0.5, testing=1
                         result5 = timeseries.base1[[3]],  #Rt=1, npi1=49, npi2 =1, vaccinating=0.5, testing=1.2
                         result6 = timeseries.base1[[4]],  #Rt=1, npi1=49, npi2 =1, vaccinating=1, testing=0.8
                         result7 = timeseries.base1[[5]],  #Rt=1, npi1=49, npi2 =1, vaccinating=1, testing=1
                         result8 = timeseries.base1[[6]],  #Rt=1, npi1=49, npi2 =1, vaccinating=1, testing=1.2
                         result9 = timeseries.base1[[7]],  #Rt=1, npi1=49, npi2 =1, vaccinating=1.5, testing=0.8
                         result10 = timeseries.base1[[8]], #Rt=1, npi1=49, npi2 =1, vaccinating=1.5, testing=1
                         result11 = timeseries.base1[[9]]  #Rt=1, npi1=49, npi2 =1, vaccinating=1.5, testing=1.2
)
out_plot1 ##Rt=1, npi1=49, npi2 =1, test=0.8/1/1.2;; vac=0.5, 1, 1.5

# ggsave(out_plot1, filename = "plot1_Rt1_npi1_87_npi2_1_test_vac.png", width = 8, height = 3)
# graphics.off()

out_plot2 <- SEIR_smplot(result3 = timeseries.base2[[1]],  #Rt=1, npi1=64, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result4 = timeseries.base2[[2]],   #Rt=1, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1
                         result5 = timeseries.base2[[3]],   #Rt=1, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1.2
                         result6 = timeseries.base2[[4]],   #Rt=1, npi1=64, npi2 =0.95, vaccinating=1, testing=0.8
                         result7 = timeseries.base2[[5]],   #Rt=1, npi1=64, npi2 =0.95, vaccinating=1, testing=1
                         result8 = timeseries.base2[[6]],   #Rt=1, npi1=64, npi2 =0.95, vaccinating=1, testing=1.2
                         result9 = timeseries.base2[[7]],   #Rt=1, npi1=64, npi2 =0.95, vaccinating=1.5, testing=0.8
                         result10 = timeseries.base2[[8]],  #Rt=1, npi1=64, npi2 =0.95, vaccinating=1.5, testing=1
                         result11 = timeseries.base2[[9]]   #Rt=1, npi1=64, npi2 =0.95, vaccinating=1.5, testing=1.2
) 
#out_plot2 ##Rt=1, npi1=64, npi2 =0.95, test=0.8/1/1.2;; vac=0.5, 1, 1.5

# ggsave(out_plot2, filename = "plot2_Rt1_npi1_117_npi2_0.9_test_vac.png", width = 8, height = 3)
# graphics.off()

out_plot3 <- SEIR_smplot(result3 = timeseries.base3[[1]], #Rt=1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result4 = timeseries.base3[[2]], #Rt=1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1
                         result5 = timeseries.base3[[3]], #Rt=1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1.2
                         result6 = timeseries.base3[[4]], #Rt=1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result7 = timeseries.base3[[5]], #Rt=1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1
                         result8 = timeseries.base3[[6]], #Rt=1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1.2
                         result9 = timeseries.base3[[7]], #Rt=1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result10 = timeseries.base3[[8]], #Rt=1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1
                         result11 = timeseries.base3[[9]] #Rt=1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1.2
) 
#out_plot3 ##Rt=1, npi1=78, npi2 =0.95, test=0.8/1/1.2;; vac=0.5, 1, 1.5

# ggsave(out_plot3, filename = "plot3_Rt1_npi1_147_npi2_0.9_test_vac.png", width = 8, height = 3)
# graphics.off()
plot.all <- ggarrange(
  out_plot1,out_plot2,out_plot3, 
  ncol = 1, nrow = 3, common.legend = TRUE, legend = "bottom",
  #hjust=-2, #vjust=0.2,
  labels = c(  "   No NPI (Vaccination low/base/high)",   "1month NPI (Vaccination low/base/high)",   "2months NPI (Vaccination low/base/high)"  ),
  font.label = list(size = 10, color = "dark blue")) +  
  theme(legend.title = element_text(size = 10, face = "bold"))

annotate_figure(
  plot.all, 
  top = text_grob("Rt 1.1",color = "black",  face = "bold",  size = 12  ),
  bottom = text_grob("",  color = "black",  hjust = 1,  x = 1,  face = "italic",  size = 8  ),
  left = text_grob( "",   color = "black",  rot = 90,  size = 8   ),
  right = "",
  #fig.lab = "Figure",
  fig.lab.face = "bold"
)



out_plot4 <- SEIR_smplot(result3 = timeseries.base4[[1]], #Rt=1.05, npi1=49, npi2 =1, vaccinating=0.5, testing=0.8
                         result4 = timeseries.base4[[2]], #Rt=1.05, npi1=49, npi2 =1, vaccinating=0.5, testing=1
                         result5 = timeseries.base4[[3]], #Rt=1.05, npi1=49, npi2 =1, vaccinating=0.5, testing=1.2
                         result6 = timeseries.base4[[4]], #Rt=1.05, npi1=49, npi2 =1, vaccinating=0.5, testing=0.8
                         result7 = timeseries.base4[[5]], #Rt=1.05, npi1=49, npi2 =1, vaccinating=0.5, testing=1
                         result8 = timeseries.base4[[6]], #Rt=1.05, npi1=49, npi2 =1, vaccinating=0.5, testing=1.2
                         result9 = timeseries.base4[[7]], #Rt=1.05, npi1=49, npi2 =1, vaccinating=0.5, testing=0.8
                         result10 = timeseries.base4[[8]],#Rt=1.05, npi1=49, npi2 =1, vaccinating=0.5, testing=1
                         result11 = timeseries.base4[[9]] #Rt=1.05, npi1=49, npi2 =1, vaccinating=0.5, testing=1.2
) 
#out_plot4 ##Rt=1.05, npi1=49, npi2 =1, test=0.8/1/1.2;; vac=0.5, 1, 1.5
# ggsave(out_plot4, filename = "plot4_Rt1.05_npi1_87_npi2_1_test_vac.png", width = 8, height = 3)
# graphics.off()

out_plot5 <- SEIR_smplot(result3 = timeseries.base5[[1]], #Rt=1.05, npi1=64, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result4 = timeseries.base5[[2]], #Rt=1.05, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1
                         result5 = timeseries.base5[[3]], #Rt=1.05, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1.2
                         result6 = timeseries.base5[[4]], #Rt=1.05, npi1=64, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result7 = timeseries.base5[[5]], #Rt=1.05, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1
                         result8 = timeseries.base5[[6]], #Rt=1.05, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1.2
                         result9 = timeseries.base5[[7]], #Rt=1.05, npi1=64, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result10 = timeseries.base5[[8]], #Rt=1.05, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1
                         result11 = timeseries.base5[[9]] #Rt=1.05, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1.2
) 
#out_plot5 ##Rt=1.05, npi1=64, npi2 =0.95, test=0.8/1/1.2;; vac=0.5, 1, 1.5
# ggsave(out_plot5, filename = "plot5_Rt1.05_npi1_117_npi2_0.9_test_vac.png", width = 8, height = 3)
# graphics.off()

out_plot6 <- SEIR_smplot(result3 = timeseries.base6[[1]], #Rt=1.05, npi1=78, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result4 = timeseries.base6[[2]], #Rt=1.05, npi1=78, npi2 =0.95,  vaccinating=0.5, testing=1
                         result5 = timeseries.base6[[3]], #Rt=1.05, npi1=78, npi2 =0.95,  vaccinating=0.5, testing=1.2
                         result6 = timeseries.base6[[4]], #Rt=1.05, npi1=78, npi2 =0.95,  vaccinating=0.5, testing=0.8
                         result7 = timeseries.base6[[5]], #Rt=1.05, npi1=78, npi2 =0.95,  vaccinating=0.5, testing=1
                         result8 = timeseries.base6[[6]], #Rt=1.05, npi1=78, npi2 =0.95,  vaccinating=0.5, testing=1.2
                         result9 = timeseries.base6[[7]], #Rt=1.05, npi1=78, npi2 =0.95,  vaccinating=0.5, testing=0.8
                         result10 = timeseries.base6[[8]],#Rt=1.05, npi1=78, npi2 =0.95,  vaccinating=0.5, testing=1
                         result11 = timeseries.base6[[9]] #Rt=1.05, npi1=78, npi2 =0.95,  vaccinating=0.5, testing=1.2
) 
#out_plot6 ##Rt=1.05, npi1=78, npi2 =0.95, test=0.8/1/1.2;; vac=0.5, 1, 1.5

plot.all <- ggarrange(
  out_plot4,out_plot5,out_plot6, 
  ncol = 1, nrow = 3, common.legend = TRUE, legend = "bottom",
  #hjust=-2, #vjust=0.2,
  labels = c(  "   No NPI (Vaccination low/base/high)",   "1month NPI (Vaccination low/base/high)",   "2months NPI (Vaccination low/base/high)"  ),
  font.label = list(size = 10, color = "dark blue")) +  
  theme(legend.title = element_text(size = 10, face = "bold"))

annotate_figure(
  plot.all, 
  top = text_grob("Rt 1.16",color = "black",  face = "bold",  size = 12  ),
  bottom = text_grob("",  color = "black",  hjust = 1,  x = 1,  face = "italic",  size = 8  ),
  left = text_grob( "",   color = "black",  rot = 90,  size = 8   ),
  right = "",
  #fig.lab = "Figure",
  fig.lab.face = "bold"
)

# ggsave(out_plot6, filename = "plot6_Rt1.05_npi1_147_npi2_0.9_test_vac.png", width = 8, height = 3)
# graphics.off()

out_plot7 <- SEIR_smplot(result3 = timeseries.base7[[1]], #Rt=1.1, npi1=49, npi2 =1, vaccinating=0.5, testing=0.8
                         result4 = timeseries.base7[[2]], #Rt=1.1, npi1=49, npi2 =1,  vaccinating=0.5, testing=1
                         result5 = timeseries.base7[[3]], #Rt=1.1, npi1=49, npi2 =1,  vaccinating=0.5, testing=1.2
                         result6 = timeseries.base7[[4]], #Rt=1.1, npi1=49, npi2 =1,  vaccinating=0.5, testing=0.8
                         result7 = timeseries.base7[[5]], #Rt=1.1, npi1=49, npi2 =1,  vaccinating=0.5, testing=1
                         result8 = timeseries.base7[[6]], #Rt=1.1, npi1=49, npi2 =1,  vaccinating=0.5, testing=1.2
                         result9 = timeseries.base7[[7]], #Rt=1.1, npi1=49, npi2 =1,  vaccinating=0.5, testing=0.8
                         result10 = timeseries.base7[[8]],#Rt=1.1, npi1=49, npi2 =1,  vaccinating=0.5, testing=1
                         result11 = timeseries.base7[[9]] #Rt=1.1, npi1=49, npi2 =1,  vaccinating=0.5, testing=1.2
) 
#out_plot7  ##Rt=1.1, npi1=49, npi2 =1, test=0.8/1/1.2;; vac=0.5, 1, 1.5

# ggsave(out_plot7, filename = "plot7_Rt1.1_npi1_87_npi2_1_test_vac.png", width = 8, height = 3)
# graphics.off()

out_plot8 <- SEIR_smplot(result3 = timeseries.base8[[1]], #Rt=1.1, npi1=64, npi2 =0.95 vaccinating=0.5, testing=0.8
                         result4 = timeseries.base8[[2]], #Rt=1.1, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1
                         result5 = timeseries.base8[[3]], #Rt=1.1, npi1=64, npi2 =0.95 vaccinating=0.5, testing=1.2
                         result6 = timeseries.base8[[4]], #Rt=1.1, npi1=64, npi2 =0.95 vaccinating=0.5, testing=0.8
                         result7 = timeseries.base8[[5]], #Rt=1.1, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1
                         result8 = timeseries.base8[[6]], #Rt=1.1, npi1=64, npi2 =0.95 vaccinating=0.5, testing=1.2
                         result9 = timeseries.base8[[7]], #Rt=1.1, npi1=64, npi2 =0.95 vaccinating=0.5, testing=0.8
                         result10 = timeseries.base8[[8]],#Rt=1.1, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1
                         result11 = timeseries.base8[[9]] #Rt=1.1, npi1=64, npi2 =0.95 vaccinating=0.5, testing=1.2
) 
#out_plot8 ##Rt=1.1, npi1=64, npi2 =0.95, test=0.8/1/1.2;; vac=0.5, 1, 1.5

# ggsave(out_plot8, filename = "plot8_Rt1.1_npi1_117_npi2_0.9_test_vac.png", width = 8, height = 3)
# graphics.off()

out_plot9 <- SEIR_smplot(result3 = timeseries.base9[[1]], #Rt=1.1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result4 = timeseries.base9[[2]], #Rt=1.1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1
                         result5 = timeseries.base9[[3]], #Rt=1.1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1.2
                         result6 = timeseries.base9[[4]], #Rt=1.1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result7 = timeseries.base9[[5]], #Rt=1.1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1
                         result8 = timeseries.base9[[6]], #Rt=1.1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1.2
                         result9 = timeseries.base9[[7]], #Rt=1.1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result10 = timeseries.base9[[8]],#Rt=1.1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1
                         result11 = timeseries.base9[[9]] #Rt=1.1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1.2
) 
#out_plot9 ##Rt=1.1, npi1=78, npi2 =0.95, test=0.8/1/1.2;; vac=0.5, 1, 1.5

# ggsave(out_plot9, filename = "plot9_Rt1.1_npi1_147_npi2_0.9_test_vac.png", width = 8, height = 3)
# graphics.off()


plot.all <- ggarrange(
  out_plot7,out_plot8,out_plot9, 
  ncol = 1, nrow = 3, common.legend = TRUE, legend = "bottom",
  #hjust=-2, #vjust=0.2,
  labels = c(  "   No NPI (Vaccination low/base/high)",   "1month NPI (Vaccination low/base/high)",   "2months NPI (Vaccination low/base/high)"  ),
  font.label = list(size = 10, color = "dark blue")) +  
  theme(legend.title = element_text(size = 10, face = "bold"))

annotate_figure(
  plot.all, 
  top = text_grob("Rt 1.2",color = "black",  face = "bold",  size = 12  ),
  bottom = text_grob("",  color = "black",  hjust = 1,  x = 1,  face = "italic",  size = 8  ),
  left = text_grob( "",   color = "black",  rot = 90,  size = 8   ),
  right = "",
  #fig.lab = "Figure",
  fig.lab.face = "bold"
)


