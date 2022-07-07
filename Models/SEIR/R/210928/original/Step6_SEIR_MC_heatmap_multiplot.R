#plot(result$time, result$TP.incidence)

vac.test.base1 <-mapply(ts.sir.calc1, Rt=1, npi1=87, npi2 =1, vaccinating=rep(c(0.8, 0.9,1, 1.1,1.2), each=5), testing=rep(c(0.8,0.9, 1, 1.1, 1.2), times=5), SIMPLIFY=FALSE)
vac.test.base2 <-mapply(ts.sir.calc1, Rt=1, npi1=117, npi2 =0.85, vaccinating=rep(c(0.8, 0.9,1, 1.1,1.2), each=5), testing=rep(c(0.8,0.9, 1, 1.1, 1.2), times=5), SIMPLIFY=FALSE)
vac.test.base3 <-mapply(ts.sir.calc1, Rt=1, npi1=147, npi2 =0.85, vaccinating=rep(c(0.8, 0.9,1, 1.1,1.2), each=5), testing=rep(c(0.8,0.9, 1, 1.1, 1.2), times=5), SIMPLIFY=FALSE)
vac.test.base4 <-mapply(ts.sir.calc1, Rt=1.05, npi1=87, npi2 =1, vaccinating=rep(c(0.8, 0.9,1, 1.1,1.2), each=5), testing=rep(c(0.8,0.9, 1, 1.1, 1.2), times=5), SIMPLIFY=FALSE)
vac.test.base5 <-mapply(ts.sir.calc1, Rt=1.05, npi1=117, npi2 =0.85, vaccinating=rep(c(0.8, 0.9,1, 1.1,1.2), each=5), testing=rep(c(0.8,0.9, 1, 1.1, 1.2), times=5), SIMPLIFY=FALSE)
vac.test.base6 <-mapply(ts.sir.calc1, Rt=1.05, npi1=147, npi2 =0.85, vaccinating=rep(c(0.8, 0.9,1, 1.1,1.2), each=5), testing=rep(c(0.8,0.9, 1, 1.1, 1.2), times=5), SIMPLIFY=FALSE)
vac.test.base7 <-mapply(ts.sir.calc1, Rt=1.1, npi1=87, npi2 =1, vaccinating=rep(c(0.8, 0.9,1, 1.1,1.2), each=5), testing=rep(c(0.8,0.9, 1, 1.1, 1.2), times=5), SIMPLIFY=FALSE)
vac.test.base8 <-mapply(ts.sir.calc1, Rt=1.1, npi1=117, npi2 =0.85, vaccinating=rep(c(0.8, 0.9,1, 1.1,1.2), each=5), testing=rep(c(0.8,0.9, 1, 1.1, 1.2), times=5), SIMPLIFY=FALSE)
vac.test.base9 <-mapply(ts.sir.calc1, Rt=1.1, npi1=147, npi2 =0.85, vaccinating=rep(c(0.8, 0.9,1, 1.1,1.2), each=5), testing=rep(c(0.8,0.9, 1, 1.1, 1.2), times=5), SIMPLIFY=FALSE)

vac.test.df1 <- as.data.frame(do.call("rbind", vac.test.base1))
vac.test.df2 <- as.data.frame(do.call("rbind", vac.test.base2))
vac.test.df3 <- as.data.frame(do.call("rbind", vac.test.base3))
vac.test.df4 <- as.data.frame(do.call("rbind", vac.test.base4))
vac.test.df5 <- as.data.frame(do.call("rbind", vac.test.base5))
vac.test.df6 <- as.data.frame(do.call("rbind", vac.test.base6))
vac.test.df7 <- as.data.frame(do.call("rbind", vac.test.base7))
vac.test.df8 <- as.data.frame(do.call("rbind", vac.test.base8))
vac.test.df9 <- as.data.frame(do.call("rbind", vac.test.base9))

df_hm <- read.csv("heatmap_kr.csv")

df_hm$npi <- factor(df_hm$npi, levels = c("49", "78", "103"),
                  labels = c("No NPI", "4wk NPI",  "8wk NPI"))

df_hm$test <- factor(df_hm$test, levels = c("0.8", "0.9", "1",  "1.1", "1.2"),
                        labels = c("0.8", "0.9", "1",  "1.1", "1.2"))

df_hm$vac <- factor(df_hm$vac, levels = c("0.8", "0.9", "1",  "1.1", "1.2"),
                        labels = c("0.8", "0.9", "1",  "1.1", "1.2"))

df_hm$age <- factor(df_hm$age, levels = c("1", "2", "3",  "4"),
                    labels = c("0-19", "20-39", "40-59",  "60+"))

# names(df1)[names(df1) == 'test_eff'] <- "Effectiveness"
# names(df1)[names(df1) == 'frequency'] <- "Freq per week"

df_hm$Rt<-1
df_hm_Rt1<-df_hm
df_hm_Rt1$cum_inc<-c(vac.test.df1$TP1.incidence,  vac.test.df1$TP2.incidence,  vac.test.df1$TP3.incidence,  vac.test.df1$TP4.incidence,
                     vac.test.df2$TP1.incidence,  vac.test.df2$TP2.incidence,  vac.test.df2$TP3.incidence,  vac.test.df2$TP4.incidence,
                     vac.test.df3$TP1.incidence,  vac.test.df3$TP2.incidence,  vac.test.df3$TP3.incidence,  vac.test.df3$TP4.incidence
)/1000

df_hm$Rt<-1.025
df_hm_Rt2<-df_hm
df_hm_Rt2$cum_inc<-c(vac.test.df4$TP1.incidence,  vac.test.df4$TP2.incidence,  vac.test.df4$TP3.incidence,  vac.test.df4$TP4.incidence,
                     vac.test.df5$TP1.incidence,  vac.test.df5$TP2.incidence,  vac.test.df5$TP3.incidence,  vac.test.df5$TP4.incidence,
                     vac.test.df6$TP1.incidence,  vac.test.df6$TP2.incidence,  vac.test.df6$TP3.incidence,  vac.test.df6$TP4.incidence
)/1000

df_hm$Rt<-1.05
df_hm_Rt3<-df_hm
df_hm_Rt3$cum_inc<-c(vac.test.df7$TP1.incidence,  vac.test.df7$TP2.incidence,  vac.test.df7$TP3.incidence,  vac.test.df7$TP4.incidence,
                     vac.test.df8$TP1.incidence,  vac.test.df8$TP2.incidence,  vac.test.df8$TP3.incidence,  vac.test.df8$TP4.incidence,
                     vac.test.df9$TP1.incidence,  vac.test.df9$TP2.incidence,  vac.test.df9$TP3.incidence,  vac.test.df9$TP4.incidence
)/1000

df_hm_Rt_all<-rbind(df_hm_Rt1, df_hm_Rt2, df_hm_Rt3)
###########################################
#Heatmaps
###########################################
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
  theme(strip.text.y = element_text(size = 6, colour = "black", angle = 270))
heatmap.all 
ggsave(heatmap.all, filename = "heatmap_all.png", width = 7, height = 5)
graphics.off()


heatmap1 <- ggplot(data = df_hm_Rt1, 
                   mapping = aes(x = `vac`,
                                 y = `test`,
                                 fill = cum_inc)) +
  geom_tile() +
  xlab(label = "vaccination") +
  ylab(label = "testing") +
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
                   mapping = aes(x = `vac`,
                                 y = `test`,
                                 fill = cum_inc)) +
  geom_tile() +
  xlab(label = "vaccination") +
  ylab(label = "testing") +
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
                   mapping = aes(x = `vac`,
                                 y = `test`,
                                 fill = cum_inc)) +
  geom_tile() +
  xlab(label = "vaccination") +
  ylab(label = "testing") +
  facet_grid(age ~  npi, scales = "free_y", labeller = label_both) +
  scale_fill_distiller(palette=8, trans="reverse", name = "cum_inc") +
  theme(strip.placement = "outside") +
  theme_classic() +
  geom_text(aes(label = paste0(round(cum_inc), "")), size=2.5) +
  theme(strip.text.y = element_text(size = 6, colour = "black", angle = 270))
heatmap3 

ggsave(heatmap3, filename = "heatmap_Rt3.png", width = 7, height = 5)
graphics.off()




##Graphs: total 81 graphs : 9 graphs per vac.test.base x 9 vac.test.base = 81
vac.test.base1 <-mapply(ts.sir.calc2, Rt=1, npi1=87, npi2 =1, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
vac.test.base2 <-mapply(ts.sir.calc2, Rt=1, npi1=117, npi2 =0.85, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
vac.test.base3 <-mapply(ts.sir.calc2, Rt=1, npi1=147, npi2 =0.85, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
vac.test.base4 <-mapply(ts.sir.calc2, Rt=1.05, npi1=87, npi2 =1, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
vac.test.base5 <-mapply(ts.sir.calc2, Rt=1.05, npi1=117, npi2 =0.85, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
vac.test.base6 <-mapply(ts.sir.calc2, Rt=1.05, npi1=147, npi2 =0.85, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
vac.test.base7 <-mapply(ts.sir.calc2, Rt=1.1, npi1=87, npi2 =1, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
vac.test.base8 <-mapply(ts.sir.calc2, Rt=1.1, npi1=117, npi2 =0.85, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
vac.test.base9 <-mapply(ts.sir.calc2, Rt=1.1, npi1=147, npi2 =0.85, vaccinating=rep(c(0.8, 1, 1.2), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
#  
# lapply(vac.test.base1)
# 
# 
source("../Codes/Step5_SEIR_MC_simplot_fun.R")
out_plot1 <- SEIR_smplot(result3 = vac.test.base1[[1]], #Rt=1, npi1=49, npi2 =1, vaccinating=0.5, testing=0.8
                         result4 = vac.test.base1[[2]],  #Rt=1, npi1=49, npi2 =1, vaccinating=0.5, testing=1
                         result5 = vac.test.base1[[3]],  #Rt=1, npi1=49, npi2 =1, vaccinating=0.5, testing=1.2
                         result6 = vac.test.base1[[4]],  #Rt=1, npi1=49, npi2 =1, vaccinating=1, testing=0.8
                         result7 = vac.test.base1[[5]],  #Rt=1, npi1=49, npi2 =1, vaccinating=1, testing=1
                         result8 = vac.test.base1[[6]],  #Rt=1, npi1=49, npi2 =1, vaccinating=1, testing=1.2
                         result9 = vac.test.base1[[7]],  #Rt=1, npi1=49, npi2 =1, vaccinating=1.5, testing=0.8
                         result10 = vac.test.base1[[8]], #Rt=1, npi1=49, npi2 =1, vaccinating=1.5, testing=1
                         result11 = vac.test.base1[[9]]  #Rt=1, npi1=49, npi2 =1, vaccinating=1.5, testing=1.2
)
out_plot1 ##Rt=1, npi1=49, npi2 =1, test=0.8/1/1.2;; vac=0.5, 1, 1.5

ggsave(out_plot1, filename = "plot1_Rt1_npi1_87_npi2_1_test_vac.png", width = 8, height = 3)
graphics.off()

out_plot2 <- SEIR_smplot(result3 = vac.test.base2[[1]],  #Rt=1, npi1=64, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result4 = vac.test.base2[[2]],   #Rt=1, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1
                         result5 = vac.test.base2[[3]],   #Rt=1, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1.2
                         result6 = vac.test.base2[[4]],   #Rt=1, npi1=64, npi2 =0.95, vaccinating=1, testing=0.8
                         result7 = vac.test.base2[[5]],   #Rt=1, npi1=64, npi2 =0.95, vaccinating=1, testing=1
                         result8 = vac.test.base2[[6]],   #Rt=1, npi1=64, npi2 =0.95, vaccinating=1, testing=1.2
                         result9 = vac.test.base2[[7]],   #Rt=1, npi1=64, npi2 =0.95, vaccinating=1.5, testing=0.8
                         result10 = vac.test.base2[[8]],  #Rt=1, npi1=64, npi2 =0.95, vaccinating=1.5, testing=1
                         result11 = vac.test.base2[[9]]   #Rt=1, npi1=64, npi2 =0.95, vaccinating=1.5, testing=1.2
) 
out_plot2 ##Rt=1, npi1=64, npi2 =0.95, test=0.8/1/1.2;; vac=0.5, 1, 1.5

ggsave(out_plot2, filename = "plot2_Rt1_npi1_117_npi2_0.9_test_vac.png", width = 8, height = 3)
graphics.off()

out_plot3 <- SEIR_smplot(result3 = vac.test.base3[[1]], #Rt=1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result4 = vac.test.base3[[2]], #Rt=1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1
                         result5 = vac.test.base3[[3]], #Rt=1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1.2
                         result6 = vac.test.base3[[4]], #Rt=1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result7 = vac.test.base3[[5]], #Rt=1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1
                         result8 = vac.test.base3[[6]], #Rt=1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1.2
                         result9 = vac.test.base3[[7]], #Rt=1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result10 = vac.test.base3[[8]], #Rt=1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1
                         result11 = vac.test.base3[[9]] #Rt=1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1.2
) 
out_plot3 ##Rt=1, npi1=78, npi2 =0.95, test=0.8/1/1.2;; vac=0.5, 1, 1.5

ggsave(out_plot3, filename = "plot3_Rt1_npi1_147_npi2_0.9_test_vac.png", width = 8, height = 3)
graphics.off()

out_plot4 <- SEIR_smplot(result3 = vac.test.base4[[1]], #Rt=1.05, npi1=49, npi2 =1, vaccinating=0.5, testing=0.8
                         result4 = vac.test.base4[[2]], #Rt=1.05, npi1=49, npi2 =1, vaccinating=0.5, testing=1
                         result5 = vac.test.base4[[3]], #Rt=1.05, npi1=49, npi2 =1, vaccinating=0.5, testing=1.2
                         result6 = vac.test.base4[[4]], #Rt=1.05, npi1=49, npi2 =1, vaccinating=0.5, testing=0.8
                         result7 = vac.test.base4[[5]], #Rt=1.05, npi1=49, npi2 =1, vaccinating=0.5, testing=1
                         result8 = vac.test.base4[[6]], #Rt=1.05, npi1=49, npi2 =1, vaccinating=0.5, testing=1.2
                         result9 = vac.test.base4[[7]], #Rt=1.05, npi1=49, npi2 =1, vaccinating=0.5, testing=0.8
                         result10 = vac.test.base4[[8]],#Rt=1.05, npi1=49, npi2 =1, vaccinating=0.5, testing=1
                         result11 = vac.test.base4[[9]] #Rt=1.05, npi1=49, npi2 =1, vaccinating=0.5, testing=1.2
) 
out_plot4 ##Rt=1.05, npi1=49, npi2 =1, test=0.8/1/1.2;; vac=0.5, 1, 1.5

ggsave(out_plot4, filename = "plot4_Rt1.05_npi1_87_npi2_1_test_vac.png", width = 8, height = 3)
graphics.off()

out_plot5 <- SEIR_smplot(result3 = vac.test.base5[[1]], #Rt=1.05, npi1=64, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result4 = vac.test.base5[[2]], #Rt=1.05, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1
                         result5 = vac.test.base5[[3]], #Rt=1.05, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1.2
                         result6 = vac.test.base5[[4]], #Rt=1.05, npi1=64, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result7 = vac.test.base5[[5]], #Rt=1.05, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1
                         result8 = vac.test.base5[[6]], #Rt=1.05, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1.2
                         result9 = vac.test.base5[[7]], #Rt=1.05, npi1=64, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result10 = vac.test.base5[[8]], #Rt=1.05, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1
                         result11 = vac.test.base5[[9]] #Rt=1.05, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1.2
) 
out_plot5 ##Rt=1.05, npi1=64, npi2 =0.95, test=0.8/1/1.2;; vac=0.5, 1, 1.5

ggsave(out_plot5, filename = "plot5_Rt1.05_npi1_117_npi2_0.9_test_vac.png", width = 8, height = 3)
graphics.off()

out_plot6 <- SEIR_smplot(result3 = vac.test.base6[[1]], #Rt=1.05, npi1=78, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result4 = vac.test.base6[[2]], #Rt=1.05, npi1=78, npi2 =0.95,  vaccinating=0.5, testing=1
                         result5 = vac.test.base6[[3]], #Rt=1.05, npi1=78, npi2 =0.95,  vaccinating=0.5, testing=1.2
                         result6 = vac.test.base6[[4]], #Rt=1.05, npi1=78, npi2 =0.95,  vaccinating=0.5, testing=0.8
                         result7 = vac.test.base6[[5]], #Rt=1.05, npi1=78, npi2 =0.95,  vaccinating=0.5, testing=1
                         result8 = vac.test.base6[[6]], #Rt=1.05, npi1=78, npi2 =0.95,  vaccinating=0.5, testing=1.2
                         result9 = vac.test.base6[[7]], #Rt=1.05, npi1=78, npi2 =0.95,  vaccinating=0.5, testing=0.8
                         result10 = vac.test.base6[[8]],#Rt=1.05, npi1=78, npi2 =0.95,  vaccinating=0.5, testing=1
                         result11 = vac.test.base6[[9]] #Rt=1.05, npi1=78, npi2 =0.95,  vaccinating=0.5, testing=1.2
) 
out_plot6 ##Rt=1.05, npi1=78, npi2 =0.95, test=0.8/1/1.2;; vac=0.5, 1, 1.5

ggsave(out_plot6, filename = "plot6_Rt1.05_npi1_147_npi2_0.9_test_vac.png", width = 8, height = 3)
graphics.off()

out_plot7 <- SEIR_smplot(result3 = vac.test.base7[[1]], #Rt=1.1, npi1=49, npi2 =1, vaccinating=0.5, testing=0.8
                         result4 = vac.test.base7[[2]], #Rt=1.1, npi1=49, npi2 =1,  vaccinating=0.5, testing=1
                         result5 = vac.test.base7[[3]], #Rt=1.1, npi1=49, npi2 =1,  vaccinating=0.5, testing=1.2
                         result6 = vac.test.base7[[4]], #Rt=1.1, npi1=49, npi2 =1,  vaccinating=0.5, testing=0.8
                         result7 = vac.test.base7[[5]], #Rt=1.1, npi1=49, npi2 =1,  vaccinating=0.5, testing=1
                         result8 = vac.test.base7[[6]], #Rt=1.1, npi1=49, npi2 =1,  vaccinating=0.5, testing=1.2
                         result9 = vac.test.base7[[7]], #Rt=1.1, npi1=49, npi2 =1,  vaccinating=0.5, testing=0.8
                         result10 = vac.test.base7[[8]],#Rt=1.1, npi1=49, npi2 =1,  vaccinating=0.5, testing=1
                         result11 = vac.test.base7[[9]] #Rt=1.1, npi1=49, npi2 =1,  vaccinating=0.5, testing=1.2
) 
out_plot7  ##Rt=1.1, npi1=49, npi2 =1, test=0.8/1/1.2;; vac=0.5, 1, 1.5

ggsave(out_plot7, filename = "plot7_Rt1.1_npi1_87_npi2_1_test_vac.png", width = 8, height = 3)
graphics.off()

out_plot8 <- SEIR_smplot(result3 = vac.test.base8[[1]], #Rt=1.1, npi1=64, npi2 =0.95 vaccinating=0.5, testing=0.8
                         result4 = vac.test.base8[[2]], #Rt=1.1, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1
                         result5 = vac.test.base8[[3]], #Rt=1.1, npi1=64, npi2 =0.95 vaccinating=0.5, testing=1.2
                         result6 = vac.test.base8[[4]], #Rt=1.1, npi1=64, npi2 =0.95 vaccinating=0.5, testing=0.8
                         result7 = vac.test.base8[[5]], #Rt=1.1, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1
                         result8 = vac.test.base8[[6]], #Rt=1.1, npi1=64, npi2 =0.95 vaccinating=0.5, testing=1.2
                         result9 = vac.test.base8[[7]], #Rt=1.1, npi1=64, npi2 =0.95 vaccinating=0.5, testing=0.8
                         result10 = vac.test.base8[[8]],#Rt=1.1, npi1=64, npi2 =0.95, vaccinating=0.5, testing=1
                         result11 = vac.test.base8[[9]] #Rt=1.1, npi1=64, npi2 =0.95 vaccinating=0.5, testing=1.2
) 
out_plot8 ##Rt=1.1, npi1=64, npi2 =0.95, test=0.8/1/1.2;; vac=0.5, 1, 1.5

ggsave(out_plot8, filename = "plot8_Rt1.1_npi1_117_npi2_0.9_test_vac.png", width = 8, height = 3)
graphics.off()

out_plot9 <- SEIR_smplot(result3 = vac.test.base9[[1]], #Rt=1.1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result4 = vac.test.base9[[2]], #Rt=1.1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1
                         result5 = vac.test.base9[[3]], #Rt=1.1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1.2
                         result6 = vac.test.base9[[4]], #Rt=1.1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result7 = vac.test.base9[[5]], #Rt=1.1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1
                         result8 = vac.test.base9[[6]], #Rt=1.1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1.2
                         result9 = vac.test.base9[[7]], #Rt=1.1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=0.8
                         result10 = vac.test.base9[[8]],#Rt=1.1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1
                         result11 = vac.test.base9[[9]] #Rt=1.1, npi1=78, npi2 =0.95, vaccinating=0.5, testing=1.2
) 
out_plot9 ##Rt=1.1, npi1=78, npi2 =0.95, test=0.8/1/1.2;; vac=0.5, 1, 1.5

ggsave(out_plot9, filename = "plot9_Rt1.1_npi1_147_npi2_0.9_test_vac.png", width = 8, height = 3)
graphics.off()






