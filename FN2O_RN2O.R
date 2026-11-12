library(blockCV)
library(correlation)
library(dagitty)
library(FSA)
library(ggsignif)
library(ggpubr)
library(ggparty)
library(grid)
library(ggdag)
library(partykit)
library(readxl)
library(rstatix)
library(sf)
library(tidyverse)
library(terra)
library(xgboost)


#####################################################################################
#####################################################################################
##############################################################################FN2O

#####Gerneral information in database
k2o<-read_excel("C:/Users/wang/Desktop/Denitrification/Data5/K2/K2_mea.xlsx", sheet = 2)

k2<-k2o[, c(5:21)]

k2$Sand<-as.numeric(k2$Sand)
k2$Silt<-as.numeric(k2$Silt)
k2$Clay<-as.numeric(k2$Clay)
k2$BD<-as.numeric(k2$BD)
k2$pH<-as.numeric(k2$pH)
k2$CN_ratio<-as.numeric(k2$CN_ratio)
k2$real_AI<-as.numeric(k2$real_AI)
k2$Soil_T<-as.numeric(k2$Soil_T)
k2$WFPS<-as.numeric(k2$WFPS)
k2$FN2O<-as.numeric(k2$FN2O)
k2$logFN2O<-log10(k2$FN2O)


summary(k2)
unique(k2$Continent)
unique(k2$Land_cover)
unique(k2$Soil_texture)
unique(k2$K_climate)


k2$var<-rep("FN2O", 260 )

ggplot(k2, aes(x=var,y=log10(FN2O)))+
  geom_violin(lwd=0.3)+
  labs(x="", y=expression(Log[10]*(Value)))+
  scale_x_discrete(labels=c(expression(F[N2O])))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.line.x.bottom =  element_line(linewidth = 0.6, color = "white"),
        axis.line.y.left =  element_line(linewidth = 0.6, color = "white"),
        panel.border = element_rect(color = "white", linewidth = 0.6),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "none",
        axis.title.y = element_text(margin = margin(r = -1)),
        axis.ticks = element_line(linewidth = 0.6, color = "white"),
        axis.text.x = element_text(size = 11, color = "white"),
        axis.text.y = element_text(size = 10, color = "white"),
        axis.title = element_text(size = 12, color = "white"))




#differences between continents
continent<-k2[, c(3,18)] %>% na.omit()

continent<-continent %>% group_by(Continent) %>% mutate(mean=mean(FN2O))
continent<-continent %>% group_by(Continent) %>% mutate(median=median(FN2O))


stat_test_con <- continent %>% t_test(logFN2O ~ Continent) %>%
  adjust_pvalue(method = "BH") %>% add_significance("p.adj") %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "Continent") %>% mutate(y.position = log10(y.position))


K2_con<-ggplot() +
  geom_boxplot(aes(x = continent$Continent, y = continent$logFN2O, 
                   fill = continent$Continent), outlier.size = 0.2, size = 0.5) +
  stat_pvalue_manual(stat_test_con, label = "p.adj.signif", tip.length = 0.01,
                     size = 7, vjust = 1, step.increase = 0.05) +
  labs(x="Continents", y=expression(Log[10](F[N2O])), fill="")+
  scale_fill_brewer(palette = "Set3")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.3),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "none",
        axis.text = element_text(size = 10),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12))




#differences between land cover
Landcover<-k2[, c(5,18)] %>% na.omit()

Landcover <- Landcover %>% group_by(Land_cover) %>% mutate(mean=mean(FN2O))
Landcover <- Landcover %>% group_by(Land_cover) %>% mutate(median=median(FN2O))


stat_test_lc <- Landcover %>% t_test(logFN2O ~ Land_cover) %>%
  adjust_pvalue(method = "BH") %>% add_significance("p.adj") %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "Land_cover") %>% mutate(y.position = log10(y.position))


K2_lc<-ggplot() +
  geom_boxplot(aes(x = Landcover$Land_cover, y = Landcover$logFN2O, 
                   fill = Landcover$Land_cover), outlier.size = 0.2, size = 0.5) +
  stat_pvalue_manual(stat_test_lc, label = "p.adj.signif", tip.length = 0.01,
                     size = 7, vjust = 1, step.increase = 0.05) +
  labs(x="Land cover", y="", fill="")+
  scale_fill_brewer(palette = "Set3")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.3),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "none",
        axis.text = element_text(size = 10),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12))




#differences between soil texture
soiltexture<-k2[, c(6,18)] %>% na.omit()
soilttest<-soiltexture[-193,]

soiltexture <- soiltexture %>% group_by(Soil_texture) %>% mutate(mean=mean(fraction))
soiltexture <- soiltexture %>% group_by(Soil_texture) %>% mutate(median=median(fraction))


stat_test_st <- soilttest %>% t_test(logFN2O ~ Soil_texture) %>%
  adjust_pvalue(method = "BH") %>% add_significance("p.adj") %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "Soil_texture") %>% mutate(y.position = log10(y.position))


ggplot() +
  geom_boxplot(aes(x = soiltexture$Soil_texture, y = soiltexture$logFN2O, 
                   fill = soiltexture$Soil_texture), outlier.size = 0.2, size = 0.5) +
  stat_pvalue_manual(stat_test_st, label = "p.adj.signif", tip.length = 0.01,
                     size = 7, vjust = 1, step.increase = 0.05) +
  labs(x="Soil texture", y=expression(Log[10](F[N2O])), fill="")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_brewer(palette = "Set3")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.3),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "none",
        axis.text = element_text(size = 10),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12))




#differences between climates
climate<-k2[, c(14,18)] %>% na.omit()

climate <- climate %>% group_by(K_climate) %>% mutate(mean=mean(FN2O))
climate <- climate %>% group_by(K_climate) %>% mutate(median=median(FN2O))


stat_test_cli <- climate %>% t_test(logFN2O ~ K_climate) %>%
  adjust_pvalue(method = "BH") %>% add_significance("p.adj") %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "K_climate") %>% mutate(y.position = log10(y.position))


K2_cli<-ggplot() +
  geom_boxplot(aes(x = climate$K_climate, y = climate$logFN2O, fill = climate$K_climate),
               outlier.size = 0.2, size = 0.5) +
  stat_pvalue_manual(stat_test_cli, label = "p.adj.signif", tip.length = 0.01,
                     size = 7, vjust = 1, step.increase = 0.05) +
  labs(x="Climate types", y="", fill="")+
  scale_fill_brewer(palette = "Set3")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.3),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "none",
        axis.text = element_text(size = 10),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12))




#differences between measurement design
design<-k2[, c(1, 18)] %>% na.omit()
design<-design[-8,]

design <- design %>% group_by(Design) %>% mutate(mean=mean(FN2O))
design <- design %>% group_by(Design) %>% mutate(median=median(FN2O))


stat_test_des <- design %>% t_test(logFN2O ~ Design) %>%
  adjust_pvalue(method = "BH") %>% add_significance("p.adj") %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "Design")


K2_des<-ggplot() +
  geom_boxplot(aes(x = design$Design, y = design$logFN2O, fill = design$Design),
               outlier.size = 0.2, size = 0.5) +
  stat_pvalue_manual(stat_test_des, label = "p.adj.signif", tip.length = 0.01,
                     size = 7, vjust = 1, step.increase = 0.05) +
  labs(x=NULL, y=expression(Log[10](F[N2O])))+
  scale_fill_brewer(palette = "Set3")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.3),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "none",
        axis.text = element_text(size = 10),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12))




#differences between measurement period
period<-k2[, c(2, 18)] %>% na.omit()

period <- period %>% group_by(Period) %>% mutate(mean=mean(FN2O))
period <- period %>% group_by(Period) %>% mutate(median=median(FN2O))


stat_test_per <- period %>% t_test(logFN2O ~ Period) %>%
  adjust_pvalue(method = "BH") %>% add_significance("p.adj") %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "Period")


K2_per<-ggplot() +
  geom_boxplot(aes(x = period$Period, y = period$logFN2O, fill = period$Period),
               outlier.size = 0.2, size = 0.5) +
  stat_pvalue_manual(stat_test_per, label = "p.adj.signif", tip.length = 0.01,
                     size = 7, vjust = 1, step.increase = 0.05) +
  labs(x=NULL, y=expression(Log[10](F[N2O])))+
  scale_fill_brewer(palette = "Set3")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.3),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "none",
        axis.text = element_text(size = 10),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12))





##############################################description
k2o<-read_excel("C:/Users/wang/Desktop/Denitrification/Data4/K2/K2_v2.xlsx", sheet = 3)

k2<-k2o[, c(4:19)]
k2$FN2O<-as.numeric(k2$FN2O)


#continent
continent<-k2[, c(1,16)] %>% na.omit()

continent<-continent %>% group_by(Continent) %>% mutate(mean=mean(FN2O))
continent<-continent %>% group_by(Continent) %>% mutate(median=median(FN2O))

ca<-unique(continent[,c(1,3,4)])

continent %>% group_by(Continent) %>% summarise( Q1 = quantile(FN2O, 0.25, na.rm = TRUE),
                                                 Median = median(FN2O, na.rm = TRUE), 
                                                 Q3 = quantile(FN2O, 0.75, na.rm = TRUE))



#land cover
Landcover<-k2[, c(3,16)] %>% na.omit()

Landcover <- Landcover %>% group_by(Land_cover) %>% mutate(mean=mean(fraction))
Landcover <- Landcover %>% group_by(Land_cover) %>% mutate(median=median(fraction))

la<-unique(Landc[, c(1, 3, 4)])

Landcover %>% group_by(Land_cover) %>% 
  summarise( Q1 = quantile(FN2O, 0.25, na.rm = TRUE), 
             Median = median(FN2O, na.rm = TRUE), Q3 = quantile(FN2O, 0.75, na.rm = TRUE))



#climates
climate<-k2[, c(13,16)] %>% na.omit()

climate <- climate %>% group_by(K_climate) %>% mutate(mean=mean(fraction))
climate <- climate %>% group_by(K_climate) %>% mutate(median=median(fraction))

ka<-unique(koppen[, c(1,3,4,5)])

climate %>% group_by(K_climate) %>% 
  summarise( Q1 = quantile(FN2O, 0.25, na.rm = TRUE),
             Median = median(FN2O, na.rm = TRUE), Q3 = quantile(FN2O, 0.75, na.rm = TRUE))



#soil texture
soiltexture<-k2[, c(4,16)] %>% na.omit()

soiltexture <- soiltexture %>% group_by(Soil_texture) %>% mutate(mean=mean(fraction))
soiltexture <- soiltexture %>% group_by(Soil_texture) %>% mutate(median=median(fraction))

st<-unique(Soiltex[, c(1,3,4,5)])

soiltexture %>% group_by(Soil_texture) %>% 
  summarise( Q1 = quantile(FN2O, 0.25, na.rm = TRUE),
             Median = median(FN2O, na.rm = TRUE), Q3 = quantile(FN2O, 0.75, na.rm = TRUE))






##################################relationships between qualitative variables
k2o<-read_excel("C:/Users/wang/Desktop/Denitrification/Data5/K2/K2_mea.xlsx", sheet = 2)

k2<-k2o[, c(5:7, 9:10, 18, 21)]

k2$FN2O<-as.numeric(k2$FN2O)
k2$logk2<-log10(k2$FN2O)


k2$Continent<-factor(k2$Continent, levels = c("Europe","North America", "Asia","Australia"))

k2$Land_cover<-factor(k2$Land_cover, levels = c("Grassland", "Cropland","Forest"))

k2$Soil_texture<-factor(k2$Soil_texture, levels = c( "Sand","Sandy loam","Silt loam",  "Silt","Clay loam",         
                                                     "Loamy sand","Silty clay loam", "Loam", "Sandy clay loam",
                                                     "Clay","Silty clay"  ))

k2$K_climate<-factor(k2$K_climate, levels = c("Temperate","Cold","Arid"))

k2$Design<- factor(k2$Design, levels = c("Laboratory", "Field"))

k2$Period<- factor(k2$Period, levels = c("Long-term", "Short-term"))



K2_ctree <- partykit::ctree(k2$logk2 ~ Continent+Land_cover+Soil_texture+K_climate
                            +Design + Period, data = k2,
                            control = partykit::ctree_control(alpha = 0.001))


plot(K2_ctree, gp = gpar(fontsize = 6.5))



png('C:/Users/wang/Desktop/Denitrification/Data4/K2/K2_tree.png', 
    width=4340, height=2968, res=900)


plot(K2_ctree, gp = gpar(fontsize = 7.5))


grid.force()

txt <- grid.ls(print = FALSE)$name
txt <- txt[grep("text", txt)]

for (t in txt) {
  lab <- grid.get(t)$label
  if (is.character(lab)) {
    lab_new <- paste(strwrap(lab, width = 18), collapse = "\n")
    
    grid.gedit(t, label = lab_new)
  }
}

dev.off()







########################################confounder
k2o<-read_excel("C:/Users/wang/Desktop/Denitrification/Data5/K2/K2_mea.xlsx", sheet = 2)

k2<-k2o[, c(5:21)]

k2$Sand<-as.numeric(k2$Sand)
k2$Silt<-as.numeric(k2$Silt)
k2$Clay<-as.numeric(k2$Clay)
k2$BD<-as.numeric(k2$BD)
k2$pH<-as.numeric(k2$pH)
k2$CN_ratio<-as.numeric(k2$CN_ratio)
k2$real_AI<-as.numeric(k2$real_AI)
k2$Soil_T<-as.numeric(k2$Soil_T)
k2$WFPS<-as.numeric(k2$WFPS)
k2$FN2O<-as.numeric(k2$FN2O)


dag <- dagitty("
dag {
    
    FN2O <- Continent
    FN2O <- Land_cover
    FN2O <- Soil_texture
    FN2O <- Design
    FN2O <- Period
    FN2O <- Sand
    FN2O <- Silt
    FN2O <- Clay
    FN2O <- BD
    FN2O <- pH
    FN2O <- CN_ratio
    FN2O <- K_climate
    FN2O <- real_AI
    FN2O <- Soil_T
    FN2O <- WFPS
    
    Land_cover<- Continent
    Land_cover<-Soil_texture
    Land_cover<-Sand
    Land_cover<-Silt
    Land_cover<-Clay
    Land_cover<-BD
    Land_cover<-pH
    Land_cover<-CN_ratio
    Land_cover<-real_AI
    Land_cover<-K_climate
    Land_cover<-Soil_T
    Land_cover<-WFPS
    
    Soil_texture<-Land_cover
    Soil_texture<-Continent
    Soil_texture<-K_climate
    
    BD<-Continent
    BD<-Land_cover
    BD<-Soil_texture
    BD<-K_climate
    BD<-real_AI
    BD<-WFPS
    
    pH<-Continent
    pH<-Land_cover
    pH<-Soil_texture
    pH<- Design
    pH<- Period
    pH<-CN_ratio
    pH<-K_climate
    pH<-real_AI
    
    CN_ratio<-Continent
    CN_ratio<-Land_cover
    CN_ratio<-Soil_texture
    CN_ratio<- Design
    CN_ratio<- Period
    CN_ratio<-K_climate
    CN_ratio<-real_AI
    
    K_climate<-Continent
    real_AI<-Continent
    
    Soil_T<-real_AI
    Soil_T<-K_climate
    Soil_T<-Land_cover
    Soil_T<-Soil_texture
    Soil_T<- Design
    Soil_T <- Period
    Soil_T<-Continent
    
    WFPS<-real_AI
    WFPS<-K_climate
    WFPS<-Land_cover
    WFPS<-Soil_texture
    WFPS<- Design
    WFPS <- Period
    WFPS<-Continent

}
")


##################################plot DAG
ggdag(dag, layout="circle", 
      text_size = 5, text= T, text_col = "black",node = F)+
  geom_dag_label(aes(color = name == "Continent"), size = 5) +
  scale_color_manual(values = c("TRUE" = "grey54", "FALSE"="grey54"), guide = "none") +
  theme_dag()+
  theme(plot.margin = unit(c(0,0,0,0), "pt"))



#################################identify confounders that need to be adjusted
adjustmentSets(dag, exposure = "BD", outcome = "FN2O")

##highlight confounders
ggdag_adjustment_set(dag, exposure = "Continent", outcome = "FN2O",
                     node_size = 8, text_size = 2,
                     text_col = "black", shadow = F) +
  theme_dag()+
  theme(legend.position = "none")







#############################################normality test
k2o<-read_excel("C:/Users/wang/Desktop/Denitrification/Data4/K2/K2_v2.xlsx", sheet = 3)

k2<-k2o[, c(4:19)]

k2$Sand<-as.numeric(k2$Sand)
k2$Silt<-as.numeric(k2$Silt)
k2$Clay<-as.numeric(k2$Clay)
k2$BD<-as.numeric(k2$BD)
k2$pH<-as.numeric(k2$pH)
k2$CN_ratio<-as.numeric(k2$CN_ratio)
k2$real_AI<-as.numeric(k2$real_AI)
k2$Soil_T<-as.numeric(k2$Soil_T)
k2$WFPS<-as.numeric(k2$WFPS)
k2$FN2O<-as.numeric(k2$FN2O)


shapiro.test(k2$Sand)
shapiro.test(k2$Silt)
shapiro.test(k2$Clay)
shapiro.test(k2$BD)
shapiro.test(k2$pH)
shapiro.test(k2$CN_ratio)
shapiro.test(k2$real_AI)
shapiro.test(k2$Soil_T)
shapiro.test(k2$WFPS)
shapiro.test(k2$FN2O)
shapiro.test(log10(k2$FN2O))


ggqqplot(k2$Sand)
ggqqplot(k2$Silt)
ggqqplot(k2$Clay)
ggqqplot(k2$BD)
ggqqplot(k2$pH)
ggqqplot(k2$CN_ratio)
ggqqplot(k2$real_AI)
ggqqplot(k2$Soil_T)
ggqqplot(k2$WFPS)
ggqqplot(k2$FN2O)
ggqqplot(log10(k2$FN2O))






################################################correlation analysis
k2o<-read_excel("C:/Users/wang/Desktop/Denitrification/Data4/K2/K2_v2.xlsx", sheet = 3)

k2<-k2o[, c(4:19)]

k2$Sand<-as.numeric(k2$Sand)
k2$Silt<-as.numeric(k2$Silt)
k2$Clay<-as.numeric(k2$Clay)
k2$BD<-as.numeric(k2$BD)
k2$pH<-as.numeric(k2$pH)
k2$CN_ratio<-as.numeric(k2$CN_ratio)
k2$real_AI<-as.numeric(k2$real_AI)
k2$Soil_T<-as.numeric(k2$Soil_T)
k2$WFPS<-as.numeric(k2$WFPS)
k2$FN2O<-as.numeric(k2$FN2O)


correlation::correlation(k2, include_factors = F, method = "spearman")


#qualitative
kruskal.test(FN2O ~ Continent, data = k2)
kruskal.test(FN2O ~ K_climate, data = k2)
kruskal.test(FN2O ~ Soil_texture, data = k2)
kruskal.test(FN2O ~ Land_cover, data = k2) 

dunnTest(FN2O~ Continent, data = k2, method = "bonferroni")
dunnTest(FN2O~ K_climate, data = k2, method = "bonferroni")
dunnTest(FN2O~ Soil_texture, data = k2, method = "bonferroni")
dunnTest(FN2O~ Land_cover, data = k2, method = "bonferroni") 





################################################non-linear analysis
k2o<-read_excel("C:/Users/wang/Desktop/Denitrification/Data4/K2/K2_v2.xlsx", sheet = 3)

k2<-k2o[, c(4:19)]

k2$Sand<-as.numeric(k2$Sand)
k2$Silt<-as.numeric(k2$Silt)
k2$Clay<-as.numeric(k2$Clay)
k2$BD<-as.numeric(k2$BD)
k2$pH<-as.numeric(k2$pH)
k2$CN_ratio<-as.numeric(k2$CN_ratio)
k2$real_AI<-as.numeric(k2$real_AI)
k2$Soil_T<-as.numeric(k2$Soil_T)
k2$WFPS<-as.numeric(k2$WFPS)
k2$FN2O<-as.numeric(k2$FN2O)



####################################################Sand
KSand<-k2[,c(5, 16)] %>% na.omit()

#quadratic model
Sand_Nlm<-lm(FN2O ~ Sand + I(Sand^2), KSand)

Sand_Nfit<-nls(FN2O ~ a*Sand^2 + b*Sand +c, KSand,
               start = list(a=-4.193e-06, b=5.862e-04, c=-4.493e-03))


#log quadratic model
Sand_Nlm<-lm(log10(FN2O) ~ Sand + I(Sand^2), KSand) 

Sand_Nfit<-nls(log10(FN2O) ~ a*Sand^2 + b*Sand +c, KSand,
               start = list(a=-7.889e-05, b=1.245e-02 , c=-2.961e+00))


#exponential model
Sand_Nlm<-lm(log(FN2O) ~ Sand , KSand)

Sand_Nfit<-nls(FN2O ~ a*exp(b*Sand), KSand,
               start = list(a=0.001409134, b=0.01251)) 


ggplot(KSand, aes(Sand, log10(FN2O))) +
  geom_point() +
  geom_line(aes(y=predict(Sand_Nfit)))+
  theme_bw()


ggplot(KSand, aes(Sand, FN2O)) +
  geom_point() +
  geom_line(aes(y=predict(Sand_Nfit)))+
  theme_bw()


rss <- sum((log10(KSand$FN2O) - predict(Sand_Nfit))^2)     
tss <- sum((log10(KSand$FN2O) - mean(log10(KSand$FN2O)))^2)  
r_squared <- 1 - rss/tss 


rss <- sum((KSand$FN2O - predict(Sand_Nfit))^2)     
tss <- sum((KSand$FN2O - mean(KSand$FN2O))^2)  
r_squared <- 1 - rss/tss 




#####################################################Silt
KSilt<-k2[,c(6, 16)] %>% na.omit()

#quadratic model
Silt_Nlm<-lm(FN2O ~ Silt + I(Silt^2), KSilt) 

Silt_Nfit<-nls(FN2O ~ a*Silt^2 + b*Silt +c, KSilt,
               start = list(a=-2.917e-07, b=-2.353e-04, c=2.046e-02))


#log quadratic model
Silt_Nlm<-lm(log10(FN2O) ~ Silt + I(Silt^2), KSilt) 

Silt_Nfit<-nls(log10(FN2O) ~ a*Silt^2 + b*Silt +c, KSilt,
               start = list(a=5.649e-06, b=-6.269e-03, c=-2.381e+00))


#exponential model
Silt_Nlm<-lm(log(FN2O) ~ Silt , KSilt)

Silt_Nfit<-nls(FN2O ~ a*exp(b*Silt), KSilt,
               start = list(a=0.004066714, b=-0.01328))


ggplot(KSilt, aes(Silt, log10(FN2O))) +
  geom_point() +
  geom_line(aes(y=predict(Silt_Nfit)))+
  theme_bw()

ggplot(KSilt, aes(Silt, FN2O)) +
  geom_point() +
  geom_line(aes(y=predict(Silt_Nfit)))+
  theme_bw()


rss <- sum((KSilt$FN2O - predict(Silt_Nfit))^2)     
tss <- sum((KSilt$FN2O - mean(KSilt$FN2O))^2)  
r_squared <- 1 - rss/tss 



##########################################################Clay
KClay<-k2[,c(7, 16)] %>% na.omit()

#quadratic model
Clay_Nlm<-lm(FN2O ~ Clay + I(Clay^2), KClay) 

Clay_Nfit<-nls(FN2O ~ a*Clay^2 + b*Clay +c, KClay,
               start = list(a=8.818e-07, b=-3.207e-04, c=1.469e-02))


#log quadratic model
Clay_Nlm<-lm(log10(FN2O) ~ Clay + I(Clay^2), KClay) 

Clay_Nfit<-nls(log10(FN2O) ~ a*Clay^2 + b*Clay +c, KClay,
               start = list(a=0.0003372, b=-0.0273154, c=-2.3123265))


#exponential model
Clay_Nlm<-lm(log(FN2O) ~ Clay , KClay)

Clay_Nfit<-nls(FN2O ~ a*exp(b*Clay), KClay,
               start = list(a=0.00340667, b=-0.02436))


ggplot(KClay, aes(Clay, log10(FN2O))) +
  geom_point() +
  geom_line(aes(y=predict(Clay_Nfit)))+
  theme_bw()


ggplot(KClay, aes(Clay, FN2O)) +
  geom_point() +
  geom_line(aes(y=predict(Clay_Nfit)))+
  theme_bw()


rss <- sum((KClay$FN2O - predict(Clay_Nfit))^2)     
tss <- sum((KClay$FN2O - mean(KClay$FN2O))^2)  
r_squared <- 1 - rss/tss 



##############################################################BD
KBD<-k2[,c(8, 16)] %>% na.omit()
KBD<-KBD[KBD$BD>0.7,]

#quadratic model
BD_Nlm<-lm(FN2O ~ BD + I(BD^2), KBD) 

BD_Nfit<-nls(FN2O ~ a*BD^2 + b*BD +c, KBD,
             start = list(a=0.1107, b=-0.1631, c=0.0619)) 


#log quadratic model
BD_Nlm<-lm(log10(FN2O) ~ BD + I(BD^2), KBD) 

BD_Nfit<-nls(log10(FN2O) ~ a*BD^2 + b*BD +c, KBD,
             start = list(a=18.46, b=-40.5, c=19.22)) 


#exponential model
BD_Nlm<-lm(log(FN2O) ~ BD , KBD)

BD_Nfit<-nls(FN2O ~ a*exp(b*BD), KBD,
             start = list(a=0.003008842, b=-0.2683)) 


ggplot(KBD, aes(BD, FN2O)) +
  geom_point() +
  geom_line(aes(y=predict(BD_Nfit)))+
  theme_bw()


ggplot(KBD, aes(BD, log10(FN2O))) +
  geom_point() +
  geom_line(aes(y=predict(BD_Nfit)))+
  theme_bw()


rss <- sum((KBD$FN2O - predict(BD_Nfit))^2)     
tss <- sum((KBD$FN2O - mean(KBD$FN2O))^2)  
r_squared <- 1 - rss/tss 


rss <- sum((log10(KBD$FN2O) - predict(BD_Nfit))^2)     
tss <- sum((log10(KBD$FN2O) - mean(log10(KBD$FN2O)))^2)  
r_squared <- 1 - rss/tss 


rss <- sum((log10(KBD$FN2O) - predict(BD_Nfit))^2)
nrss<-rss/71
rmse<-sqrt(nrss) 



################################################################pH
KpH<-k2[,c(9, 16)] %>% na.omit()

#quadratic model
pH_Nlm<-lm(FN2O ~ pH + I(pH^2), KpH) 

pH_Nfit<-nls(FN2O ~ a*pH^2 + b*pH +c, KpH,
             start = list(a=0.002442, b=-0.034217, c=0.125995)) 


#log quadratic model
pH_Nlm<-lm(log10(FN2O) ~ pH + I(pH^2), KpH) 

pH_Nfit<-nls(log10(FN2O) ~ a*pH^2 + b*pH +c, KpH,
             start = list(a=0.006785, b=-0.147052, c=-2.097559))


#exponential model
pH_Nlm<-lm(log(FN2O) ~ pH , KpH)

pH_Nfit<-nls(FN2O ~ a*exp(b*pH), KpH,
             start = list(a=0.004639726, b=-0.1503)) 


ggplot(KpH, aes(pH, FN2O)) +
  geom_point() +
  geom_line(aes(y=predict(pH_Nfit)))+
  theme_bw()


ggplot(KpH, aes(pH, log10(FN2O))) +
  geom_point() +
  geom_line(aes(y=predict(pH_Nfit)))+
  theme_bw()

ggplot(KpH, aes(pH, log10(FN2O))) +
  geom_point() +
  geom_line(aes(y=log10(predict(pH_Nfit))))+
  theme_bw()


rss <- sum((KpH$FN2O - predict(pH_Nfit))^2)     
tss <- sum((KpH$FN2O - mean(KpH$FN2O))^2)  
r_squared <- 1 - rss/tss 



##############################################################CN_ratio
KCNRatio<-k2[,c(10, 16)] %>% na.omit()
KCNRatio<-KCNRatio[KCNRatio$CN_ratio<40,]

#quadratic model
CNRatio_Nlm<-lm(FN2O ~ CN_ratio + I(CN_ratio^2), KCNRatio) 

CNRatio_Nfit<-nls(FN2O ~ a*CN_ratio^2 + b*CN_ratio +c, KCNRatio,
                  start = list(a=-8.557e-05, b=3.481e-03, c=-1.652e-02))


#log quadratic model
CNRatio_Nlm<-lm(log10(FN2O) ~ CN_ratio + I(CN_ratio^2), KCNRatio) 

CNRatio_Nfit<-nls(log10(FN2O) ~ a*CN_ratio^2 + b*CN_ratio +c, KCNRatio,
                  start = list(a=-0.003322, b=0.131045, c=-3.709658))


#exponential model
CNRatio_Nlm<-lm(log(FN2O) ~ CN_ratio , KCNRatio)

CNRatio_Nfit<-nls(FN2O ~ a*exp(b*CN_ratio), KCNRatio,
                  start = list(a=0.001082854, b=0.05696479)) 


ggplot(KCNRatio, aes(CN_ratio, FN2O)) +
  geom_point() +
  geom_line(aes(y=predict(CNRatio_Nfit)))+
  theme_bw()


ggplot(KCNRatio, aes(CN_ratio, log10(FN2O))) +
  geom_point() +
  geom_line(aes(y=predict(CNRatio_Nfit)))+
  theme_bw()


rss <- sum((KCNRatio$FN2O - predict(CNRatio_Nfit))^2)     
tss <- sum((KCNRatio$FN2O - mean(KCNRatio$FN2O))^2)  
r_squared <- 1 - rss/tss 



#############################################################real_AI
KAI<-k2[,c(11, 16)] %>% na.omit()
KAI<-KAI[KAI$real_AI<1.5,]

#quadratic model
AI_Nlm<-lm(FN2O ~ real_AI + I(real_AI^2), KAI) 

AI_Nfit<-nls(FN2O ~ a*real_AI^2 + b*real_AI +c, KAI,
             start = list(a=-0.06026, b=0.09112, c=-0.01834)) 


#log quadratic model
AI_Nlm<-lm(log10(FN2O) ~ real_AI + I(real_AI^2), KAI) 

AI_Nfit<-nls(log10(FN2O) ~ a*real_AI^2 + b*real_AI +c, KAI,
             start = list(a=-1.600, b=2.655, c=-3.688))


#exponential model
AI_Nlm<-lm(log(FN2O) ~ real_AI , KAI)

AI_Nfit<-nls(FN2O ~ a*exp(b*real_AI), KAI,
             start = list(a=0.001369924, b=0.523)) 


ggplot(KAI, aes(real_AI, log10(FN2O))) +
  geom_point() +
  geom_line(aes(y=predict(AI_Nfit)))+
  theme_bw()


ggplot(KAI, aes(real_AI, FN2O)) +
  geom_point() +
  geom_line(aes(y=predict(AI_Nfit)))+
  theme_bw()


rss <- sum((log10(KAI$FN2O) - predict(AI_Nfit))^2)     
tss <- sum((log10(KAI$FN2O) - mean(log10(KAI$FN2O)))^2)  
r_squared <- 1 - rss/tss 




###########################################################Soil_T
KSoilT<-k2[,c(14, 16)] %>% na.omit()

#quadratic model
SoilT_Nlm<-lm(FN2O ~ Soil_T + I(Soil_T^2), KSoilT) 

SoilT_Nfit<-nls(FN2O ~ a*Soil_T^2 + b*Soil_T +c, KSoilT,
                start = list(a=-3.730e-06, b=4.026e-04, c=-8.007e-05)) 


#log quadratic model
SoilT_Nlm<-lm(log10(FN2O) ~ Soil_T + I(Soil_T^2), KSoilT) 

SoilT_Nfit<-nls(log10(FN2O) ~ a*Soil_T^2 + b*Soil_T +c, KSoilT,
                start = list(a=0.001268, b=-0.056174, c=-2.263913))



#exponential model
SoilT_Nlm<-lm(log(FN2O) ~ Soil_T , KSoilT)

SoilT_Nfit<-nls(FN2O ~ a*exp(b*Soil_T), KSoilT,
                start = list(a=0.002189483, b=-0.01315)) 


ggplot(KSoilT, aes(Soil_T, log10(FN2O))) +
  geom_point() +
  geom_line(aes(y=predict(SoilT_Nfit)))+
  theme_bw()


ggplot(KSoilT, aes(Soil_T, FN2O)) +
  geom_point() +
  geom_line(aes(y=predict(SoilT_Nfit)))+
  theme_bw()


rss <- sum((log10(KSoilT$FN2O) - predict(SoilT_Nfit))^2)     
tss <- sum((log10(KSoilT$FN2O) - mean(log10(KSoilT$FN2O)))^2)  
r_squared <- 1 - rss/tss 



###########################################################WFPS
KWFPS<-k2[,c(15, 16)] %>% na.omit()
KWFPS<-KWFPS[KWFPS$WFPS<95,]

#quadratic model
WFPS_Nlm<-lm(FN2O ~ WFPS + I(WFPS^2), KWFPS) 

WFPS_Nfit<-nls(FN2O ~ a*WFPS^2 + b*WFPS +c, KWFPS,
               start = list(a=8.133e-06, b=-4.661e-04, c=5.919e-03)) 


#log quadratic model
WFPS_Nlm<-lm(log10(FN2O) ~ WFPS + I(WFPS^2), KWFPS) 

WFPS_Nfit<-nls(log10(FN2O) ~ a*WFPS^2 + b*WFPS +c, KWFPS,
               start = list(a=0.0005685, b=-0.0452186, c=-2.1692589)) 


#exponential model
WFPS_Nlm<-lm(log(FN2O) ~ WFPS , KWFPS)

WFPS_Nfit<-nls(FN2O ~ a*exp(b*WFPS), KWFPS,
               start = list(a=0.0003003176, b=0.03306)) 


ggplot(KWFPS, aes(WFPS, FN2O)) +
  geom_point() +
  geom_line(aes(y=predict(WFPS_Nfit)))+
  theme_bw()


ggplot(KWFPS, aes(WFPS, log10(FN2O))) +
  geom_point() +
  geom_line(aes(y=predict(WFPS_Nfit)))+
  theme_bw()


rss <- sum((log10(KWFPS$FN2O) - predict(WFPS_Nfit))^2)     
tss <- sum((log10(KWFPS$FN2O) - mean(log10(KWFPS$FN2O)))^2)  
r_squared <- 1 - rss/tss 


rss <- sum((KWFPS$FN2O - predict(WFPS_Nfit))^2)     
tss <- sum((KWFPS$FN2O - mean(KWFPS$FN2O))^2)  
r_squared <- 1 - rss/tss 


rss <- sum((log10(KWFPS$FN2O) - predict(WFPS_Nfit))^2)
nrss<-rss/95
rmse<-sqrt(nrss)  




######################################################################plot
k2o<-read_excel("C:/Users/wang/Desktop/Denitrification/Data4/K2/K2_v2.xlsx", sheet = 3)

k2<-k2o[, c(4:19)]

k2$Sand<-as.numeric(k2$Sand)
k2$Silt<-as.numeric(k2$Silt)
k2$Clay<-as.numeric(k2$Clay)
k2$BD<-as.numeric(k2$BD)
k2$pH<-as.numeric(k2$pH)
k2$CN_ratio<-as.numeric(k2$CN_ratio)
k2$real_AI<-as.numeric(k2$real_AI)
k2$Soil_T<-as.numeric(k2$Soil_T)
k2$WFPS<-as.numeric(k2$WFPS)
k2$FN2O<-as.numeric(k2$FN2O)


###########################################################WFPS
KWFPS<-k2[,c(15, 16)] %>% na.omit()
KWFPS<-KWFPS[KWFPS$WFPS<95,]

#log quadratic model
WFPS_Nlm<-lm(log10(FN2O) ~ WFPS + I(WFPS^2), KWFPS) 

WFPS_Nfit<-nls(log10(FN2O) ~ a*WFPS^2 + b*WFPS +c, KWFPS,
               start = list(a=0.0005685, b=-0.0452186, c=-2.1692589)) 


K2WFPS<-ggplot(KWFPS, aes(WFPS, log10(FN2O))) +
  geom_point(size=0.6) +
  geom_line(aes(y=predict(WFPS_Nfit)), col="blue", lwd=0.8)+
  labs(x="WFPS (%)", y=expression(Log[10]*(F[N2O])))+
  annotate(geom = "text", x=40, y=-1, size=2.9,col="green4",
           label=expression(y==0.00057*x^2-0.045*x-2.17))+
  annotate(geom = "text", x=30, y=-1.5, size=3, col="green4",label=expression(R^2==0.31))+
  annotate(geom = "text", x=30, y=-1.9, size=3, col="green4",label=expression(RMSE==0.58))+
  annotate(geom = "text", x=30, y=-2.4, size=3, col="green4",label="p<0.001")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.2),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))




##############################################################BD
KBD<-k2[,c(8, 16)] %>% na.omit()
KBD<-KBD[KBD$BD>0.7,]


#log quadratic model
BD_Nlm<-lm(log10(FN2O) ~ BD + I(BD^2), KBD) 

BD_Nfit<-nls(log10(FN2O) ~ a*BD^2 + b*BD +c, KBD,
             start = list(a=18.46, b=-40.5, c=19.22)) 


K2BD<-ggplot(KBD, aes(BD, log10(FN2O))) +
  geom_point(size=0.6) +
  geom_line(aes(y=predict(BD_Nfit)), col="blue", lwd=0.8)+
  labs(x="Bulk density", y=expression(Log[10]*(F[N2O])))+
  annotate(geom = "text", x=1.15, y=-1, size=2.9,col="green4",
           label=expression(y==18.46*x^2-40.5*x+19.22))+
  annotate(geom = "text", x=1.1, y=-1.5, size=3, col="green4",label=expression(R^2==0.32))+
  annotate(geom = "text", x=1.1, y=-1.9, size=3, col="green4",label=expression(RMSE==0.65))+
  annotate(geom = "text", x=1.1, y=-2.4, size=3, col="green4",label="p<0.001")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.2),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))






















###################################################################################
###################################################################################
###############################################################################RN2O

#####explore the relationships between ratio values and variables
ratioo<-read_excel("C:/Users/wang/Desktop/Denitrification/Data5/Ratio/ratio_mea.xlsx", sheet = 2)

ratio<-ratioo[, c(3, 7:23)]

ratio$Sand<-as.numeric(ratio$Sand)
ratio$Silt<-as.numeric(ratio$Silt)
ratio$Clay<-as.numeric(ratio$Clay)
ratio$BD<-as.numeric(ratio$BD)
ratio$pH<-as.numeric(ratio$pH)
ratio$CN_ratio<-as.numeric(ratio$CN_ratio)
ratio$real_AI<-as.numeric(ratio$real_AI)
ratio$Soil_T<-as.numeric(ratio$Soil_T)
ratio$WFPS<-as.numeric(ratio$WFPS)
ratio$RN2O<-as.numeric(ratio$RN2O)


summary(ratio)
unique(ratio$Continent)
Africa<-ratio[which(ratio$Continent=="Africa"),] 
Asia<-ratio[which(ratio$Continent=="Asia"),]
Name<-ratio[which(ratio$Continent=="North America"),]
Same<-ratio[which(ratio$Continent=="South America"),]
Austra<-ratio[which(ratio$Continent=="Australia"),]
Europe<-ratio[which(ratio$Continent=="Europe"),]


unique(ratio$Land_cover)
cropland<-ratio[which(ratio$Land_cover=="Cropland"),]
fallow<-ratio[which(ratio$Land_cover=="Fallow"),]
forest<-ratio[which(ratio$Land_cover=="Forest"),]
grassland<-ratio[which(ratio$Land_cover=="Grassland"),]


unique(ratio$soil_texture)
silt_loam<-ratio[which(ratio$soil_texture=="Silt loam"),]
siltyclayloam<-ratio[which(ratio$soil_texture=="Silty clay loam"),]
loam<-ratio[which(ratio$soil_texture=="Loam"),]
silt<-ratio[which(ratio$soil_texture=="Silt"),]
sandyloam<-ratio[which(ratio$soil_texture=="Sandy loam"),]
clayloam<-ratio[which(ratio$soil_texture=="Clay loam"),]


unique(ratio$K_climate)
arid<-ratio[which(ratio$K_climate=="Arid"),]
cold<-ratio[which(ratio$K_climate=="Cold"),]
temperate<-ratio[which(ratio$K_climate=="Temperate"),]
tropical<-ratio[which(ratio$K_climate=="Tropical"),]



ratio$var<-rep("RN2O", 2453)

ggplot(ratio, aes(x=ratio$var, y=ratio$RN2O))+
  geom_violin(lwd=0.3)+
  labs(x="", y="Value")+
  scale_x_discrete(labels=c(expression(R[N2O])))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.line.x.bottom =  element_line(linewidth = 0.6, color = "white"),
        axis.line.y.left =  element_line(linewidth = 0.6, color = "white"),
        panel.border = element_rect(color = "white", linewidth = 0.6),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "none",
        axis.title.y = element_text(margin = margin(r = -1)),
        axis.ticks = element_line(linewidth = 0.6, color = "white"),
        axis.text.x = element_text(size = 11, color = "white"),
        axis.text.y = element_text(size = 10, color = "white"),
        axis.title = element_text(size = 12, color = "white"))




#differences between continents
rcontinent<-ratio[, c(1,18)] %>% na.omit()

rcontinent<-rcontinent %>% group_by(Continent) %>% mutate(mean=mean(RN2O))
rcontinent<-rcontinent %>% group_by(Continent) %>% mutate(median=median(RN2O))


stat_test_rcon <- rcontinent %>% t_test(RN2O ~ Continent) %>% 
  adjust_pvalue(method = "BH") %>% add_significance("p.adj") %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "Continent")


ratio_con<-ggplot() +
  geom_boxplot(aes(x = rcontinent$Continent, y = rcontinent$RN2O, 
                   fill = rcontinent$Continent), outlier.size = 0.2, size = 0.5) +
  stat_pvalue_manual(stat_test_rcon, label = "p.adj.signif", tip.length = 0.01,
                     size = 5, vjust = 1, step.increase = 0.05) +
  labs(x="Continents", y=expression(R[N2O]), fill="")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_brewer(palette = "Set3")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.3),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "none",
        axis.text = element_text(size = 10),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12))




#differences between land cover
rLandcover<-ratio[, c(3,18)] %>% na.omit()

rLandcover <- rLandcover %>% group_by(Land_cover) %>% mutate(mean=mean(RN2O))
rLandcover <- rLandcover %>% group_by(Land_cover) %>% mutate(median=median(RN2O))

stat_test_rlc <- rLandcover %>% t_test(RN2O ~ Land_cover) %>%
  adjust_pvalue(method = "BH") %>% add_significance("p.adj") %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "Land_cover")


ratio_lc<-ggplot() +
  geom_boxplot(aes(x = rLandcover$Land_cover, y = rLandcover$RN2O, 
                   fill = rLandcover$Land_cover), outlier.size = 0.2, size = 0.5) +
  stat_pvalue_manual(stat_test_rlc, label = "p.adj.signif", tip.length = 0.01,
                     size = 5, vjust = 1, step.increase = 0.05) +
  labs(x="Land cover", y="", fill="")+
  scale_fill_brewer(palette = "Set3")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.3),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "none",
        axis.text = element_text(size = 10),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12))




#differences between soil texture
ratio<-ratio[!ratio$Biochar=="Biochar",]
rsoiltexture<-ratio[, c(4,18)] %>% na.omit()

rsoiltexture <- rsoiltexture %>% group_by(soil_texture) %>% mutate(mean=mean(ratio))
rsoiltexture <- rsoiltexture %>% group_by(soil_texture) %>% mutate(median=median(ratio))


stat_test_rst <- rsoiltexture %>% t_test(RN2O ~ soil_texture) %>%
  adjust_pvalue(method = "BH") %>% add_significance("p.adj") %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "soil_texture") 


ggplot() +
  geom_boxplot(aes(x = rsoiltexture$soil_texture, y = rsoiltexture$RN2O, 
                   fill = rsoiltexture$soil_texture), outlier.size = 0.2, size = 0.5) +
  stat_pvalue_manual(stat_test_rst, label = "p.adj.signif", tip.length = 0.01,
                     size = 5, vjust = 1, step.increase = 0.01) +
  labs(x="Soil texture", y=expression(R[N2O]), fill="")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_brewer(palette = "Set3")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.3),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "none",
        axis.text = element_text(size = 10),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12))




#differences between climates
rclimate<-ratio[, c(14,18)] %>% na.omit()

rclimate <- rclimate %>% group_by(K_climate) %>% mutate(mean=mean(RN2O))
rclimate <- rclimate %>% group_by(K_climate) %>% mutate(median=median(RN2O))

stat_test_rcli <- rclimate %>% t_test(RN2O ~ K_climate) %>%
  adjust_pvalue(method = "BH") %>% add_significance("p.adj") %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "K_climate")


ratio_cli<-ggplot() +
  geom_boxplot(aes(x = rclimate$K_climate, y = rclimate$RN2O, fill = rclimate$K_climate),
               outlier.size = 0.2, size = 0.5) +
  stat_pvalue_manual(stat_test_rcli, label = "p.adj.signif", tip.length = 0.01,
                     size = 5, vjust = 1, step.increase = 0.05) +
  scale_fill_brewer(palette = "Set3")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.3),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "none",
        axis.text = element_text(size = 10),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12))



#differences between biochar
biochar<-read_excel("C:/Users/wang/Desktop/Denitrification/Data4/Ratio/Biochar.xlsx", sheet = 6)
rbiochar<-biochar[, c(17, 28)] %>% na.omit()

rbiochar <- rbiochar %>% group_by(Biochar) %>% mutate(mean=mean(RN2O))
rbiochar <- rbiochar %>% group_by(Biochar) %>% mutate(median=median(RN2O))

stat_test_rbio <- rbiochar %>% t_test(lnRR ~ Biochar) %>%
  adjust_pvalue(method = "BH") %>% add_significance("p.adj") %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "Biochar")


ratio_bio<-ggplot() +
  geom_boxplot(aes(x = rbiochar$Biochar, y = rbiochar$lnRR, fill = rbiochar$Biochar),
               outlier.size = 0.2, size = 0.5) +
  stat_pvalue_manual(stat_test_rbio, label = "p.adj.signif", tip.length = 0.01,
                     size = 5, vjust = 1, step.increase = 0.05) +
  labs(x="Treatments", y="lnRR", fill="")+
  scale_fill_brewer(palette = "Set3")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.3),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "none",
        axis.text = element_text(size = 10),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12))+
  geom_hline(yintercept = 0, col="red", linetype="dashed")




#differences between experimental design
rdesign<-ratio[, c(2,18)] %>% na.omit()

rdesign <- rdesign %>% group_by(Design) %>% mutate(mean=mean(RN2O))
rdesign <- rdesign %>% group_by(Design) %>% mutate(median=median(RN2O))


stat_test_rdes <- rdesign %>% t_test(RN2O ~ Design) %>%
  adjust_pvalue(method = "BH") %>% add_significance("p.adj") %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "Design")


ratio_des<-ggplot() +
  geom_boxplot(aes(x = rdesign$Design, y = rdesign$RN2O, fill = rdesign$Design),
               outlier.size = 0.2, size = 0.5) +
  stat_pvalue_manual(stat_test_rdes, label = "p.adj.signif", tip.length = 0.01,
                     size = 5, vjust = 1, step.increase = 0.05) +
  scale_fill_brewer(palette = "Set3")+
  labs(x=NULL, y=expression(R[N2O]))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.3),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "none",
        axis.text = element_text(size = 10),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12))



#differences between experimental period
rperiod<-ratio[, c(3,18)] %>% na.omit()

rperiod <- rperiod %>% group_by(Period) %>% mutate(mean=mean(RN2O))
rperiod <- rperiod %>% group_by(Period) %>% mutate(median=median(RN2O))

stat_test_rper <- rperiod %>% t_test(RN2O ~ Period) %>%
  adjust_pvalue(method = "BH") %>% add_significance("p.adj") %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "Period")


ratio_per<-ggplot() +
  geom_boxplot(aes(x = rperiod$Period, y = rperiod$RN2O, fill = rperiod$Period),
               outlier.size = 0.2, size = 0.5) +
  stat_pvalue_manual(stat_test_rper, label = "p.adj.signif", tip.length = 0.01,
                     size = 5, vjust = 1, step.increase = 0.05) +
  scale_fill_brewer(palette = "Set3")+
  labs(x=NULL, y=expression(R[N2O]))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.3),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "none",
        axis.text = element_text(size = 10),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12))




#meansurement varies as continent
mea_con<-ratio[, c(1,2,18)] %>% na.omit()

ggplot(mea_con, aes(Design, fill = Design))+
  geom_bar(stat = "count", width = 0.3)+
  stat_count(geom = "text",aes(label = after_stat(count)), vjust = 0.8, size=2.5) +
  facet_wrap(.~Continent)+
  labs(x=NULL, y="Count")+
  scale_fill_brewer(palette = 16)+ #10
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 7),
        plot.margin = unit(c(0,0,0,0), "pt"),
        legend.position = "none",
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8))





####################################description
ratioo<-read_excel("C:/Users/wang/Desktop/Denitrification/Data4/Ratio/ratio_v2.xlsx", sheet = 4)
ratio<-ratioo[, c(3, 7:23)]

ratio$RN2O<-as.numeric(ratio$RN2O)


#continent
rcontinent<-ratio[, c(1,18)] %>% na.omit()

rcontinent<-rcontinent %>% group_by(Continent) %>% mutate(mean=mean(RN2O))
rcontinent<-rcontinent %>% group_by(Continent) %>% mutate(median=median(RN2O))

ca<-unique(rcontinent[,c(1,3,4)])

rcontinent %>% group_by(Continent) %>% summarise( Q1 = quantile(RN2O, 0.25, na.rm = TRUE),
                                                  Median = median(RN2O, na.rm = TRUE), 
                                                  Q3 = quantile(RN2O, 0.75, na.rm = TRUE))



#land cover
rLandcover<-ratio[, c(3,18)] %>% na.omit()

rLandcover <- rLandcover %>% group_by(Land_cover) %>% mutate(mean=mean(RN2O))
rLandcover <- rLandcover %>% group_by(Land_cover) %>% mutate(median=median(RN2O))

la<-unique(rLandcover[, c(1, 3, 4)])

rLandcover %>% group_by(Land_cover) %>% 
  summarise( Q1 = quantile(RN2O, 0.25, na.rm = TRUE), 
             Median = median(RN2O, na.rm = TRUE), Q3 = quantile(RN2O, 0.75, na.rm = TRUE))



#climates
rclimate<-ratio[, c(14,18)] %>% na.omit()

rclimate <- rclimate %>% group_by(K_climate) %>% mutate(mean=mean(RN2O))
rclimate <- rclimate %>% group_by(K_climate) %>% mutate(median=median(RN2O))

ka<-unique(rclimate[, c(1,3,4)])

rclimate %>% group_by(K_climate) %>% 
  summarise( Q1 = quantile(RN2O, 0.25, na.rm = TRUE),
             Median = median(RN2O, na.rm = TRUE), Q3 = quantile(RN2O, 0.75, na.rm = TRUE))


#soil texture
rsoiltexture<-ratio[, c(4,18)] %>% na.omit()

rsoiltexture <- rsoiltexture %>% group_by(soil_texture) %>% mutate(mean=mean(RN2O))
rsoiltexture <- rsoiltexture %>% group_by(soil_texture) %>% mutate(median=median(RN2O))

st<-unique(rsoiltexture[, c(1,3,4)])

rsoiltexture %>% group_by(soil_texture) %>% 
  summarise( Q1 = quantile(RN2O, 0.25, na.rm = TRUE),
             Median = median(RN2O, na.rm = TRUE), Q3 = quantile(RN2O, 0.75, na.rm = TRUE))



#biochar
rbiochar<-ratio[, c(11,18)] %>% na.omit()

rbiochar <- rbiochar %>% group_by(Biochar) %>% mutate(mean=mean(RN2O))
rbiochar <- rbiochar %>% group_by(Biochar) %>% mutate(median=median(RN2O))

boi<-unique(rbiochar[, c(1,3,4)])

rbiochar %>% group_by(Biochar) %>% 
  summarise( Q1 = quantile(RN2O, 0.25, na.rm = TRUE),
             Median = median(RN2O, na.rm = TRUE), Q3 = quantile(RN2O, 0.75, na.rm = TRUE))






##################################relationships between qualitative variables
ratioo<-read_excel("C:/Users/wang/Desktop/Denitrification/Data5/Ratio/ratio_mea.xlsx", sheet = 2)
ratio<-ratioo[, c(3, 7, 8, 10, 11, 18, 20, 23)]

ratio$RN2O<-as.numeric(ratio$RN2O)

ratio$Continent<-factor(ratio$Continent, levels = c("Europe", "Asia", "North America",
                                                    "South America", "Australia", "Africa"))

ratio$Land_cover<-factor(ratio$Land_cover, levels = c("Cropland", "Fallow", "Grassland", "Forest" ))

ratio$soil_texture<-factor(ratio$soil_texture, levels = c("Clay loam", "Sandy loam","Loam", "Silt loam",      
                                                          "Silty clay loam", "Clay","Loamy sand", "Sandy clay",
                                                          "Silty clay", "Sandy clay loam", "Sand", "Silt"))

ratio$K_climate<-factor(ratio$K_climate, levels = c("Arid", "Cold", "Temperate", "Tropical"))

ratio$Biochar<- factor(ratio$Biochar, levels = c("Control", "Fertilizer", "Residue", "Biochar"))

ratio$Design<-factor(ratio$Design, levels = c("Laboratory", "Field"))

ratio$Period<-factor(ratio$Period, levels = c("Long-term", "Short-term"))


ratio_ctree <- partykit::ctree(log1p(ratio$RN2O) ~ Continent+Land_cover+soil_texture+K_climate+
                                 Biochar + Design + Period, data = ratio,
                               control = partykit::ctree_control(alpha = 0.01))


plot(ratio_ctree, gp = gpar(fontsize = 6.5))


png('C:/Users/wang/Desktop/Denitrification/Data5/Figures/tree.png', 
    width=10388, height=5789, res=900) 


plot(ratio_ctree, gp = gpar(fontsize = 7.5, bg="transparent"))

grid.force()

txt <- grid.ls(print = FALSE)$name
txt <- txt[grep("text", txt)]

for (t in txt) {
  lab <- grid.get(t)$label
  if (is.character(lab)) {
    lab_new <- paste(strwrap(lab, width = 18), collapse = "\n")
    
    grid.gedit(t, label = lab_new)
  }
}

dev.off()





########################################confounder
ratioo<-read_excel("C:/Users/wang/Desktop/Denitrification/Data5/Ratio/ratio_mea.xlsx", sheet = 2)

RN2O<-ratioo[, c(3, 7:23)]

RN2O$Sand<-as.numeric(RN2O$Sand)
RN2O$Silt<-as.numeric(RN2O$Silt)
RN2O$Clay<-as.numeric(RN2O$Clay)
RN2O$BD<-as.numeric(RN2O$BD)
RN2O$pH<-as.numeric(RN2O$pH)
RN2O$CN_ratio<-as.numeric(RN2O$CN_ratio)
RN2O$real_AI<-as.numeric(RN2O$real_AI)
RN2O$Soil_T<-as.numeric(RN2O$Soil_T)
RN2O$WFPS<-as.numeric(RN2O$WFPS)
RN2O$RN2O<-as.numeric(RN2O$RN2O)


dag <- dagitty("
dag {
    
    RN2O <- Continent
    RN2O <- Land_cover
    RN2O <- Soil_texture
    RN2O <- Design
    RN2O <- Period
    RN2O <- Sand
    RN2O <- Silt
    RN2O <- Clay
    RN2O <- BD
    RN2O <- pH
    RN2O <- CN_ratio
    RN2O <- K_climate
    RN2O <- real_AI
    RN2O <- Biochar
    RN2O <- Soil_T
    RN2O <- WFPS
    
    Land_cover<- Continent
    Land_cover<-Soil_texture
    Land_cover<-Period
    Land_cover<-Design
    Land_cover<-Sand
    Land_cover<-Silt
    Land_cover<-Clay
    Land_cover<-BD
    Land_cover<-pH
    Land_cover<-CN_ratio
    Land_cover<-real_AI
    Land_cover<-K_climate
    
    Soil_texture<-Land_cover
    Soil_texture<-Continent
    Soil_texture<-Design
    Soil_texture<-Period
    
    BD<-Continent
    BD<-Land_cover
    BD<-Soil_texture
    BD<-Design
    BD<-Period
    BD<-K_climate
    BD<-real_AI
    BD<-WFPS
    
    pH<-Continent
    pH<-Land_cover
    pH<-Soil_texture
    pH<-Design
    pH<-Period
    pH<-CN_ratio
    pH<-Biochar
    pH<-K_climate
    pH<-real_AI
    
    CN_ratio<-Continent
    CN_ratio<-Land_cover
    CN_ratio<-Biochar
    CN_ratio<-Soil_texture
    CN_ratio<-Design
    CN_ratio<-Period
    CN_ratio<-K_climate
    CN_ratio<-real_AI
    
    K_climate<-Continent
    real_AI<-Continent
    
    Biochar<-Continent
    Biochar<-Land_cover
    Biochar<-Soil_texture
    Biochar<-K_climate
    Biochar<-real_AI
    Biochar<-Design
    Biochar<-Period
    
    Soil_T<-real_AI
    Soil_T<-K_climate
    Soil_T<-Land_cover
    Soil_T<-Soil_texture
    Soil_T<-Design
    Soil_T<-Period
    Soil_T<-Continent
    
    WFPS<-real_AI
    WFPS<-K_climate
    WFPS<-Land_cover
    WFPS<-Design
    WFPS<-Period
    WFPS<-Soil_texture
    WFPS<-Continent

}
")


##################################plot DAG 
ggdag(dag, layout="circle", 
      text_size = 5, text= T, text_col = "black", node = F)+
  geom_dag_label(aes(color = name == "Continent"), size = 5) +
  scale_color_manual(values = c("TRUE" = "red"), guide = "none") +
  theme_dag()+
  theme(plot.margin = unit(c(0,0,0,0), "pt"))



#################################identify confounders that need to be adjusted
adjustmentSets(dag, exposure = "Biochar", outcome = "RN2O")

ggdag_adjustment_set(dag, exposure = "Continent", outcome = "ratio",
                     node_size = 8, text_size = 2,
                     text_col = "black", shadow = F) +
  theme_dag()+
  theme(legend.position = "none")






#############################################normality test
ratioo<-read_excel("C:/Users/wang/Desktop/Denitrification/Data4/Ratio/ratio_v2.xlsx", sheet = 4)
RN2O<-ratioo[, c(3, 7:23)]

RN2O$Sand<-as.numeric(RN2O$Sand)
RN2O$Silt<-as.numeric(RN2O$Silt)
RN2O$Clay<-as.numeric(RN2O$Clay)
RN2O$BD<-as.numeric(RN2O$BD)
RN2O$pH<-as.numeric(RN2O$pH)
RN2O$CN_ratio<-as.numeric(RN2O$CN_ratio)
RN2O$real_AI<-as.numeric(RN2O$real_AI)
RN2O$precipitation<-as.numeric(RN2O$precipitation)
RN2O$Soil_T<-as.numeric(RN2O$Soil_T)
RN2O$WFPS<-as.numeric(RN2O$WFPS)
RN2O$RN2O<-as.numeric(RN2O$RN2O)


shapiro.test(RN2O$Sand)
shapiro.test(RN2O$Silt)
shapiro.test(RN2O$Clay)
shapiro.test(RN2O$BD)
shapiro.test(RN2O$pH)
shapiro.test(RN2O$CN_ratio)
shapiro.test(RN2O$real_AI)
shapiro.test(RN2O$Soil_T)
shapiro.test(RN2O$WFPS)
shapiro.test(RN2O$RN2O)
shapiro.test(log1p(RN2O$RN2O))


ggqqplot(RN2O$Sand)
ggqqplot(RN2O$Silt)
ggqqplot(RN2O$Clay)
ggqqplot(RN2O$BD)
ggqqplot(RN2O$pH)
ggqqplot(RN2O$CN_ratio)
ggqqplot(RN2O$real_AI)
ggqqplot(RN2O$Soil_T)
ggqqplot(RN2O$WFPS)
ggqqplot(RN2O$RN2O)
ggqqplot(log1p(RN2O$RN2O))





################################################correlation analysis
ratioo<-read_excel("C:/Users/wang/Desktop/Denitrification/Data4/Ratio/ratio_v2.xlsx", sheet = 4)

RN2O<-ratioo[, c(3, 8:23)]

RN2O$Sand<-as.numeric(RN2O$Sand)
RN2O$Silt<-as.numeric(RN2O$Silt)
RN2O$Clay<-as.numeric(RN2O$Clay)
RN2O$BD<-as.numeric(RN2O$BD)
RN2O$pH<-as.numeric(RN2O$pH)
RN2O$CN_ratio<-as.numeric(RN2O$CN_ratio)
RN2O$real_AI<-as.numeric(RN2O$real_AI)
RN2O$Soil_T<-as.numeric(RN2O$Soil_T)
RN2O$WFPS<-as.numeric(RN2O$WFPS)
RN2O$RN2O<-as.numeric(RN2O$RN2O)


correlation::correlation(RN2O, include_factors = F, method = "spearman")


#qualitative
kruskal.test(RN2O ~ Continent, data = RN2O)
kruskal.test(RN2O ~ K_climate, data = RN2O)
kruskal.test(RN2O ~ soil_texture, data = RN2O)
kruskal.test(RN2O ~ Land_cover, data = RN2O) 


dunnTest(RN2O~ Continent, data = RN2O, method = "bonferroni")
dunnTest(RN2O~ K_climate, data = RN2O, method = "bonferroni")
dunnTest(RN2O~ soil_texture, data = RN2O, method = "bonferroni")
dunnTest(RN2O~ Land_cover, data = RN2O, method = "bonferroni") 
dunnTest(RN2O~ Biochar, data = RN2O, method = "bonferroni")






################################################Non-linear regression analysis
ratioo<-read_excel("C:/Users/wang/Desktop/Denitrification/Data5/Ratio/ratio_mea.xlsx", sheet = 2)

RN2O<-ratioo[, c(3, 7:23)]

RN2O$Sand<-as.numeric(RN2O$Sand)
RN2O$Silt<-as.numeric(RN2O$Silt)
RN2O$Clay<-as.numeric(RN2O$Clay)
RN2O$BD<-as.numeric(RN2O$BD)
RN2O$pH<-as.numeric(RN2O$pH)
RN2O$CN_ratio<-as.numeric(RN2O$CN_ratio)
RN2O$real_AI<-as.numeric(RN2O$real_AI)
RN2O$Soil_T<-as.numeric(RN2O$Soil_T)
RN2O$WFPS<-as.numeric(RN2O$WFPS)
RN2O$RN2O<-as.numeric(RN2O$RN2O)


#################################################################Sand
RSand<-RN2O[,c(4, 17)] %>% na.omit()
RSand<-RSand[-c(100,111,608),]

#quadratic model
Sand_Nlm<-lm(RN2O ~ Sand + I(Sand^2), RSand) 

Sand_Nfit<-nls(RN2O ~ a*Sand^2 + b*Sand +c, RSand,
               start = list(a=-6.685e-06, b=1.066e-04, c=4.099e-01))


#log quadratic model
Sand_Nlm<-lm(log10(RN2O) ~ Sand + I(Sand^2), RSand) 

Sand_Nfit<-nls(log10(RN2O) ~ a*Sand^2 + b*Sand +c, RSand,
               start = list(a=-1.338e-05, b=7.340e-04, c=-6.114e-01))


#exponential model
Sand_Nlm<-lm(log(RN2O) ~ Sand , RSand)

Sand_Nfit<-nls(RN2O ~ a*exp(b*Sand), RSand,
               start = list(a=0.2556318, b=-0.001276)) 
 

ggplot(RSand, aes(Sand, log10(RN2O))) +
  geom_point() +
  geom_line(aes(y=predict(Sand_Nfit)))+
  theme_bw()

ggplot(RSand, aes(Sand, RN2O)) +
  geom_point() +
  geom_line(aes(y=predict(Sand_Nfit)))+
  theme_bw()


rss <- sum((log10(KSand$FN2O) - predict(Sand_Nfit))^2)     
tss <- sum((log10(KSand$FN2O) - mean(log10(KSand$FN2O)))^2)  
r_squared <- 1 - rss/tss 


rss <- sum((RSand$RN2O - predict(Sand_Nfit))^2)     
tss <- sum((RSand$RN2O - mean(RSand$RN2O))^2)  
r_squared <- 1 - rss/tss 




####################################################################Silt
RSilt<-RN2O[,c(5, 17)] %>% na.omit()
RSilt<-RSilt[-c(100,111,608),]

#quadratic model
Silt_Nlm<-lm(RN2O ~ Silt + I(Silt^2), RSilt) 

Silt_Nfit<-nls(RN2O ~ a*Silt^2 + b*Silt +c, RSilt,
               start = list(a=-2.262e-05, b=2.065e-03, c=3.619e-01))


#log quadratic model
Silt_Nlm<-lm(log10(RN2O) ~ Silt + I(Silt^2), RSilt) 

Silt_Nfit<-nls(log10(RN2O) ~ a*Silt^2 + b*Silt +c, RSilt,
               start = list(a=-0.0001004, b=0.0067647, c=-0.6781315))


#exponential model
Silt_Nlm<-lm(log(RN2O) ~ Silt , RSilt)

Silt_Nfit<-nls(RN2O ~ a*exp(b*Silt), RSilt,
               start = list(a=0.2604952, b=-0.001814))


ggplot(RSilt, aes(Silt, log10(RN2O))) +
  geom_point() +
  geom_line(aes(y=predict(Silt_Nfit)))+
  theme_bw()

ggplot(RSilt, aes(Silt, RN2O)) +
  geom_point() +
  geom_line(aes(y=predict(Silt_Nfit)))+
  theme_bw()


rss <- sum((KSilt$FN2O - predict(Silt_Nfit))^2)     
tss <- sum((KSilt$FN2O - mean(KSilt$FN2O))^2)  
r_squared <- 1 - rss/tss 


rss <- sum((log10(RSilt$RN2O) - predict(Silt_Nfit))^2)    
tss <- sum((log10(RSilt$RN2O) - mean(log10(RSilt$RN2O)))^2)  
r_squared <- 1 - rss/tss 




###################################################################Clay
RClay<-RN2O[,c(6, 17)] %>% na.omit()
RClay<-RClay[-c(100,111,661),]

#quadratic model
Clay_Nlm<-lm(RN2O ~ Clay + I(Clay^2), RClay) 

Clay_Nfit<-nls(RN2O ~ a*Clay^2 + b*Clay +c, RClay,
               start = list(a=-4.063e-05, b=3.779e-03, c=3.393e-01))


#log quadratic model
Clay_Nlm<-lm(log10(RN2O) ~ Clay + I(Clay^2), RClay) 

Clay_Nfit<-nls(log10(RN2O) ~ a*Clay^2 + b*Clay +c, RClay,
               start = list(a=-0.0001802, b=0.0137117, c=-0.7915810))


#exponential model
Clay_Nlm<-lm(log(RN2O) ~ Clay , RClay)

Clay_Nfit<-nls(RN2O ~ a*exp(b*Clay), RClay,
               start = list(a=0.013, b=0.187121))


ggplot(RClay, aes(Clay, log10(RN2O))) +
  geom_point() +
  geom_line(aes(y=predict(Clay_Nfit)))+
  theme_bw()


ggplot(RClay, aes(Clay, RN2O)) +
  geom_point() +
  geom_line(aes(y=predict(Clay_Nfit)))+
  theme_bw()


rss <- sum((RClay$RN2O - predict(Clay_Nfit))^2)     
tss <- sum((RClay$RN2O - mean(RClay$RN2O))^2)  
r_squared <- 1 - rss/tss 


rss <- sum((log10(RClay$RN2O) - predict(Clay_Nfit))^2)     
tss <- sum((log10(RClay$RN2O) - mean(log10(RClay$RN2O)))^2)  
r_squared <- 1 - rss/tss 




#####################################################################BD
RBD<-RN2O[,c(7, 17)] %>% na.omit()
RBD<-RBD[-c(90,101),]  


#quadratic model
BD_Nlm<-lm(RN2O ~ BD + I(BD^2), RBD) 

BD_Nfit<-nls(RN2O ~ a*BD^2 + b*BD +c, RBD,
             start = list(a=0.05173, b=-0.10963, c=0.46193))


#log quadratic model
BD_Nlm<-lm(log10(RN2O) ~ BD + I(BD^2), RBD) 

BD_Nfit<-nls(log10(RN2O) ~ a*BD^2 + b*BD +c, RBD,
             start = list(a=0.05173, b=-0.10963, c=0.46193))


#exponential model
BD_Nlm<-lm(log(RN2O) ~ BD , RBD)

BD_Nfit<-nls(RN2O ~ a*exp(b*BD), RBD,
             start = list(a=0.2648398, b= 0.006754)) 


ggplot(RBD, aes(BD, RN2O)) +
  geom_point() +
  geom_line(aes(y=predict(BD_Nfit)))+
  theme_bw()


ggplot(RBD, aes(BD, log10(RN2O))) +
  geom_point() +
  geom_line(aes(y=predict(BD_Nfit)))+
  theme_bw()


rss <- sum((RBD$RN2O - predict(BD_Nfit))^2)     
tss <- sum((RBD$RN2O - mean(RBD$RN2O))^2)  
r_squared <- 1 - rss/tss 


rss <- sum((log10(RBD$RN2O) - predict(BD_Nfit))^2)     
tss <- sum((log10(RBD$RN2O) - mean(log10(RBD$RN2O)))^2)  
r_squared <- 1 - rss/tss 



#####################################################################pH
RpH<-RN2O[,c(8, 17)] %>% na.omit()
RpH<-RpH[-c(154,165,910,912,1132),]

#quadratic model
pH_Nlm<-lm(RN2O ~ pH + I(pH^2), RpH) 

pH_Nfit<-nls(RN2O ~ a*pH^2 + b*pH +c, RpH,
             start = list(a=-0.01040, b=0.07257, c=0.38607)) 


#log quadratic model
pH_Nlm<-lm(log10(RN2O) ~ pH + I(pH^2), RpH) 

pH_Nfit<-nls(log10(RN2O) ~ a*pH^2 + b*pH +c, RpH,
             start = list(a=-0.04172, b=0.44436, c=-1.67385)) 


#exponential model
pH_Nlm<-lm(log(RN2O) ~ pH , RpH)

pH_Nfit<-nls(RN2O ~ a*exp(b*pH), RpH,
             start = list(a=0.08716, b=0.8006511)) 


ggplot(RpH, aes(pH, RN2O)) +
  geom_point() +
  geom_line(aes(y=predict(pH_Nfit)))+
  theme_bw()


ggplot(RpH, aes(pH, log10(RN2O))) +
  geom_point() +
  geom_line(aes(y=predict(pH_Nfit)))+
  theme_bw()


rss <- sum((RpH$RN2O - predict(pH_Nfit))^2)     
tss <- sum((RpH$RN2O - mean(RpH$RN2O))^2)  
r_squared <- 1 - rss/tss 




##########################################################################CN ratio
RCNRatio<-RN2O[,c(9, 17)] %>% na.omit()
RCNRatio<-RCNRatio[RCNRatio$CN_ratio<100,]
RCNRatio<-RCNRatio[-c(61,399,401),]

#quadratic model
CNRatio_Nlm<-lm(RN2O ~ CN_ratio + I(CN_ratio^2), RCNRatio) 

CNRatio_Nfit<-nls(RN2O ~ a*CN_ratio^2 + b*CN_ratio +c, RCNRatio,
                  start = list(a=1.404e-05, b=-6.038e-03, c=4.866e-01))


#log quadratic model
CNRatio_Nlm<-lm(log10(RN2O) ~ CN_ratio + I(CN_ratio^2), RCNRatio) 

CNRatio_Nfit<-nls(log10(RN2O) ~ a*CN_ratio^2 + b*CN_ratio +c, RCNRatio,
                  start = list(a=0.0001819, b=-0.0251285, c=-0.3311073))


#exponential model
CNRatio_Nlm<-lm(log(RN2O) ~ CN_ratio , RCNRatio)

CNRatio_Nfit<-nls(RN2O ~ a*exp(b*CN_ratio), RCNRatio,
                  start = list(a=-1.02782, b=-0.02919)) 


ggplot(RCNRatio, aes(CN_ratio, RN2O)) +
  geom_point() +
  geom_line(aes(y=predict(CNRatio_Nfit)))+
  theme_bw()


ggplot(RCNRatio, aes(CN_ratio, log10(RN2O))) +
  geom_point() +
  geom_line(aes(y=predict(CNRatio_Nfit)))+
  theme_bw()


rss <- sum((RCNRatio$RN2O - predict(CNRatio_Nfit))^2)     
tss <- sum((RCNRatio$RN2O - mean(RCNRatio$RN2O))^2)  
r_squared <- 1 - rss/tss 


rss <- sum((log10(RCNRatio$RN2O) - predict(CNRatio_Nfit))^2)     
tss <- sum((log10(RCNRatio$RN2O) - mean(log10(RCNRatio$RN2O)))^2)  
r_squared <- 1 - rss/tss 




###########################################################################real AI
RAI<-RN2O[,c(11, 17)] %>% na.omit()
RAI<-RAI[-c(172,  183, 1053, 1055, 1391, 1392, 1420, 1459, 1482, 1483),]


#quadratic model
AI_Nlm<-lm(RN2O ~ real_AI + I(real_AI^2), RAI) 

AI_Nfit<-nls(RN2O ~ a*real_AI^2 + b*real_AI +c, RAI,
             start = list(a=-0.03179, b=0.20668, c=0.25943)) 


#log quadratic model
AI_Nlm<-lm(log10(RN2O) ~ real_AI + I(real_AI^2), RAI) 

AI_Nfit<-nls(log10(RN2O) ~ a*real_AI^2 + b*real_AI +c, RAI,
             start = list(a=-0.05295, b=0.33118, c=-0.81552))


#exponential model
AI_Nlm<-lm(log(RN2O) ~ real_AI , RAI)

AI_Nfit<-nls(RN2O ~ a*exp(b*real_AI), RAI,
             start = list(a=-1.7470, b=0.4752)) 


ggplot(RAI, aes(real_AI, log10(RN2O))) +
  geom_point() +
  geom_line(aes(y=predict(AI_Nfit)))+
  theme_bw()


ggplot(RAI, aes(real_AI, RN2O)) +
  geom_point() +
  geom_line(aes(y=predict(AI_Nfit)))+
  theme_bw()


rss <- sum((log10(KAI$FN2O) - predict(AI_Nfit))^2)     
tss <- sum((log10(KAI$FN2O) - mean(log10(KAI$FN2O)))^2)  
r_squared <- 1 - rss/tss 




#########################################################################soil T
RSoilT<-RN2O[,c(15, 17)] %>% na.omit()
RSoilT<-RSoilT[RSoilT$Soil_T<50,]
RSoilT<-RSoilT[-c(83,85),]

#quadratic model
SoilT_Nlm<-lm(RN2O ~ Soil_T + I(Soil_T^2), RSoilT) 

SoilT_Nfit<-nls(RN2O ~ a*Soil_T^2 + b*Soil_T +c, RSoilT,
                start = list(a=0.000109, b=-0.015362, c=0.569680)) 


#log quadratic model
SoilT_Nlm<-lm(log10(RN2O) ~ Soil_T + I(Soil_T^2), RSoilT) 

SoilT_Nfit<-nls(log10(RN2O) ~ a*Soil_T^2 + b*Soil_T +c, RSoilT,
                start = list(a= -0.0000821, b=-0.0259255, c=-0.2525247)) 


#exponential model
SoilT_Nlm<-lm(log(RN2O) ~ Soil_T , RSoilT)

SoilT_Nfit<-nls(RN2O ~ a*exp(b*Soil_T), RSoilT,
                start = list(a=0.53856, b=-0.06641)) 


ggplot(RSoilT, aes(Soil_T, log10(RN2O))) +
  geom_point() +
  geom_line(aes(y=predict(SoilT_Nfit)))+
  theme_bw()


ggplot(RSoilT, aes(Soil_T, RN2O)) +
  geom_point() +
  geom_line(aes(y=predict(SoilT_Nfit)))+
  theme_bw()


rss <- sum((log10(RSoilT$RN2O) - predict(SoilT_Nfit))^2)     
tss <- sum((log10(RSoilT$RN2O) - mean(log10(RSoilT$RN2O)))^2)  
r_squared <- 1 - rss/tss 


rss <- sum((RSoilT$RN2O - predict(SoilT_Nfit))^2)     
tss <- sum((RSoilT$RN2O - mean(RSoilT$RN2O))^2)  
r_squared <- 1 - rss/tss 




##########################################################################WFPS
RWFPS<-RN2O[,c(16, 17)] %>% na.omit()
RWFPS<-RWFPS[-c(41,52,420,440),]


#quadratic model
WFPS_Nlm<-lm(RN2O ~ WFPS + I(WFPS^2), RWFPS) 

WFPS_Nfit<-nls(RN2O ~ a*WFPS^2 + b*WFPS +c, RWFPS,
               start = list(a=3.031e-06, b=-2.793e-03, c=5.849e-01)) 


#log quadratic model
WFPS_Nlm<-lm(log10(RN2O) ~ WFPS + I(WFPS^2), RWFPS) 

WFPS_Nfit<-nls(log10(RN2O) ~ a*WFPS^2 + b*WFPS +c, RWFPS,
               start = list(a=8.044e-06, b=-4.332e-03, c=-3.276e-01))


#exponential model
WFPS_Nlm<-lm(log(RN2O) ~ WFPS , RWFPS)

WFPS_Nfit<-nls(RN2O ~ a*exp(b*WFPS), RWFPS,
               start = list(a=-0.864759, b=-0.007012)) 


ggplot(RWFPS, aes(WFPS, RN2O)) +
  geom_point() +
  geom_line(aes(y=predict(WFPS_Nfit)))+
  theme_bw()


ggplot(RWFPS, aes(WFPS, log10(RN2O))) +
  geom_point() +
  geom_line(aes(y=predict(WFPS_Nfit)))+
  theme_bw()


rss <- sum((log10(RWFPS$RN2O) - predict(WFPS_Nfit))^2)     
tss <- sum((log10(RWFPS$RN2O) - mean(log10(RWFPS$RN2O)))^2)  
r_squared <- 1 - rss/tss 


rss <- sum((RWFPS$RN2O - predict(WFPS_Nfit))^2)    
tss <- sum((RWFPS$RN2O - mean(RWFPS$RN2O))^2)  
r_squared <- 1 - rss/tss 


rss <- sum((RWFPS$RN2O - predict(WFPS_Nfit))^2)
nrss<-rss/994
rmse<-sqrt(nrss)  




###################################plot non-linear regression
ratioo<-read_excel("C:/Users/wang/Desktop/Denitrification/Data4/Ratio/ratio_v2.xlsx", sheet = 2)

RN2O<-ratioo[, c(3, 8:23)]

RN2O$Sand<-as.numeric(RN2O$Sand)
RN2O$Silt<-as.numeric(RN2O$Silt)
RN2O$Clay<-as.numeric(RN2O$Clay)
RN2O$BD<-as.numeric(RN2O$BD)
RN2O$pH<-as.numeric(RN2O$pH)
RN2O$CN_ratio<-as.numeric(RN2O$CN_ratio)
RN2O$real_AI<-as.numeric(RN2O$real_AI)
RN2O$Soil_T<-as.numeric(RN2O$Soil_T)
RN2O$WFPS<-as.numeric(RN2O$WFPS)
RN2O$RN2O<-as.numeric(RN2O$RN2O)


#####################################################################pH
RpH<-RN2O[,c(8, 17)] %>% na.omit()
RpH<-RpH[-c(154,165,910,912,1132),]

#quadratic model
pH_Nlm<-lm(RN2O ~ pH + I(pH^2), RpH) 

pH_Nfit<-nls(RN2O ~ a*pH^2 + b*pH +c, RpH,
             start = list(a=-0.01040, b=0.07257, c=0.38607)) 


RpH<-ggplot(RpH, aes(pH, RN2O)) +
  geom_point(size=0.6) +
  geom_line(aes(y=predict(pH_Nfit)), col="blue", lwd=0.8)+
  labs(x="pH", y=expression(R[N2O]))+
  annotate(geom = "text", x=8.8, y=1, size=2.9, col="green4",
           label=expression(y==-0.012*x^2+0.096*x+0.30))+
  annotate(geom = "text", x=9.5, y=0.9, size=3, col="green4", label=expression(R^2==0.10))+
  annotate(geom = "text", x=9.5, y=0.8, size=3, col="green4",label=expression(RMSE==0.28))+
  annotate(geom = "text", x=9.5, y=0.7, size=3, col="green4",label="p<0.01")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.2),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))




##########################################################################CN ratio
RCNRatio<-RN2O[,c(9, 17)] %>% na.omit()
RCNRatio<-RCNRatio[RCNRatio$CN_ratio<100,]
RCNRatio<-RCNRatio[-c(61,399,401),]


#log quadratic model
CNRatio_Nlm<-lm(log10(RN2O) ~ CN_ratio + I(CN_ratio^2), RCNRatio) 

CNRatio_Nfit<-nls(log10(RN2O) ~ a*CN_ratio^2 + b*CN_ratio +c, RCNRatio,
                  start = list(a=0.0001812, b=-0.0250868, c=-0.3310590))


RCN<-ggplot(RCNRatio, aes(CN_ratio, log10(RN2O))) +
  geom_point(size=0.6) +
  geom_line(aes(y=predict(CNRatio_Nfit)), col="blue", lwd=0.8)+
  labs(x="C/N ratio", y=expression(Log[10](R[N2O])))+
  annotate(geom = "text", x=65, y=-1.7, size=2.9, col="green4",
           label=expression(y==0.00018*x^2-0.025*x-0.33))+
  annotate(geom = "text", x=70, y=-2.1, size=3, col="green4", label=expression(R^2==0.10))+
  annotate(geom = "text", x=70, y=-2.5, size=3, col="green4",label=expression(RMSE==0.29))+
  annotate(geom = "text", x=70, y=-2.9, size=3, col="green4",label="p<0.001")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.2),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))




#########################################################################soil T
RSoilT<-RN2O[,c(15, 17)] %>% na.omit()
RSoilT<-RSoilT[RSoilT$Soil_T<50,]
RSoilT<-RSoilT[-c(83,85),]


########linear 
SoilT_Nlm<-lm(log10(RN2O) ~ Soil_T, RSoilT) 

SoilT_Nfit<-nls(log10(RN2O) ~ b*Soil_T +c, RSoilT,
                start = list(b=-0.02884, c=-0.23389)) 


soilT<-ggplot(RSoilT, aes(Soil_T, log10(RN2O))) +
  geom_point(size=0.6) +
  geom_line(aes(y=predict(SoilT_Nfit)), col="blue", lwd=0.8)+
  labs(x="Soil T (°C)", y=expression(Log[10](R[N2O])))+
  annotate(geom = "text", x=8.5, y=-1.9, size=2.9, col="green4",
           label=expression(y==-0.029*x-0.23))+
  annotate(geom = "text", x=5, y=-2.3, size=3, col="green4", label=expression(R^2==0.17))+
  annotate(geom = "text", x=5, y=-2.7, size=3, col="green4",label=expression(RMSE==0.57))+
  annotate(geom = "text", x=5, y=-3.1, size=3, col="green4",label="p<0.001")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.2),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))



##########################################################################WFPS
RWFPS<-RN2O[,c(16, 17)] %>% na.omit()
RWFPS<-RWFPS[-c(41,52,420,440),]


#exponential model
WFPS_Nlm<-lm(log(RN2O) ~ WFPS , RWFPS)

WFPS_Nfit<-nls(RN2O ~ a*exp(b*WFPS), RWFPS,
               start = list(a=0.4224974, b=-0.007042)) 


RWFPS<-ggplot(RWFPS, aes(WFPS, RN2O)) +
  geom_point(size=0.6) +
  geom_line(aes(y=predict(WFPS_Nfit)), col="blue", lwd=0.8)+
  labs(x="WFPS (%)", y=expression(R[N2O]))+
  annotate(geom = "text", x=150, y=0.9, size=2.9, col="green4",
           label=expression(y==0.42*e^(-0.0070*x)))+
  annotate(geom = "text", x=150, y=0.8, size=3, col="green4", label=expression(R^2==0.10))+
  annotate(geom = "text", x=150, y=0.7, size=3, col="green4",label=expression(RMSE==0.27))+
  annotate(geom = "text", x=150, y=0.6, size=3, col="green4",label="p<0.001")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.2),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))
































#####################################################################################
#####################################################################################
###########################################################################Machine learning


#####################################################################################
#####################################################################################
###########################################################################FN2O

#############################################################Random forest
##########################################Required data
K2<-read_excel("C:/Users/wang/Desktop/Denitrification/Machine_learning/K2.xlsx", sheet = 2)

K2$FN2O<-as.numeric(K2$FN2O)
K2<-K2[, -1]


data_dir <- "C:/Users/wang/Desktop/Denitrification/Machine_learning/Soil_data"

Soiltex<- read.csv(file.path(data_dir, "Soil_texture.tif"))
SoilpH<-read.csv(file.path(data_dir, "Soil_pH.tif"))
Soilwater<-read.csv(file.path(data_dir, "Soil_water.tif"))
Soilawater<-read.csv(file.path(data_dir, "Soil_awater.tif"))
SoilOC<-read.csv(file.path(data_dir, "Soil_OC.tif"))
SoilBD<-read.csv(file.path(data_dir, "Soil_BD.tif"))

Land_cover<- rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Land_cover/Land_cover.tif")
Climate<- rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Koeppen_climate_types/Beck_KG_V1_present_0p0083.tif")
Continent<-rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/World_Continents/Continent.tif")



###extract data that have same coordinates as meta RN2O
K2co <- terra::vect(K2, geom = c("Longitude", "Latitude"),crs = "EPSG:4326")

pH<- terra::extract(SoilpH, K2co, xy= TRUE)
texture<-terra::extract(Soiltex, K2co, xy=TRUE)
water<-terra::extract(Soilwater, K2co, xy=TRUE)
awater<-terra::extract(Soilawater, K2co, xy=TRUE)
OC<-terra::extract(SoilOC, K2co, xy=TRUE)
BD<-terra::extract(SoilBD, K2co, xy=TRUE)
Cli<-terra::extract(Climate, K2co, xy=TRUE)
LC<-terra::extract(Land_cover, K2co, xy=TRUE)
Con<-terra::extract(Continent, K2co, xy=TRUE)


All_vari<-cbind(K2, pH, texture, water, Cli, LC, Con, OC, BD)



##clearing data, which will be used for model training
Traindata<-All_vari[, c(12, 14, 15, 16, 18, 22, 26, 30, 34, 38, 42)]

names(Traindata)[names(Traindata) == "x"] <- "Longitude"
names(Traindata)[names(Traindata) == "y"] <- "Latitude"
names(Traindata)[names(Traindata) == "Beck_KG_V1_present_0p0083"] <- "Climate"

Traindata$Soil_texture<-as.factor(Traindata$Soil_texture)
Traindata$Land_cover<-as.factor(Traindata$Land_cover)
Traindata$Continent<-as.factor(Traindata$Continent)
Traindata$Climate<-as.factor(Traindata$Climate)


#remove incomplete observations
TD_ml <- Traindata[complete.cases(Traindata[, c("FN2O", "Soil_pH", "Soil_texture", "Soil_water",
                                                "Climate", "Land_cover", "Continent","Soil_OC", "Soil_BD")]),]


#random forest model without balanced sampling
rf <- ranger(FN2O ~ Soil_pH + Soil_texture + Soil_water + Climate + Land_cover +Continent,
             data=TD_ml, num.trees = 35,importance = "permutation") 

rf

importance(rf)


#rf predicted
TD_ml$K2_rf<- predict(rf, data = TD_ml)$predictions


#R2
rss <- sum((TD_ml$FN2O -TD_ml$K2_rf)^2)     
tss <- sum((TD_ml$FN2O - mean(TD_ml$FN2O))^2)  
r_squared <- 1 - rss/tss


#RMSE
sqrt(mean((TD_ml$FN2O - TD_ml$K2_rf)^2, na.rm = TRUE))  


#MAE
mean(abs(TD_ml$FN2O - TD_ml$K2_rf),na.rm = TRUE) 


#bias
mean(TD_ml$K2_rf - TD_ml$FN2O, na.rm = TRUE) 





##########balanced sampling
TD_ml$K2_bin <- cut(
  TD_ml$FN2O,
  breaks = quantile(
    TD_ml$FN2O,
    probs = seq(0, 1, length.out = 6),
    na.rm = TRUE
  ),
  include.lowest = TRUE)



bin_n <- table(TD_ml$K2_bin)
TD_ml$weight <- 1 / bin_n[TD_ml$K2_bin]
TD_ml$weight <- TD_ml$weight / mean(TD_ml$weight)


#random forest model with balanced sampling
set.seed(123)

rf_balanced <- ranger(
  FN2O ~ Soil_pH + Soil_texture + Soil_water + Climate + Land_cover +Continent,
  data = TD_ml,
  num.trees = 35,
  importance = "permutation",
  case.weights = TD_ml$weight)


#rf predicted
TD_ml$K2_rf_balan<- predict(rf_balanced, data = TD_ml)$predictions


#R2
rss <- sum((TD_ml$FN2O -TD_ml$K2_rf_balan)^2)     
tss <- sum((TD_ml$FN2O - mean(TD_ml$FN2O))^2)  
r_squared <- 1 - rss/tss


#RMSE
sqrt(mean((TD_ml$FN2O - TD_ml$K2_rf_balan)^2, na.rm = TRUE))  


#MAE
mean(abs(TD_ml$FN2O - TD_ml$K2_rf_balan),na.rm = TRUE)  


#bias
mean(TD_ml$K2_rf_balan - TD_ml$FN2O, na.rm = TRUE) 




#########predict global with random forest
template <- SoilpH


Soiltex  <- project(Soiltex, template)
Soilwater <- project(Soilwater, template)
gAI      <- project(gAI, template)
Land_cover<- project(Land_cover, template)
SoilOC<- project(SoilOC, template)
SoilBD<- project(SoilBD, template)


SoilpH2 <- resample(SoilpH, template, method = "bilinear")
Soilwater2 <- resample(Soilwater, template, method = "bilinear")
gAI2 <- resample(gAI, template, method = "bilinear")
Soiltex2 <- resample(Soiltex, template, method = "near")
Land_cover2<- resample(Land_cover, template, method = "near")


global_predictors <- c(Soiltex, SoilpH, Soilwater, gAI, Land_cover)

names(global_predictors) <- c("Soil_texture", "Soil_pH", "Soil_awater", 
                              "AI", "Land_cover", "Soil_OC", "Soil_BD")


RN2O_global <- terra::predict(global_predictors, rf, overwrite = TRUE,
                              filename = "C:/Users/wang/Desktop/Denitrification/Machine_learning/Global_RN2O.tif")






###################################################################XGBoost
##########################################Required data
K2<-read_excel("C:/Users/wang/Desktop/Denitrification/Machine_learning/K2.xlsx", sheet = 2)

K2$FN2O<-as.numeric(K2$FN2O)

K2<-K2[, -1]

Soiltex<-rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Soil_data/Soil_texture.tif")
SoilpH<-rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Soil_data/Soil_pH.tif")
Soilwater<-rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Soil_data/Soil_water.tif")
Soilawater<-rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Soil_data/Soil_awater.tif")
SoilOC<-rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Soil_data/Soil_OC.tif")
SoilBD<-rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Soil_data/Soil_BD.tif")

Land_cover<- rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Land_cover/Land_cover.tif")
Climate<- rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Koeppen_climate_types/Beck_KG_V1_present_0p0083.tif")
Continent<-rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/World_Continents/Continent.tif")



###extract data that have same coordinates as meta RN2O
K2co <- terra::vect(K2, geom = c("Longitude", "Latitude"),crs = "EPSG:4326")

pH<- terra::extract(SoilpH, K2co, xy= TRUE)
texture<-terra::extract(Soiltex, K2co, xy=TRUE)
water<-terra::extract(Soilwater, K2co, xy=TRUE)
awater<-terra::extract(Soilawater, K2co, xy=TRUE)
OC<-terra::extract(SoilOC, K2co, xy=TRUE)
BD<-terra::extract(SoilBD, K2co, xy=TRUE)
Cli<-terra::extract(Climate, K2co, xy=TRUE)
LC<-terra::extract(Land_cover, K2co, xy=TRUE)
Con<-terra::extract(Continent, K2co, xy=TRUE)


All_vari<-cbind(K2, pH, texture, water, Cli, LC, Con, OC, BD)



##clearing data, which will be used for model training
Traindata<-All_vari[, c(12, 14, 15, 16, 18, 22, 26, 30, 34, 38, 42)]   

names(Traindata)[names(Traindata) == "x"] <- "Longitude"
names(Traindata)[names(Traindata) == "y"] <- "Latitude"
names(Traindata)[names(Traindata) == "Beck_KG_V1_present_0p0083"] <- "Climate"


#remove incomplete observations
TD_ml <- Traindata[complete.cases(Traindata[, c("FN2O", "Soil_pH", "Soil_texture", "Soil_water",
                                                "Climate", "Land_cover", "Continent","Soil_OC", "Soil_BD")]),]



X <- TD_ml[, -c(1,3,4,10,11)]
y <- TD_ml$FN2O

xg <- xgboost(
  x = X,
  y = y,
  nthreads = 1,
  nrounds = 35,
  objective = "reg:squarederror"
)

TD_ml$K2_xg <- predict(xg, X)


#R2
rss <- sum((TD_ml$FN2O - TD_ml$K2_xg)^2)     
tss <- sum((TD_ml$FN2O - mean(TD_ml$FN2O))^2)  
r_squared <- 1 - rss/tss      


#RMSE
sqrt(mean((TD_ml$FN2O - TD_ml$K2_xg)^2, na.rm = TRUE))


#MAE
mean(abs(TD_ml$FN2O - TD_ml$K2_xg), na.rm = TRUE)


#Bias
mean(TD_ml$K2_xg - TD_ml$FN2O, na.rm = TRUE)






#############################################################compare models 
##MLR
MLR_model<-lm(FN2O ~ Soil_pH + Soil_texture + Soil_water + Climate + Land_cover +Continent, data=TD_ml)
TD_ml$K2_MLR <- predict(MLR_model, TD_ml)
valid <- complete.cases(TD_ml$FN2O, TD_ml$K2_MLR)

obs <- TD_ml$FN2O[valid]
pred <- TD_ml$K2_MLR[valid]


#R2
rss <- sum((obs - pred)^2)     
tss <- sum((obs - mean(obs))^2)  
r_squared <- 1 - rss/tss

#RMSE
sqrt(mean((obs - pred)^2, na.rm = TRUE))  

#MAE
mean(abs(obs - pred),na.rm = TRUE)  

#bias
mean(pred - obs,na.rm = TRUE) 




##GAM
gam_model<-mgcv::gam(FN2O ~ s(Soil_pH) + Soil_texture + s(Soil_water) + Climate
                     + Land_cover + Continent, data=TD_ml)


TD_ml$K2_gam <- predict(gam_model, TD_ml)
valid <- complete.cases(TD_ml$FN2O, TD_ml$K2_gam)

obs <- TD_ml$FN2O[valid]
pred <- TD_ml$K2_gam[valid]


#R2
rss <- sum((obs - pred)^2)     
tss <- sum((obs - mean(obs))^2)  
r_squared <- 1 - rss/tss

#RMSE
sqrt(mean((obs - pred)^2, na.rm = TRUE))  

#MAE
mean(abs(obs - pred),na.rm = TRUE)  

#bias
mean(pred - obs,na.rm = TRUE) 





##########balanced sampling
TD_ml$K2_bin <- cut(
  TD_ml$FN2O,
  breaks = quantile(
    TD_ml$FN2O,
    probs = seq(0, 1, length.out = 6),
    na.rm = TRUE
  ),
  include.lowest = TRUE)



bin_n <- table(TD_ml$K2_bin)
TD_ml$weight <- 1 / bin_n[TD_ml$K2_bin]
TD_ml$weight <- TD_ml$weight / mean(TD_ml$weight)


freq <- table(TD_ml$Continent)
TD_ml$weight <- 1 / freq[TD_ml$Continent]
TD_ml$weight <- TD_ml$weight / mean(TD_ml$weight)



#######################################XGboost with balanced sampling
X <- TD_ml[, -c(1,3,4,10,11,12,13)]
y <- TD_ml$FN2O


xg_balance <- xgboost(
  x = X,
  y = y,
  weight = as.numeric(TD_ml$weight),
  nrounds = 500,
  objective = "reg:squarederror"
)

TD_ml$K2_xg <- predict(xg_balance, X)


#R2
rss <- sum((TD_ml$FN2O - TD_ml$K2_xg)^2)     
tss <- sum((TD_ml$FN2O - mean(TD_ml$FN2O))^2)  
r_squared <- 1 - rss/tss      


#RMSE
sqrt(mean((TD_ml$FN2O - TD_ml$K2_xg)^2, na.rm = TRUE))

#MAE
mean(abs(TD_ml$FN2O - TD_ml$K2_xg), na.rm = TRUE)

#Bias
mean(TD_ml$K2_xg - TD_ml$FN2O, na.rm = TRUE)



#predict with XGBoost
template <- SoilpH

Soiltex  <- project(Soiltex, template)
Soilwater <- project(Soilwater, template)
Climate      <- project(Climate, template)
Land_cover<- project(Land_cover, template)
Continent<-project(Continent, template)


Soilwater2 <- resample(Soilwater, template, method = "bilinear")
Climate <- resample(Climate, template, method = "near")
Soiltex2 <- resample(Soiltex, template, method = "near")
Land_cover2<- resample(Land_cover, template, method = "near")
Continent2<-resample(Continent, template, method="near")


global_predictors <- c(SoilpH, Soiltex, Soilwater, Climate, Land_cover, Continent)


names(global_predictors) <- c("Soil_pH", "Soil_texture", "Soil_water", "Climate",
                              
                              "Land_cover", "Continent")


N2O_global_xg<-terra::predict(global_predictors, xg, overwrite = TRUE,
                              filename = "C:/Users/wang/Desktop/Denitrification/Machine_learning/Global_K2.tif")






#test the difference between num.trees set as different numbers
trees <- c(10, 25, 35, 50, 100, 250, 500, 1000)


models <- lapply(trees, function(nt) {
  ranger(RN2O ~ Soil_pH + Soil_texture + Soil_awater + AI +
           Land_cover + Soil_OC + Soil_BD,
         data = TD_ml,
         num.trees = nt,
         importance = "permutation")})


pred <- sapply(models, function(m) {
  predict(m, data = TD_ml)$predictions})


colnames(pred) <- paste0("trees_", trees)
head(pred)


cor(pred[, "trees_35"], pred[, "trees_1000"]) 

mean(abs(pred[, "trees_35"] - pred[, "trees_1000"])) 





###################Spatial CV R2
coordinates <- st_as_sf(TD_ml, coords = c("Longitude", "Latitude"), crs = 4326)


sb<-cv_spatial(x=coordinates, column = "FN2O", size = 50000, k=5, 
               selection = "random", iteration = 100) 


##random forest
pred_spatial_rf <- rep(NA, nrow(TD_ml))

for (i in 1:5) { 
  train <- TD_ml[sb$folds_ids != i, ]
  test  <- TD_ml[sb$folds_ids == i, ]

  rf <- ranger(
    FN2O ~ Soil_pH + Soil_texture + Soil_water + Climate + Land_cover +Continent,
    data = train,
    num.trees = 600,
    importance = "permutation"
  )
  
  pred_spatial_rf[sb$folds_ids == i] <-
    predict(rf, data = test)$predictions}


obs <- TD_ml$FN2O
pred <- pred_spatial_rf


#R2
rss <- sum((obs - pred)^2)     
tss <- sum((obs - mean(obs))^2)  
r_squared <- 1 - rss/tss

#RMSE
sqrt(mean((obs - pred)^2, na.rm = TRUE))

#MAE
mean(abs(obs - pred), na.rm = TRUE)

#Bias
mean(pred - obs, na.rm = TRUE)

cor(obs, pred)
plot(obs, pred)



##XGBoost
pred_spatial_xg <- rep(NA, nrow(TD_ml))

for (i in 1:5) {
  X = TD_ml[sb$folds_ids != i, -c(1,3,4, 10,11)]
  y = TD_ml[sb$folds_ids != i, ]$FN2O
  X2= TD_ml[sb$folds_ids == i, -c(1,3,4, 10,11)]
  
  xg <- xgboost(
    x = X,
    y = y,
    nthreads = 1,
    nrounds = 35,
    objective = "reg:squarederror"
  )
  
  pred_spatial_xg[sb$folds_ids == i] <-
    predict(xg, X2)}


obs <- TD_ml$FN2O
pred <- pred_spatial_xg


#R2
rss <- sum((obs - pred)^2)     
tss <- sum((obs - mean(obs))^2)  
r_squared <- 1 - rss/tss

#RMSE
sqrt(mean((obs - pred)^2, na.rm = TRUE))

#MAE
mean(abs(obs - pred), na.rm = TRUE)

#Bias
mean(pred - obs, na.rm = TRUE)





#MLR
pred_spatial_MLR <- rep(NA, nrow(TD_ml))

for (i in 1:5) { 
  train <- TD_ml[sb$folds_ids != i, ]
  test  <- TD_ml[sb$folds_ids == i, ]
  
  MLR<-lm(FN2O ~ Soil_pH + Soil_texture + Soil_water + Climate + 
            Land_cover +Continent, data=train)
  
  pred_spatial_MLR[sb$folds_ids == i] <-
    predict(MLR, data = test)}


obs <- TD_ml$FN2O
pred <- pred_spatial_MLR


#R2
rss <- sum((obs - pred)^2)     
tss <- sum((obs - mean(obs))^2)  
r_squared <- 1 - rss/tss

#RMSE
sqrt(mean((obs - pred)^2, na.rm = TRUE))

#MAE
mean(abs(obs - pred), na.rm = TRUE)

#Bias
mean(pred - obs, na.rm = TRUE)

cor(obs, pred)
plot(obs, pred)






















#####################################################################################
#####################################################################################
###########################################################################RN2O

###################################################################Random forest

##########################################Required data
ratioo<-read_excel("C:/Users/wang/Desktop/Denitrification/Machine_learning/ratio.xlsx", sheet = 2)

ratio<-ratioo[, 3:10]


Soiltex<-rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Soil_data/Soil_texture.tif")
SoilpH<-rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Soil_data/Soil_pH.tif")
Soilwater<-rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Soil_data/Soil_water.tif")
Soilawater<-rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Soil_data/Soil_awater.tif")
SoilOC<-rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Soil_data/Soil_OC.tif")
SoilBD<-rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Soil_data/Soil_BD.tif")
gAI<- rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Global_AI_ET0_v3_annual/ai_v3_yr.tif")
Land_cover<- rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Land_cover/Land_cover.tif")


###extract data that have same coordinates as meta RN2O
ratioco <- vect(ratio, geom = c("Longitude", "Latitude"),crs = "EPSG:4326")


pH<-extract(SoilpH, ratioco, xy=TRUE)
texture<-extract(Soiltex, ratioco, xy=TRUE)
water<-extract(Soilwater, ratioco, xy=TRUE)
awater<-extract(Soilawater, ratioco, xy=TRUE) 
OC<-extract(SoilOC, ratioco, xy=TRUE)
BD<-extract(SoilBD, ratioco, xy=TRUE)
AI<-extract(gAI, ratioco, xy=TRUE)
LC<-extract(Land_cover, ratioco, xy=TRUE)


All_vari<-cbind(ratio, pH, texture, awater, AI, LC, OC, BD)



##clearing data, which will be used for model training
Traindata<-All_vari[, c(7, 10, 11, 12, 14, 18, 22, 26, 30, 34)]

names(Traindata)[names(Traindata) == "x"] <- "Longitude"
names(Traindata)[names(Traindata) == "y"] <- "Latitude"
names(Traindata)[names(Traindata) == "awi_pm_sr_yr"] <- "AI"

Traindata$Soil_texture<-as.factor(Traindata$Soil_texture)
Traindata$Land_cover<-as.factor(Traindata$Land_cover)


#remove incomplete observations
TD_ml <- Traindata[complete.cases(Traindata[, c("RN2O", "Soil_pH", "Soil_texture", "Soil_awater",
                                                "AI", "Land_cover", "Soil_OC", "Soil_BD")]),]

##########balanced sampling
TD_ml$RN2O_bin <- cut(
  TD_ml$RN2O,
  breaks = quantile(
    TD_ml$RN2O,
    probs = seq(0, 1, length.out = 6),
    na.rm = TRUE
  ),
  include.lowest = TRUE)



bin_n <- table(TD_ml$RN2O_bin)
TD_ml$weight <- 1 / bin_n[TD_ml$RN2O_bin]
TD_ml$weight <- TD_ml$weight / mean(TD_ml$weight)



#random forest model with balanced sampling
set.seed(123)

rf_balanced <- ranger(
  RN2O ~ Soil_pH + Soil_texture + Soil_awater + AI + Land_cover,
  data = TD_ml,
  num.trees = 35,
  importance = "permutation",
  case.weights = TD_ml$weight)


#rf predicted
TD_ml$N2O_rf_balan<- predict(rf_balanced, data = TD_ml)$predictions


#R2
rss <- sum((TD_ml$RN2O -TD_ml$N2O_rf_balan)^2)     
tss <- sum((TD_ml$RN2O - mean(TD_ml$RN2O))^2)  
r_squared <- 1 - rss/tss

#RMSE
sqrt(mean((TD_ml$RN2O - TD_ml$N2O_rf_balan)^2, na.rm = TRUE))  

#MAE
mean(abs(TD_ml$RN2O - TD_ml$N2O_rf_balan),na.rm = TRUE)  

#bias
mean(TD_ml$N2O_rf_balan - TD_ml$RN2O, na.rm = TRUE) 




#random forest without balanced sampling
rf <- ranger(RN2O ~ Soil_pH + Soil_texture + Soil_awater + AI + Land_cover,
             data=TD_ml, num.trees = 35,importance = "permutation") 

rf

importance(rf)


#rf predicted
TD_ml$N2O_rf<- predict(rf, data = TD_ml)$predictions

#R2
rss <- sum((TD_ml$RN2O -TD_ml$N2O_rf)^2)     
tss <- sum((TD_ml$RN2O - mean(TD_ml$RN2O))^2)  
r_squared <- 1 - rss/tss

#RMSE
sqrt(mean((TD_ml$RN2O - TD_ml$N2O_rf)^2, na.rm = TRUE))  

#MAE
mean(abs(TD_ml$RN2O - TD_ml$N2O_rf),na.rm = TRUE)  

#bias
mean(TD_ml$N2O_rf - TD_ml$RN2O, na.rm = TRUE) 




#########predict global with random forest: soil pH as template (less pixels)
template <- SoilpH


Soiltex  <- project(Soiltex, template)
Soilwater <- project(Soilwater, template)
Soilawater <-project(Soilawater, template)
gAI      <- project(gAI, template)
Land_cover<- project(Land_cover, template)


global_predictors <- c(Soiltex, SoilpH, Soilawater, gAI, Land_cover)


names(global_predictors) <- c("Soil_texture", "Soil_pH", "Soil_awater", 
                              "AI", "Land_cover")


RN2O_global <- terra::predict(global_predictors, rf_balanced, overwrite = TRUE, progress=1, #progresss to show progress.
                              filename = "C:/Users/wang/Desktop/Denitrification/Machine_learning/Global_RN2O_SB.tif")



#########predict global with random forest: land cover as template (more pixels)
template <- Land_cover


Soiltex  <- project(Soiltex, template, method= "near")
SoilpH <- project(SoilpH, template, method="near")
Soilwater <- project(Soilwater, template, method="near")
Soilawater <- project(Soilawater, template, method="near")
gAI      <- project(gAI, template, method="near")


global_predictors <- c(Soiltex, SoilpH, Soilawater, gAI, Land_cover)


names(global_predictors) <- c("Soil_texture", "Soil_pH", "Soil_awater", "AI", "Land_cover")



RN2O_global <- terra::predict(global_predictors, rf, overwrite = TRUE, progress=1,
                              filename = "C:/Users/wang/Desktop/Denitrification/Machine_learning/Global_RN2O_lc.tif")






#####################################################################XGBoost
##########################################Required data
ratioo<-read_excel("C:/Users/wang/Desktop/Denitrification/Machine_learning/ratio.xlsx", sheet = 2)

ratio<-ratioo[, 3:10]


soildata_dir <- "C:/Users/wang/Desktop/Denitrification/Machine_learning/Soil_data"


Soiltex<-rast(file.path(soildata_dir,"Soil_texture.tif"))
SoilpH<-rast(file.path(soildata_dir, "Soil_pH.tif"))
Soilwater<-rast(file.path(soildata_dir, "Soil_water.tif"))
Soilawater<-rast(file.path(soildata_dir, "Soil_awater.tif"))
SoilOC<-rast(file.path(soildata_dir, "Soil_OC.tif"))
SoilBD<-rast(file.path(soildata_dir, "Soil_BD.tif"))
gAI<- rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Global_AI_ET0_v3_annual/ai_v3_yr.tif")
Land_cover<- rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Land_cover/Land_cover.tif")



###extract data that have same coordinates as meta RN2O
ratioco <- vect(ratio, geom = c("Longitude", "Latitude"),crs = "EPSG:4326")


pH<-extract(SoilpH, ratioco, xy=TRUE)
texture<-extract(Soiltex, ratioco, xy=TRUE)
water<-extract(Soilwater, ratioco, xy=TRUE)
awater<-extract(Soilawater, ratioco, xy=TRUE) 
OC<-extract(SoilOC, ratioco, xy=TRUE)
BD<-extract(SoilBD, ratioco, xy=TRUE)
AI<-extract(gAI, ratioco, xy=TRUE)
LC<-extract(Land_cover, ratioco, xy=TRUE)


All_vari<-cbind(ratio, pH, texture, awater, AI, LC, OC, BD)



##clearing data, which will be used for model training
Traindata<-All_vari[, c(7, 10, 11, 12, 14, 18, 22, 26, 30, 34)]   

names(Traindata)[names(Traindata) == "x"] <- "Longitude"
names(Traindata)[names(Traindata) == "y"] <- "Latitude"
names(Traindata)[names(Traindata) == "awi_pm_sr_yr"] <- "AI"


#remove incomplete observations
TD_ml <- Traindata[complete.cases(Traindata[, c("RN2O", "Soil_pH", "Soil_texture", "Soil_awater",
                                                "AI", "Land_cover", "Soil_OC", "Soil_BD")]),]


X <- TD_ml[, -c(1,3,4)]
y <- TD_ml$RN2O

xg <- xgboost(
  x = X,
  y = y,
  nthreads = 1,
  nrounds = 35,
  objective = "reg:squarederror"
)

TD_ml$RN2O_xg <- predict(xg, X)


#R2
rss <- sum((TD_ml$RN2O - TD_ml$RN2O_xg)^2)     
tss <- sum((TD_ml$RN2O - mean(TD_ml$RN2O))^2)  
r_squared <- 1 - rss/tss #0.46        

#RMSE
sqrt(mean((TD_ml$RN2O - TD_ml$RN2O_xg)^2, na.rm = TRUE))

#MAE
mean(abs(TD_ml$RN2O - TD_ml$RN2O_xg), na.rm = TRUE)

#Bias
mean(TD_ml$RN2O_xg - TD_ml$RN2O, na.rm = TRUE)




#############################################################compare models 
##MLR
MLR_model<-lm(RN2O ~ Soil_pH + Soil_texture + Soil_awater + AI + Land_cover, data=TD_ml)
TD_ml$RN2O_MLR <- predict(MLR_model, TD_ml)

valid <- complete.cases(TD_ml$RN2O, TD_ml$RN2O_MLR)

obs <- TD_ml$RN2O[valid]
pred <- TD_ml$RN2O_MLR[valid]


#R2
rss <- sum((obs - pred)^2)     
tss <- sum((obs - mean(obs))^2)  
r_squared <- 1 - rss/tss

#RMSE
sqrt(mean((obs - pred)^2, na.rm = TRUE))  

#MAE
mean(abs(obs - pred),na.rm = TRUE)  

#bias
mean(pred - obs,na.rm = TRUE) 



##GAM
gam_model<-mgcv::gam(RN2O ~ s(Soil_pH) + Soil_texture + Soil_awater + s(AI) + Land_cover, data=TD_ml)


TD_ml$RN2O_gam <- predict(gam_model, TD_ml)
valid <- complete.cases(TD_ml$RN2O, TD_ml$RN2O_gam)

obs <- TD_ml$RN2O[valid]
pred <- TD_ml$RN2O_gam[valid]


#R2
rss <- sum((obs - pred)^2)     
tss <- sum((obs - mean(obs))^2)  
r_squared <- 1 - rss/tss

#RMSE
sqrt(mean((obs - pred)^2, na.rm = TRUE))  

#MAE
mean(abs(obs - pred),na.rm = TRUE)  

#bias
mean(pred - obs,na.rm = TRUE) 




##########balanced sampling
TD_ml$RN2O_bin <- cut(
  TD_ml$RN2O,
  breaks = quantile(
    TD_ml$RN2O,
    probs = seq(0, 1, length.out = 6),
    na.rm = TRUE
  ),
  include.lowest = TRUE)


bin_n <- table(TD_ml$RN2O_bin)
TD_ml$weight <- 1 / bin_n[TD_ml$RN2O_bin]
TD_ml$weight <- TD_ml$weight / mean(TD_ml$weight)



#######################################XGboost with balanced sampling
X <- TD_ml[, -c(1,3,4,9,10,11,12)] 
y <- TD_ml$RN2O

xgb_model <- xgboost(
  x = X,
  y = y,
  weight = as.numeric(TD_ml$weight),
  nrounds = 500,
  objective = "reg:squarederror"
)

TD_ml$RN2O_xg_balan <- predict(xgb_model, X)


#R2
rss <- sum((TD_ml$RN2O - TD_ml$RN2O_xg_balan)^2)     
tss <- sum((TD_ml$RN2O - mean(TD_ml$RN2O))^2)  
r_squared <- 1 - rss/tss      

#RMSE
sqrt(mean((TD_ml$RN2O - TD_ml$RN2O_xg_balan)^2, na.rm = TRUE))

#MAE
mean(abs(TD_ml$RN2O - TD_ml$RN2O_xg_balan), na.rm = TRUE)

#Bias
mean(TD_ml$RN2O_xg_balan - TD_ml$RN2O, na.rm = TRUE)



#predict with XGBoost
template <- SoilpH

Soiltex  <- project(Soiltex, template)
Soilawater <- project(Soilawater, template)
gAI      <- project(gAI, template)
Land_cover<- project(Land_cover, template)
SoilOC<- project(SoilOC, template)
SoilBD<- project(SoilBD, template)


SoilpH2 <- resample(SoilpH, template, method = "bilinear")
Soilwater2 <- resample(Soilwater, template, method = "bilinear")
gAI2 <- resample(gAI, template, method = "bilinear")
Soiltex2 <- resample(Soiltex, template, method = "near")
Land_cover2<- resample(Land_cover, template, method = "near")


global_predictors <- c(SoilpH, Soiltex, Soilawater, gAI, Land_cover, SoilOC, SoilBD)


names(global_predictors) <- c("Soil_pH", "Soil_texture", "Soil_awater", "AI",
                              
                              "Land_cover", "Soil_OC", "Soil_BD")

N2O_global_xg<-terra::predict(global_predictors, xg, overwrite = TRUE, progress =1,
                              filename = "C:/Users/wang/Desktop/Denitrification/Machine_learning/Global_RN2O_xg.tif")




#test the difference between num.trees set as different numbers
trees <- c(10, 25, 35, 50, 100, 250, 500, 1000)


models <- lapply(trees, function(nt) {
  ranger(RN2O ~ Soil_pH + Soil_texture + Soil_awater + AI +
           Land_cover + Soil_OC + Soil_BD,
         data = TD_ml,
         num.trees = nt,
         importance = "permutation")})


pred <- sapply(models, function(m) {
  predict(m, data = TD_ml)$predictions})


colnames(pred) <- paste0("trees_", trees)
head(pred)


cor(pred[, "trees_35"], pred[, "trees_1000"]) 
mean(abs(pred[, "trees_35"] - pred[, "trees_1000"])) 




###################Spatial CV R2
coordinates <- st_as_sf(TD_ml, coords = c("Longitude", "Latitude"), crs = 4326)


sb<-cv_spatial(x=coordinates, column = "RN2O", size = 50000, k=5, 
               selection = "random", iteration = 100) 


##random forest
pred_spatial_rf <- rep(NA, nrow(TD_ml))

for (i in 1:5) { 
  train <- TD_ml[sb$folds_ids != i, ]
  test  <- TD_ml[sb$folds_ids == i, ]
  
  rf <- ranger(
    RN2O ~ Soil_pH + Soil_texture + Soil_awater +
      AI + Land_cover + Soil_OC + Soil_BD,
    data = train,
    num.trees = 600,
    importance = "permutation")
  
  pred_spatial_rf[sb$folds_ids == i] <-
    predict(rf, data = test)$predictions}


obs <- TD_ml$RN2O
pred <- pred_spatial_rf


#R2
rss <- sum((obs - pred)^2)     
tss <- sum((obs - mean(obs))^2)  
r_squared <- 1 - rss/tss

#RMSE
sqrt(mean((obs - pred)^2, na.rm = TRUE))

#MAE
mean(abs(obs - pred), na.rm = TRUE)

#Bias
mean(pred - obs, na.rm = TRUE)

cor(obs, pred)
plot(obs, pred)




##XGBoost
pred_spatial_xg <- rep(NA, nrow(TD_ml))

for (i in 1:5) {
  
  X = TD_ml[sb$folds_ids != i, -c(1,3,4, 11)]
  y = TD_ml[sb$folds_ids != i, ]$RN2O
  X2= TD_ml[sb$folds_ids == i, -c(1,3,4, 11)]
  
  xg <- xgboost(
    x = X,
    y = y,
    nthreads = 1,
    nrounds = 35,
    objective = "reg:squarederror")
  
  pred_spatial_xg[sb$folds_ids == i] <-
    predict(xg, X2)}

obs <- TD_ml$RN2O
pred <- pred_spatial_xg


#R2
rss <- sum((obs - pred)^2)     
tss <- sum((obs - mean(obs))^2)  
r_squared <- 1 - rss/tss

#RMSE
sqrt(mean((obs - pred)^2, na.rm = TRUE))

#MAE
mean(abs(obs - pred), na.rm = TRUE)

#Bias
mean(pred - obs, na.rm = TRUE)




#MLR
pred_spatial_MLR <- rep(NA, nrow(TD_ml))

for (i in 1:5) { 
  train <- TD_ml[sb$folds_ids != i, ]
  test  <- TD_ml[sb$folds_ids == i, ]
  
  MLR<-lm(RN2O ~ Soil_pH + Soil_texture + Soil_awater + AI + Land_cover, data=train)
  
  pred_spatial_MLR[sb$folds_ids == i] <-
    predict(MLR, data = test)}


obs <- TD_ml$RN2O
pred <- pred_spatial_MLR


#R2
rss <- sum((obs - pred)^2)     
tss <- sum((obs - mean(obs))^2)  
r_squared <- 1 - rss/tss

#RMSE
sqrt(mean((obs - pred)^2, na.rm = TRUE))

#MAE
mean(abs(obs - pred), na.rm = TRUE)

#Bias
mean(pred - obs, na.rm = TRUE)

cor(obs, pred)
plot(obs, pred)























###################################################################################
###################################################################################
#############################################################################Global N2O
#########################################required data
Nit<-read_excel("C:/Users/wang/Desktop/Denitrification/Literature/Global_nit_denit/Li_2019_nit/Li_2019_nit.xlsx", sheet=1)
Nit<-Nit[, c(2,3,18)]
Nit$Nitrification<-as.numeric(Nit$Nitrification)
Nit$Latitude<-as.numeric(Nit$Latitude)
Nit$Longitude<-as.numeric(Nit$Longitude)
Nit$Latitude<-round(Nit$Latitude, digits = 1)
Nit$Longitude<-round(Nit$Longitude, digits = 1)


Denit<-read_excel("C:/Users/wang/Desktop/Denitrification/Literature/Global_nit_denit/Li_2021_denit/Denit.xlsx", sheet =1)
Denit<-Denit[, c(2,3,19)]
Denit$Denitrification<-as.numeric(Denit$Denitrification)
Denit$Latitude<-as.numeric(Denit$Latitude)
Denit$Longitude<-as.numeric(Denit$Longitude)
Denit$Latitude<-round(Denit$Latitude, digits = 1)
Denit$Longitude<-round(Denit$Longitude, digits = 1)


FN2O<-rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Final_global_FN2O.tif")
RN2O<-rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Final_global_RN2O.tif")
Continent<-rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/World_Continents/Continent.tif")
Land_cover<- rast("C:/Users/wang/Desktop/Denitrification/Machine_learning/Land_cover/Land_cover.tif")



##########################################################extract nit and denit coordinate
nit_denit<-merge(Nit, Denit, by=c("Longitude","Latitude")) %>% na.omit()

coordin<- vect(nit_denit, geom = c("Longitude", "Latitude"),crs = "EPSG:4326")

con<-terra::extract(Continent, coordin, xy=TRUE)

K2<-terra::extract(FN2O, coordin, xy=TRUE)

ratio<-terra::extract(RN2O, coordin, xy=TRUE)


alldata<-cbind(nit_denit, con, K2, ratio)
alldata<-alldata[, c(3,4,6, 10, 14)]

names(alldata)[names(alldata)=="lyr1"] <-"FN2O"
names(alldata)[names(alldata)=="Final_global_RN2O"] <-"RN2O"




############################Separately calcualte global N2Onit and N2Odenit
######nitrificaiton N2O
coor_nit<- vect(Nit, geom = c("Longitude", "Latitude"),crs = "EPSG:4326")


con_nit<-terra::extract(Continent, coor_nit, xy=TRUE)
K2_nit<-terra::extract(FN2O, coor_nit, xy=TRUE)
LC<-terra::extract(Land_cover, coor_nit, xy=TRUE)


global_N2O_nit<-cbind(Nit, con_nit, K2_nit, LC)
global_N2O_nit<-global_N2O_nit[, c(1,2,3,5,9,13)] %>% na.omit()
names(global_N2O_nit)[names(global_N2O_nit)== "lyr1"]<- "FN2O"
global_N2O_nit$N2O<-global_N2O_nit$Nitrification*abs(global_N2O_nit$FN2O) #mg/kg/day
global_N2O_nit$Continent<-as.character(global_N2O_nit$Continent)


global_N2O_nit$Continent[global_N2O_nit$Continent == 1]<-"Africa" 
global_N2O_nit$Continent[global_N2O_nit$Continent == 2]<-"Asia"
global_N2O_nit$Continent[global_N2O_nit$Continent == 3]<-"Australia"
global_N2O_nit$Continent[global_N2O_nit$Continent == 4]<-"Oceania"
global_N2O_nit$Continent[global_N2O_nit$Continent == 5]<-"South America"
global_N2O_nit$Continent[global_N2O_nit$Continent == 6]<-"Antarctica"
global_N2O_nit$Continent[global_N2O_nit$Continent == 7]<-"Europe"
global_N2O_nit$Continent[global_N2O_nit$Continent == 8]<-"North America"

global_N2O_nit$Land_cover<-as.character(global_N2O_nit$Land_cover)

global_N2O_nit$Land_cover[global_N2O_nit$Land_cover %in% c(1,2,3,4,5,6,7)]<-"Forest"
global_N2O_nit$Land_cover[global_N2O_nit$Land_cover %in% c(8,9)]<- "Grassland"
global_N2O_nit$Land_cover[global_N2O_nit$Land_cover %in% c(11,12,13)]<-"Cropland"
global_N2O_nit$Land_cover[global_N2O_nit$Land_cover %in% c(16,17)]<-"Fallow"


ggplot()+
  geom_boxplot(aes(x=global_N2O_nit$Continent, y=global_N2O_nit$N2O))+
  scale_x_discrete(labels= function(x) str_wrap(x, width = 8))+
  labs(x=NULL, y=expression(N[2]*O~from~nitrification))+
  theme_bw()





######denitrificaiton N2O with NO clculated by 5.76% * denitrification
coor_denit<- vect(Denit, geom = c("Longitude", "Latitude"),crs = "EPSG:4326")

con_denit<-terra::extract(Continent, coor_denit, xy=TRUE)
Ratio_denit<-terra::extract(RN2O, coor_denit, xy=TRUE)
LC_denit<-terra::extract(Land_cover, coor_denit, xy=TRUE)


global_N2O_denit<-cbind(Denit, con_denit, Ratio_denit, LC_denit)
global_N2O_denit<-global_N2O_denit[, c(1,2,3,5,9,13)] %>% na.omit()
names(global_N2O_denit)[names(global_N2O_denit)== "Final_global_RN2O"]<- "RN2O"
global_N2O_denit$netdenit<-global_N2O_denit$Denitrification- global_N2O_denit$Denitrification*0.0576
global_N2O_denit$rawN2O<-global_N2O_denit$Denitrification*global_N2O_denit$RN2O/1000 #mg/kg/day
global_N2O_denit$N2O<-global_N2O_denit$netdenit *global_N2O_denit$RN2O/1000 #mg/kg/day

global_N2O_denit$Continent<-as.character(global_N2O_denit$Continent)

global_N2O_denit$Continent[global_N2O_denit$Continent == 1]<-"Africa" 
global_N2O_denit$Continent[global_N2O_denit$Continent == 2]<-"Asia"
global_N2O_denit$Continent[global_N2O_denit$Continent == 3]<-"Australia"
global_N2O_denit$Continent[global_N2O_denit$Continent == 4]<-"Oceania"
global_N2O_denit$Continent[global_N2O_denit$Continent == 5]<-"South America"
global_N2O_denit$Continent[global_N2O_denit$Continent == 6]<-"Antarctica"
global_N2O_denit$Continent[global_N2O_denit$Continent == 7]<-"Europe"
global_N2O_denit$Continent[global_N2O_denit$Continent == 8]<-"North America"

global_N2O_denit$Land_cover<-as.character(global_N2O_denit$Land_cover)

global_N2O_denit$Land_cover[global_N2O_denit$Land_cover %in% c(1,2,3,4,5,6,7)]<-"Forest"
global_N2O_denit$Land_cover[global_N2O_denit$Land_cover %in% c(8,9)]<-"Grassland"
global_N2O_denit$Land_cover[global_N2O_denit$Land_cover %in% c(11,12,13)]<-"Cropland"
global_N2O_denit$Land_cover[global_N2O_denit$Land_cover %in% c(16,17)]<- "Fallow"


ggplot(global_N2O_denit, aes(x=Continent, y=N2O))+
  geom_boxplot()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  labs(x=NULL, y=expression(N[2]*O~"from"~denitrification))+
  theme_bw()




#################denitrification N2O with NO calculted as NO/N2O
coor_denit<- vect(Denit, geom = c("Longitude", "Latitude"),crs = "EPSG:4326")

con_denit<-terra::extract(Continent, coor_denit, xy=TRUE)
Ratio_denit<-terra::extract(RN2O, coor_denit, xy=TRUE)
LC_denit<-terra::extract(Land_cover, coor_denit, xy=TRUE)


options(scipen = 999)


global_N2O_denit_m2<-cbind(Denit, con_denit, Ratio_denit, LC_denit)
global_N2O_denit_m2<-global_N2O_denit_m2[, c(1,2,3,5,9,13)] %>% na.omit()
names(global_N2O_denit_m2)[names(global_N2O_denit_m2)== "Final_global_RN2O"]<- "RN2O"
global_N2O_denit_m2$rawN2O<-global_N2O_denit_m2$Denitrification*global_N2O_denit_m2$RN2O/1000 #mg/kg/day
global_N2O_denit_m2$rate<-1/global_N2O_denit_m2$RN2O +0.79
global_N2O_denit_m2$N2O<- global_N2O_denit_m2$Denitrification/global_N2O_denit_m2$rate/1000 #mg/kg/day


global_N2O_denit_m2$diff<-global_N2O_denit_m2$rawN2O- global_N2O_denit_m2$N2O
global_N2O_denit_m2$Pdiff<- global_N2O_denit_m2$diff/global_N2O_denit_m2$N2O*100

global_N2O_denit_m2$Continent<-as.character(global_N2O_denit_m2$Continent)

global_N2O_denit_m2$Continent[global_N2O_denit_m2$Continent == 1]<-"Africa" 
global_N2O_denit_m2$Continent[global_N2O_denit_m2$Continent == 2]<-"Asia"
global_N2O_denit_m2$Continent[global_N2O_denit_m2$Continent == 3]<-"Australia"
global_N2O_denit_m2$Continent[global_N2O_denit_m2$Continent == 4]<-"Oceania"
global_N2O_denit_m2$Continent[global_N2O_denit_m2$Continent == 5]<-"South America"
global_N2O_denit_m2$Continent[global_N2O_denit_m2$Continent == 6]<-"Antarctica"
global_N2O_denit_m2$Continent[global_N2O_denit_m2$Continent == 7]<-"Europe"
global_N2O_denit_m2$Continent[global_N2O_denit_m2$Continent == 8]<-"North America"

global_N2O_denit_m2$Land_cover<-as.character(global_N2O_denit_m2$Land_cover)

global_N2O_denit_m2$Land_cover[global_N2O_denit_m2$Land_cover %in% c(1,2,3,4,5,6,7)]<-"Forest"
global_N2O_denit_m2$Land_cover[global_N2O_denit_m2$Land_cover %in% c(8,9)]<-"Grassland"
global_N2O_denit_m2$Land_cover[global_N2O_denit_m2$Land_cover %in% c(11,12,13)]<-"Cropland"
global_N2O_denit_m2$Land_cover[global_N2O_denit_m2$Land_cover %in% c(16,17)]<- "Fallow"


ggplot(global_N2O_denit_m2, aes(x=Continent, y=N2O))+
  geom_boxplot()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  labs(x=NULL, y=expression(N[2]*O~"from"~denitrification))+
  theme_bw()





##########################calculate
##nitrification
global_N2O_nit<- global_N2O_nit %>% group_by(Continent) %>% mutate(mean=mean(N2O))
global_N2O_nit_con<- unique(global_N2O_nit[, c(4,8)])


#raw N2O from denitrification
global_N2O_denit <- global_N2O_denit %>% group_by(Continent) %>% mutate(rmean=mean(rawN2O)) 
global_N2O_denit_raw_con <-unique(global_N2O_denit[, c(4,11)])
sd(global_N2O_denit[global_N2O_denit$Continent=="Asia",8]$rawN2O)


#N2O from denitrificaiton with fraction 5.67%
global_N2O_denit <- global_N2O_denit %>% group_by(Continent) %>% mutate(mean=mean(N2O)) 
global_N2O_denit_con <-unique(global_N2O_denit[, c(4,10)])
sd(global_N2O_denit[global_N2O_denit$Continent=="Asia",9]$N2O)


#N2O from denitrificaiton with NO/N2O ratio
global_N2O_denit_m2 <-global_N2O_denit_m2 %>% group_by(Continent) %>% mutate(mean=mean(N2O))
global_N2O_denit_m2_con<-unique(global_N2O_denit_m2[, c(4, 12)])
sd(global_N2O_denit_m2[global_N2O_denit_m2$Continent=="Asia", 9]$N2O)




###########################plot for each continent
global_N2O_nit<-global_N2O_nit[, c(4,7)]
global_N2O_denit<- global_N2O_denit[, c(4,9)]
global_N2O_denit_m2<- global_N2O_denit_m2[, c(4,9)]

#overall
global_N2O_nit$Process<-rep("Nitrification", 2827)
global_N2O_denit$Process<-rep("Denit2", 3301)
global_N2O_denit_m2$Process<- rep("Denit1", 3301)


globaN2O<-rbind(global_N2O_nit, global_N2O_denit, global_N2O_denit_m2)
globaN2O$Process<-factor(globaN2O$Process, 
                         levels = c("Nitrification", "Denit1", "Denit2"))


stat_test_over <- globaN2O[, c(2, 3)] %>% t_test(N2O ~ Process) %>%
  adjust_pvalue(method = "BH")%>%add_significance("p.adj") %>%
  mutate(p.adj.signif = ifelse(p.adj.signif == "****", "***",
                               p.adj.signif)) %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "Process") %>% mutate(y.position=log(y.position))



ggplot(globaN2O, aes(x=Process, y=log(N2O), fill = Process))+
  geom_boxplot(outlier.size = 0.6, size=0.3, position = position_dodge(width = 0.3), width=0.5)+
  geom_point(aes(col=Process), size=0.1, alpha=0.3,
             position = position_jitterdodge(jitter.width = 0.4,dodge.width = 0.3))+
  stat_pvalue_manual(stat_test_over, label = "p.adj.signif", tip.length = 0.01,
                     size = 4, vjust = 1, step.increase = 0.05) +
  scale_fill_brewer(palette = 9)+
  labs(x=NULL, y=expression(Ln(N[2]*O)~(mg~kg^-1~day^-1)), tag = "A")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.tag = element_text(size = 12, face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))



#Europe
EUN2Onit<-global_N2O_nit[global_N2O_nit$Continent=="Europe",]
EUN2Odenit<-global_N2O_denit[global_N2O_denit$Continent=="Europe",]
EUN2Odenit_m2<-global_N2O_denit_m2[global_N2O_denit_m2$Continent=="Europe",]

EUN2O<-rbind(EUN2Onit, EUN2Odenit, EUN2Odenit_m2)
EUN2O$Process<- factor(EUN2O$Process, levels = c("Nitrification", "Denit1", "Denit2"))

stat_test_EU <- EUN2O[, c(2, 3)] %>% t_test(N2O ~ Process) %>%
  adjust_pvalue(method = "BH")%>%add_significance("p.adj") %>%
  mutate(p.adj.signif = ifelse(p.adj.signif == "****", "***",
                               p.adj.signif)) %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "Process") %>% mutate(y.position=log(y.position))


ggplot(EUN2O, aes(x=Process, y=log(N2O), fill = Process))+
  geom_boxplot(outlier.size = 0.6, size=0.3, position = position_dodge(width = 0.3), width=0.5)+
  geom_point(aes(col=Process), size=0.1, alpha=0.3,
             position = position_jitterdodge(jitter.width = 0.4,dodge.width = 0.3))+
  stat_pvalue_manual(stat_test_EU, label = "p.adj.signif", tip.length = 0.01,
                     size = 4, vjust = 1, step.increase = 0.05) +
  scale_fill_brewer(palette = 9)+ 
  labs(x=NULL, y=expression(Ln(N[2]*O)~(mg~kg^-1~day^-1)), tag = "B")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.tag = element_text(size = 12, face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))



#Aisa
AisN2Onit<-global_N2O_nit[global_N2O_nit$Continent=="Asia",]
AisN2Odenit<-global_N2O_denit[global_N2O_denit$Continent=="Asia",]
AisN2Odenit_m2<-global_N2O_denit_m2[global_N2O_denit_m2$Continent=="Asia",]

AisN2O<-rbind(AisN2Onit, AisN2Odenit, AisN2Odenit_m2)
AisN2O$Process<- factor(AisN2O$Process, levels = c("Nitrification", "Denit1", "Denit2"))

stat_test_Asia <- AisN2O[, c(2, 3)] %>% t_test(N2O ~ Process) %>%
  adjust_pvalue(method = "BH")%>%add_significance("p.adj") %>%
  mutate(p.adj.signif = ifelse(p.adj.signif == "****", "***",
                               p.adj.signif)) %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "Process") %>% mutate(y.position=log(y.position))


ggplot(AisN2O, aes(x=Process, y=log(N2O), fill = Process))+
  geom_boxplot(outlier.size = 0.6, size=0.3, position = position_dodge(width = 0.3), width=0.5)+
  geom_point(aes(col=Process), size=0.1, alpha=0.3,
             position = position_jitterdodge(jitter.width = 0.4,dodge.width = 0.3))+
  stat_pvalue_manual(stat_test_Asia, label = "p.adj.signif", tip.length = 0.01,
                     size = 4, vjust = 1, step.increase = 0.05) +
  scale_fill_brewer(palette = 9)+
  labs(x=NULL, y=expression(Ln(N[2]*O)~(mg~kg^-1~day^-1)), tag = "C")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.tag = element_text(size = 12, face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))



#North America
NamN2Onit<-global_N2O_nit[global_N2O_nit$Continent=="North America",]
NamN2Odenit<-global_N2O_denit[global_N2O_denit$Continent=="North America",]
NamN2Odenit_m2<-global_N2O_denit_m2[global_N2O_denit_m2$Continent=="North America",]

NamN2O<-rbind(NamN2Onit, NamN2Odenit, NamN2Odenit_m2)
NamN2O$Process<- factor(NamN2O$Process, levels = c("Nitrification", "Denit1", "Denit2"))

stat_test_NA <- NamN2O[, c(2, 3)] %>% t_test(N2O ~ Process) %>%
  adjust_pvalue(method = "BH")%>%add_significance("p.adj") %>%
  mutate(p.adj.signif = ifelse(p.adj.signif == "****", "***",
                               p.adj.signif)) %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "Process") %>% mutate(y.position=log(y.position))


ggplot(NamN2O, aes(x=Process, y=log(N2O), fill = Process))+
  geom_boxplot(outlier.size = 0.6, size=0.3, position = position_dodge(width = 0.3), width=0.5)+
  geom_point(aes(col=Process), size=0.1, alpha=0.3,
             position = position_jitterdodge(jitter.width = 0.4,dodge.width = 0.3))+
  stat_pvalue_manual(stat_test_NA, label = "p.adj.signif", tip.length = 0.01,
                     size = 4, vjust = 1, step.increase = 0.05) +
  scale_fill_brewer(palette = 9)+
  labs(x=NULL, y=expression(Ln(N[2]*O)~(mg~kg^-1~day^-1)), tag = "D")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.tag = element_text(size = 12, face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))



#South America
SamN2Onit<-global_N2O_nit[global_N2O_nit$Continent=="South America",]
SamN2Odenit<-global_N2O_denit[global_N2O_denit$Continent=="South America",]
SamN2Odenit_m2<-global_N2O_denit_m2[global_N2O_denit_m2$Continent=="South America",]

SamN2O<-rbind(SamN2Onit, SamN2Odenit, SamN2Odenit_m2)
SamN2O$Process<- factor(SamN2O$Process, levels = c("Nitrification", "Denit1", "Denit2"))


stat_test_SA <- SamN2O[, c(2, 3)] %>% t_test(N2O ~ Process) %>%
  adjust_pvalue(method = "BH")%>%add_significance("p.adj") %>%
  mutate(p.adj.signif = ifelse(p.adj.signif == "****", "***",
                               p.adj.signif)) %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "Process") %>% mutate(y.position=log(y.position))


ggplot(SamN2O, aes(x=Process, y=log(N2O), fill = Process))+
  geom_boxplot(outlier.size = 0.6, size=0.3, position = position_dodge(width = 0.3), width=0.5)+
  geom_point(aes(col=Process), size=0.1, alpha=0.3,
             position = position_jitterdodge(jitter.width = 0.4,dodge.width = 0.3))+
  stat_pvalue_manual(stat_test_SA, label = "p.adj.signif", tip.length = 0.01,
                     size = 4, vjust = 1, step.increase = 0.05) +
  scale_fill_brewer(palette = 9)+
  labs(x=NULL, y=expression(Ln(N[2]*O)~(mg~kg^-1~day^-1)), tag = "E")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.tag = element_text(size = 12, face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))




#Africa
AfN2Onit<-global_N2O_nit[global_N2O_nit$Continent=="Africa",]
AfN2Odenit<-global_N2O_denit[global_N2O_denit$Continent=="Africa",]
AfN2Odenit_m2<-global_N2O_denit_m2[global_N2O_denit_m2$Continent=="Africa",]

AfN2O<-rbind(AfN2Onit, AfN2Odenit, AfN2Odenit_m2)
AfN2O$Process<- factor(AfN2O$Process, levels = c("Nitrification", "Denit1", "Denit2"))

stat_test_Africa <- AfN2O[, c(2, 3)] %>% t_test(N2O ~ Process) %>%
  adjust_pvalue(method = "BH")%>%add_significance("p.adj") %>%
  mutate(p.adj.signif = ifelse(p.adj.signif == "****", "***",
                               p.adj.signif)) %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "Process") %>% mutate(y.position=log(y.position))


ggplot(AfN2O, aes(x=Process, y=log(N2O), fill = Process))+
  geom_boxplot(outlier.size = 0.6, size=0.3, position = position_dodge(width = 0.3), width=0.5)+
  geom_point(aes(col=Process), size=0.1, alpha=0.3,
             position = position_jitterdodge(jitter.width = 0.4,dodge.width = 0.3))+
  stat_pvalue_manual(stat_test_Africa, label = "p.adj.signif", tip.length = 0.01,
                     size = 4, vjust = 1, step.increase = 0.05) +
  scale_fill_brewer(palette = 9)+
  labs(x=NULL, y=expression(Ln(N[2]*O)~(mg~kg^-1~day^-1)), tag = "F")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.tag = element_text(size = 12, face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))




#Australia
AuN2Onit<-global_N2O_nit[global_N2O_nit$Continent=="Australia",]
AuN2Odenit<-global_N2O_denit[global_N2O_denit$Continent=="Australia",]
AuN2Odenit_m2<-global_N2O_denit_m2[global_N2O_denit_m2$Continent=="Australia",]

AuN2O<-rbind(AuN2Onit, AuN2Odenit, AuN2Odenit_m2)
AuN2O$Process<- factor(AuN2O$Process, levels = c("Nitrification", "Denit1", "Denit2"))

stat_test_Aus <- AuN2O[, c(2, 3)] %>% t_test(N2O ~ Process) %>%
  adjust_pvalue(method = "BH")%>%add_significance("p.adj") %>%
  mutate(p.adj.signif = ifelse(p.adj.signif == "****", "***",
                               p.adj.signif)) %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "Process") %>% mutate(y.position=log(y.position))


ggplot(AuN2O, aes(x=Process, y=log(N2O), fill = Process))+
  geom_boxplot(outlier.size = 0.6, size=0.3, position = position_dodge(width = 0.3), width=0.5)+
  geom_point(aes(col=Process), size=0.1, alpha=0.3,
             position = position_jitterdodge(jitter.width = 0.4,dodge.width = 0.3))+
  stat_pvalue_manual(stat_test_Aus, label = "p.adj.signif", tip.length = 0.01,
                     size = 4, vjust = 1, step.increase = 0.05) +
  scale_fill_brewer(palette = 9)+
  labs(x=NULL, y=expression(Ln(N[2]*O)~(mg~kg^-1~day^-1)), tag = "G")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.tag = element_text(size = 12, face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))





#N2O for each land cover
#forest
Forest_nit<-global_N2O_nit[global_N2O_nit$Land_cover=="Forest",]
Forest_denit<-global_N2O_denit[global_N2O_denit$Land_cover=="Forest",]
Forest_denit_m2<-global_N2O_denit_m2[global_N2O_denit_m2$Land_cover=="Forest",]

mean(Forest_nit$N2O) 
sd(Forest_nit$N2O) 
mean(Forest_denit$N2O) 
sd(Forest_denit$N2O) 
mean(Forest_denit_m2$N2O) 
sd(Forest_denit_m2$N2O) 

median(Forest_nit$N2O) 
median(Forest_denit$N2O) 


ggplot()+
  geom_violin(aes(x=Forest_nit$Land_cover, y=log(Forest_nit$N2O)))+
  theme_bw()

ggplot()+
  geom_violin(aes(x=Forest_denit$Land_cover, y=Forest_denit$N2O))+
  theme_bw()



#grassland
Grass_nit<-global_N2O_nit[global_N2O_nit$Land_cover=="Grassland",]
Grass_denit<-global_N2O_denit[global_N2O_denit$Land_cover=="Grassland",]
Grass_denit_m2<-global_N2O_denit_m2[global_N2O_denit_m2$Land_cover=="Grassland",]

mean(Grass_nit$N2O) 
sd(Grass_nit$N2O) 
mean(Grass_denit$N2O) 
sd(Grass_denit$N2O) 
mean(Grass_denit_m2$N2O) 
sd(Grass_denit_m2$N2O) 

median(Grass_nit$N2O) 
median(Grass_denit$N2O) 



#Cropland
Crop_nit<-global_N2O_nit[global_N2O_nit$Land_cover=="Cropland",]
Crop_denit<-global_N2O_denit[global_N2O_denit$Land_cover=="Cropland",]
Crop_denit_m2<-global_N2O_denit_m2[global_N2O_denit_m2$Land_cover=="Cropland",]


mean(Crop_nit$N2O) 
sd(Crop_nit$N2O) 
mean(Crop_denit$N2O) 
sd(Crop_denit$N2O) 
mean(Crop_denit_m2$N2O) 
sd(Crop_denit_m2$N2O) 

median(Crop_nit$N2O) 
median(Crop_denit$N2O) 




#difference
globaN2O$logN2O<-log(globaN2O$N2O)

stat_test_process <- globaN2O[, c(2, 3, 13)] %>% t_test(logN2O ~ Process) %>%
  adjust_pvalue(method = "BH")%>%add_significance("p.adj") %>%
  mutate(p.adj.signif = ifelse(p.adj.signif == "****", "***",
                               p.adj.signif)) %>%
  filter(p.adj.signif != "ns") %>% 
  add_xy_position(x = "Process")

ggplot() +
  geom_boxplot(aes(x = globaN2O$Process, y = globaN2O$logN2O), outlier.size = 0.2, size = 0.5) +
  stat_pvalue_manual(stat_test_process, label = "p.adj.signif", tip.length = 0.01,
                     size = 7, vjust = 1, step.increase = 0.05)+
  labs(x=NULL, y=expression(Log[10](N[2]*O)))+
  theme_bw()