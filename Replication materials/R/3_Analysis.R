############# Written by JMA
############# 20/03/2023
#rm(list=ls(all=TRUE))
library(data.table)
library(reshape)
library(ggplot2)
library(expint)
library(viridis)
library(patchwork)

# Load data ---------------------------------------------------------------
#load("Output/GompertzParam_HMD_1.RData")
#load("Output/GompertzParam_HMD_2.RData")
load("Output/GompertzParam_HMD_3.RData")

source('R/0_Functions.R')

#Rename dataset depending on which age scenario we want to use
#1922
data_analysis <- Gompertz.parameters.3[PopName %in% c('AUS', 'CHE', 'DNK', 
                                                      'FIN', 'ISL', 'SWE', 
                                                      'NOR', 'NLD', 'ITA',
                                                      'GBRTENW','GBR_NIR','GBR_SCO',
                                                      'FRATNP', 'CAN', 'ESP')]

unique(data_analysis$PopName)

# Analysis from equations -------------------------------------------------

#life expectancy from gompertz
data_analysis[,e0.gompertz := ex.gompertz(A = A,B = B)] 

#lifespan disparity from gompertz
data_analysis[,ed.gompertz := edx.gompertz(A = A,B = B,ex = e0.gompertz)] 

#first component of Eq.3 = ae_0
data_analysis[,Eq.3.a := A*e0.gompertz] 

#second component of Eq.3 = ae_0
data_analysis[,Eq.3.b :=  B*ed.gompertz] 

#entropy for gompertz
data_analysis[,H.gompertz := ed.gompertz/e0.gompertz] 

#consistency check, A(e0) + B(ed) = 1 [equation 3]
data_analysis[,Eq.1 := A*e0.gompertz + B*ed.gompertz] 

#d.e0/d.a = - ed/a [equation 8]
data_analysis[,Eq.8 := -ed.gompertz/A]

#d.e0/d.b = (1/B)(ed - e0) [equation 9]
data_analysis[,Eq.9 := (1/B) * (ed.gompertz - e0.gompertz)]

#d.ed/a = d.e0/d.b
data_analysis[,Eq.10 := Eq.9]

#d.ed/b = (a/b)(d.e0/d.a - d.e0/d.b)
data_analysis[,Eq.11 := (A/B)*(Eq.8 - Eq.9)]

#d.H/d.a = (1/b)(H/(ae0) - 1)
data_analysis[,Eq.17 := (1/B)*(H.gompertz/(A*e0.gompertz) -1 )]

#d.H/d.b = -(a/b)*d.H/d.a
data_analysis[,Eq.19 := (A/B)*Eq.17]

#Exclude Belgium, an outlier point
data_analysis <- data_analysis[PopName != 'BEL']

###############################################################################
# make results relative to maximum effect

data_analysis[,Eq.8.rel := Eq.8/max(abs(Eq.8))]
data_analysis[,Eq.9.rel := Eq.9/max(abs(Eq.9))]
data_analysis[,Eq.10.rel := Eq.10/max(abs(Eq.10))]
data_analysis[,Eq.11.rel := Eq.11/max(abs(Eq.11))]
data_analysis[,Eq.17.rel := Eq.17/max(abs(Eq.17))]
data_analysis[,Eq.19.rel := Eq.19/max(abs(Eq.19))]

###############################################################################
# Figure 1

Fig0.a <- ggplot(data = data_analysis, aes(x = Year, y = Eq.3.a, color=Year)) +
  #Fig1.a <- ggplot(data = data_analysis, aes(x = e0.gompertz, y = Eq.8, color=Year)) +
  geom_point(alpha=I(1/5),shape=16,show.legend = T)+
  scale_x_continuous('Year')+
  scale_color_viridis(discrete=F,option = 'A') +
  scale_y_continuous(expression(a~e[0]))+
  theme(text = element_text(size = 15))+
  theme_bw()+
  theme(legend.position = 'left') 
Fig0.a

Fig0.b <- ggplot(data = data_analysis, aes(x = Year, y = Eq.3.b, color=Year)) +
  #Fig1.a <- ggplot(data = data_analysis, aes(x = e0.gompertz, y = Eq.8, color=Year)) +
  geom_point(alpha=I(1/5),shape=16,show.legend = F)+
  scale_x_continuous('Year')+
  scale_color_viridis(discrete=F,option = 'A') +
  scale_y_continuous(expression(b~e^t))+
  theme(text = element_text(size = 15))+
  theme_bw()+
  theme(legend.position = 'left') 
Fig0.b

Fig0 <- Fig0.a + Fig0.b
Fig0

ggsave(filename = 'Output/Figure_1.pdf',plot = Fig0,height = 4,width = 9)


###############################################################################
#Figure_7_v2 👉 before and after 1950

Fig7_v2_a <- ggplot(data = data_analysis[Year < 1950,], aes(x = Year, y = A/B)) +
  #Fig6 <- ggplot(data = data_analysis, aes(x = Eq.17, y = Eq.19, color=Year)) +
  geom_point(alpha=I(1/5),shape=16,show.legend = F)+
  scale_x_continuous('Year')+
  #scale_color_viridis(discrete=F,option = 'A') +
  scale_y_continuous(expression(a/b),limits = c(0, 0.1))+
  theme(text = element_text(size = 15))+
  theme_bw()+
  theme(legend.position = 'left') 
Fig7_v2_a

Fig7_v2_b <- ggplot(data = data_analysis[Year >= 1950,], aes(x = Year, y = A/B)) +
  #Fig6 <- ggplot(data = data_analysis, aes(x = Eq.17, y = Eq.19, color=Year)) +
  geom_point(alpha=I(1/5),shape=16,show.legend = F)+
  scale_x_continuous('Year')+
  #scale_color_viridis(discrete=F,option = 'A') +
  scale_y_continuous(expression(a/b),limits = c(0, 0.1))+
  theme(text = element_text(size = 15))+
  theme_bw()+
  theme(legend.position = 'left') 
Fig7_v2_b

Fig7_v2 <- Fig7_v2_a - Fig7_v2_b
Fig7_v2

ggsave(filename = 'Output/Figure_2.pdf',plot = Fig7_v2,height = 4,width = 9)


###############################################################################


Fig3 <- ggplot(data = data_analysis, aes(x = Eq.8.rel, y = Eq.17.rel, color=Year)) +
#Fig3 <- ggplot(data = data_analysis, aes(x = Eq.8, y = Eq.17, color=Year)) +
  geom_point(alpha=I(1/5),shape=16,show.legend = T)+
  scale_x_continuous(expression(delta~e[0]/delta~a))+
  scale_color_viridis(discrete=F,option = 'A') +
  scale_y_continuous(expression(delta~H/delta~a))+
  theme(text = element_text(size = 15))+
  theme_bw()+
  theme(legend.position = 'left') 
Fig3

ggsave(filename = 'Output/Figure_3.pdf',plot = Fig3,height = 4,width = 4.5)



###############################################################################


Fig7_v1 <- ggplot(data = data_analysis[Year %in% 1800:1899,], aes(x = Year, y = A/B, color=Year)) +
  #Fig6 <- ggplot(data = data_analysis, aes(x = Eq.17, y = Eq.19, color=Year)) +
  geom_point(alpha=I(1/5),shape=16,show.legend = F)+
  scale_x_continuous('Year')+
  scale_color_viridis(discrete=F,option = 'A') +
  scale_y_continuous(expression(a/b),limits = c(0, 0.2))+
  theme(text = element_text(size = 15))+
  theme_bw()+
  theme(legend.position = 'left') 
Fig7_v1

ggsave(filename = 'Output/Figure_4.pdf',plot = Fig7_v1,height = 4,width = 4.5)


###############################################################################

Fig4 <- ggplot(data = data_analysis, aes(x = Eq.9.rel, y = Eq.19.rel, color=Year)) +
  #Fig4 <- ggplot(data = data_analysis, aes(x = Eq.9, y = Eq.19, color=Year)) +
  geom_point(alpha=I(1/5),shape=16,show.legend = T)+
  scale_x_continuous(expression(delta~e[0]/delta~b))+
  scale_color_viridis(discrete=F,option = 'A') +
  scale_y_continuous(expression(delta~H/delta~b))+
  theme(text = element_text(size = 15))+
  theme_bw()+
  theme(legend.position = 'left') 
Fig4

ggsave(filename = 'Output/Figure_5.pdf',plot = Fig4,height = 4,width = 4.5)


