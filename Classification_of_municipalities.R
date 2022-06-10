#library
library(tidyverse)
library(factoextra)
library(gridExtra)
library(dplyr)
library(plotly)

set.seed(12)

#load data, view, move ID to rows
municipios <- read.table("data/municipios.csv", sep = ";", header = T, dec = ",")
View(municipios)
rownames(municipios) <- municipios[,1]
municipios <- municipios[,-1]
View(municipios)
#                   Pop_Menos_15Anos_2014 ...   Habitantes    area      PIB
# São Paulo         19.73                       11245983      1521.10   499375.40
# Campinas          18.17                       1079140       794.43    42766.02
# Guarulhos         22.50                       1220653       318.68    44670.72
#   ...   
# Marília           18.12                       216576        1170.25   4585.31
# Itatiba           18.90                       101283        101283    3663.94
# Mongaguá          21.72                       46186         46186     603.80

#standardize variables
municipios.padronizado <- scale(municipios)
#                   Pop_Menos_15Anos_2014 ... Habitantes      PIB
# São Paulo         0.03420908                24.6260121405   24.7282598386      
# Campinas          -0.56777014               2.2358113873    2.0183672795   
# Guarulhos         1.10310807                2.5474621602    2.1130993070
#   ...   
# Marília           -0.587064349              0.3362068471    0.1194141263
# Itatiba           -0.286074740              0.0822997674    0.0735889317
# Mongaguá          0.802118463               -0.0390390681   -0.0786099798

#generate 3 to 6 groups to analyze which is the best fit
municipios.k3 <- kmeans(municipios.padronizado, centers = 3)
municipios.k4 <- kmeans(municipios.padronizado, centers = 4)
municipios.k5 <- kmeans(municipios.padronizado, centers = 5)
municipios.k6 <- kmeans(municipios.padronizado, centers = 6)

#graphics
G1 <- fviz_cluster(municipios.k3, geom = "point", data = municipios.padronizado) + ggtitle("k = 3")
G2 <- fviz_cluster(municipios.k4, geom = "point",  data = municipios.padronizado) + ggtitle("k = 4")
G3 <- fviz_cluster(municipios.k5, geom = "point",  data = municipios.padronizado) + ggtitle("k = 5")
G4 <- fviz_cluster(municipios.k6, geom = "point",  data = municipios.padronizado) + ggtitle("k = 6")

#create a matrix with 4 graphs
grid.arrange(G1, G2, G3, G4, nrow = 2)

#elbow method
fviz_nbclust(municipios.padronizado, FUN = hcut, method = "wss")

#join group to original base
municipios2 <- read.table("data/municipios.csv", sep = ";", header = T, dec = ",")
municipiosfit <- data.frame(municipios.k6$cluster)
#                     municipios.k5.cluster
# São Paulo           1
# Campinas            5
# Guarulhos           3
# Ribeirão Preto      2
# Jundiaí             2

#group cluster and database
MunicipioFinal <-  cbind(municipios2, municipiosfit)
#                   Pop_Menos_15Anos_2014 ...   PIB         municipios.k5.cluster
# São Paulo         19.73                       499375.40   1
# Campinas          18.17                       42766.02    5
# Guarulhos         22.50                       44670.72    3
#   ...   
# Marília           18.12                       4585.31     5
# Itatiba           18.90                       3663.94     4
# Mongaguá          21.72                       603.80      6

#creating new population density column
MunicipioFinal["Densidade_Demografica"] <- MunicipioFinal["Habitantes"]  / MunicipioFinal["area"] 

#descriptive analysis
mediagrupo <- MunicipioFinal %>% 
  group_by(municipios.k6.cluster) %>% 
  summarise(n = n(),
            mean_tx_natalidade = mean(taxa_natalidade), 
            mean_Esgoto_2010 = mean(Esgoto_2010), 
            mean_PIB = mean(PIB),
            mean_Pop_Menos_15Anos_2014 = mean(Pop_Menos_15Anos_2014), 
            mean_Pop_com_60Anoso_2014 = mean(Pop_com_60Anoso_2014), 
            mean_Habitantes = mean(Habitantes),
            mean_area = mean(area), 
            mean_Empregos_formais_2013 = mean(Empregos_formais_2013),
            mean_densidade_demografica = mean(Densidade_Demografica),
            sd_tx_natalidade = sd(taxa_natalidade),
            sd_PIB = sd(PIB),
            sd_Habitantes = sd(Habitantes),
            sd_area = sd(area),
            sd_densidade_demografica = sd(Densidade_Demografica)
            )
df <- data.frame(mediagrupo)
View(df)
# municipios.k6.cluster   n   mean_tx_natalidade ...  sd_area   sd_densidade_demografica
# 1                       1   15.09000                NA        NA
# 2                       240 12.57529                185.6099  443.2272
# 3                       132 15.05962                153.4491  1064.1366
# 4                       139 10.06820                177.7426  227.1989
# 5                       81  13.49333                300.0643  492.9061  
# 6                       52  14.75038                190.9607  260.2587

#filtering only sao paulo, creating database
SaoPaulo <- MunicipioFinal%>%filter(municipios.k6.cluster== 6)

#filtering only sao paulo,  creating database
Demais_cidades <- MunicipioFinal%>%filter(municipios.k6.cluster!= 6)

#rename column name
Demais_cidades <- rename(Demais_cidades, Grupo = municipios.k6.cluster)

#graphic too many cities
# part I
paste0(nome, " ", sobrenome)
a <-  ggplot(
              Demais_cidades, 
              aes(x = Habitantes, y = PIB)) + 
              geom_point(aes(color = Grupo)) + 
              geom_point(aes(
                color = Grupo, 
                text= paste0("cidade: ", Município
                ))) + 
              facet_wrap(~Grupo)
ggplotly(a)

# part II
b <-  ggplot(
  Demais_cidades, 
  aes(x = Empregos_formais_2013, y = Habitantes)) + 
  geom_point(aes(color = Grupo)) + 
  geom_point(aes(
    color = Grupo, 
    text= paste0("cidade: ", Município
    ))) + 
  facet_wrap(~Grupo)
ggplotly(b)

