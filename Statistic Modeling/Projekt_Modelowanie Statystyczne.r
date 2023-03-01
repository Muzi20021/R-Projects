library(tidyverse)
library(ggrepel)
library(GGally)
library(DataExplorer) 
library(fpc)    
library(plotly)
library(ggfortify)
library(FactoMineR)
library(cluster)
library(factoextra)
library(corrplot)
library(readr) 
library(psych)


fr <- read_csv("./data/hlth_ehis_fv3e__custom_2544306_page_linear.csv")
# Daily consumption of fruit and vegetables by sex, age and educational attainment level
na<- read_csv("./data/nama_10_pc__custom_2536017_page_linear.csv")
# Main GDP aggregates per capita
lf <- read_csv("./data/demo_mlexpec__custom_2544020_page_linear.csv")
# Life expectancy by age and sex
pe<- read_csv("./data/trng_lfs_02__custom_2542364_page_linear.csv")
# Participation rate in education and training (last 4 weeks) by sex, age and educational attainment level
ac <- read_csv("./data/hlth_ehis_pe2e__custom_2539561_page_linear.csv")
# Time spent on health-enhancing
md <- read_csv("./data/ilc_mddd11__custom_2565067_page_linear.csv")
# Severe material deprivation rate by age and sex
# Severe material deprivation rate by age and sex
fr <- fr %>% select(9:11)

BZ <- fr %>% rename(`food` = OBS_VALUE) %>% 
                      mutate(PKD = na$OBS_VALUE,
                      life = lf$OBS_VALUE,
                      education = pe$OBS_VALUE,
                      activity = 100 - ac$OBS_VALUE,
                      status = 100 - md$OBS_VALUE
                      )

# ?r?d?a danych:
# (fr) Food:https://ec.europa.eu/eurostat/databrowser/view/HLTH_EHIS_FV3E__custom_2544306/default/table?lang=en&fbclid=IwAR1A0c06k3BWzDo0k_f1ojqFhm2Ip-7JaPgGHzNKc3WQa873TRtspm4RFNk
# (na) PKD:https://ec.europa.eu/eurostat/databrowser/view/NAMA_10_PC__custom_2565194/default/table?lang=en
# (lf) ?ycie:https://ec.europa.eu/eurostat/databrowser/view/DEMO_MLEXPEC__custom_2544020/default/table?lang=en&fbclid=IwAR33GHqA1EXUdd0z168fqLsFeFK7UyUq6-CrHQdR5udsMeCvDF3ODKqD1Zk
# (pe) Education:https://ec.europa.eu/eurostat/databrowser/view/TRNG_LFS_02__custom_2565159/default/table?lang=en
# (ac) Activity:https://ec.europa.eu/eurostat/databrowser/view/HLTH_EHIS_PE2E__custom_2539561/default/table?lang=en&fbclid=IwAR0Se5u3jmfJQtVeLVU96X9nVeusIAC38moFLsA0BFP3v8k432O8SvV-HO8
# (md) Status: https://ec.europa.eu/eurostat/databrowser/view/ILC_MDDD11__custom_2565067/default/table?lang=en&fbclid=IwAR1kWNA4ugdW_XDKXPXCuZUkJl81C_zaaHv0-ZrC4SnK_FBI9WlS_9aQM5o

dat.scaled <- scale(BZ[, 3:8]) %>% as.data.frame()
summary(BZ)

corrplot(cor(BZ[, 3:8]), order = "hclust", tl.cex = 0.7)
cor(BZ[,3:8]) %>% round(2) %>% kable()
# zmienne education, activity, PKD oraz status sa ze soba mocno skorelowane.

cortest.bartlett(cor(BZ[, 3:8]), n = nrow(BZ))
# p-value jest bliskie 0, wiec mozna odrzucic hipoteze, ze macierz korelacji
# jest macierza jednostkowa
KMO(cor(BZ[, 3:8]))
# KMO > 0.5, wiec PCA jest dopuszczalna


# education, activity, PKD oraz status to 
# zmienne mocno ze sob? skorolowane w spos?b dodatni

d <- dist(dat.scaled , method = "euclidean")

dat <- BZ[, -c(1, 2)]

pca1 <- PCA(dat, graph = FALSE) 

fviz_screeplot(pca1, addlabels = TRUE)
fviz_pca_var(pca1, repel = TRUE)
# o? Y determinuje zmienne activity, food i status 
# o? X determinuje zmienne PKD, life  i education
fviz_pca(pca1, repel = TRUE)
fviz_pca_ind(pca1, repel = TRUE)

fviz_pca_ind(pca1, 
             habillage = factor(BZ$geo), label = BZ$geo,
             addEllipses = TRUE)




fviz_nbclust(scale(dat), pam, method = "wss")
fviz_nbclust(scale(dat), pam, method = "silhouette")
#  wybieramy 2 grupy

pam1 <- pam(scale(dat), k = 2, nstart = 10)
fviz_cluster(pam1, data = dat, repel = TRUE)

x <- rep(0, 10) # wektor z wss, poczatkowo zerowy
 ##petla:
for(i in 1:10)
  x[i] <- kmeans(dat.scaled, centers = i, nstart = 10)$tot.withinss
km.ch <- kmeansruns(dat.scaled, criterion = "ch", runs = 10)
plot(km.ch$crit, type = "b") 
#metoda k srednich i kryterium Calinskiego-Harabasza r?wnie? wskazuje na utworzenie 2 grup 
km.asw <- kmeansruns(scale(dat.scaled), criterion = "asw", runs = 10)
plot(km.asw$crit, type = "b")

prc <- principal(BZ[, 3:8], nfactors = 6, rotate = "none")


prc$values
plot(prc$values, type = "b")
abline(h = 1, col = "red", lty = 2)
# czerwona linia wskazuje na wyb?r 2 warto?ci
pr <- principal(BZ[, 3:8], nfactors = 2, rotate = "none")

pr$scores
pr$loadings
# PKD,education,acitivity i status sa mocno skorelowane z PC1,
# zmienna food jest mocno skorelowana z PC2
old.par <- par() # zapis ustawien
biplot(pr)
par(old.par) 







# grupowanie hierarchiczne ####

str(BZ)
summary(BZ)

pairs(BZ[,3:8])

ggpairs(BZ[,3:8])

BZ.scaled <- scale(BZ[,3:8]) %>% as.data.frame()
summary(BZ.scaled)


E <- dist(BZ.scaled, method = "euclidean")

ec1 <- hclust(E,method = "ward.D2")
plot(ec1)
# Dendogram pokazuje podzial na 2 lub 3 grupy.

cutree(ec1, k = 2) %>% table()
cutree(ec1, k = 3) %>% table()
cutree(ec1, k = 4) %>% table()
# podzia? na 3 grupy dzieli dane najbardziej r?wnomiernie
BZ$cluster.w <- cutree(ec1, k = 2) %>% as.factor()

plot_boxplot(BZ[, 3:9], by = "cluster.w")
# pierwsza grupa wypada lepiej we wszystkich zmiennych



# grupowanie k-means ####

set.seed(10)
ec1 <- kmeans(BZ.scaled, centers = 2, nstart = 10)

ec1$cluster

ec1$withinss

ec1$tot.withinss



x <- rep(0, 10)
for(i in 1:10)
  x[i] <- kmeans(BZ.scaled, centers = i, nstart = 10)$tot.withinss

plot(x, type = "b")

es.ch <- kmeansruns(BZ.scaled, criterion = "ch", runs = 10)
plot(es.ch$crit, type = "b")


es.asw <- kmeansruns(scale(BZ.scaled), criterion = "asw", runs = 10)
plot(es.asw$crit, type = "b")

es.ch$cluster
es.asw$cluster


#Wybór dwóch grup według metody k średnich i  jest najlepszy
table(es.ch$cluster, es.asw$cluster)
table(es.ch$cluster, ec1$cluster)

# według kryterium Calinskiego-Harabasza i k średnich podział liczby zmiennych na grupy jest taki sam


BZ$cluster.km <- ec1$cluster %>% as.factor()

summary(BZ)

plot_boxplot(BZ[, c(3:10)], by = "cluster.km")
# grupa pierwsza ponownie wypada lepiej
table( cluster.km =BZ$cluster.km )


table(BZ$cluster.w)
table(BZ$cluster.km)

pairs(BZ[, 3:8], col = BZ$cluster.w, main = "Podzial wg hclust")
pairs(BZ[, 3:8], col = BZ$cluster.km, main = "Podzial wg kmeans")

BZ.pca <- pr$scores %>% 
  as.data.frame() %>% 
  rename(jakosc_zycia = PC1,
         jakosc_posilkow = PC2) %>% 
  mutate( cluster.w =BZ$cluster.w ,
          cluster.km =BZ$cluster.km ,
    id   = BZ$geo,
    
  )




BZ %>% 
  select(geo, cluster.w, cluster.km) %>% 
  filter(geo %in% c("PL"))  
# Polska w obu kryteriach należy do słabszej grupy

BZ.pca %>% 
  ggplot(aes(x = jakosc_zycia, y = jakosc_posilkow, color = cluster.w)) +
  geom_point() +
  geom_text_repel(aes(label = id), size = 3, show.legend = FALSE) +
  ggtitle("Grupowanie wg hclust")

BZ.pca %>% 
  ggplot(aes(x = jakosc_zycia, y = jakosc_posilkow, color = cluster.km)) +
  geom_point() +
  geom_text_repel(aes(label = id), size = 3, show.legend = FALSE) +
  ggtitle("Grupowanie wg kmeans")
# metodą "kmeans" PT IT MT CY EE Si zostały dobrane do grupy 2 w przeciwieństwie do metody hclust
# gdzie te zmienne zostały umieszczone w grupie 1


# Irlandia wyroznia sie wysoka jakoscia zycia i posilkow
# Szwecja i Finlandia cechuje wysoka jakoscia zycia i niska wartoscia spozycia warzyw i owocow
# Z kolei Bulgaria i Rumunia s? dwoma najgorszymi krajami Unii Europejskiej 
# Pod wzgledem jakosci zycia, a ich dieta jest uboga w warzywa i owoce