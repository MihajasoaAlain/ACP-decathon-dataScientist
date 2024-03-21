library("ggplot2")
library("FactoMineR")
library("factoextra")
library("psych")


#IMPORTATION DES DONNES
decathlon = read.table("ACP_decathlon.csv",header = TRUE,
                       sep = ";",dec =".",row.names = 1,fileEncoding = "latin1",check.names = FALSE)

decathlon
dim(decathlon)
summary(decathlon)
mesDonnees.active<-decathlon[1:41,1:12]
mesDonnees.active
matrice.cor=cor(mesDonnees.active)
pairs(mesDonnees.active)
det(matrice.cor)
cortest.bartlett(matrice.cor,n=12)

#si p_value <0.05 l'hypotese alternative 
#teste matrice identite ou pas
#KMO mijery ny hatsarany le in
KMO(matrice.cor)

#Grafique
res.acp=PCA(mesDonnees.active,scale.unit = TRUE,ncp = 5,graph = TRUE)
res.acp
valeur.propre<-get_eigenvalue(res.acp)
valeur.propre
screen.plot=fviz_eig(res.acp,addlabels = T,ylim=c(0,50))

screen.plot
res.var = get_pca_var(res.acp)
res.var$coord
res.var$cor
res.var$cos2
res.var$contrib

res.dec =dimdesc(res.acp,axes = c(1,2),proba =0.05)
res.dec
fviz_contrib(res.acp,choice ="var",axes = 1,top = 10 )
fviz_contrib(res.acp,choice ="var",axes = 2,top = 10 )
fviz_contrib(res.acp,choice ="var",axes = 1:2,top = 10 )

