##### Pemetaan Kasus Kekerasan dengan Analisis Korespondensi #####
################ (Studi Kasus Provinsi Jawa Barat) ###############


# install.packages("devtools", dependencies=TRUE)
library(devtools)
# install_github("gianmarcoalberti/CAinterprTools@v0.24")
library(CAinterprTools)

#===== Input Data
data=read.csv(file.choose(),header=TRUE,sep=";")
View(data)
jenis_kekerasan1=as.matrix(data[,-1])
dimnames(jenis_kekerasan1)=list(KabupatenKota=c(data[,1]),JenisKekerasan=c("Eksploitasi dan Fisik","Penelantaran","Psikis","Seksual","Trafficking","Lainnya"))
jenis_kekerasan1

kekerasan <- jenis_kekerasan1[,colSums(jenis_kekerasan1) > 1]

chisq.test(kekerasan)

#===== Profil Baris
prop.table(kekerasan,1)

#===== Profil Kolom
prop.table(kekerasan,2)

# Library CAinterprtools
caCorr(kekerasan)
caPercept(kekerasan)
caPlot(kekerasan)

# Library FactoMineR
library(FactoMineR)
library(factoextra)

#===== Plot Pengelompokkan
CA_Kekerasan <- CA(kekerasan)
plot(CA_Kekerasan)

#===== Inersia dan Dimensi
summary(CA_Kekerasan)
CA_Kekerasan$col$contrib
CA_Kekerasan$row$contrib

#===== Eigen Value dan Scree Plot
eig.val <- get_eigenvalue(CA_Kekerasan);eig.val
fviz_screeplot(CA_Kekerasan, addlabels = TRUE, ylim = c(0, 50))
