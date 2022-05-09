##### Clustering of Fishery Management Areas #####
# Based on The Level of Utilization in Indonesia #


#===== Input Data
data<-read.csv(file.choose(),header=T,sep=";",dec=",")
attach(data)
str(data)

#===== Uji Asumsi
# Non-Multikolinieritas
q=cbind(Cumi.cumi,Ikan.Demersal,Ikan.Karang,Ikan.Pelagis.Besar,
        Ikan.Pelagis.Kecil,Kepiting,Lobster,Rajungan,Udang.Penaeid)
f=cor(q)
vif=diag(solve(f))
vif

#===== Perhitungan Jarak Euclidean
Komoditas=data.frame(Cumi.cumi,Ikan.Demersal,Ikan.Karang,Ikan.Pelagis.Besar,
                     Ikan.Pelagis.Kecil,Kepiting,Lobster,Rajungan,Udang.Penaeid)
d<-dist(Komoditas, method="euclidean")
d

#===== Analisis Klaster Hierarki
# Single Linkage
fit1<-hclust(d, method="single")
plot(fit1,main="Dendogram Single Linkage",col="royalblue")

cop1<-cophenetic(fit1)
cor(d,cop1)

# Complete Linkage
fit2<-hclust(d, method="complete")
plot(fit2,main="Dendogram Complete Linkage",col="forestgreen")

cop2<-cophenetic(fit2)
cor(d,cop2)

# Average Linkage
fit3<-hclust(d, method="average")
plot(fit3,main="Dendogram Average Linkage",col="indianred")

cop3<-cophenetic(fit3)
cor(d,cop3)

# Ward's Linkage
fit4<-hclust(d, method="ward.D")
plot(fit4,main="Dendogram Ward's Linkage",col="darkorange")

cop4<-cophenetic(fit4)
cor(d,cop4)

# Bandingkan hasil korelasi, yang paling mendekati 1 = metode bagus dan tepat
# Metode average linkage nilai korelasinya terbesar

#===== Penentuan Metode terbaik dan Pembentukan Klaster
fit3<-hclust(d, method="average")
plot(fit3,main="Dendogram Average Linkage")
rect.hclust(fit3,k=3,border=2:6)

#===== Profiling Hasil Klaster
groups<-cutree(fit3,k=3)
klaster<-data.frame(Komoditas,groups)
klaster
klaster1<-data.frame(klaster[which(groups==1),1:9])
klaster2<-data.frame(klaster[which(groups==2),1:9])
klaster3<-data.frame(klaster[which(groups==3),1:9])
summary(klaster1)
summary(klaster2)
summary(klaster3)