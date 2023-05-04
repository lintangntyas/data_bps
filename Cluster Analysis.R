#Banyaknya Desa_Kelurahan Menurut 
#Jenis Bencana Alam dalam Tiga Tahun Terakhir 2021
data <- read.delim(pipe("pbpaste"))
View(data)

summary(data[,2:4])
plot(data[,2:4])

#uji asumsi multikolinearitas
library(car)
multikol <- cor(data[,2:4])
multikol

#Melakukan perhitungan jarak antar satu data ke data lainnya
jarak_data <- dist(data[,2:4])
jarak_data

#analisis cluster dengan metode average 
ave_hirarkie <- hclust(dist(scale(data[,2:4])),
                       method <- "ave")
ave_hirarkie

#korelasi cophenetic
d1 <- dist(data[,2:4])
hc <- hclust(d1, "ave")
d2 <- cophenetic(hc)
ave_cor <- cor(d1, d2)
ave_cor

#analisis cluster dengan metode Complete
comp_hirarkie <- hclust(dist(scale(data[,2:4])),
                       method <- "complete")
comp_hirarkie

#korelasi cophenetic
d1 <- dist(data[,2:4])
hc <- hclust(d1, "complete")
d2 <- cophenetic(hc)
comp_cor <- cor(d1, d2)
comp_cor

#analisis cluster dengan metode Single
single_hirarkie <- hclust(dist(scale(data[,2:4])),
                        method <- "single")
single_hirarkie

#korelasi cophenetic
d1 <- dist(data[,2:4])
hc <- hclust(d1, "single")
d2 <- cophenetic(hc)
single_cor <- cor(d1, d2)
single_cor

#analisis cluster dengan metode Ward
ward_hirarkie <- hclust(dist(scale(data[,2:4])),
                          method <- "ward.D")
ward_hirarkie

#korelasi cophenetic
d1 <- dist(data[,2:4])
hc <- hclust(d1, "ward.D")
d2 <- cophenetic(hc)
ward_cor <- cor(d1, d2)
ward_cor

#analisis cluster dengan metode centroid
centroid_hirarkie <- hclust(dist(scale(data[,2:4])),
                        method <- "centroid")
centroid_hirarkie

#korelasi cophenetic
d1 <- dist(data[,2:4])
hc <- hclust(d1, "centroid")
d2 <- cophenetic(hc)
centroid_cor <- cor(d1, d2)
centroid_cor

#dendogram
plot(ave_hirarkie, 
     labels <- data$Provinsi, 
     main= "Cluster Dendogram")

rect.hclust(ave_hirarkie, 3)

kelas <- cutree(ave_hirarkie, k <- 3)
table(kelas)

#kelas masing-masing cluster
rownames(data)[anggota==1]
rownames(data)[anggota==2]
rownames(data)[anggota==3]
