# SYNTAX FINAL PROJECT ANDAT : SEMIVARIOGRAM
# Data : Bijih Terkira dan Bijih Terbukti Emas dalam lingkup Jawa Barat - Banten

# run library
library(sp)
library(gstat)
library(dplyr)
library(ggplot2)
library(maptools)

# import data
setwd("D:/OneDrive - Institut Teknologi Bandung/[AKADEMIK]/B. Jurusan/Semester 3/Analisis Data/Assignments/Andat Final Project")
library(readxl)
data <- read_excel("Data Andat FIX.xlsx", sheet = "Emas 14 data")
the_data <- data.frame(data)

# Mengubah Mengubah "data frame" menjadi "spatial point data frame"
coordinates(the_data) = ~x+y
# Melihat cuplikan informasi data
glimpse(the_data)

# Membuat diagram pencar dengan ukuran titik menunjukan besar kecilnya nilai di lokasi tersebut.
ggplot(data = as.data.frame(the_data), aes(x,y)) + 
  geom_point(aes(size = Total), col = "red", alpha = 0.6) + 
  ggtitle("Total Bijih") + coord_equal() + theme_bw()

# utk EMAS (2)
(vgm.ln.2 <- variogram(log(Total) ~ 1, the_data, width = 0.05))
plot(vgm.ln.2, main = "var.ln wdth = 0.05", cex = 1)
# Menampilkan model variogram terbaik antara Eksponensial, Sperikal, dan Gaussian.
(fit <- fit.variogram(vgm.ln.2, model = vgm(model = c("Exp", "Sph", "Gau"))))
plot(vgm.ln.2, fit)


# ______________________________________________________________________________
# KRIGING

# Untuk menaksir nilai observasi menggunakan kriging, praktikan memerlukan 
# titik-titik lokasi tak terobservasi. Titik-titik tersebut akan dibuat kedalam 
# bentuk grid data.
n <- 0.02
kolom <- seq(the_data@bbox[1,1]-n,the_data@bbox[1,2]+n,by = n)
baris <- seq(the_data@bbox[2,1]-n,the_data@bbox[2,2]+n,by = n)
the.grid <- expand.grid(x = kolom, y = baris)
coordinates(the.grid) <- ~x+y
gridded(the.grid) <- T

# Plot grid dan titik observasi
par(mar=c(1,1,1,1))
plot(the.grid, cex=0.5, col="dark grey")
points(the_data, pch=1, col='blue', cex=1)

# Melakukan penaksiran untuk membentuk peta kontur dengan ordinary kriging
kriging <- krige(Total~1,the_data, the.grid, model=fit)

# Plot kontur sebagai hasil taksiran oleh kriging
titik <- SpatialPoints(the_data@coords)
LayoutPoints <- list('sp.points', titik, pch = 19, cex =  0.8, col = 'red')

# Sesuaikan kolom data yang digunakan pada the_data[[n]]
LayoutLabels <- list('sp.pointLabel', titik, label = as.character(the_data[[1]]), cex = 0.8, col = 'white')
spplot(kriging["var1.pred"], main = "Kontur dan Data", sp.layout=list(LayoutPoints,LayoutLabels))

# Menentukan koordinat; 
# contoh 5 lokasi  (0.2,-1),(0.4,-0.8),(0.8,-1.0),(1,-1.4),dan (1.2,-0.5) 
titik2 <- SpatialPoints(cbind(c(106.4),c(-6.84 )))  #masukkan titik-titiknya
taksiran <- krige(Total~1, the_data, titik2, model=fit)[[1]]

LayoutPoints.T <- list('sp.points', titik2, pch=19, cex=0.8, col='green')
LayoutLabels.T <- list('sp.pointLabel', titik2, label = as.character(taksiran), cex = 0.8, col = 'white')
spplot(kriging["var1.pred"], main = "Ordinary Kriging Data Tanpa Pencilan di titik (106.4,-6.84)", sp.layout=list(LayoutPoints.T,LayoutLabels.T))

