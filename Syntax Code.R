#LIBRARY
library(readxl)
library(spdep)
library(sf)
library(dplyr)
library(ggplot2)
library(writexl)
library(igraph)
library(spdep) 
library(lattice) 
library(e1071) 
library(RColorBrewer) 
library(spData) 
library(sf) 
library(terra) 
library(car) 
library(psych) 
library(spatial) 
library(factoextra) 
library(FactoMineR)

#DATA
indo_shp <- st_read("D:/KULIII AH/Road to Sarjana/skripsi/Batas Kabupaten/Batas Kabupaten.shp")
jatim_shp <- indo_shp %>% filter(Provinsi == "JAWA TIMUR")
jatim_shp_valid <- st_make_valid(jatim_shp)
data_var <- read_excel("D:/KULIII AH/Road to Sarjana/skripsi/SKRIPSI.xlsx")
data <- left_join(jatim_shp_valid, data_var, by = "Kab_Kota")
data
View(data_var)
summary(data_var)
sd(data_var$X1)
sd(data_var$X2)
sd(data_var$X3)
sd(data_var$X4)
sd(data_var$X5)
sd(data_var$X6)
sd(data_var$X7)

# Plot peta sebaran variabel
# Pastikan geometry valid
data_valid <- st_make_valid(data)
label_coords <- st_coordinates(st_point_on_surface(data_valid))
data_label <- cbind(data_valid, label_coords)
idx_gresik <- which(data_label$Kab_Kota == "GRESIK")
data_label[idx_gresik, c("X", "Y")] <- 
  st_coordinates(st_centroid(data_valid[idx_gresik, ]))

##X1
ggplot(data_valid) +
  geom_sf(aes(fill = X1), color = "black") +
  scale_fill_gradient(low = "lightyellow", high = "green", name = "Nilai X1") +
  geom_text(data = data_label,
            aes(X, Y, label = Kab_Kota),
            size = 1.8, color = "black") +
  labs(title = "Peta Sebaran Variabel Laju 
pertumbuhan pengeluaran rumah tangga (X1) di Jawa Timur",
       caption = "Sumber: Data Skripsi") +
  theme_minimal()

##X2
ggplot(data_valid) +
  geom_sf(aes(fill = X2), color = "black") +
  scale_fill_gradient(low = "lightyellow", high = "green", name = "Nilai X2") +
  geom_text(data = data_label,
            aes(X, Y, label = Kab_Kota),
            size = 1.8, color = "black") +
  labs(title = "Peta Sebaran Variabel Rata-rata Lama Sekolah (X2) di Jawa Timur",
       caption = "Sumber: Data Skripsi") +
  theme_minimal()

##X3
ggplot(data_valid) +
  geom_sf(aes(fill = X3), color = "black") +
  scale_fill_gradient(low = "lightyellow", high = "green", name = "Nilai X3") +
  geom_text(data = data_label,
            aes(X, Y, label = Kab_Kota),
            size = 1.8, color = "black") +
  labs(title = "Peta Sebaran Variabel Tingkat Pengangguran Terbuka (X3) di Jawa Timur",
       caption = "Sumber: Data Skripsi") +
  theme_minimal()

##X4
ggplot(data_valid) +
  geom_sf(aes(fill = X4), color = "black") +
  scale_fill_gradient(low = "lightyellow", high = "green", name = "Nilai X4") +
  geom_text(data = data_label,
            aes(X, Y, label = Kab_Kota),
            size = 1.8, color = "black") +
  labs(title = "Peta Sebaran Variabel Angka Melek huruf (X4) di Jawa Timur",
       caption = "Sumber: Data Skripsi") +
  theme_minimal()

##X5
ggplot(data_valid) +
  geom_sf(aes(fill = X5), color = "black") +
  scale_fill_gradient(low = "lightyellow", high = "green", name = "Nilai X5") +
  geom_text(data = data_label,
            aes(X, Y, label = Kab_Kota),
            size = 1.8, color = "black") +
  labs(title = "Peta Sebaran Variabel Rumah Tangga dengan Sanitasi Layak (X5) di Jawa Timur",
       caption = "Sumber: Data Skripsi") +
  theme_minimal()

##X6
ggplot(data_valid) +
  geom_sf(aes(fill = X6), color = "black") +
  scale_fill_gradient(low = "lightyellow", high = "green", name = "Nilai X6") +
  geom_text(data = data_label,
            aes(X, Y, label = Kab_Kota),
            size = 1.8, color = "black") +
  labs(title = "Peta Sebaran Variabel PDRB perkapita (X6) di Jawa Timur",
       caption = "Sumber: Data Skripsi") +
  theme_minimal()

##X7
ggplot(data_valid) +
  geom_sf(aes(fill = X7), color = "black") +
  scale_fill_gradient(low = "lightyellow", high = "green", name = "Nilai X7") +
  geom_text(data = data_label,
            aes(X, Y, label = Kab_Kota),
            size = 1.8, color = "black") +
  labs(title = "Peta Sebaran Variabel Indeks Pembangunan Manusia (X7) di Jawa Timur",
       caption = "Sumber: Data Skripsi") +
  theme_minimal()

##Pembentukan Matriks Pembobot Spasial
# Melakukan PCA pada subset data 
pca_result <- PCA(data_var[,2:8],  
                  scale.unit = TRUE,  
                  ncp = 7,  
                  graph = FALSE) 
summary(pca_result)
pca_data <- as.data.frame(pca_result$ind$coord[,1]) 
pca_data 
write_xlsx(pca_data, path="D:/KULIII AH/Road to Sarjana/skripsi/pca.xlsx")
pc1 <- pca_data[,1]
pc1 <- as.numeric(pca_data[,1])

#jarak Euclidean semua pasangan wilayah (i ke j)
centroids <- st_centroid(jatim_shp_valid)
coords_knn <- st_coordinates(centroids)
str(coords_knn)
dist_matrix <- as.matrix(dist(coords_knn, method = "euclidean"))
neighbor_order <- list()
for (i in 1:nrow(dist_matrix)) {
  dists <- dist_matrix[i, ]
  dists[i] <- NA
  ord <- order(dists, na.last = NA)
  neighbor_order[[i]] <- data.frame(
    from = i,
    to = ord,
    distance = dists[ord]
  )
}
neighbor_order

#K=2
coords_knn <- st_coordinates(st_point_on_surface(jatim_shp_valid))
nb_knn2 <- knearneigh(coords_knn, k = 2)
nb_knn2
nb_knn2 <- knn2nb(nb_knn2)
listw_knn2 <- nb2listw(nb_knn2, style = "W")
listw_knn2


##Visualisasi Pembobot
coords <- st_coordinates(st_point_on_surface(jatim_shp_valid))
idx_gresik <- which(jatim_shp_valid$Kab_Kota == "GRESIK")
coords[idx_gresik, ] <- st_coordinates(st_centroid(jatim_shp_valid[idx_gresik, ]))
plot(st_geometry(jatim_shp_valid),
     border = "grey40",
     col = "grey90",
     main = "Matriks Pembobot KNN")
text(coords[, 1], coords[, 2],
     labels = jatim_shp_valid$Kab_Kota,
     cex = 0.6, font = 0.5, col = "black")
plot(listw_knn2, coords, add = TRUE, col = "red")


#K=3
coords_knn <- st_coordinates(st_point_on_surface(jatim_shp_valid))
nb_knn3 <- knearneigh(coords_knn, k = 3)
nb_knn3
nb_knn3 <- knn2nb(nb_knn3)
listw_knn3 <- nb2listw(nb_knn3, style = "W")
listw_knn3

##Visualisasi Pembobot
coords <- st_coordinates(st_point_on_surface(jatim_shp_valid))
idx_gresik <- which(jatim_shp_valid$Kab_Kota == "GRESIK")
coords[idx_gresik, ] <- st_coordinates(st_centroid(jatim_shp_valid[idx_gresik, ]))
plot(st_geometry(jatim_shp_valid),
     border = "grey40",
     col = "grey90",
     main = "Matriks Pembobot KNN")
text(coords[, 1], coords[, 2],
     labels = 1:nrow(jatim_shp_valid),
     cex = 0.8, font = 2, col = "black")
plot(listw_knn3, coords, add = TRUE, col = "red")


#K=4
coords_knn <- st_coordinates(st_point_on_surface(jatim_shp_valid))
nb_knn4 <- knearneigh(coords_knn, k = 4)
nb_knn4
nb_knn4 <- knn2nb(nb_knn4)
listw_knn4 <- nb2listw(nb_knn4, style = "W")
listw_knn4

##Visualisasi Pembobot
coords <- st_coordinates(st_point_on_surface(jatim_shp_valid))
idx_gresik <- which(jatim_shp_valid$Kab_Kota == "GRESIK")
coords[idx_gresik, ] <- st_coordinates(st_centroid(jatim_shp_valid[idx_gresik, ]))
plot(st_geometry(jatim_shp_valid),
     border = "grey40",
     col = "grey90",
     main = "Matriks Pembobot KNN")
text(coords[, 1], coords[, 2],
     labels = 1:nrow(jatim_shp_valid),
     cex = 0.8, font = 2, col = "black")
plot(listw_knn4, coords, add = TRUE, col = "red")


#K=5
coords_knn <- st_coordinates(st_point_on_surface(jatim_shp_valid))
nb_knn5 <- knearneigh(coords_knn, k = 5)
nb_knn5
nb_knn5 <- knn2nb(nb_knn5)
listw_knn5 <- nb2listw(nb_knn5, style = "W")
listw_knn5

##Visualisasi Pembobot
coords <- st_coordinates(st_point_on_surface(jatim_shp_valid))
idx_gresik <- which(jatim_shp_valid$Kab_Kota == "GRESIK")
coords[idx_gresik, ] <- st_coordinates(st_centroid(jatim_shp_valid[idx_gresik, ]))
plot(st_geometry(jatim_shp_valid),
     border = "grey40",
     col = "grey90",
     main = "Matriks Pembobot KNN")
text(coords[, 1], coords[, 2],
     labels = 1:nrow(jatim_shp_valid),
     cex = 0.8, font = 2, col = "black")
plot(listw_knn5, coords, add = TRUE, col = "red")



#K=6
coords_knn <- st_coordinates(st_point_on_surface(jatim_shp_valid))
nb_knn6 <- knearneigh(coords_knn, k = 6)
nb_knn6
nb_knn6 <- knn2nb(nb_knn6)
listw_knn6 <- nb2listw(nb_knn6, style = "W")
listw_knn6

##Visualisasi Pembobot
coords <- st_coordinates(st_point_on_surface(jatim_shp_valid))
idx_gresik <- which(jatim_shp_valid$Kab_Kota == "GRESIK")
coords[idx_gresik, ] <- st_coordinates(st_centroid(jatim_shp_valid[idx_gresik, ]))
plot(st_geometry(jatim_shp_valid),
     border = "grey40",
     col = "grey90",
     main = "Matriks Pembobot KNN")
text(coords[, 1], coords[, 2],
     labels = 1:nrow(jatim_shp_valid),
     cex = 0.8, font = 2, col = "black")
plot(listw_knn6, coords, add = TRUE, col = "red")


#K=7
coords_knn <- st_coordinates(st_point_on_surface(jatim_shp_valid))
nb_knn7 <- knearneigh(coords_knn, k = 7)
nb_knn7
nb_knn7 <- knn2nb(nb_knn7)
listw_knn7 <- nb2listw(nb_knn7, style = "W")
listw_knn7

##Visualisasi Pembobot
coords <- st_coordinates(st_point_on_surface(jatim_shp_valid))
idx_gresik <- which(jatim_shp_valid$Kab_Kota == "GRESIK")
coords[idx_gresik, ] <- st_coordinates(st_centroid(jatim_shp_valid[idx_gresik, ]))
plot(st_geometry(jatim_shp_valid),
     border = "grey40",
     col = "grey90",
     main = "Matriks Pembobot KNN")
text(coords[, 1], coords[, 2],
     labels = 1:nrow(jatim_shp_valid),
     cex = 0.8, font = 2, col = "black")
plot(listw_knn7, coords, add = TRUE, col = "red")



#K=8
coords_knn <- st_coordinates(st_point_on_surface(jatim_shp_valid))
nb_knn8 <- knearneigh(coords_knn, k = 8)
nb_knn8
nb_knn8 <- knn2nb(nb_knn8)
listw_knn8 <- nb2listw(nb_knn8, style = "W")
listw_knn8

##Visualisasi Pembobot
coords <- st_coordinates(st_point_on_surface(jatim_shp_valid))
idx_gresik <- which(jatim_shp_valid$Kab_Kota == "GRESIK")
coords[idx_gresik, ] <- st_coordinates(st_centroid(jatim_shp_valid[idx_gresik, ]))
plot(st_geometry(jatim_shp_valid),
     border = "grey40",
     col = "grey90",
     main = "Matriks Pembobot KNN")
text(coords[, 1], coords[, 2],
     labels = 1:nrow(jatim_shp_valid),
     cex = 0.8, font = 2, col = "black")
plot(listw_knn8, coords, add = TRUE, col = "red")



#K=9
coords_knn <- st_coordinates(st_point_on_surface(jatim_shp_valid))
nb_knn9 <- knearneigh(coords_knn, k = 9)
nb_knn9
nb_knn9 <- knn2nb(nb_knn9)
listw_knn9 <- nb2listw(nb_knn9, style = "W")
listw_knn9

##Visualisasi Pembobot
jatim_utm <- st_transform(jatim_shp_valid, 32749)
coords <- st_coordinates(st_point_on_surface(jatim_utm))
idx_gresik <- which(jatim_utm$Kab_Kota == "GRESIK")
coords[idx_gresik, ] <- st_coordinates(st_centroid(jatim_utm[idx_gresik, ]))
plot(st_geometry(jatim_utm),
     col = "grey90",
     border = "grey40",
     main = "Matriks Pembobot KNN (UTM Zone 49S)",
     axes = TRUE)
bb <- st_bbox(jatim_utm)
abline(v = seq(bb["xmin"], bb["xmax"], by = 100000),
       h = seq(bb["ymin"], bb["ymax"], by = 100000),
       col = "grey70", lty = 3)
text(coords[, 1], coords[, 2],
     labels = 1:nrow(jatim_shp_valid),
     cex = 0.8, font = 2, col = "black")
plot(listw_knn2, coords, add = TRUE, col = "red")
legend("bottomleft",
       legend = c("Kode wilayah (1–38)",
                  "Garis kedekatan (KNN)",
                  "Batas wilayah"),
       col = c("black", "red", "grey40"),
       lty = c(NA, 1, 1),
       lwd = c(NA, 1.5, 1),
       cex = 0.8,
       bty = "n")

# Pastikan geometry valid
data_valid <- st_make_valid(data)
label_coords <- st_coordinates(st_point_on_surface(data_valid))
data_label <- cbind(data_valid, label_coords)
idx_gresik <- which(data_label$Kab_Kota == "GRESIK")
data_label[idx_gresik, c("X", "Y")] <- 
  st_coordinates(st_centroid(data_valid[idx_gresik, ]))

##Uji Moran
moran.test(pc1, listw_knn2)
moran.test(pc1, listw_knn3)
moran.test(pc1, listw_knn4)
moran.test(pc1, listw_knn5)
moran.test(pc1, listw_knn6)
moran.test(pc1, listw_knn7)
moran.test(pc1, listw_knn8)
moran.test(pc1, listw_knn9)

#Standarisasi Data
data2 <- dplyr::select(data_var,'X2','X3','X4','X5','X7') 
data_std <- data.frame(scale(data2))
data_std

#Metode SKATER---------
#perhitungan jarak euclidean menurut matriks pembobot
nb_obj <- listw_knn2$neighbours
jarak_euclid <- nbcosts(nb_obj, data_std)
jarak_euclid


#Pembentukan MST
coords_knn <- st_coordinates(st_point_on_surface(jatim_shp_valid))
nb_knn2 <- knearneigh(coords_knn, k = 2) 
nb_obj_asym <- knn2nb(nb_knn2) 
jarak_euclid_asym <- spdep::nbcosts(nb_obj_asym, as.matrix(data_std))
jarak_euclid_asym
edges <- data.frame(
  from = rep(1:length(nb_obj_asym), lengths(nb_obj_asym)),
  to   = unlist(nb_obj_asym),
  weight = unlist(jarak_euclid_asym)
)
edges_unique <- edges[!duplicated(t(apply(edges[, 1:2], 1, sort))), ]
g <- igraph::graph_from_data_frame(edges_unique, directed = FALSE)
mst_graph <- igraph::mst(g, weights = igraph::E(g)$weight) 
mst_graph
edges_mst_final <- igraph::as_data_frame(mst_graph, what = "edges")
edges_mst_final


# Visualisasi
edges_mst_df <- as.data.frame(edges_mst_final)
colnames(edges_mst_df) <- c("from", "to", "weight")
edges_mst_df$from <- as.integer(edges_mst_df$from)
edges_mst_df$to <- as.integer(edges_mst_df$to)
mst_lines <- data.frame(
  x    = coords[edges_mst_df$from, 1],
  y    = coords[edges_mst_df$from, 2],
  xend = coords[edges_mst_df$to,   1],
  yend = coords[edges_mst_df$to,   2])
coords_df <- data.frame(
  X = coords[, 1], 
  Y = coords[, 2], 
  Label = jatim_shp_valid$Kab_Kota)

ggplot() +
  geom_sf(data = jatim_shp_valid, fill = "grey80", color = "white") +

  geom_segment(data = mst_lines,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "blue", linewidth = 0.8) +
  
  geom_point(data = coords_df,
             aes(x = X, y = Y),
             color = "red", size = 1.5) +
  
  geom_text(data = coords_df, 
            aes(x = X, y = Y, label = coords_df$Label), # <-- Akses langsung $Label
            size = 2, vjust = -0.5) +
  
  labs(title = "Minimum Spanning Tree (MST) - Jawa Timur",
       subtitle = "K = 2 (K-Nearest Neighbor)",
       caption = "Sudah disinkronkan: vertex MST = kabupaten/kota") +
  
  coord_sf(xlim = c(110.5, 116.5), ylim = c(-8.8, -6), expand = FALSE) +
  theme_minimal()


# Jalankan SKATER
cluster1 <- spdep::skater(mst_edges, as.matrix(data_std),method = "euclidean", ncuts = 0)


#Partisi MST k = 2 dengan metode SKATER ---
cluster2 <- spdep::skater(mst_edges, as.matrix(data_std), method = "euclidean", ncuts = 1)
table(cluster2$groups)
cluster_members <- split(1:length(cluster2$groups), cluster2$groups)
cluster_members


# --- Partisi MST k = 3 dengan metode SKATER ---
cluster3 <- spdep::skater(mst_edges, as.matrix(data_std), method = "euclidean", ncuts = 2)
table(cluster3$groups)
cluster_members <- split(1:length(cluster3$groups), cluster3$groups)
cluster_members

#Plot hasil klaster
plot(st_geometry(jatim_shp_valid), col = "darkgrey",border = "white", 
     main = "Hasil Klastering SKATER (3 Klaster )")
plot(cluster3, coords, cex.labels = 0.6, groups.colors = c("red", "blue", "darkgreen"), 
     cex.circles = 0.08, add = TRUE )
legend( "topleft", legend = paste("Klaster", unique(cluster3$groups)), 
        col = c("red", "blue", "darkgreen"), pch = 19, pt.cex = 1 )

# --- Partisi MST k = 4 dengan metode SKATER ---
cluster4 <- spdep::skater(mst_edges, as.matrix(data_std), method = "euclidean", ncuts = 3)
table(cluster4$groups)
cluster_members <- split(1:length(cluster4$groups), cluster4$groups)
cluster_members

#Plot hasil klaster
plot(st_geometry(jatim_shp_valid), col = "darkgrey",border = "white", 
     main = "Hasil Klastering SKATER (4 Klaster )")
plot(cluster4, coords, cex.labels = 0.6, groups.colors = c("red", "blue", "darkgreen", "darkorange"), 
     cex.circles = 0.08, add = TRUE )
legend( "topleft", legend = paste("Klaster", unique(cluster4$groups)), 
        col = c("red", "blue", "darkgreen", "darkorange"), pch = 19, pt.cex = 1 )

# --- Partisi MST k = 5 dengan metode SKATER ---
cluster5 <- spdep::skater(mst_edges, as.matrix(data_std), method = "euclidean", ncuts = 4)
table(cluster5$groups)
cluster_members <- split(1:length(cluster5$groups), cluster5$groups)
cluster_members

#Plot hasil klaster
plot(st_geometry(jatim_shp_valid), col = "darkgrey",border = "white", 
     main = "Hasil Klastering SKATER (5 Klaster )")
plot(cluster5, coords, cex.labels = 0.6, groups.colors = c("red", "blue", "darkgreen", "darkorange", "purple"), 
     cex.circles = 0.08, add = TRUE )
legend( "topleft", legend = paste("Klaster", unique(cluster5$groups)), 
        col = c("red", "blue", "darkgreen", "darkorange", "purple"), pch = 19, pt.cex = 1 )

# --- Partisi MST k = 6 dengan metode SKATER ---
cluster6 <- spdep::skater(mst_edges, as.matrix(data_std), method = "euclidean", ncuts = 5)
table(cluster6$groups)
cluster_members <- split(1:length(cluster6$groups), cluster6$groups)
cluster_members

#Plot hasil klaster
plot(st_geometry(jatim_shp_valid), col = "darkgrey",border = "white", 
     main = "Hasil Klastering SKATER (6 Klaster )")
plot(cluster6, coords, cex.labels = 0.6, groups.colors = c("red", "blue", "darkgreen", "darkorange", "purple", "cyan"), 
     cex.circles = 0.08, add = TRUE )
legend( "topleft", legend = paste("Klaster", unique(cluster6$groups)), 
        col = c("red", "blue", "darkgreen", "darkorange", "purple", "cyan"), pch = 19, pt.cex = 1 )

# --- Partisi MST k = 7 dengan metode SKATER ---
cluster7 <- spdep::skater(mst_edges, as.matrix(data_std), method = "euclidean", ncuts = 6)
table(cluster7$groups)
cluster_members <- split(1:length(cluster7$groups), cluster7$groups)
cluster_members

#Plot hasil klaster
plot(st_geometry(jatim_shp_valid), col = "darkgrey",border = "white", 
     main = "Hasil Klastering SKATER (7 Klaster )")
plot(cluster7, coords, cex.labels = 0.6, groups.colors = c("red", "blue", "darkgreen", "darkorange", "purple", "cyan", "magenta"), 
     cex.circles = 0.08, add = TRUE )
legend( "topleft", legend = paste("Klaster", unique(cluster7$groups)), 
        col = c("red", "blue", "darkgreen", "darkorange", "purple", "cyan", "magenta"), pch = 19, pt.cex = 1 )

# --- Partisi MST k = 8 dengan metode SKATER ---
cluster8 <- spdep::skater(mst_edges, as.matrix(data_std), method = "euclidean", ncuts = 7)
table(cluster8$groups)
cluster_members <- split(1:length(cluster8$groups), cluster8$groups)
cluster_members

#Plot hasil klaster
plot(st_geometry(jatim_shp_valid), col = "darkgrey",border = "white", 
     main = "Hasil Klastering SKATER (8 Klaster )")
plot(cluster8, coords, cex.labels = 0.6, groups.colors = c("red", "blue", "darkgreen", "darkorange", "purple", "cyan", "magenta", "sienna"), 
     cex.circles = 0.08, add = TRUE )
legend( "topleft", legend = paste("Klaster", unique(cluster8$groups)), 
        col = c("red", "blue", "darkgreen", "darkorange", "purple", "cyan", "magenta", "sienna"), pch = 19, pt.cex = 1 )

# --- Partisi MST k = 9 dengan metode SKATER ---
cluster9 <- spdep::skater(mst_edges, as.matrix(data_std), method = "euclidean", ncuts = 8)
table(cluster9$groups)
cluster_members <- split(1:length(cluster9$groups), cluster9$groups)
cluster_members

#Plot hasil klaster
plot(st_geometry(jatim_shp_valid), col = "darkgrey",border = "white", 
     main = "Hasil Klastering SKATER (9 Klaster )")
plot(cluster9, coords, cex.labels = 0.6, groups.colors = c("red", "blue", "darkgreen", "darkorange", "purple", "cyan", "magenta", "sienna", "gold"), 
     cex.circles = 0.08, add = TRUE )
legend( "topleft", legend = paste("Klaster", unique(cluster9$groups)), 
        col = c("red", "blue", "darkgreen", "darkorange", "purple", "cyan", "magenta", "sienna", "gold"), pch = 19, pt.cex = 1 )

# Ambil MST awal
mst_edges <- igraph::as_edgelist(mst_graph)
mst_edges <- apply(mst_edges, 2, as.numeric)
groups <- cluster3$groups
cut_edges <- mst_edges[groups[mst_edges[,1]] != groups[mst_edges[,2]], ]
cut_edges


# --- Validasi Cluster k = 2 ---
data_num <- data %>% 
  st_drop_geometry() %>% 
  select(X2, X3, X4, X5, X7)
xbartotal <- mean(as.matrix(data_num), na.rm = TRUE)
xbartotal
k2 <- data.frame(data_num, cluster = cluster2$groups)
k2c1 <- subset(k2, cluster == 1)
k2clus1 <- c(k2c1$X2, k2c1$X3, k2c1$X4, k2c1$X5, 
             k2c1$X7)
k2c2 <- subset(k2, cluster == 2)
k2clus2 <- c(k2c1$X2, k2c1$X3, k2c1$X4, k2c1$X5, 
             k2c1$X7)

# Membuat klaster berdasarkan kode 26–29
k2 <- data.frame(data_num)
k2$cluster <- ifelse(rownames(k2) %in% c(26, 27, 28, 29), 1, 2)

# Data klaster 1 dan 2
k2c1 <- subset(k2, cluster == 1)
k2c2 <- subset(k2, cluster == 2)

# Menggabungkan variabel numerik untuk masing-masing klaster
k2clus1 <- c(k2c1$X2, k2c1$X3, k2c1$X4, k2c1$X5, k2c1$X7)
k2clus2 <- c(k2c2$X2, k2c2$X3, k2c2$X4, k2c2$X5, k2c2$X7)


#Simpangan Baku Dalam CLuster
sdk2c1 <- sd(k2clus1)
sdk2c2 <- sd(k2clus2)
swk2 <- ((sdk2c1 + sdk2c2) / 2)
swk2

# Simpangan Baku Antar Klaster
sdk2c1bar <- mean(k2clus1)
sdk2c2bar <- mean(k2clus2)
sdk2c1.2 <- ((sdk2c1bar - xbartotal)^2)
sdk2c2.2 <- ((sdk2c2bar - xbartotal)^2)
sbk2 <- (((sdk2c1.2 + sdk2c2.2) / 1) ^ 0.5)
sbk2

# rasio simpangan baku
rk2 <- swk2 / sbk2
rk2


#Validitas Cluster k=3
k3 <- data.frame(data_num, cluster = cluster3$groups)
k3c1 <- subset(k3, cluster == 1)
k3clus1 <- c(k3c1$X2, k3c1$X3, k3c1$X4, k3c1$X5, 
             k3c1$X7)
k3c2 <- subset(k3, cluster == 2)
k3clus2 <- c(k3c2$X2, k3c2$X3, k3c2$X4, k3c2$X5, 
             k3c2$X7)
k3c3 <- subset(k3, cluster == 3)
k3clus3 <- c(k3c3$X2, k3c3$X3, k3c3$X4, k3c3$X5, 
             k3c3$X7)
#Simpangan Baku Dalam CLuster
sdk3c1 <- sd(k3clus1)
sdk3c2 <- sd(k3clus2)
sdk3c3 <- sd(k3clus3)
swk3 <- ((sdk2c1 + sdk2c2 + sdk3c3) / 3)
swk3
# Simpangan Baku Antar Klaster
sdk3c1bar <- mean(k3clus1)
sdk3c2bar <- mean(k3clus2)
sdk3c2bar <- mean(k3clus3)
sdk3c1.3 <- ((sdk3c1bar - xbartotal)^2)
sdk3c2.3 <- ((sdk3c2bar - xbartotal)^2)
sdk3c3.3 <- ((sdk3c2bar - xbartotal)^2)
sbk3 <- (((sdk3c1.3 + sdk3c2.3 + sdk3c3.3) / 2) ^ 0.5)
sbk3
# rasio simpangan baku
rk3 <- swk3 / sbk3
rk3


# Validitas Cluster k = 4
k4 <- data.frame(data_num, cluster = cluster4$groups)

k4c1 <- subset(k4, cluster == 1)
k4clus1 <- c(k4c1$X2, k4c1$X3, k4c1$X4, k4c1$X5, k4c1$X7)

k4c2 <- subset(k4, cluster == 2)
k4clus2 <- c(k4c2$X2, k4c2$X3, k4c2$X4, k4c2$X5, k4c2$X7)

k4c3 <- subset(k4, cluster == 3)
k4clus3 <- c(k4c3$X2, k4c3$X3, k4c3$X4, k4c3$X5, k4c3$X7)

k4c4 <- subset(k4, cluster == 4)
k4clus4 <- c(k4c4$X2, k4c4$X3, k4c4$X4, k4c4$X5, k4c4$X7)

# Simpangan baku dalam cluster
sdk4c1 <- sd(k4clus1)
sdk4c2 <- sd(k4clus2)
sdk4c3 <- sd(k4clus3)
sdk4c4 <- sd(k4clus4)
swk4 <- (sdk4c1 + sdk4c2 + sdk4c3 + sdk4c4) / 4
swk4

# Simpangan Baku Antar Klaster
sdk4c1bar <- mean(k4clus1)
sdk4c2bar <- mean(k4clus2)
sdk4c3bar <- mean(k4clus3)
sdk4c4bar <- mean(k4clus4)
sdk4c1.4 <- ((sdk4c1bar - xbartotal)^2)
sdk4c2.4 <- ((sdk4c2bar - xbartotal)^2)
sdk4c3.4 <- ((sdk4c3bar - xbartotal)^2)
sdk4c4.4 <- ((sdk4c4bar - xbartotal)^2)
sbk4 <- (((sdk4c1.4 + sdk4c2.4 + sdk4c3.4+sdk4c4.4) / 3) ^ 0.5)
sbk4
# rasio simpangan baku
rk4 <- swk4 / sbk4
rk4

# Validitas Cluster k = 5
k5 <- data.frame(data_num, cluster = cluster5$groups)

k5c1 <- subset(k5, cluster == 1)
k5clus1 <- c(k5c1$X2, k5c1$X3, k5c1$X4, k5c1$X5, k5c1$X7)

k5c2 <- subset(k5, cluster == 2)
k5clus2 <- c(k5c2$X2, k5c2$X3, k5c2$X4, k5c2$X5, k5c2$X7)

k5c3 <- subset(k5, cluster == 3)
k5clus3 <- c(k5c3$X2, k5c3$X3, k5c3$X4, k5c3$X5, k5c3$X7)

k5c4 <- subset(k5, cluster == 4)
k5clus4 <- c(k5c4$X2, k5c4$X3, k5c4$X4, k5c4$X5, k5c4$X7)

k5c5 <- subset(k5, cluster == 5)
k5clus5 <- c(k5c5$X2, k5c5$X3, k5c5$X4, k5c5$X5, k5c5$X7)

# Simpangan baku dalam cluster
sdk5c1 <- sd(k5clus1)
sdk5c2 <- sd(k5clus2)
sdk5c3 <- sd(k5clus3)
sdk5c4 <- sd(k5clus4)
sdk5c5 <- sd(k5clus5)
swk5 <- (sdk5c1 + sdk5c2 + sdk5c3 + sdk5c4 + sdk5c5) / 5
swk5

# Simpangan baku antar cluster
sdk5c1bar <- mean(k5clus1)
sdk5c2bar <- mean(k5clus2)
sdk5c3bar <- mean(k5clus3)
sdk5c4bar <- mean(k5clus4)
sdk5c5bar <- mean(k5clus5)

# Simpangan Baku Antar Klaster
sdk5c1bar <- mean(k5clus1)
sdk5c2bar <- mean(k5clus2)
sdk5c3bar <- mean(k5clus3)
sdk5c4bar <- mean(k5clus4)
sdk5c5bar <- mean(k5clus5)
sdk5c1.5 <- ((sdk5c1bar - xbartotal)^2)
sdk5c2.5 <- ((sdk5c2bar - xbartotal)^2)
sdk5c3.5 <- ((sdk5c3bar - xbartotal)^2)
sdk5c4.5 <- ((sdk5c4bar - xbartotal)^2)
sdk5c5.5 <- ((sdk5c5bar - xbartotal)^2)
sbk5 <- (((sdk5c1.5 + sdk5c2.5 + sdk5c3.5 + sdk5c4.5 + sdk5c5.5) / 4) ^ 0.5)
sbk5
# rasio simpangan baku
rk5 <- swk5 / sbk5
rk5

# Validitas Cluster k = 6
k6 <- data.frame(data_num, cluster = cluster6$groups)
k6c1 <- subset(k6, cluster == 1)
k6clus1 <- c(k6c1$X2, k6c1$X3, k6c1$X4, k6c1$X5, k6c1$X7)
k6c2 <- subset(k6, cluster == 2)
k6clus2 <- c(k6c2$X2, k6c2$X3, k6c2$X4, k6c2$X5, k6c2$X7)
k6c3 <- subset(k6, cluster == 3)
k6clus3 <- c(k6c3$X2, k6c3$X3, k6c3$X4, k6c3$X5, k6c3$X7)
k6c4 <- subset(k6, cluster == 4)
k6clus4 <- c(k6c4$X2, k6c4$X3, k6c4$X4, k6c4$X5, k6c4$X7)
k6c5 <- subset(k6, cluster == 5)
k6clus5 <- c(k6c5$X2, k6c5$X3, k6c5$X4, k6c5$X5, k6c5$X7)
k6c6 <- subset(k6, cluster == 6)
k6clus6 <- c(k6c6$X2, k6c6$X3, k6c6$X4, k6c6$X5, k6c6$X7)

# SB dalam klaster
sdk6c1 <- sd(k6clus1)
sdk6c2 <- sd(k6clus2)
sdk6c3 <- sd(k6clus3)
sdk6c4 <- sd(k6clus4)
sdk6c5 <- sd(k6clus5)
sdk6c6 <- sd(k6clus6)
swk6 <- (sdk6c1 + sdk6c2 + sdk6c3 + sdk6c4 + sdk6c5 + sdk6c6) / 6
swk6

# SB antar klaster
sdk6c1bar <- mean(k6clus1)
sdk6c2bar <- mean(k6clus2)
sdk6c3bar <- mean(k6clus3)
sdk6c4bar <- mean(k6clus4)
sdk6c5bar <- mean(k6clus5)
sdk6c6bar <- mean(k6clus6)

sdk6c1.6 <- (sdk6c1bar - xbartotal)^2
sdk6c2.6 <- (sdk6c2bar - xbartotal)^2
sdk6c3.6 <- (sdk6c3bar - xbartotal)^2
sdk6c4.6 <- (sdk6c4bar - xbartotal)^2
sdk6c5.6 <- (sdk6c5bar - xbartotal)^2
sdk6c6.6 <- (sdk6c6bar - xbartotal)^2

sbk6 <- ((sdk6c1.6 + sdk6c2.6 + sdk6c3.6 + sdk6c4.6 + sdk6c5.6 + sdk6c6.6) / 5) ^ 0.5
sbk6

rk6 <- swk6 / sbk6
rk6

# Validitas Cluster k = 7
k7 <- data.frame(data_num, cluster = cluster7$groups)
k7c1 <- subset(k7, cluster == 1)
k7clus1 <- c(k7c1$X2, k7c1$X3, k7c1$X4, k7c1$X5, k7c1$X7)
k7c2 <- subset(k7, cluster == 2)
k7clus2 <- c(k7c2$X2, k7c2$X3, k7c2$X4, k7c2$X5, k7c2$X7)
k7c3 <- subset(k7, cluster == 3)
k7clus3 <- c(k7c3$X2, k7c3$X3, k7c3$X4, k7c3$X5, k7c3$X7)
k7c4 <- subset(k7, cluster == 4)
k7clus4 <- c(k7c4$X2, k7c4$X3, k7c4$X4, k7c4$X5, k7c4$X7)
k7c5 <- subset(k7, cluster == 5)
k7clus5 <- c(k7c5$X2, k7c5$X3, k7c5$X4, k7c5$X5, k7c5$X7)
k7c6 <- subset(k7, cluster == 6)
k7clus6 <- c(k7c6$X2, k7c6$X3, k7c6$X4, k7c6$X5, k7c6$X7)
k7c7 <- subset(k7, cluster == 7)
k7clus7 <- c(k7c7$X2, k7c7$X3, k7c7$X4, k7c7$X5, k7c7$X7)

# SB dalam klaster
sdk7c1 <- sd(k7clus1)
sdk7c2 <- sd(k7clus2)
sdk7c3 <- sd(k7clus3)
sdk7c4 <- sd(k7clus4)
sdk7c5 <- sd(k7clus5)
sdk7c6 <- sd(k7clus6)
sdk7c7 <- sd(k7clus7)
swk7 <- (sdk7c1 + sdk7c2 + sdk7c3 + sdk7c4 + sdk7c5 + sdk7c6 + sdk7c7) / 7
swk7

# SB antar klaster
sdk7c1bar <- mean(k7clus1)
sdk7c2bar <- mean(k7clus2)
sdk7c3bar <- mean(k7clus3)
sdk7c4bar <- mean(k7clus4)
sdk7c5bar <- mean(k7clus5)
sdk7c6bar <- mean(k7clus6)
sdk7c7bar <- mean(k7clus7)

sdk7c1.7 <- (sdk7c1bar - xbartotal)^2
sdk7c2.7 <- (sdk7c2bar - xbartotal)^2
sdk7c3.7 <- (sdk7c3bar - xbartotal)^2
sdk7c4.7 <- (sdk7c4bar - xbartotal)^2
sdk7c5.7 <- (sdk7c5bar - xbartotal)^2
sdk7c6.7 <- (sdk7c6bar - xbartotal)^2
sdk7c7.7 <- (sdk7c7bar - xbartotal)^2

sbk7 <- ((sdk7c1.7 + sdk7c2.7 + sdk7c3.7 + sdk7c4.7 + sdk7c5.7 + sdk7c6.7 + sdk7c7.7) / 6) ^ 0.5
sbk7

rk7 <- swk7 / sbk7
rk7

# Validitas Cluster k = 8
k8 <- data.frame(data_num, cluster = cluster8$groups)
k8c1 <- subset(k8, cluster == 1)
k8clus1 <- c(k8c1$X2, k8c1$X3, k8c1$X4, k8c1$X5, k8c1$X7)
k8c2 <- subset(k8, cluster == 2)
k8clus2 <- c(k8c2$X2, k8c2$X3, k8c2$X4, k8c2$X5, k8c2$X7)
k8c3 <- subset(k8, cluster == 3)
k8clus3 <- c(k8c3$X2, k8c3$X3, k8c3$X4, k8c3$X5, k8c3$X7)
k8c4 <- subset(k8, cluster == 4)
k8clus4 <- c(k8c4$X2, k8c4$X3, k8c4$X4, k8c4$X5, k8c4$X7)
k8c5 <- subset(k8, cluster == 5)
k8clus5 <- c(k8c5$X2, k8c5$X3, k8c5$X4, k8c5$X5, k8c5$X7)
k8c6 <- subset(k8, cluster == 6)
k8clus6 <- c(k8c6$X2, k8c6$X3, k8c6$X4, k8c6$X5, k8c6$X7)
k8c7 <- subset(k8, cluster == 7)
k8clus7 <- c(k8c7$X2, k8c7$X3, k8c7$X4, k8c7$X5, k8c7$X7)
k8c8 <- subset(k8, cluster == 8)
k8clus8 <- c(k8c8$X2, k8c8$X3, k8c8$X4, k8c8$X5, k8c8$X7)

# SB dalam klaster
sdk8c1 <- sd(k8clus1)
sdk8c2 <- sd(k8clus2)
sdk8c3 <- sd(k8clus3)
sdk8c4 <- sd(k8clus4)
sdk8c5 <- sd(k8clus5)
sdk8c6 <- sd(k8clus6)
sdk8c7 <- sd(k8clus7)
sdk8c8 <- sd(k8clus8)
swk8 <- (sdk8c1 + sdk8c2 + sdk8c3 + sdk8c4 + sdk8c5 + sdk8c6 + sdk8c7 + sdk8c8) / 8
swk8

# SB antar klaster
sdk8c1bar <- mean(k8clus1)
sdk8c2bar <- mean(k8clus2)
sdk8c3bar <- mean(k8clus3)
sdk8c4bar <- mean(k8clus4)
sdk8c5bar <- mean(k8clus5)
sdk8c6bar <- mean(k8clus6)
sdk8c7bar <- mean(k8clus7)
sdk8c8bar <- mean(k8clus8)

sdk8c1.8 <- (sdk8c1bar - xbartotal)^2
sdk8c2.8 <- (sdk8c2bar - xbartotal)^2
sdk8c3.8 <- (sdk8c3bar - xbartotal)^2
sdk8c4.8 <- (sdk8c4bar - xbartotal)^2
sdk8c5.8 <- (sdk8c5bar - xbartotal)^2
sdk8c6.8 <- (sdk8c6bar - xbartotal)^2
sdk8c7.8 <- (sdk8c7bar - xbartotal)^2
sdk8c8.8 <- (sdk8c8bar - xbartotal)^2

sbk8 <- ((sdk8c1.8 + sdk8c2.8 + sdk8c3.8 + sdk8c4.8 + sdk8c5.8 + sdk8c6.8 + sdk8c7.8 + sdk8c8.8) / 7) ^ 0.5
sbk8

rk8 <- swk8 / sbk8
rk8

# Validitas Cluster k = 9
k9 <- data.frame(data_num, cluster = cluster9$groups)
k9c1 <- subset(k9, cluster == 1)
k9clus1 <- c(k9c1$X2, k9c1$X3, k9c1$X4, k9c1$X5, k9c1$X7)
k9c2 <- subset(k9, cluster == 2)
k9clus2 <- c(k9c2$X2, k9c2$X3, k9c2$X4, k9c2$X5, k9c2$X7)
k9c3 <- subset(k9, cluster == 3)
k9clus3 <- c(k9c3$X2, k9c3$X3, k9c3$X4, k9c3$X5, k9c3$X7)
k9c4 <- subset(k9, cluster == 4)
k9clus4 <- c(k9c4$X2, k9c4$X3, k9c4$X4, k9c4$X5, k9c4$X7)
k9c5 <- subset(k9, cluster == 5)
k9clus5 <- c(k9c5$X2, k9c5$X3, k9c5$X4, k9c5$X5, k9c5$X7)
k9c6 <- subset(k9, cluster == 6)
k9clus6 <- c(k9c6$X2, k9c6$X3, k9c6$X4, k9c6$X5, k9c6$X7)
k9c7 <- subset(k9, cluster == 7)
k9clus7 <- c(k9c7$X2, k9c7$X3, k9c7$X4, k9c7$X5, k9c7$X7)
k9c8 <- subset(k9, cluster == 8)
k9clus8 <- c(k9c8$X2, k9c8$X3, k9c8$X4, k9c8$X5, k9c8$X7)
k9c9 <- subset(k9, cluster == 9)
k9clus9 <- c(k9c9$X2, k9c9$X3, k9c9$X4, k9c9$X5, k9c9$X7)

# SB dalam klaster
sdk9c1 <- sd(k9clus1)
sdk9c2 <- sd(k9clus2)
sdk9c3 <- sd(k9clus3)
sdk9c4 <- sd(k9clus4)
sdk9c5 <- sd(k9clus5)
sdk9c6 <- sd(k9clus6)
sdk9c7 <- sd(k9clus7)
sdk9c8 <- sd(k9clus8)
sdk9c9 <- sd(k9clus9)
swk9 <- (sdk9c1 + sdk9c2 + sdk9c3 + sdk9c4 + sdk9c5 + sdk9c6 + sdk9c7 + sdk9c8 + sdk9c9) / 9
swk9

# SB antar klaster
sdk9c1bar <- mean(k9clus1)
sdk9c2bar <- mean(k9clus2)
sdk9c3bar <- mean(k9clus3)
sdk9c4bar <- mean(k9clus4)
sdk9c5bar <- mean(k9clus5)
sdk9c6bar <- mean(k9clus6)
sdk9c7bar <- mean(k9clus7)
sdk9c8bar <- mean(k9clus8)
sdk9c9bar <- mean(k9clus9)

sdk9c1.9 <- (sdk9c1bar - xbartotal)^2
sdk9c2.9 <- (sdk9c2bar - xbartotal)^2
sdk9c3.9 <- (sdk9c3bar - xbartotal)^2
sdk9c4.9 <- (sdk9c4bar - xbartotal)^2
sdk9c5.9 <- (sdk9c5bar - xbartotal)^2
sdk9c6.9 <- (sdk9c6bar - xbartotal)^2
sdk9c7.9 <- (sdk9c7bar - xbartotal)^2
sdk9c8.9 <- (sdk9c8bar - xbartotal)^2
sdk9c9.9 <- (sdk9c9bar - xbartotal)^2

sbk9 <- ((sdk9c1.9 + sdk9c2.9 + sdk9c3.9 + sdk9c4.9 + sdk9c5.9 + sdk9c6.9 + sdk9c7.9 + sdk9c8.9 + sdk9c9.9) / 8) ^ 0.5
sbk9

rk9 <- swk9 / sbk9
rk9

cluster_members <- split(data$Kab_Kota, cluster3$groups)
cluster_members

x2.1 <- mean(k3c1$X2) 
x3.1 <- mean(k3c1$X3) 
x4.1 <- mean(k3c1$X4) 
x5.1 <- mean(k3c1$X5) 
x7.1 <- mean(k3c1$X7) 
c1 <- data.frame(x2.1,x3.1,x4.1,x5.1,x7.1) 
c1 

x2.2 <- mean(k3c2$X2) 
x3.2 <- mean(k3c2$X3) 
x4.2 <- mean(k3c2$X4) 
x5.2 <- mean(k3c2$X5) 
x7.2 <- mean(k3c2$X7) 
c2 <- data.frame(x2.2,x3.2,x4.2,x5.2,x7.2) 
c2 

x2.3 <- mean(k3c3$X2) 
x3.3 <- mean(k3c3$X3) 
x4.3 <- mean(k3c3$X4) 
x5.3 <- mean(k3c3$X5) 
x7.3 <- mean(k3c3$X7) 
c3 <- data.frame(x2.3,x3.3,x4.3,x5.3,x7.3) 
c3 
