# 🗺️ Pembentukan Klaster Spasial (SKATER) pada Kabupaten/Kota di Provinsi Jawa Timur Berdasarkan Indikator Kemiskinan Tahun 2024

![R](https://img.shields.io/badge/Language-R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![ArcMap](https://img.shields.io/badge/GIS-ArcMap-116530?style=for-the-badge&logo=arcgis&logoColor=white)
![Spatial Analysis](https://img.shields.io/badge/Domain-Spatial%20Statistics%20&%20SKATER-00b4d8?style=for-the-badge)
![Status](https://img.shields.io/badge/Status-Completed-success?style=for-the-badge)

---

## 📌 1. Project Overview

Repositori ini berisi dokumen dan kode sumber penelitian skripsi yang berfokus pada penerapan metode **Spatial 'K'luster Analysis by Tree Edge Removal (SKATER)** untuk mengelompokkan 38 Kabupaten/Kota di Provinsi Jawa Timur berdasarkan indikator kemiskinan tahun 2024[cite: 60, 63]. 

Pendekatan regionalisasi spasial ini digunakan untuk mengatasi keterbatasan metode klaster konvensional dengan tetap memperhatikan aspek keterhubungan geografis antarwilayah[cite: 60, 61]. Variabel yang digunakan mencakup:
* $X_1$: Laju Pertumbuhan Pengeluaran Rumah Tangga
* $X_2$: Rata-rata Lama Sekolah
* $X_3$: Tingkat Pengangguran Terbuka (TPT)[cite: 60]
* $X_4$: Angka Melek Huruf[cite: 60]
* $X_5$: Rumah Tangga dengan Sanitasi Layak[cite: 60]
* $X_6$: PDRB Perkapita[cite: 60]
* $X_7$: Indeks Pembangunan Manusia (IPM)[cite: 60]

---

## 📐 2. Metodologi dan Rumus Matematis

### A. Statistika Deskriptif & Standarisasi Data
* **Rata-rata:** Menunjukkan nilai pemusatan data[cite: 61].
* **Simpangan Baku:** Mengukur tingkat penyebaran data terhadap rata-rata, dirumuskan sebagai[cite: 62]:
  $$s = \sqrt{\frac{\sum_{i=1}^{n}(y_i - \bar{y})^2}{n-1}}$$
* **Standarisasi Z-score:** Digunakan untuk menyamakan skala satuan variabel sebelum analisis[cite: 62]:
  $$Z_i = \frac{x_i - \bar{x}}{s}$$

### B. Matriks Pembobot Spasial K-Nearest Neighbor (KNN)
Pembobot spasial disusun berdasarkan kedekatan jarak terdekat ($k$) antarwilayah. Pemilihan nilai $k$ optimal diuji secara iteratif dengan memaksimalkan nilai indeks Moran's I[cite: 62].

### C. Uji Autokorelasi Spasial (Indeks Moran's I)
Digunakan untuk menguji keterkaitan nilai variabel antarwilayah berdekatan[cite: 62], dengan statistik pengujian[cite: 63]:
$$I = \frac{n \sum_{i=1}^{n}\sum_{j=1}^{n} w_{ij}(x_i - \bar{x})(x_j - \bar{x})}{\left(\sum_{i=1}^{n}(x_i - \bar{x})^2\right) \sum_{i=1}^{n}\sum_{j=1}^{n} w_{ij}}$$

### D. Metode SKATER (Spatial K-Cluster Analysis by Tree Edge Removal)
1. **Minimum Spanning Tree (MST):** Membentuk struktur graf $G=(V,L)$ yang menghubungkan seluruh simpul lokasi dengan total bobot minimum menggunakan Jarak Euclidean[cite: 63]:
   $$d(i,j) = d(x_i, x_j) = \sqrt{\sum_{l=1}^{p}(x_{il} - x_{jl})^2}$$
2. **Partisi Rekursif MST:** Menghapus *edge* pada MST secara iteratif untuk meminimalkan jumlah kuadrat deviasi dalam kelompok ($SSD_c$) dengan fungsi objektif[cite: 63]:
   $$f(S_l^T) = SSD_T - (SSD_{Ta} + SSD_{Tb})$$

### E. Validasi Klaster (Rasio Simpangan Baku)
Kualitas klaster dievaluasi menggunakan rasio simpangan baku dalam klaster ($S_W$) terhadap antar klaster ($S_B$). Nilai rasio minimum menunjukkan homogenitas dalam klaster dan heterogenitas antar klaster yang paling optimal.

---

## 📈 3. Hasil Utama Analisis

* **Uji Autokorelasi:** Variabel Rata-rata Lama Sekolah ($X_2$), TPT ($X_3$), Angka Melek Huruf ($X_4$), Sanitasi Layak ($X_5$), dan IPM ($X_7$) terbukti memiliki autokorelasi spasial[cite: 60]. Nilai Moran's I optimum tercapai pada **$k = 2$**.
* **Jumlah Klaster Optimal:** Berdasarkan rasio simpangan baku terendah (**5,627**), jumlah klaster terbaik yang terbentuk adalah **3 klaster**[cite: 66]:
  * **Klaster 1:** Kategori Rendah[cite: 60]
  * **Klaster 2:** Kategori Tinggi[cite: 60]
  * **Klaster 3:** Kategori Menengah[cite: 60]

---

## 📁 4. Repository Structure

```text
.
├── Batas Kabupaten/                            # Folder shapefile (.shp) wilayah administrasi Jatim
├── Data Indikator Kemiskinan Jawa Timur 2024.xlsx # Dataset utama penelitian
├── Jurnal_Skripsi_Ahmad Jalaludin Rahman.pdf   # Dokumen naskah jurnal skripsi
├── Peta Sebaran.mxd                            # Project file ArcMap visualisasi peta tematik
├── Syntax Code.R                               # Script R untuk analisis spasial & SKATER
├── Tugas-Akhir-Skripsi.Rproj                   # RStudio Project File
└── README.md                                   # Dokumentasi repositori
