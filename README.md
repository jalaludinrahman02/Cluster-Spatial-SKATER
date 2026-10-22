# 🗺️ Analisis Regionalisasi Spasial & Klasterisasi Jawa Timur (Skripsi)
**Analisis Spasial menggunakan Algoritma SKATER, Minimum Spanning Tree, & Sistem Informasi Geografis**

![R](https://img.shields.io/badge/Language-R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![ArcMap](https://img.shields.io/badge/GIS-ArcMap-116530?style=for-the-badge&logo=arcgis&logoColor=white)
![Status](https://img.shields.io/badge/Status-Completed-00b4d8?style=for-the-badge)

---

## 📌 Project Overview
Repositori ini berisi dokumen lengkap, dataset, serta *script* pemrograman pendukung untuk penyusunan laporan skripsi. Penelitian ini berfokus pada analisis spasial dan pembentukan regionalisasi klaster di **38 Kabupaten/Kota di Provinsi Jawa Timur** berdasarkan indikator sosio-ekonomi dan kemiskinan menggunakan pendekatan *Spatial Analysis* (Algoritma **SKATER** - *Spatial 'K' Clusters Analysis by Tree Edge Removal* dan pembobotan berbasis wilayah).

---

## 🛠️ Tools & Libraries yang Digunakan
- **R Language (`spdep`, `igraph`, `tidyverse`)**: Digunakan untuk pengolahan data spasial, konstruksi *Minimum Spanning Tree* (MST), dan penerapan algoritma regionalisasi SKATER.
- **ArcMap (GIS)**: Digunakan untuk pemetaan tematik, pengelolaan batas wilayah administratif, dan ekspor peta sebaran (*shapefile* Jawa Timur).
- **Microsoft Excel**: Manajemen data awal indikator kemiskinan dan pembersihan tabel data (*data cleaning*).

---

## 📁 Repository Structure
```text
.
├── Batas Kabupaten/                  # Folder berisi file shapefile (.shp, .shx, .dbf) batas wilayah
├── Data Indikator Kemiskinan Jawa Timur 2024.xlsx  # Dataset utama indikator penelitian
├── Draft Laporan Skripsi.pdf         # Dokumen lengkap laporan skripsi
├── Peta Sebaran.mxd                  # File project ArcMap untuk visualisasi peta spasial
├── Syntax Code.R                     # Script R untuk analisis spasial & regionalisasi SKATER
├── Tugas-Akhir-Skripsi.Rproj         # RStudio Project File
└── README.md                         # Dokumentasi repositori
