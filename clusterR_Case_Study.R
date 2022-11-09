# Install packages yang dibutuh untuk Studi Kasus
# Abaikan ini jika packages telah terinstall sebelumnya
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("reshape")

# Cari tau terlebih dahulu dimana directory kita berada
print(getwd())

# Set directory yang akan kita gunakan
setwd("C:/Data/Kuliah/Semester 3/Machine Learning/UTS/Studi Kasus")

# Tampilkan kembali diaman directory kita berada untuk memastikan
print(getwd())

# Gunakan Library dari packages yang dibutuhkan
library(ClusterR)
library(tidyverse)
library(reshape)

# Import datase lalu tampilkan sample data nya
# Disini kita menggunakan dataset California House Pricing Dataset
data_housing <- read.csv(file = 'housing.csv')
head(data_housing)

# Simpan juga data longitude dari data yang telah kita import
# Yang nantinya kita akan gunakan dalam plot
long_X = data_housing[, "longitude"]
# Simpan juga data latitude dari data yang telah kita import
# Yang nantinya kita akan gunakan dalam plot
lat_y = data_housing[, "latitude"]

# Plot  all the latitudes and longitudes
plot(long_X, lat_y)

# +------------------------------------------------------------------------+
# |                      START Clustering Region                           |
# +------------------------------------------------------------------------+

# Select longitude dan latitude nya lalu simpan ke dalam variable housing
# Data ini yang akan kita gunakan untuk clustering
housing = data_housing %>% select(longitude, latitude)

# Lakukan clustering K-Means menggunakan KMeans_arma dari clusterR package
km = KMeans_arma(housing,
                 clusters = 2,
                 n_iter = 300,
                 seed_mode = "random_subset",
                 verbose = T,
                 CENTROIDS = NULL)

# Tampilkan hasil clustering nya
head(km)

# Lakukan prediksi data dengan hasil clustering menggunakan predict_KMeans dari clusterR package
# Prediksi ini menghasilkan 2 cluster yaitu Cluster Region 1 (Region A) dan Cluster Region 2 (Region B)
pr = predict_KMeans(housing, km)

table(pr)

class(km) = 'matrix'

# Tampilkan hasil predict clustering menggunakan plot_2d
plot_2d(data = housing, clusters = as.vector(pr), centroids_medoids = as.matrix(km))

# Karena di dalam dataset kita belum mempunyai column hasil cluster region
# Maka dari itu kita buat column baru bernama cluster_region yang berisi hasil prediksi clustering
cluster_region <- pr
data_housing <- cbind(data_housing, cluster_region)
head(data_housing)

# Buat variable result_A untuk menyimpan Cluster Region 1 atau Region A
result_A = data_housing[data_housing$cluster_region == 1,]
head(result_A, n=10)

long_result_A = result_A[, "longitude"]
lat_result_A = result_A[, "latitude"]

# Tampilkan plot hasil dari result_A yaitu cluster 1 atau Region A
plot(long_result_A, lat_result_A, col="#00BFC4")

# Buat variable result_B untuk menyimpan Cluster Region 2 atau Region B
result_B = data_housing[data_housing$cluster_region == 2,]
head(result_B, n=10)

long_result_B = result_B[, "longitude"]
lat_result_B = result_B[, "latitude"]

# Tampilkan plot hasil dari result_B yaitu cluster 2 atau Region B
plot(long_result_B, lat_result_B, col="#C77CFF")

# +------------------------------------------------------------------------+
# |                       END Clustering Region                            |
# +------------------------------------------------------------------------+


# +------------------------------------------------------------------------+
# |       START Clustering House Value Cluster Region 1 atau Region A      |
# +------------------------------------------------------------------------+

# Selectmedian_house_value nya lalu simpan ke dalam variable house_data_A
# Data ini yang akan kita gunakan untuk clustering
# pada Cluster Region 1 atau Region A
house_data_A = result_A[, "median_house_value"]
head(house_data_A, n=10)

# Lakukan scaling untuk menormalikasikan nilai
# Sehingga model tidak bias ke nilai yang memiliki varians lebih tinggi.
house_data_A_scaled = scale(house_data_A)
head(house_data_A_scaled, n=10)

# Lakukan clustering K-Means menggunakan KMeans_arma dari clusterR package
# Terhadap data house_data_A_scaled
km_A = KMeans_arma(house_data_A_scaled,
                 clusters = 2,
                 n_iter = 300,
                 seed_mode = "random_subset",
                 verbose = T,
                 CENTROIDS = NULL)

# Tampilkan hasil clustering nya
head(km_A)

# Lakukan prediksi data dengan hasil clustering menggunakan predict_KMeans dari clusterR package
# Prediksi ini menghasilkan 2 cluster yaitu
# Cluster House Price 1 (House Price A) dan
# Cluster House Price 2 (House Price B)
pr_A = predict_KMeans(house_data_A_scaled, km_A)

# Tampilkan hasil prediksi
table(pr_A)

# Masukan hasil prediksi ke dalam data result_A sebagai column baru bernama cluster_house_price
cluster_house_price <- pr_A
result_A <- cbind(result_A, cluster_house_price)
head(result_A)

# Ambil data Cluster House Price 1 pada result_A
result_A_1 = result_A[result_A$cluster_house_price == 1,]

# Cari rata rata dari median_house_value pada result_A_1
median(result_A_1[, "median_house_value"])

long_result_A_1 = result_A_1[, "longitude"]
lat_result_A_1 = result_A_1[, "latitude"]

# Tampilkan plot untuk Cluster House Price 1 (House Price A) pada data result_A
plot(long_result_A_1, lat_result_A_1, col="#00BFC4")

# Ambil data Cluster House Price 2 pada result_A
result_A_2 = result_A[result_A$cluster_house_price == 2,]

# Cari rata rata dari median_house_value pada result_A_2
median(result_A_2[, "median_house_value"])

long_result_A_2 = result_A_2[, "longitude"]
lat_result_A_2 = result_A_2[, "latitude"]

# Tampilkan plot untuk Cluster House Price 2 (House Price B) pada data result_A
plot(long_result_A_2, lat_result_A_2, col="#00BFC4")

# +------------------------------------------------------------------------+
# |       END Clustering House Value Cluster Region 1 atau Region A        |
# +------------------------------------------------------------------------+


# +------------------------------------------------------------------------+
# |       START Clustering House Value Cluster Region 2 atau Region B      |
# +------------------------------------------------------------------------+

# Selectmedian_house_value nya lalu simpan ke dalam variable house_data_B
# Data ini yang akan kita gunakan untuk clustering
# pada Cluster Region 2 atau Region B
house_data_B = result_B[, "median_house_value"]
head(house_data_B, n=10)

# Lakukan scaling untuk menormalikasikan nilai
# Sehingga model tidak bias ke nilai yang memiliki varians lebih tinggi.
house_data_B_scaled = scale(house_data_B)
head(house_data_B_scaled, n=10)

# Lakukan clustering K-Means menggunakan KMeans_arma dari clusterR package
# Terhadap data house_data_B_scaled
km_B = KMeans_arma(house_data_B_scaled,
                   clusters = 2,
                   n_iter = 300,
                   seed_mode = "random_subset",
                   verbose = T,
                   CENTROIDS = NULL)

# Tampilkan hasil clustering nya
head(km_B)

# Lakukan prediksi data dengan hasil clustering menggunakan predict_KMeans dari clusterR package
# Prediksi ini menghasilkan 2 cluster yaitu
# Cluster House Price 1 (House Price A) dan
# Cluster House Price 2 (House Price B)
pr_B = predict_KMeans(house_data_B_scaled, km_B)

# Tampilkan hasil prediksi
table(pr_B)

# Masukan hasil prediksi ke dalam data result_A sebagai column baru bernama cluster_house_price
cluster_house_price <- pr_B
result_B <- cbind(result_B, cluster_house_price)
head(result_B)

# Ambil data Cluster House Price 1 pada result_B
result_B_1 = result_B[result_B$cluster_house_price == 1,]

# Cari rata rata dari median_house_value pada result_B_1
median(result_B_1[, "median_house_value"])

long_result_B_1 = result_B_1[, "longitude"]
lat_result_B_1 = result_B_1[, "latitude"]

# Tampilkan plot untuk Cluster House Price 1 (House Price A) pada data result_B
plot(long_result_B_1, lat_result_B_1, col="#00BFC4")

# Ambil data Cluster House Price 2 pada result_B
result_B_2 = result_B[result_B$cluster_house_price == 2,]

# Cari rata rata dari median_house_value pada result_B_2
median(result_B_2[, "median_house_value"])

long_result_B_2 = result_B_2[, "longitude"]
lat_result_B_2 = result_B_2[, "latitude"]

# Tampilkan plot untuk Cluster House Price 2 (House Price B) pada data result_B
plot(long_result_B_2, lat_result_B_2, col="#00BFC4")

# +------------------------------------------------------------------------+
# |        END Clustering House Value Cluster Region 2 atau Region B       |
# +------------------------------------------------------------------------+