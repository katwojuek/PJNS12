# ładowaie bibliotek
library(dendextend)
library(corrplot)
library(flexclust)
library(proxy)

# zmiana katalogu roboczego
work_dir <- "L:\\lato21na22\\PJNS12"
setwd(work_dir)

# lokalizacja katalogu ze skryptami
scripts_dir <- ".\\scripts"

# zdefiniowanie funkcji do tworzenia ścieżek dostępu do plików
create_path <- function(parent, child) 
  paste(parent, child, sep = "\\")

# zdefiniowanie funkcji do tworzenia nazw plików
create_filename <- function(name, extension) 
  paste(name, extension, sep = ".")

# wczytanie i wykonanie skryptu frequency_matrix.R
source_file <- create_path(
  scripts_dir,
  "frequency_matrix.R"
)
# source(source_file)
eval(
  parse(
    source_file, 
    encoding = "UTF-8"
  )
)

# analiza skupień
# metoda hierarchiczna
# parametry metody:
# 1. macierz częstości
# a. waga (weighting)
# b. zakres zmiennych (bounds)
# 2. miara odległości (euclidean, jaccard, cosine)
# 3. sposób wyznaczania odległości skupień (complete, single, ward.D2)
 
# przygotowanie eksperymentów
doc_names <- rownames(dtm_tf_all)
doc_count <- length(doc_names)
legend <- paste(
  paste(
    "d",
    1:doc_count,
    sep = ""
  ),
  doc_names,
  sep = " - "
)
rownames(dtm_tf_all_m) <-  paste(
  "d",
  1:doc_count,
  sep = ""
)
rownames(dtm_bin_all_m) <-  paste(
  "d",
  1:doc_count,
  sep = ""
)
rownames(dtm_tfidf_2_16_m) <-  paste(
  "d",
  1:doc_count,
  sep = ""
)
clusters_pattern <- c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3)
cols <- c("lightskyblue", "darkseagreen", "hotpink", "darkred", "khaki", "darkslateblue", "darkorange")
cols_pattern <- c()
for (doc_no in 1:doc_count) {
  cols_pattern[doc_no] <- cols[clusters_pattern[doc_no]]
}
names(clusters_pattern) <- paste(
  "d",
  1:doc_count,
  sep = ""
)
names(cols_pattern) <- paste(
  "d",
  1:doc_count,
  sep = ""
)

# eksperyment 1
dist_matrix_1 <- dist(dtm_tf_all_m, method = "euclidean")
h_clust_1 <- hclust(dist_matrix_1, method = "complete")
plot(h_clust_1)
barplot(h_clust_1$height, names.arg = (doc_count-1):1)
dend_1 <- as.dendrogram(h_clust_1)
clusters_count_1 <- find_k(dend_1)$k
cols_dend_1 <- colour_branches(
  dend_1,
  k = clusters_count_1,
  col = cols[1:clusters_count_1]
)
plot(cols_dend_1)
legend(
  "topright",
  legend,
  cex = 0.3
)
cols_dend_1 <- colour_branches(
  dend_1,
  col = cols_pattern[dend_1 %>% labels]
)
plot(cols_dend_1)
legend(
  "topright",
  legend,
  cex = 0.3
)
clusters_1 <- cutree(
  h_clust_1, 
  k = clusters_count_1
)
clusters_matrix_1 <- matrix(
  0,
  doc_count,
  clusters_count_1
)
rownames(clusters_matrix_1) <- doc_names
for (doc_no in 1:doc_count) {
  clusters_matrix_1[doc_no, clusters_1[doc_no]] <- 1
}
corrplot(clusters_matrix_1)

# eksperyment 2
dist_matrix_2 <- dist(dtm_bin_all_m, method = "jaccard")
h_clust_2 <- hclust(dist_matrix_2, method = "single")
plot(h_clust_2)
barplot(h_clust_2$height, names.arg = (doc_count-1):1)
dend_2 <- as.dendrogram(h_clust_2)
clusters_count_2 <- find_k(dend_2)$k
cols_dend_2 <- colour_branches(
  dend_2,
  k = clusters_count_2,
  col = cols[1:clusters_count_2]
)
plot(cols_dend_2)
legend(
  "topright",
  legend,
  cex = 0.3
)
cols_dend_2 <- colour_branches(
  dend_2,
  col = cols_pattern[dend_2 %>% labels]
)
plot(cols_dend_2)
legend(
  "topright",
  legend,
  cex = 0.3
)
clusters_2 <- cutree(
  h_clust_2, 
  k = clusters_count_2
)
clusters_matrix_2 <- matrix(
  0,
  doc_count,
  clusters_count_2
)
rownames(clusters_matrix_2) <- doc_names
for (doc_no in 1:doc_count) {
  clusters_matrix_2[doc_no, clusters_2[doc_no]] <- 1
}
corrplot(clusters_matrix_2)
dist_matrix_2_m <- as.matrix(dist_matrix_2)
corrplot(dist_matrix_2_m)

# eksperyment 3
dist_matrix_3 <- dist(dtm_tfidf_2_16_m, method = "cosine")
h_clust_3 <- hclust(dist_matrix_3, method = "ward.D2")
plot(h_clust_3)
barplot(h_clust_3$height, names.arg = (doc_count-1):1)
dend_3 <- as.dendrogram(h_clust_3)
clusters_count_3 <- find_k(dend_3)$k
cols_dend_3 <- colour_branches(
  dend_3,
  k = clusters_count_3,
  col = cols[1:clusters_count_3]
)
plot(cols_dend_3)
legend(
  "topright",
  legend,
  cex = 0.3
)
cols_dend_3 <- colour_branches(
  dend_3,
  col = cols_pattern[dend_3 %>% labels]
)
plot(cols_dend_3)
legend(
  "topright",
  legend,
  cex = 0.3
)
clusters_3 <- cutree(
  h_clust_3, 
  k = clusters_count_3
)
clusters_matrix_3 <- matrix(
  0,
  doc_count,
  clusters_count_3
)
rownames(clusters_matrix_3) <- doc_names
for (doc_no in 1:doc_count) {
  clusters_matrix_3[doc_no, clusters_3[doc_no]] <- 1
}
corrplot(clusters_matrix_3)
dist_matrix_3_m <- as.matrix(dist_matrix_3)
corrplot(dist_matrix_3_m)

# analiza skupień
# metoda niehierarchiczna
# parametry metody:
# 1. macierz częstości
# a. waga (weighting)
# b. zakres zmiennych (bounds)
# 2. liczba skupień

# eksperyment 4
clusters_count_4 <- 3
k_means_4 <- kmeans(dtm_tfidf_2_16, centers = clusters_count_4)
clusters_4 <- k_means_4$cluster
clusters_matrix_4 <- matrix(
  0,
  doc_count,
  clusters_count_4
)
rownames(clusters_matrix_4) <- doc_names
for (doc_no in 1:doc_count) {
  clusters_matrix_4[doc_no, clusters_4[doc_no]] <- 1
}
corrplot(clusters_matrix_4)

# porównanie wyników eksperymentów
Bk_plot(
  dend_1,
  dend_2,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Indeks Fawlkes'a Mallows'a",
  ylab = "Indeks Fawlkes'a Mallows'a",
)
Bk_plot(
  dend_1,
  dend_3,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Indeks Fawlkes'a Mallows'a",
  ylab = "Indeks Fawlkes'a Mallows'a",
)
Bk_plot(
  dend_3,
  dend_2,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Indeks Fawlkes'a Mallows'a",
  ylab = "Indeks Fawlkes'a Mallows'a",
)

rand_exp1_pattern <- comPart(clusters_1, clusters_pattern)
rand_exp2_pattern <- comPart(clusters_2, clusters_pattern)
rand_exp3_pattern <- comPart(clusters_3, clusters_pattern)
rand_exp4_pattern <- comPart(clusters_4, clusters_pattern)
rand_exp1_exp2 <- comPart(clusters_1, clusters_2)
rand_exp1_exp4 <- comPart(clusters_1, clusters_4)
rand_exp2_exp4 <- comPart(clusters_2, clusters_4)
