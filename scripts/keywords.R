# ładowaie bibliotek
library(wordcloud)

# zmiana katalogu roboczego
work_dir <- "D:\\KW\\PJNS12"
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
  "lda.R"
)
# source(source_file)
eval(
  parse(
    source_file, 
    encoding = "UTF-8"
  )
)

# zdefiniowanie lokalizacji katalogu na chmury tagów
clouds_dir <- create_path(
  output_dir,
  "clouds"
)
dir.create(clouds_dir, showWarnings = F)

# waga tf jako miara ważności słów
for (doc_no in 1:length(rownames(dtm_tf_all_m))) {
  print(rownames(dtm_tf_all_m)[doc_no])
  print(head(sort(dtm_tf_all_m[doc_no,], decreasing = T)))
}

# waga tfidf jako miara ważności słów
for (doc_no in 1:length(rownames(dtm_tfidf_all_m))) {
  print(rownames(dtm_tfidf_all_m)[doc_no])
  print(head(sort(dtm_tfidf_all_m[doc_no,], decreasing = T)))
}

# prawdopodobieństwo w LDA jako miara ważności słów
for (doc_no in 1:length(lda@documents)) {
  terms_importance <- c(results$topics[doc_no,]%*%results$terms)
  names(terms_importance) <- colnames(results$terms)
  print(rownames(results$topics)[doc_no])
  print(head(sort(terms_importance, decreasing = T)))
}

# chmury tagów
for (doc_no in 1:length(corpus)) {
  cloud_file <- create_path(
    clouds_dir,
    create_filename(
      corpus[[doc_no]]$meta$id,
      "png"
    )
  )
  png(cloud_file)
  par(mai = c(0,0,0,0))
  wordcloud(
    corpus[doc_no],
    max.words = 200,
    colors = brewer.pal(8, "YlGnBu")
  )
  dev.off()
}
