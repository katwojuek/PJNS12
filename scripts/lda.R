# ładowaie bibliotek
library(topicmodels)

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
  "frequency_matrix.R"
)
# source(source_file)
eval(
  parse(
    source_file, 
    encoding = "UTF-8"
  )
)

# zdefiniowanie lokalizacji katalogu na wykresy
topics_dir <- create_path(
  output_dir,
  "topics"
)
dir.create(topics_dir, showWarnings = F)

# analiza ukrytej alokacji Dirichlet'a
n_topics <- 4
lda <- LDA(
  dtm_tf_all,
  n_topics,
  method = "Gibbs",
  control = list(
    burnin = 2000,
    thin = 100, 
    iter = 3000
  )
)
results <- posterior(lda)

# przygotowanie do wykresów
cols <- c("darkslateblue", "darkseagreen", "lightskyblue", "khaki", "darkred", "darkorange")

# prezentacja tematów
for (topic_no in 1:n_topics) {
  topic_file <- create_path(
    topics_dir,
    create_filename(
      topic_no,
      "png"
    )
  )
  png(topic_file)
  par(mai = c(1,2,1,1))
  terms <- tail(sort(results$terms[topic_no,]),20)
  barplot(
    terms, 
    horiz = TRUE,
    las = 1, 
    main = paste("Temat", topic_no),
    xlab = "Prawdopodobieństwo",
    col = cols[topic_no]
  )
  dev.off()
}

# prezentacja dokumentów
for (doc_no in 1:length(lda@documents)) {
  doc_file <- create_path(
    topics_dir,
    create_filename(
      rownames(results$topics)[doc_no],
      "png"
    )
  )
  png(doc_file)
  par(mai = c(1,2,1,1))
  topics <- results$topics[doc_no,]
  barplot(
    topics, 
    #horiz = TRUE,
    las = 1, 
    main = rownames(results$topics)[doc_no],
    xlab = "Prawdopodobieństwo",
    col = cols
  )
  dev.off()
}

# udział tematów w słowach
words_1 <- c("czas", "czarodziej", "czarownica", "wampir")
round(results$terms[,words_1], 2)

words_2 <- c("albus", "harry", "aslan", "bell")
round(results$terms[,words_2], 2)
