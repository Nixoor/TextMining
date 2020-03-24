#Włączenie biblioteki
library(tm)
library(hunspell)
library(stringr)

#zmiana katalogu roboczego
workDir <- "C:\\Users\\JS\\Documents\\GitHub\\TextMining\\"
setwd(workDir)

#definicja katalogów funkcjonalnych
inputDir <- ".\\data"
outputDir <- ".\\results"
scriptsDir <- ".\\scripts"
workspaceDir <- ".\\workspace"
dir.create(outputDir, showWarnings = FALSE)

#utworzenie korpusu dokumentów
corpusDir <- paste(inputDir,"Literatura - streszczenia - orygina�", sep="\\")
corpus <- VCorpus(
  DirSource(corpusDir,pattern = "*.txt",encoding = "UTF-8"),
  readerControl = list(langu = "pl_PL")
)

#wstępne przetwarzanie
corpus <-tm_map(corpus, removeNumbers)
corpus <-tm_map(corpus, removePunctuation)
corpus <-tm_map(corpus, content_transformer(tolower))
stoplistFile <- paste(inputDir,"\\","stopwords_pl.txt", sep="")
stoplist <- readLines(
  stoplistFile,
  encoding = "UTF-8"
)
corpus <-tm_map(corpus, removeWords, stoplist)
corpus <-tm_map(corpus, stripWhitespace)

remove_char <- content_transformer(function(x, pattern) gsub(pattern, "", x))
corpus <- tm_map(corpus, remove_char, intToUtf8(8722))
corpus <- tm_map(corpus, intToUtf8(190))


polish <- dictionary(lang="pl_PL")

lemmatize <-function(text) {
  simple_text <- str_trim(as.character(text))
  parsed_text <- strsplit(simple_text, split = " ")
  new_text_vec <- hunspell_stem(parsed_text[[1]], dict = polish)
  for (i in 1:length(new_text_vec)) {
    if (length(new_text_vec[[i]]) == 0) new_text_vec[i] <- parsed_text[[1]][i]
    if (length(new_text_vec[[i]]) > 1) new_text_vec[i] <- new_text_vec[[1]][i]
  }
  new_text <- paste(new_text_vec, collapse = " ")
  return(new_text)
}
corpus <- tm_map(corpus, content_transformer(lemmatize))

cut_extensions <- function(document) {
  meta(document, "id") <- gsub(pattern = "\\.txt$", replacement = "", meta(document, "id"))
  return(document)
}
corpus <- tm_map(corpus, cut_extensions)

preprocessedDir <- paste(
  outputDir,
  "Literatura - streszczenia - przetworzone",
  sep = "\\"
)
dir.create(preprocessedDir)
writeCorpus(corpus, path = preprocessedDir)

writeLines(as.character(corpus[1]))
writeLines(corpus[[1]]$content)
