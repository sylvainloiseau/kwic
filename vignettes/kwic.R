## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(kwic)
data(dickensl)
kwic(dickensl, "the")

## ------------------------------------------------------------------------
data(dickensl)
k <- kwic(dickensl, "the")
print(k, sort.by="right")

## ------------------------------------------------------------------------
data(dickensl)
k <- kwic(dickensl, "(is|are|was)", fixed=FALSE)
print(k, sort.by="right")

## ------------------------------------------------------------------------
data(dickensl)
k <- kwic(dickensl, "\\b(is|are|was)\\b", fixed=FALSE)
print(k, sort.by="right")

## ------------------------------------------------------------------------
k <- kwic(dickensl, "the")
print(k, sort.by="right", from=3, to=4)

## ------------------------------------------------------------------------
data(dickensl)
k <- kwic(dickensl, "the", 5, 5, unit="token")
print(k)

## ------------------------------------------------------------------------
print(k, sort.by="left")

## ------------------------------------------------------------------------
print(k, sort.by=-2)
print(k, sort.by=2)

## ------------------------------------------------------------------------
d <- system.file("plaintexts", package="kwic")
corpus <- VCorpus(
  DirSource(directory=d, encoding="UTF-8"),
  readerControl = list(reader=readPlain)
)
kwic(corpus, "the")

## ------------------------------------------------------------------------
d <- system.file("taggedtexts", package="kwic")
files <- dir(d, pattern = "*.txt")

## ------------------------------------------------------------------------
corpusl <- lapply(
  files,
  function(x) read.table(
    paste(d, x, sep="/"),
    quote="", sep="\t", header = TRUE, fileEncoding="ISO-8859-1", stringsAsFactors = FALSE
    )
  )

corpus <- do.call("rbind", corpusl)

corpus$doc_id <- rep(files, times=sapply(corpusl, nrow))

kwic(corpus, "Paris", token.column="lemme", left=30, right=30) #, unit="token"

