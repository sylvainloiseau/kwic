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
mydir <- system.file("text", package="kwic")
corpus <- VCorpus(
  DirSource(directory=mydir, encoding="UTF-8"),
  readerControl = list(reader=readPlain)
)
kwic(corpus, "the")

