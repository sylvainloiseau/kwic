#' Print Textual Corpora as Key Word in Context (aka concordance)
#'
#' @exportClass KwicLine
#' @exportClass KwicToken
#' @importFrom methods new
#' @importFrom methods setOldClass
#' @aliases kwic-package
"_PACKAGE"

#' Three sentences of dickens into a data frame
#'
#' Each row is a token. The variables are as follows:
#'
#' \itemize{
#'   \item token : wordform
#'   \item pos : part of speach
#'   \item lemma : the citation form of the token
#'   \item sentence numeric : the id of the sentence
#'   \item np numeric : the id of the phrase
#'   \item doc_id : the id of the document
#' }
#'
#' doc_id and token are two mandatory colums according to
#' the TIF (Text interchange format) specification (https://github.com/ropensci/tif)
#'
#' @format a data frame with 34 rows and 6 variables
"dickensdf"

#' Three sentences of dickens into a list
#'
#' Each slot is a character vector. The elements of the character vector contain the tokens.
#'
#' @format a list of 3 slots containing character vector.
"dickensl"

#' Three sentences of dickens into a character vector
#'
#' Each element of the character vector contain an untokenized sentence.
#'
#' @format a list of 3 slots containing character vector.
"dickensv"
