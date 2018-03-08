#' Create a kwic from vector, list, data.frame or other structure containing linguistic corpora
#'
#' @param corpus the corpus (various data structure)
#' @param pattern length-1 character vector or either regexpr or fixed string to be search for
#' @param left length-1 integer vector : number of chars/tokens (see unit) on the right size
#' @param right length-1 integer vector : number of chars/tokens (see unit) on the left size
#' @param unit length-1 character vector : one of "char" or "token" : defines the left and right contexts as number of character or as number of words
#' @param fixed length-1 logical vector : is the pattern argument to be interpreted as a regexpr or as a fixed string
#' @param ... unused arguments
#' @param ref character vectors: the name for the different parts of the corpus
#'
#' @return a KwicLine or KwicWord object (depending on the value of unit)
#'
#' @references Text Interchange Formats : https://github.com/ropensci/tif
#'
#' @name kwic
#' @exportMethod kwic
setGeneric("kwic", function(corpus,
                            pattern,
                            left = ifelse(unit=="char", 20, 5),
                            right = ifelse(unit=="char", 20, 5),
                            unit = "char",
                            fixed = TRUE,
                            ref = NULL,
                            ...) {
  return(standardGeneric("kwic"))
})

#' @export
#' @rdname kwic
#' @examples
#' # Concordance with a vector of untokenized strings
#' data(dickensv)
#' kwic(dickensv, "the")
setMethod("kwic", "character", function(corpus,
                                        pattern,
                                        left = 20,
                                        right = 20,
                                        unit = "char",
                                        fixed = TRUE,
                                        ref = names(corpus)) {
  if (unit == "char") {
    return(.kwic_line(
      corpus,
      pattern,
      left,
      right,
      fixed = fixed,
      ref = ref
    ))
  } else if (unit == "token") {
    stop("Unavailable for untokenized corpus")
  } else {
    stop(paste0("Unknown unit type: ", unit, ". Available unit: 'char', 'token'"))
  }
})

#' @export
#' @rdname kwic
#'
#' @examples
#' # Concordance with a list of tokens
#' data(dickensl)
#' kwic(dickensl, "the")
#'
setMethod("kwic", "list", function(corpus,
                                   pattern,
                                   left = 20,
                                   right = 20,
                                   unit = "char",
                                   fixed = TRUE,
                                   ref = names(corpus)) {
  if (unit == "char") {
    lines <- lapply(corpus, function(x)
      paste(x, collapse = " "))

    lines <- as.character(lines)

    return(.kwic_line(
      lines,
      pattern,
      left,
      right,
      fixed = fixed,
      ref = ref
    ))

  } else if (unit == "token") {
    return(
      .kwic_word(
        corpus = corpus,
        pattern = pattern,
        left = left,
        right = right,
        fixed = fixed,
        ref = ref
      )
    )
  } else {
    stop(paste0("Unknown unit type: ", unit, ". Available unit: 'char', 'token'"))
  }
})

#' "VCorpus" class
#'
#' @name VCorpus-class
#' @aliases VCorpus
#' @family VCorpus
#' @importFrom tm VCorpus
#' @exportClass VCorpus
setOldClass("VCorpus")

#' @export
#' @importFrom NLP content
#'
#' @rdname kwic
#' @examples
#' # Concordance with a tm object
#' library(tm)
#' data(acq)
#' kwic(acq, "stock")
setMethod("kwic", "VCorpus", function(corpus,
                                      pattern,
                                      left = 20,
                                      right = 20,
                                      unit = "char",
                                      fixed = TRUE,
                                      ref = names(corpus)) {
  stopifnot(is.character(pattern))
  stopifnot(length(pattern) == 1)
  if (unit == "char") {
    lines <-
      sapply(corpus, function(x)
        paste(content(x), collapse = " "))
    lines <- as.character(lines)
    return(.kwic_line(
      lines,
      pattern,
      left,
      right,
      fixed = fixed,
      ref = ref
    ))
  } else if (unit == "token") {
    return(.kwic_word(
      lapply(corpus, content),
      pattern,
      left,
      right,
      fixed = fixed,
      ref = ref
    ))
  } else {
    stop(paste0("Unknown unit type: ", unit, ". Available unit: line, token"))
  }
})

#' @param token.column length-1 character vector : the name of the column containing the occurrences. `token` is the default, according to Text Interchange Formats (see reference).
#' @param id.column length-1 character vector : the name column of the column for creating textual unit you don't wan't the kwic to cross. `doc_id` is the default, as it is supposed to exist in all data.frame according to Text Interchange Formats (see reference).
#' @param interlinearize.with character vector : the name of other column with which one can search.
#'
#' @rdname kwic
#'
#' @export
#' @examples
#' # Concordance with a data frame. Defaults are used for the arguments
#' # 'token.column' 'id.column' (ie column names 'token' and 'doc_id')
#' data(dickensdf)
#' kwic(dickensdf, "the")
setMethod("kwic", "data.frame", function(corpus,
                                         pattern,
                                         left = 5,
                                         right = 5,
                                         unit = "char",
                                         ref=NULL, # = corpus[[id.column]]
                                         token.column = "token",
                                         id.column = "doc_id",
                                         interlinearize.with = NULL) {
  if (!token.column %in% colnames(corpus))
    stop(paste("Unknown token column:", token.column))

  if (!id.column %in% colnames(corpus))
    stop(paste("Unknown id column:", id.column))

  token.column <- corpus[[token.column]]

  if (is.null(ref)) {
    ref <- corpus[[id.column]]
  }

  ids <- corpus[[id.column]]
  tokensl <- split(token.column, ids)

  if (!is.null(interlinearize.with))
    stop("not implemented yet")

  kwic(tokensl,
       pattern,
       left,
       right,
       unit = unit,
       ref = ref)
})

#' Private function. Create a KwicLine object.
#'
#' @param lines character vector : untokenized strings.
#' @param pattern length-1 character vector or either regexpr or fixed string
#' @param left length-1 integer vector : number of chars on the right size
#' @param right length-1 integer vector : number of chars on the left size
#' @param fixed length-1 logical vector : is the pattern argument to be interpreted as a regexpr or as fixed string
#'
#' @return a KwicLine object
.kwic_line <- function(lines, pattern, left, right, fixed, ref) {
  if (is.null(ref))
    ref <- 1:length(lines)
  ref <- as.character(ref)

  regexp.res <- gregexpr(pattern = pattern,
                         text = lines,
                         fixed = fixed)

  matching.lines <-
    sapply(regexp.res, function(line.res) {
      line.res[1] != -1
    })

  if (!any(matching.lines))
    return(NULL)

  regexp.res <- regexp.res[matching.lines]
  nbr.occ.by.lines <- sapply(regexp.res, length)

  lines <- lines[matching.lines]
  ref <- ref[matching.lines]

  before <- paste(rep(" ", left + 1), collapse = "")
  after <- paste(rep(" ", right + 1), collapse = "")
  lines <- paste(before, lines, after, sep = "")

  lines.duplicated <- rep(lines, nbr.occ.by.lines)
  ref <- rep(ref, nbr.occ.by.lines)

  regexp.start.flat <-
    unlist(regexp.res) + left + 1 # + left + 1 since we have prefixed each line with + left + 1 white space
  regexp.length.flat <-
    unlist(lapply(regexp.res, attr, "match.length"))
  keyword.end.boundary <-
    regexp.start.flat + regexp.length.flat

  left.context.slot <-
    substr(x = lines.duplicated,
           start = regexp.start.flat - left,
           stop = regexp.start.flat - 1)
  keyword.slot <-
    substr(x = lines.duplicated,
           start = regexp.start.flat,
           stop = keyword.end.boundary - 1)
  right.context.slot <-
    substr(x = lines.duplicated,
           start = keyword.end.boundary,
           stop = keyword.end.boundary + right)

  return(
    new(
      "KwicLine",
      ref = ref,
      left.context.slot = left.context.slot,
      right.context.slot = right.context.slot,
      keyword.slot = keyword.slot,
      keyword = pattern,
      leftspan = left,
      rightspan = right,
      nbr.match = sum(nbr.occ.by.lines),
      nbr.match.by.line = nbr.occ.by.lines,
      keyword.match.length = regexp.length.flat
    )
  )
}

#' Private function. Create a KwicWord object.
#'
#' @param corpus a list of character vector : a tokenized corpus.
#' @param pattern length-1 character vector or either regexpr or fixed string
#' @param left length-1 integer vector : number of chars on the right size
#' @param right length-1 integer vector : number of chars on the left size
#' @param fixed length-1 logical vector : is the pattern argument to be interpreted as a regexpr or as fixed string
#'
#' @return a KwicWord object
.kwic_word <-
  function(corpus,
           pattern,
           left = 5,
           right = 5,
           fixed = TRUE,
           ref) {
    if (is.null(ref)) {
      if (is.null(names(corpus))) {
        ref <- paste("line", 1:length(corpus))
      } else {
        ref <- names(corpus)
      }
    } else {
      ref <- as.character(ref)
    }

    index.by.parts <-
      lapply(corpus, grep, pattern = pattern, fixed = fixed)

    nbr.match.by.parts <- sapply(index.by.parts, length)
    nbr.match <- sum(nbr.match.by.parts)

    match.flat <- unlist(index.by.parts)

    left.words.index <- matrix(0, nrow = nbr.match, 2)
    right.words.index <- matrix(0, nrow = nbr.match, 2)

    left.words.index[, 2] <- match.flat - 1
    left.words.index[, 1] <- match.flat - 1 - (left - 1)

    right.words.index[, 1] <- match.flat + 1
    right.words.index[, 2] <- match.flat + 1 + (right - 1)

    ## (if it goes out of context:
    part.length <- sapply(corpus, length)
    part.length <- rep(part.length, nbr.match.by.parts)

    left.words.index[left.words.index < 1] <- 0
    right.words.index[right.words.index[, 2] > part.length , 2] <-
      part.length[right.words.index[, 2] > part.length]
    right.words.index[right.words.index[, 1] > part.length,  2] <- 0
    right.words.index[right.words.index[, 1] > part.length,  1] <- 0
    ##)

    # the coordinate in the list.

    index.flat <- rep(1:length(corpus), nbr.match.by.parts)

    # generate the actual sequences

    left.l <-
      mapply(function(a, i, j) {
        corpus[[a]][i:j]
      } ,
      index.flat,
      left.words.index[, 1],
      left.words.index[, 2],
      SIMPLIFY = FALSE)

    right.l <-
      mapply(function(a, i, j) {
        corpus[[a]][i:j]
      },
      index.flat,
      right.words.index[, 1],
      right.words.index[, 2],
      SIMPLIFY = FALSE)


    left.m <- matrix("", nrow = length(left.l), ncol = left)

    right.m <- matrix("", nrow = length(right.l), ncol = right)

    for (i in 1:nbr.match) {
      l <- left.l[[i]]

      r <- right.l[[i]]

      if (length(l) > 0) {
        left.m[i, (left - length(l) + 1):left] <- l
      }
      if (length(r) > 0) {
        right.m[i, (1:length(r))] <- r
      }
    }

    keyword.slot <-
      unlist(mapply(function(part, i)
        part[i], corpus, index.by.parts, SIMPLIFY = FALSE))

    keyword.match.length <- nchar(keyword.slot)


    res <- cbind(left.m, keyword.slot, right.m)

    return(
      new(
        "KwicWord",
        ref = rep(ref, nbr.match.by.parts),
        concordances.matrix = res,
        keyword = pattern,
        leftspan = left,
        rightspan = right,
        nbr.match = nbr.match,
        nbr.match.by.line = nbr.match.by.parts,
        keyword.match.length = keyword.match.length
      )
    )
  }
