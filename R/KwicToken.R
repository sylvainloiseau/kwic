#' #' A class representing an extraction of kwic as a sequences of tokens from a corpora, ready to be printed.
#'
#' @slot ref character.
#' @slot concordances.matrix matrix.
#' @slot keyword character.
#' @slot leftspan numeric.
#' @slot rightspan numeric.
#' @slot nbr.match numeric.
#' @slot nbr.match.by.line numeric.
#' @slot keyword.match.length numeric.
#'
#' @export
#'
#' @examples
#' data(dickensl)
#' kwic(dickensl, "the", unit="tokens")
setClass("KwicToken",
         slot=c(
           ref="character",
           concordances.matrix="matrix",
           keyword="character",
           leftspan="numeric",
           rightspan="numeric",
           nbr.match="numeric",
           nbr.match.by.line="numeric",
           keyword.match.length="numeric"
         ));

#' Print a summary of a KwicToken object
#'
#' @param object KwicToken.
#'
#' @export
#'
#' @examples
#' data(dickensl, unit="token")
#' res <- kwic(dickensl, "the")
#' summary(res)
setMethod("summary", signature(object = "KwicToken"), function(object) {
  cat(paste(object@nbr.match, "lines of concordance for keyword: '", object@keyword, "'"));
});

#' Show a KwicToken object
#'
#' @param object KwicToken.
#'
#' @export
#'
#' @examples
#' data(dickensl, unit="token")
#' res <- kwic(dickensl, "the")
#' show(res)
setMethod("show", signature(object="KwicToken"), function(object) {
  print(object);
});

#' Print a KwicToken object
#'
#' @param x KwicToken object: the set of kwic lines
#' @param from length-1 numeric vector: index of the first line to be printed
#' @param to length-1 numeric vector: index of the last line to be printed
#' @param sort.by length-1 character or numeric vector: sorting lines. 'none': corpus order; 'right' and 'left': first word after or before the keyword; 'random': randomized order; 0: keyword, n < 0 : the n^{th} word before the keyword; n > 0 : the n^{th} word after the keyword
#' @param decreasing lenth-1 logical vector: is the sort to be done in increasing or decreasing order?
#' @param file length-1 character vector: url of the file to be used for writing the kwic lines
#' @param append length-1 logical vector: should the content of the file be erased or preserved
#' @param ... unused arguments
#' @export
#'
#' @examples
#' data(dickensl, unit="token")
#' res <- kwic(dickensl, "the")
#' print(res)
setMethod("print", signature(x="KwicToken"), function(x, from=1, to=-1, sort.by="none", decreasing=FALSE, file="", append=FALSE, ...) {

  if (to<1) to <- x@nbr.match;
  if (to > x@nbr.match) stop(paste("'to' cannot be greater than the number of matches (", x@nbr.match, ")", sep=""));
  if (from < 1) stop("'from' cannot be less than 1");
  if (from > x@nbr.match) stop(paste("'from' cannot be greater than the number of matches (", x@nbr.match, ")", sep=""));
  if (from > to) stop("'from' cannot be greater than 'to'");

  ## Ordering of the lines
  stopifnot(!is.null(sort.by));
  stopifnot(length(sort.by) == 1);
  if (is.numeric(sort.by)) {
    if(sort.by > 0) {
      stopifnot(sort.by <= x@rightspan);
      i <- order(x@concordances.matrix[,x@leftspan+1+sort.by], decreasing=decreasing);
      p <- x@concordances.matrix[i,];
    } else if (sort.by < 0) {
      stopifnot(abs(sort.by) <= x@leftspan);
      i <- order(x@concordances.matrix[,x@leftspan + 1 - abs(sort.by)], decreasing=decreasing);
      p <- x@concordances.matrix[i,];
    } else {
      i <- order(x@concordances.matrix[,x@leftspan+1], decreasing=decreasing);
      p <- x@concordances.matrix[i,];
    }
  } else {
    if (sort.by == "right") {
      i <- order(x@concordances.matrix[,x@leftspan+2], decreasing=decreasing);
      p <- x@concordances.matrix[i,];
    } else if (sort.by == "none") {
      p <- x@concordances.matrix
    } else if (sort.by == "left") {
      i <- order(x@concordances.matrix[,x@leftspan], decreasing=decreasing);
      p <- x@concordances.matrix[i,];
    } else if (sort.by == "left") {
      neworder <- sample(1:x@nbr.match, x@nbr.match, replace = FALSE);
      p <- x@concordances.matrix[neworder,];
    }
  }

  ## Selection of the elements to be printed
  p <- p[from:to, ];
  ref <- x@ref[from:to];

  ## Creation of sub-string to be printed
  left.slot <- apply(p[,1:(x@leftspan)], 1, function(x) paste(x ,collapse=" "));
  right.slot <- apply(p[,((x@leftspan)+2):((x@leftspan)+1+(x@rightspan))], 1, function(x) paste(x, collapse=" "));
  keyword.slot <- p[,1+x@leftspan];

  ## Creation of lines to be printed
  left.slot.max.length <- max(nchar(left.slot))
  right.slot.max.length <- max(nchar(right.slot))
  keyword.slot.max.length <- max(nchar(keyword.slot));
  fmt <- paste0("%20s %", left.slot.max.length,"s", " %-",keyword.slot.max.length, "s ","%-",right.slot.max.length,"s\n");
  header <- sprintf(fmt, "ref", "left ", "keyword", "  right");
  lines <- sprintf(fmt, ref, left.slot, keyword.slot, right.slot);

  ## Printing lines
  cat(paste0("Number of match: ", x@nbr.match, "\n"), file=file, append=append);
  cat(paste0("Sorted by: ", sort.by, "\n"), file=file, append=append);
  cat(paste0("Number of lines printed: ", to-from+1, "; from: ", from, "; to: ", to, "\n"), file=file, append=append);
  cat(header, file=file, append=append);
  for (i in lines) cat(i, file=file, append=append);
});

