#' A class representing an extraction of kwic as raw character sequences from a corpora, ready to be printed.
#'
#' @slot ref character.
#' @slot left.context.slot character.
#' @slot right.context.slot character.
#' @slot keyword.slot character.
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
#' kwic(dickensl, "the", unit="char")
setClass(
  "KwicLine",
  slot = c(
    ref = "character",
    left.context.slot = "character",
    right.context.slot = "character",
    keyword.slot = "character",
    keyword = "character",
    leftspan = "numeric",
    rightspan = "numeric",
    nbr.match = "numeric",
    nbr.match.by.line = "numeric",
    keyword.match.length = "numeric"
  )
);

#' Print a summary of a KwicLine object.
#'
#' @param object KwicLine.
#'
#' @export
#'
#' @examples
#' data(dickensl, unit="line")
#' res <- kwic(dickensl, "the")
#' summary(res)
setMethod("summary", signature(object = "KwicLine"), function(object) {
  cat(paste(object@nbr.match, "lines of concordance for keyword: '", object@keyword, "'"));
});

#' Show a KwicLine object
#'
#' @param object KwicLine.
#'
#' @export
#'
#' @importFrom methods show
#' @examples
#' data(dickensl, unit="line")
#' res <- kwic(dickensl, "the")
#' show(res)
setMethod("show", signature(object="KwicLine"), function(object) {
  print(object);
});

#' Print a KwicLine object
#'
#' @param x KwicLine object: the set of kwic lines
#' @param from length-1 numeric vector: index of the first line to be printed
#' @param to length-1 numeric vector: index of the last line to be printed
#' @param sort.by length-1 character vector: sort lines by 'right' context or 'left' context ('none': corpus order, 'random': randomized order)
#' @param decreasing lenth-1 logical vector: is the sort to be done in increasing or decreasing order?
#' @param file length-1 character vector: url of the file to be used for writing the kwic lines
#' @param append length-1 logical vector: should the content of the file be erased or preserved
#' @param ... unused arguments
#'
#'
#' @export
#' @importFrom stringi stri_reverse
#'
#' @examples
#' data(dickensl, unit="line")
#' res <- kwic(dickensl, "the")
#' print(res)
setMethod("print", signature(x="KwicLine"), function(x, from=1, to=-1, sort.by="none", decreasing=FALSE, file="", append=FALSE, ...) {
  stopifnot(length(from) == 1 & is.numeric(from));
  stopifnot(length(to) == 1 & is.numeric(to));

  if (to < 1) to <- x@nbr.match;
  if (to > x @nbr.match) stop(paste("'to' cannot be greater than the number of matches (", x@nbr.match, ")", sep=""));
  if (from < 1) stop("'from' cannot be less than 1");
  if (from > x@nbr.match) stop(paste("'from' cannot be greater than the number of matches (", x@nbr.match, ")", sep=""));
  if (from > to) stop("'from' cannot be greater than 'to'");

  ## Ordering of the lines
  stopifnot(!is.null(sort.by));
  stopifnot(length(sort.by) == 1 & is.character(sort.by));
  stopifnot(length(decreasing) == 1 & is.logical(decreasing));
  if (sort.by == "right") {
    o <- order(x@right.context.slot, decreasing=decreasing)
  } else if (sort.by == "none") {
    o <- 1:x@nbr.match;
  } else if (sort.by == "left") {
    o <- order(stringi::stri_reverse(x@left.context.slot), decreasing=decreasing)
  } else {
    o <- 1:length(x@right.context.slot);
  }

  ## Selection of the lines to be printed
  o <- o[from:to];

  keyword.match.length <- x@keyword.match.length[o];
  ref <- x@ref[o];
  left.context.slot <- x@left.context.slot[o];
  keyword.slot <- x@keyword.slot[o];
  right.context.slot <- x@right.context.slot[o];

  ## Creating of strings to be printed
  keyword.slot.max.length <- max(c(keyword.match.length, nchar(x@keyword)));
  fmt <- paste0("%20s %",x@leftspan,"s", "%-", keyword.slot.max.length, "s", "%-", x@rightspan, "s\n");
  header <- sprintf(fmt, "ref", "left ", x@keyword, "  right");
  lines <- sprintf(fmt, ref, left.context.slot, keyword.slot, right.context.slot);

  ## Printing of lines
  cat(paste0("Number of match: ", x@nbr.match, "\n"), file=file, append=append)
  cat(paste0("Sorted by: ", sort.by, "\n"), file=file, append=append);
  cat(paste0("Number of lines printed: ", to-from+1, "; from: ", from, "; to: ", to, "\n"), file=file, append=append);
  cat(header, file=file, append=append);
  for (i in lines) cat(i, file=file, append=append)
});

