library(Hmisc)
library(xtable)

makePmTable <- function(
  means,
  errs,
  meanRound = 2,
  errRound = 2,
  rowName = rownames(means),
  colName = colnames(means)
) {
  mm <- matrix(
    paste(
      format(round(means, meanRound), nsmall = meanRound),
      '$\\pm$',
      format(round(errs, errRound), nsmall = errRound),
      sep = ''
    ),
    nrow = nrow(means),
    ncol = ncol(means)
  )
  dd <- as.data.frame(mm)
  rownames(dd) <- Hmisc::latexTranslate(rowName)
  colnames(dd) <- Hmisc::latexTranslate(colName)
  return(dd)
}

printPmTable <- function(
  tab,
  caption = '',
  align = rep(c('l','r'), times = c(1,ncol(tab))),
  sanitize.text.function = function(x) x,
  size = '\\normalsize',
  include.rownames = TRUE,
  ...
) {
  xtable::print.xtable(
    xtable::xtable(
      tab,
      align = align,
      caption = Hmisc::latexTranslate(caption)
    ),
    sanitize.text.function = sanitize.text.function,
    size = size,
    include.rownames = include.rownames,
    ...
  )
}
