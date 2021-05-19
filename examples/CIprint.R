CIprint <- function(CItest) {
  function(x, y, S, suffStat) {
    alpha <- parent.frame()$alpha
    labels <- parent.frame()$labels
    score <- CItest(x, y, S, suffStat)

    if (length(S))
      S.text <- paste(' |', paste(labels[S], collapse = ', '))
    else
      S.text <- ''

    if (score >= alpha)
      symbol <- ' ⫫ '
    else
      symbol <- ' ⫫̷ '

    print(paste0(labels[x], symbol, labels[y], S.text))
    return(score)
  }
}