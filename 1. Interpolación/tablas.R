divdifftable <- function(x, y, n = length(x) - 1){
  df <- data.frame(x,y)
  yt = y
  for (i in 1:n){
    yt = diff(df[,length(df)][1:length(yt)])
    xt = diff(df$x, lag = i)
    newcol = yt/xt
    df <- cbind(df, append(newcol, rep(0, i)))
    colnames(df) = c(colnames(df)[-length(df)], paste0("diffdiv_", i))
  }
  df
}


difftable <- function(x, y, n = length(x) - 1){
  df <- data.frame(x,y)
  yt = y
  for (i in 1:n){
    yt = diff(df[,length(df)][1:length(yt)])
    xt = diff(df$x, lag = i)
    df <- cbind(df, append(yt, rep(0, i)))
    colnames(df) = c(colnames(df)[-length(df)], paste0("dif0fdiv_", i))
  }
  df
}
