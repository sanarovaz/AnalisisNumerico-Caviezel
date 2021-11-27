library(MASS)
library(Deriv)
library(rootSolve)

EML <- function(f, a, b, n){
  bern <- function(x){
    if (x == 2){return(1/12)}
    if (x == 4){return(-1/720)}
    if (x == 6){return(1/30240)}
  }
  
  {xs = seq(a, b, length.out = n+1)
  h = xs[2] - xs[1]}
  
  {tx = xs[2:(n+1)]
  A = sum(f(tx))
  
  B = f(b) - f(a)
  
  C = 0
  fss = c()
  for (i in 1:3){
    fsn = Deriv(f, "x", nderiv = 2*i-1)(b)
    fsp = Deriv(f, "x", nderiv = 2*i-1)(a)
    fss <- append(fss, c(fsn, fsp))
    C <- C + bern(2*i) * (fsn-fsp)
  }}
  

  aprox = A - 0.5*B - C
  
  {eq = paste0("\n\n                        MÉTODO DE EULER-MCLAURIN\n",
                'Integral definida entre ', a, '(n) y ', b , '(p)\n',
                'h = ', h, '    m = ', n,'\n\n',
                'SUMA(t = [p+1, n]) f(t) - 1/2 [f(n)-f(p)] - SUMA(s=[1, k]) b_2s [f^(2s-1)(n) - f^(2s-1)(p)] =\n',
                '= ', round(A, 4), ' - 1/2 ', B, ' - 1/12 [', fss[1], ' - ', fss[2],']  + 1/720 [', fss[3], ' - ', fss[4],']  - 1/30240 [', fss[5], ' - ', fss[6],'] =\n',
                '= ', round(A, 4), ' - 1/2 ', B, ' - ', C,' =\n',
                '= ', (aprox), '\n\n'
                
    )}
  
  {r = (b-a)*0.1
    xt = seq(from = a-r, to = b+r, r*0.05)
    yt = f(xt)
    
    if (length(uniroot.all(f, c(a,b))) == 0){
      if (f(a + r) > 0) {lims = c(0, max(yt))}
      else {lims = c(min(yt), 0)}
    } else {lims = c(min(yt), max(yt))}  
    
    plot(xt,yt,pch = -2,
         xlab = 'x', ylab = 'f(x)',
         ylim = lims)
    polygon(
      c(xt[xt >= a & xt <= b], b, a),
      c(yt[xt >= a & xt <= b], 0, 0),
      col = "#fdae6b"
    )
    lines(xt, rep(0,length(xt)))
    lines(xt,yt, lwd = 2,lty = 2)}
  
  {invisible(data.frame(A, eq, h, aprox))}
}


cat(EML(function(x) x^-2,
        4, 10, 5)$eq)

