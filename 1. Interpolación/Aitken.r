aitken <- function(x, xs, ys) {
    df <- data.frame(x = xs, y = ys)
    len <- length(xs)
    if (len == 1){
        op = df[1,2]
    } else {
        a   = x-xs[len]
        aa  = a * aitken(x,xs[-len], ys[-len])
        b   = x-xs[len-1]
        bb  = b * aitken(x,xs[-len+1], ys[-len+1])
        op  = (aa - bb)/ (a - b)
    }
op
}

xs = c(0.1, 0.35, 0.58, 0.8)
ys = 57^xs

aitken(0.5, xs, ys)

xt = c(56.6, 56.85, 57.08, 57.3)
yt = sqrt(xt)

aitken(57, xt, yt) - sqrt(57)








