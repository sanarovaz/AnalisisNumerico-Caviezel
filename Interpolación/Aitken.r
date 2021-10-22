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
ys = c(1.498261, 4.11676, 10.4329, 25.3921)
yt = 57^xs

57^0.5 - aitken(0.5, xs, ys)
57^0.5 - aitken(0.5, xs, yt)

x = 0.5









