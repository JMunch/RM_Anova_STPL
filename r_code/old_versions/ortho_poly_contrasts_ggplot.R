### Plotting orthogonal polynomial contrasts ###



# ggplot approach ---------------------------------------------------------


poly_fun1 <- function(x) {
    poly.fit.1$coefficients[1] + poly.fit.1$coefficients[2]*x
}

poly_fun2 <- function(x) {
    poly.fit.2$coefficients[1] + poly.fit.2$coefficients[2]*x + 
        poly.fit.2$coefficients[3]*(x^2)
}


poly_fun3 <- function(x) {
    poly.fit.3$coefficients[1] + poly.fit.3$coefficients[2]*x + 
        poly.fit.3$coefficients[3]*(x^2) + poly.fit.3$coefficients[4]*(x^3)
}


poly.fit.4 <- poly.fit.max

poly_fun4 <- function(x) {
    poly.fit.4$coefficients[1] + poly.fit.4$coefficients[2]*x + 
        poly.fit.4$coefficients[3]*(x^2) + poly.fit.4$coefficients[4]*(x^3) +
        poly.fit.4$coefficients[5]*(x^4)
}


ggplot(data = rma_data_long, aes(x = condition, y = value)) + geom_point() + 
    labs(col = "Order of \npolynomial", x = "Condition", y = "Value", title = "Orthogonal polynomial contrasts") +
    stat_function(fun = poly_fun1, aes(col = "1"), lwd = 1.2) + 
    stat_function(fun = poly_fun2, aes(col = "2"), lwd = 1.2) + 
    stat_function(fun = poly_fun3, aes(col = "3"), lwd = 1.2) + 
    stat_function(fun = poly_fun4)


# automatisiert -----------------------------------------------------------

poly_coef <- data.frame(matrix(0, ncol = k-1, nrow = k))


for(i in 1:maxpoly){
    pfv = paste("poly.fit.", i, sep = "")
    poly <- assign(pfv, lm(rma_data_long$value ~ poly(rma_data_long$condition, degree = i, raw = TRUE)))
    poly_coef[,i][1:(i+1)] <- poly$coef
    poly.fit.max = lm(rma_data_long$value ~ poly(rma_data_long$condition, degree = i, raw = TRUE))
}


g1 <- ggplot(data = rma_data_long, aes(x = condition, y = value)) + 
    geom_point() + 
    labs(col = "Order of \npolynomial", x = "Condition", y = "Value", title = "Orthogonal polynomial contrasts")

for(i in 1:(k-1)){
    poly_fun <- function(x){
        sum(as.vector(outer(x, 0:(k-1), FUN="^")) * poly_coef[, i])
    }
    poly_fun <- Vectorize(poly_fun)
    g1 <- (g1 + stat_function(fun = poly_fun, aes(col = "1"), lwd = 1.2))

    print(g1)
}





library(ggplot2)
# the base plot (with x limits)
xlim <- c(-5,5)
baseplot <- ggplot(data.frame(x = xlim), aes(x=x))

# the function
testfn <- function(x,a){sin(x-a)}
# a list of 10 calls (a = 1,...10)
list_fn <- lapply(seq_len(10), function(a){
    stat_function(fun = testfn, args = list(a=a))
})
# the plot
baseplot + list_fn[[2]]


poly_fun <- Vectorize(function(x, a){
    sum(as.vector(outer(x, 0:(k-1), FUN="^")) * a)
})

funs <- lapply(poly_coef, function(a){
    stat_function(fun = poly_fun, args = list(a = a))
})

g1 + funs





# stack -------------------------------------------------------------------

coef <- list(c(47, 2, 0, 0), 
             c(7, 42, -8, 0),
             c(78, -71, 43, -7))

my_data <- data.frame("x" = rep(1:4, times = 20))

cols <- c("1", "2", "3")

my_function <- Vectorize(function(x){
    sum(as.vector(outer(x, 0:3, FUN="^")) * coef[[i]])
})


g1 <- ggplot(data = my_data, aes(x = x)) 

for(i in 1:3){
    poly_fun <- function(x){
        sum(as.vector(outer(x, 0:(k-1), FUN="^")) * poly_coef[, i])
    }
    poly_fun <- Vectorize(poly_fun)
    g1 <- (g1 + stat_function(fun = my_function, aes(col = cols[i]), lwd = 1.2))
    
    print(g1)
}



ggplot(data = rma_data_long, aes(x = condition, y = value)) + 
    labs(col = "Order of \npolynomial", x = "Condition", y = "Value", title = "Orthogonal polynomial contrasts") +
    stat_function(fun = poly_fun1, aes(col = "1"), lwd = 1.2) + 
    stat_function(fun = poly_fun2, aes(col = "2"), lwd = 1.2) + 
    stat_function(fun = poly_fun3, aes(col = "3"), lwd = 1.2) + 
    stat_function(fun = poly_fun4)




# aktuelle Version --------------------------------------------------------


poly_coef <- data.frame(matrix(0, ncol = k-1, nrow = k))


for(i in 1:maxpoly){
    pfv = paste("poly.fit.", i, sep = "")
    poly <- assign(pfv, lm(rma_data_long$value ~ poly(rma_data_long$condition, degree = i, raw = TRUE)))
    poly_coef[,i][1:(i+1)] <- poly$coef
    poly.fit.max = lm(rma_data_long$value ~ poly(rma_data_long$condition, degree = i, raw = TRUE))
}


require(tidyverse)
poly_curve_data <- data.frame(x = seq(1, k, length.out = 100), 
                              tcrossprod(outer(seq(1, k, length.out = 100), 0:(k-1), `^`), do.call(rbind, poly_coef))) %>% 
    gather(var, y, -x)

g1 <- ggplot(data = rma_data_long, aes(x = condition, y = value)) + 
    geom_point() + 
    labs(col = "Order of \npolynomial", x = "Condition", y = "Value", title = "Orthogonal polynomial contrasts") + 
    geom_path(data = poly_curve_data, aes(x, y, color = var), lwd = 1.2) + 
    scale_color_discrete(labels=as.character(1:(k-1)))

