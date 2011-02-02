################################################################################
### Methods for two-dimensional numerical integration over a polygonal domain
### (see Section 3.2 of my Master's Thesis)
### 
### Author: Sebastian Meyer
### $Date: 2010-04-23 10:49:46 +0200 (Fri, 23 Apr 2010) $
###
### PARAMS:
### polyregion: a "gpc.poly" polygon (package gpclib).
###       Vertices are assumed to be ordered according to the "sp" convention,
###       i.e. _clockwise_ for normal boundaries and _anticlockwise_ for holes.
###       However, in contrast to "sp", the first vertex should NOT be repeated!
### f:  two-dimensional integrand. The function must take a coordinate matrix as
###     its first argument.
### ...: further arguments passed to f.
################################################################################


### Alternative 1: Two-dimensional midpoint rule (using spatstat image)
# polyregion can be anything coercible to "owin" by as.owin
# eps: width and height of the pixels (squares)
# dimyx: number of subdivisions in each dimension

polyCub.midpoint <- function (polyregion, f, ..., eps = NULL, dimyx = NULL)
{
    # as.im needs seperate x and y arguments
    fxy <- function (x, y, ...) f(cbind(x,y), ...)
    
    # calculate pixel values of fxy
    IM <- try(as.im.function(X = fxy, W = polyregion, ..., eps = eps, dimyx = dimyx), silent = TRUE)
    
    # if eps was to small such that the dimensions of the image would be too big
    # then the operation matrix(TRUE, nr, nc) throws an error. (try e.g. matrix(TRUE, 1e6,1e6))
    if (inherits(IM, "try-error")) {
        stop("inapplicable adaptive choice of bandwidth in midpoint rule:\n", IM)
#         cat("(used default accuracy from spatstat.options(\"npixel\"))\n")
#         IM <- as.im.function(X = fxy, W = polyregion, ...)
    } else if (is.na(IM$xstep) || is.na(IM$ystep)) { # if eps was too big (bigger than range of polyregion)
        # use default accuracy from spatstat.options("npixel")
        IM <- as.im.function(X = fxy, W = polyregion, ...)
    }
    
    # return the approximated integral
    pixelarea <- IM$xstep * IM$ystep
    int <- pixelarea * sum(IM$v, na.rm = TRUE)
    int
}



### Alternative 2: Product Gauss cubature of two-dimensional functions
### over simple polygons proposed by Sommariva & Vianello (2007)
# N: number of nodes of 1-dimensional Gauss-Legendre rule (see gaussCub)

polyCub.SV <- function (polyregion, f, ..., N)
{
    if (!require("statmod")) {
        stop("package ", sQuote("statmod"), " is needed for Gaussian cubature")
    }
    polys <- polyregion@pts
    respolys <- sapply(polys, function(poly) {
        # gaussCub function assumes anticlockwise order
        # (clockwise order leads to inverse integral value needed for holes)
        x <- rev(poly$x)
        y <- rev(poly$y)
        nw <- gaussCub(c(x,x[1]), c(y,y[1]), N = N, a = 0)
        # a = 0 seems to be better here since f (siaf) has its maximum value at (0,0)
        fvals <- f(nw$nodes, ...)
        cubature_val <- sum(nw$weights * fvals)
# if (!isTRUE(all.equal(0, cubature_val))) {
# if ((1 - 2 * as.numeric(poly$hole)) * sign(cubature_val) == -1)
# warning("wrong sign if positive integral")
# }
        cubature_val
    })
    int <- sum(respolys)
    int
}



### Function to calculate nodes and weights of the product Gauss cubature
### Code is based on the MATLAB implementation of Sommariva & Vianello (2007)
# Note: The efficiency rotation also proposed by Sommariva & Vianello (2007)
# is not implemented since it does in general only apply to convex polygons.
# Potential improvement: The efficient implementation of this function in C
# seems possible and would increase the speed of the cubature
# Parameters:
# x_bd, y_bd: coordinates of the polygon's vertices in _anticlockwise_ order
#             (otherwise the result of the cubature will have a negative sign)
#             with _repeated first vertex_ at the end
# N: degree of the one-dimensional Gauss-Legendre quadrature rule
#    (see statmod::gauss.quad, on which this function depends)
# a: base-line at x = a (see the referenced paper for an explication).
#    If NULL (the default), the midpoint of the x-range is chosen.

gaussCub <- function (x_bd, y_bd, N = 10, a = NULL)
{
    # NUMBER OF SIDES OF THE POLYGON.
    L <- length(x_bd) - 1L
    
    # base-line at x=a
    if (is.null(a)) {
        xrange <- range(x_bd)
        a <- (xrange[1] + xrange[2]) / 2
    }
    
    
    # %-------------------------------------------------------------------------
    # % COMPUTE NODES AND WEIGHTS OF 1D GAUSS-LEGENDRE RULE.
    # %-------------------------------------------------------------------------
    
    # % DEGREE "N" (as requested) (ORDER GAUSS PRIMITIVE)
    nw_N <- statmod::gauss.quad(n = N, kind = "legendre")

    # # % DEGREE "M" = N+1 (ORDER GAUSS INTEGRATION)
    M <- N + 1L
    nw_M <- statmod::gauss.quad(n = M, kind = "legendre")
    
    
    # %-------------------------------------------------------------------------
    # % COMPUTE 2D NODES (nodes_x,nodes_y) AND WEIGHTS "weights".
    # %-------------------------------------------------------------------------
    
    maxRows <- L*M
    nodes_x <- rep.int(0, maxRows*N)
    dim(nodes_x) <- c(maxRows, N)
    nodes_y <- nodes_x
    weights <- numeric(maxRows)
    K <- 0L
    
    for (side in seq_len(L))
    {
        x1 <- x_bd[side];  x2 <- x_bd[side+1L]
        y1 <- y_bd[side];  y2 <- y_bd[side+1L]
        
        if ((x1 == a && x2 == a) || (y2 == y1)) {
            # side lies on base-line or is orthogonal to it
            next
        }

        if (x2 == x1) { # side is parallel to base-line => degree N
            n_loc <- nw_N$nodes
            w_loc <- nw_N$weights
        } else { # degree M=N+1
            n_loc <- nw_M$nodes
            w_loc <- nw_M$weights
        }
        degree_loc <- length(n_loc)   # = N or M
        
        half_pt_x <- (x1+x2)/2;  half_length_x <- (x2-x1)/2;
        half_pt_y <- (y1+y2)/2;  half_length_y <- (y2-y1)/2;
        
        #% GAUSSIAN POINTS ON THE SIDE.
        x_gauss_side <- half_pt_x + half_length_x * n_loc
        y_gauss_side <- half_pt_y + half_length_y * n_loc
        
#         scaling_fact <- x_gauss_side / 2
#         scaling_fact_plus <- (x_gauss_side + a) / 2
        scaling_fact_minus <- (x_gauss_side - a) / 2
            
        local_weights <- (half_length_y * scaling_fact_minus) * w_loc
        
#         term_1 <- matrix(scaling_fact_plus, nrow = degree_loc, ncol = N)
        term_2 <- matrix(scaling_fact_minus, nrow = degree_loc, ncol = N)
#         term <- matrix(scaling_fact, nrow = degree_loc, ncol = N)
        rep_n_Np1 <- matrix(nw_N$nodes + 1, nrow = degree_loc, ncol = N, byrow = TRUE)
        
        #% x, y ARE STORED IN MATRICES. A COUPLE WITH THE SAME INDEX IS A POINT,
        #% i.e. "P_i=(x(k),y(k))" FOR SOME "k".
        # x = (term+a/2) + (term-a/2) * rep_n_N = (1+rep_n_N) * term + (1-rep_n_N) * a/2
        # x = (term_2+a) + term_2 * rep_n_N = (1+rep_n_N) * term_2 + a
        x <- rep_n_Np1 * term_2 + a
#         y = matrix(y_gauss_side, nrow = degree_loc, ncol = N)
        
        # add nodes and weights for this side of the polygon
        rowidx <- K + seq_len(degree_loc)
        nodes_x[rowidx,] <- x
        nodes_y[rowidx,] <- y_gauss_side   # fills (sub-)matrix by column
        weights[rowidx] <- local_weights
        K <- K + degree_loc
    }
    
    # only the first K entries of 'weights' and rows of 'nodes_x' and 'nodes_y'
    # have been filled, the remainder till 'maxRows', contains the zeros
    # from initialisation.
    if (K < maxRows) {
        seqK <- seq_len(K)
        weights <- weights[seqK]
        nodes_x <- nodes_x[seqK,]
        nodes_y <- nodes_y[seqK,]
    }
    nodes <- cbind(c(nodes_x), c(nodes_y))   # K*N x 2 matrix
    weightsvec <- rep(nw_N$weights, each = K) * rep.int(weights, N)  # K*N long
#     f_xy <- f(nodes, ...)   # K*N function evaluations
#     dim(f_xy) <- c(K, N)
#     .tmp <- colSums(weights * f_xy)   # equals t(weights) %*% f_xy, which is 1 x N
#     cubature_val <- sum(.tmp * nw_N$weights)   # equals .tmp %*% nw_N$weights

    ret <- list(nodes = nodes, weights = weightsvec)
    return(ret)
}
