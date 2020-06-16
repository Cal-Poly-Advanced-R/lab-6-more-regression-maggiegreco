#' @return A data frame of coefficients
 #'
 #' @import dplyr
 #' hrllo
 #'
 #' @export
 slr_gd <- function(dat, response, explanatory){
   iters <- 10000
   betas <- matrix(0, 2)

   ### Compute coefficients by gradient descent
   ### Return a data frame of the same form as in the `simple_linear_regression`
   x <- dat %>% select({{explanatory}})
   x <- as.matrix(cbind(intercept = 1, x))
   y <- dat %>% select({{response}})

   return(results)
   for(i in (1:iters)){

     if (i %% 1000 == 1) {print(betas)}
     pred <- x %*% betas
     error <- (1/nrow(x))*sum((y-pred)^2)

     deriv_b1 <- (-2/nrow(x))*sum(x[,2]*(y - pred))
     deriv_int <-  (-2/nrow(x))*sum((y - pred))

     betas[1] <- (betas[1] - .0001*deriv_int)
     betas[2] <- (betas[2] - .0001*deriv_b1)
   }

   betas <- data.frame(t(betas))
   names(betas) <- names(x)
   results <- betas
   return(results)
 }


 @@ -36,13 +54,32 @@ slr_gd <- function(dat, response, explanatory){
 #' @import dplyr
 #'
 #'@export
 mlr_gd <- function(dat, response) {
 mlr_gd <- function(dat, response){

   iters <- 10000
   betas <- matrix(0, 2)

   x <- dat %>% select({{explanatory}})
   x <- as.matrix(cbind(intercept = 1, x))
   y <- dat %>% select({{response}})

   for(i in (1:iters)){

     if (i %% 1000 == 1) {print(betas)}
     pred <- x %*% betas
     error <- (1/nrow(x))*sum((y-pred)^2)

     deriv_b1 <- (-2/nrow(x))*sum(x[,2]*(y - pred))
     deriv_int <-  (-2/nrow(x))*sum((y - pred))
     for(i in (1:iters)){
     betas[1] <- (betas[1] - .0001*deriv_int)
     betas[2] <- (betas[2] - .0001*deriv_b1)
     }

   ### Compute coefficients by gradient descent
   ### Return a data frame of the same form as in the `multiple_linear_regression`

   betas <- data.frame(t(betas))
   names(betas) <- names(x)
   results <- data.frame(betas)
   return(results)

 }
