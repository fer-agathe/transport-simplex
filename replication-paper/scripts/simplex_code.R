library(tidyverse)
rm(list=ls())
# download.file("http://freakonometrics.free.fr/toydataset.RData",
#               "../data/toydataset.RData")

load("../data/toydataset.RData")
str(toydataset)

library(compositions)



X0 = toydataset[toydataset$class == 0,1:3]
X1 = toydataset[toydataset$class == 1,1:3]

Z0 = matrix(clr(X0),ncol=3)
Z1 = matrix(clr(X1),ncol=3)

Z0 = Z0[,1:2]
Z1 = Z1[,1:2]
vx = var(Z0)
vy = var(Z1)
mx = apply(Z0,2,mean)
my = apply(Z1,2,mean)

library(expm)
A  = solve(sqrtm(vx)) %*% sqrtm(sqrtm(vx) %*% vy %*% (sqrtm(vx))) %*% solve(sqrtm(vx))

transport_x = function(x, nb=31){
  z = as.numeric(clr(as.numeric(x)))[1:2]
  vec_t = seq(0,1,lengt=nb)
  transp_x = matrix(NA,nb,3)
  for(i in 1:nb){
    t=vec_t[i]
  transp_z=(1-t)*z + t*(my+A %*% (z-mx))
  transp_z = as.numeric(transp_z)
  transp_z = c(transp_z,-sum(transp_z))
  transp_x[i,] = clrInv(transp_z)
  }
  transp_x
}

transport_x(X0[1,],11)

# ===========


valid_single_marginal <- function(mvec, M, fname){
  dname = paste0("'",deparse(substitute(mvec)),"'")
  if ((length(mvec)==0)&&is.null(mvec)){
    return(rep(1/M, M))
  } else {
    mvec = as.vector(mvec)
    if ((length(mvec)!=M)||(any(mvec<0))){
      stop(paste0("* ",fname," : ",dname," should be a nonnegative vector of length ",M,"."))
    }
    return(mvec/base::sum(mvec))
  }
}

#' Cost function for optimal transport on the unit simplex
d_s <- function(x, y) {
  d <- length(x)
  log(mean(y / x)) - mean(log(y / x))
}

compute_pdist_simplex = function(X,Y){
  M = matrix(NA,nrow(X),nrow(Y))
  for(i in 1:nrow(X)){
    for(j in 1:nrow(Y)){
      M[i,j]=d_s(X[i,],Y[j,])
    }
  }
  M
}

#' @rdname wasserstein
#' @export
wasserstein <- function(X, Y, wx=NULL, wy=NULL){
  ## CHECK INPUTS
  if (is.vector(X)){
    X = matrix(X, ncol=1)
  }
  if (is.vector(Y)){
    Y = matrix(Y, ncol=1)
  }
  if (!is.matrix(X)){    stop("* wasserstein : input 'X' should be a matrix.")  }
  if (!is.matrix(Y)){    stop("* wasserstein : input 'Y' should be a matrix.")  }
  if (base::ncol(X)!=base::ncol(Y)){
    stop("* wasserstein : input 'X' and 'Y' should be of same dimension.")
  }
  m = base::nrow(X)
  n = base::nrow(Y)

  wxname =  paste0("'",deparse(substitute(wx)),"'")
  wyname = paste0("'",deparse(substitute(wy)),"'")
  fname  = "wasserstein"

  par_wx = valid_single_marginal(wx, m, fname)
  par_wy = valid_single_marginal(wy, n, fname) #valid_weight(wy, n, wyname, fname)
  par_D  = compute_pdist_simplex(X, Y)


  output = wass_lp(par_D, par_wx, par_wy, p=2)
  return(output)
}
#' @rdname wasserstein
#' @export
wassersteinD <- function(D, wx=NULL, wy=NULL){
  ## INPUTS : EXPLICIT
  name.fun = "wassersteinD"
  name.D   = paste0("'",deparse(substitute(D)),"'")
  name.wx  = paste0("'",deparse(substitute(wx)),"'")
  name.wy  = paste0("'",deparse(substitute(wy)),"'")

  par_D  = valid_distance(D, name.D, name.fun)

  m = base::nrow(par_D)
  n = base::ncol(par_D)

  #valid_weight(wy, n, wyname, fname)
  par_wx = valid_single_marginal(wx, m, name.fun)
  par_wy = valid_single_marginal(wy, n, name.fun)
  par_p  = max(1, as.double(p))

  ## RUN
  output = wass_lp(par_D, par_wx, par_wy)
  return(output)
}
#' @keywords internal
#' @noRd
wass_lp <- function(dxy, wx, wy, p){
  cxy = (dxy)
  m   = length(wx); ww_m = matrix(wx, ncol=1)
  n   = length(wy); ww_n = matrix(wy, nrow=1)
  ones_m = matrix(rep(1,n),ncol=1)
  ones_n = matrix(rep(1,m),nrow=1)
  plan   = CVXR::Variable(m,n)

  wd.obj    <- CVXR::Minimize(CVXR::matrix_trace(t(cxy)%*%plan))
  wd.const1 <- list(plan >= 0)
  wd.const2 <- list(plan%*%ones_m==ww_m, ones_n%*%plan==ww_n)
  wd.prob   <- CVXR::Problem(wd.obj, c(wd.const1, wd.const2))
  wd.solve  <- CVXR::solve(wd.prob, solver="OSQP")

  if (all(wd.solve$status=="optimal")){ # successful
    gamma <- wd.solve$getValue(plan)
    value <- (base::sum(gamma*cxy))

    return(list(distance=value, plan=gamma))
  } else {                              # failed : use lpsolve
    cxy = (dxy)
    m   = nrow(cxy)
    n   = ncol(cxy)

    c  = as.vector(cxy)
    A1 = base::kronecker(matrix(1,nrow=1,ncol=n), diag(m))
    A2 = base::kronecker(diag(n), matrix(1,nrow=1,ncol=m))
    A  = rbind(A1, A2)

    f.obj = c
    f.con = A
    f.dir = rep("==",nrow(A))
    f.rhs = c(rep(1/m,m),rep(1/n,n))
    f.sol = (lpSolve::lp("min", f.obj, f.con, f.dir, f.rhs))

    gamma = matrix(f.sol$solution, nrow=m)
    value = (sum(gamma*cxy)^(1/p))

    return(list(distance=value, plan=gamma))
  }
}

tP0 = matrix(unlist(X0),nrow(X0))
tP1 = matrix(unlist(X1),nrow(X1))
Wxy = wasserstein(tP0,tP1)

apply(Wxy$plan,1,sum)
apply(Wxy$plan,2,sum)

M0=Wxy$plan*nrow(X0)
which(M0[1,]>.1)

# ====


library(fairml)
data(german.credit)
str(german.credit)

x = rep("other",nrow(german.credit))
x[german.credit$Purpose %in% c("car (new)","car (used)")] = "cars"
x[german.credit$Purpose %in% c("domestic appliances",
                               "furniture / equipment",
                               "radio / television")] = "equipment"
german.credit$Purpose = as.factor(x)

library(randomForest)
rf = randomForest(Purpose ~ ., data=german.credit[,-c(20,21)])
varImpPlot(rf)
library(splines)
require(nnet)
library(gbm)
library(caret)
reg1 = multinom(Purpose ~ bs(Credit_amount) + bs(Age) + bs(Duration), data = german.credit)
reg2 = multinom(Purpose ~ bs(Credit_amount) + bs(Age) + bs(Duration)
                + Present_employment_since + Savings_bonds + Property + Account_status + Credit_history +
                  Resident_since+Job+Housing,
                data = german.credit)
mod_gbm = gbm(Purpose ~.,
              data = german.credit[,-c(20,21)],
              distribution = "multinomial",
              cv.folds = 10,
              shrinkage = .01,
              n.minobsinnode = 10,
              n.trees = 2000)
pred = predict.gbm(object = mod_gbm,
                   newdata = german.credit[,-c(20,21)],
                   n.trees = 2000,
                   type = "response")
Xgbm=pred[,,1]
Xrf = predict(rf,type="prob")
Xglm1 = predict(reg1,type="probs")
Xglm2 = predict(reg2,type="probs")

german.credit |> select(Purpose) |> head()



# Adult----

library(fairadapt)
data(adult)
adults=adult
adultsf = adult[adult$sex == "Female",]
adultsm = adult[adult$sex == "Male",]
table(adult$education)/nrow(adult)*100
table(adultsm$education)/nrow(adultsm)*100
table(adultsf$education)/nrow(adultsf)*100

table(adult$marital_status)/nrow(adult)*100
table(adultsm$marital_status)/nrow(adultsm)*100
table(adultsf$marital_status)/nrow(adultsf)*100

x = rep("Never-married",nrow(adults))
x[adults$marital_status %in%
    c("Divorced","Widowed","Married-spouse-absent","Separated")] =
  "Separated"
x[adults$marital_status %in%
    c("Married-civ-spouse")] =
  "Married"
table(x)
adults$marital_status = as.factor(x)

adultsf = adults[adults$sex == "Female",]
adultsm = adults[adults$sex == "Male",]

table(adults$marital_status)/nrow(adults)*100
table(adultsm$marital_status)/nrow(adultsm)*100
table(adultsf$marital_status)/nrow(adultsf)*100

library(randomForest)
rf = randomForest(marital_status ~ ., data=adults[,-c(9,14)])
varImpPlot(rf)
library(splines)
require(nnet)
reg1 = multinom(marital_status ~ bs(age) + bs(hours_per_week) + occupation, data = adults[,-c(9,14)])
reg2 = multinom(marital_status ~ bs(age) + bs(hours_per_week) + occupation + relationship +
                  workclass +bs(education_num) +education+bs(capital_gain)  ,
                data = adults[,-c(9,14)])
#reg = multinom(Purpose ~ Credit_amount+Age+Duration, data = german.credit)
library(gbm)
library(caret)
mod_gbm = gbm(marital_status ~.,
              data = adults[,-c(9,14)],
              distribution = "multinomial",
              cv.folds = 10,
              shrinkage = .01,
              n.minobsinnode = 10,
              n.trees = 2000)
pred = predict.gbm(object = mod_gbm,
                   newdata = adults[,-c(9,14)],
                   n.trees = 2000,
                   type = "response")
Xgbm=pred[,,1]
Xrf = predict(rf,type="prob")
Xglm1 = predict(reg1,type="probs")
Xglm2 = predict(reg2,type="probs")
