# generating correlated binary data, difficult topic!


require(bindata)
set.seed(345346)
# https://stackoverflow.com/questions/41942275/simulation-of-correlated-data-with-specifications-on-covariance-matrix
dimz <- 10
N    <- 1000

pr <- runif(dimz)

rmat <- matrix(rnorm(dimz*dimz),dimz,dimz)
cov_mat <- rmat%*%t(rmat)
corr_mat <- cov_mat/sqrt(diag(cov_mat)%*%t(diag(cov_mat)))
d <- rmvbin(N,margprob=pr,sigma=corr_mat)  # rep(0.5,dimz)

# check
apply(d,2,mean)
print(corr_mat[1:10,1:10], digits=2)
print(cor(d)[1:10,1:10],   digits=2)




