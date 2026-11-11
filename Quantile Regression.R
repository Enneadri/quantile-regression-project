# install.packages("insuranceData")
data("AutoClaims", package = "insuranceData")
claims <- AutoClaims

dim(claims)
names(claims)
head(claims)
str(claims)

#sp
set.seed(315)
n<- nrow(claims) #total number of claims
train_size <- floor(0.8*n)
train_rows <- sample(1:n, size = train_size) #randomly select 0.8n from data set 

train_set <- claims[train_rows, ]
test_set <- claims[-train_rows, ]

quantile_loss <- function(y, y_hat, tau) {
  residual <- y -y_hat
  loss <- ifelse(
    residual >= 0, tau * residual,
    (tau -1) * residual
  )
  return(loss)
}