library(tibble)
library(stringr)
library(Dict)
ratings_address <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv'
ratings <- readr::read_csv(ratings_address)
details_address <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv'
details <- readr::read_csv(details_address)

#Problem 0
wilcox.test(ratings$average,ratings$bayes_average, paired = FALSE)
# ==> Our samples are not from the same distribution.
# ==> We will use buyers rates for rating

#Problem 1
#Collecting rates of each category
a <- details[,c("id", "boardgamecategory")]
scores <- list()
for(i in 1:nrow(a)){
  if(is.na(a[[i, 2]]) == FALSE){
    for(tmp in str_split(a[[i, 2]], "'")){
      for(j in 1:length(tmp)){
        if(nchar(tmp[j]) > 2){
          scores[[tmp[j]]] <- c(scores[[tmp[j]]], ratings[[which(ratings$id == a[[i, 1]]), "bayes_average"]])
        }
      }
    }
  }
}
#Calculating 95% confidence interval for each category
for(name in names(scores)){
  if(length(scores[[name]]) > 30){
    print(name)
    sum <- 0
    n <- length(scores[[name]])
    for(i in scores[[name]]){
      sum <- sum + i
    }
    average <- sum / n
    
    sum <- 0
    for(i in scores[[name]]){
      tmp <- i - average
      tmp <- tmp * tmp
      sum <- sum + tmp
    }
    s <- sum / (n - 1)
    
    dif <- (qnorm(0.95) * sqrt(s) / sqrt(n))
    cat(average - dif, average + dif, "\n")
  }
}
#==> 5.9 will be a good estimation for popular categories
#Finding most popular categories base on mean > 5.9
class(scores[["Fighting"]])
for(name in names(scores)){
  if(length(scores[[name]]) > 30){
    sum <- 0
    n <- length(scores[[name]])
    for(i in scores[[name]]){
      sum <- sum + i
    }
    average <- sum / n
    
    sum <- 0
    for(i in scores[[name]]){
      tmp <- i - average
      tmp <- tmp * tmp
      sum <- sum + tmp
    }
    s <- sum / (n - 1)
    
    z <- (average - 5.9) / (sqrt(s) / sqrt(n))
    if(z > qnorm(0.95)){
      print(name)
    }
  }
}


#Problem 2
#Calculating mean of first population
less_n <- length(which(ratings$year < 2010))
less_sum <- 0
for(i in which(ratings$year < 2010)){
  less_sum <- less_sum + ratings[[i, "bayes_average"]]
}
less_average <- (less_sum / less_n)
#Calculating variance of first population
less_sum <- 0
for(i in which(ratings$year < 2010)){
  tmp <- (ratings[[i, "bayes_average"]] - less_average)
  tmp <- tmp * tmp
  less_sum <- less_sum + tmp
}
less_s <- (less_sum / (less_n - 1))
#Calculating mean of second population
more_n <- length(which(ratings$year >= 2010))
more_sum <- 0
for(i in which(ratings$year >= 2010)){
  more_sum <- more_sum + ratings[[i, "bayes_average"]]
}
more_average <- (more_sum / length(which(ratings$year >= 2010)))
#Calculating variance of second population
more_sum <- 0
for(i in which(ratings$year >= 2010)){
  tmp <- (ratings[[i, "bayes_average"]] - more_average)
  tmp <- tmp * tmp
  more_sum <- more_sum + tmp
}
more_s <- (more_sum / (more_n - 1))
#Calculating Z_obs for mean hypothesis test
z <- (less_average - more_average) / sqrt((less_s / less_n) + (more_s / more_n))
if(z < qnorm(0.05)){
  print("average has raised")
}else{
  print("average has not raised")
}
# average has been raised during time.

#Problem 3
#Selecting needed data from dataset
data <- list()
for(i in 1:nrow(ratings)){
  if(length(which(details$id == ratings[[i, "id"]])) != 0){
    data[["average"]] <- c(data[["average"]], ratings[[i, "bayes_average"]])
    data[["owned"]] <- c(data[["owned"]], details[[which(details$id == ratings[[i, "id"]]), "owned"]])
  }
}
#Hypothesis test for rou
cor.test(data[["average"]], data[["owned"]], method = "pearson")

#Cause the hypothesis got rejected so we propose a regression line:
mod <- lm(data[["owned"]] ~ data[["average"]])

# sale = 1033 * rating - 5144

#Drawing diagram of sale base on rating
plot(data[["average"]],data[["owned"]], 
     xlab="average ratings", 
     ylab="sale",
     main="sale = 1033 * rating - 5144")
abline(coefficients(mod), lwd=2, lty=2, 
       col="red")

#Problem 4
#Building 2sided occurrence chart
n <- list()
n[[".."]] <- 0
for(i in 1:5){
  for(j in 1:5){
    tmp <- paste(as.character(i), as.character(j), sep = "")
    n[[tmp]] <- 0
  }
  tmp <- paste(as.character(i), ".", sep = "")
  n[[tmp]] <- 0
  tmp <- paste(".", as.character(i), sep = "")
  n[[tmp]] <- 0
}
#Filling occurrency chart
for(i in 1:5){
  for(k in which(ratings$bayes_average < (2 * i) & ratings$bayes_average >= (2 * (i - 1)))){
    if(length(which(details$id == ratings[[k, "id"]])) != 0){
      j <- floor(details[[which(details$id == ratings[[k, "id"]]), "playingtime"]] / 30) + 1
      if(j > 5){
        j <- 5
      }
      tmp <- paste(as.character(i), as.character(j), sep = "")
      n[[tmp]][1] <- n[[tmp]][1] + 1
      tmp <- paste(as.character(i), ".", sep = "")
      n[[tmp]][1] <- n[[tmp]][1] + 1
      tmp <- paste(".", as.character(j), sep = "")
      n[[tmp]][1] <- n[[tmp]][1] + 1
      n[[".."]][1] <- n[[".."]][1] + 1
    }
  }
}
#Calculating V** for independency test
v <- 0
for(i in 1:5){
  for(j in 1:5){
    tmp_ij <- paste(as.character(i), as.character(j), sep = "")
    tmp_i <- paste(as.character(i), ".", sep = "")
    tmp_j <- paste(".", as.character(j), sep = "")
    e <- n[[tmp_i]][1] * n[[tmp_j]][1] / n[[".."]][1]
    tmp <- n[[tmp_ij]][1] - e
    tmp <- tmp * tmp
    v <- v + (tmp / e)
  }
}
#Calculating result
if(v > qchisq(0.95, 16)){
  print("playing time and rating are dependent")
}else{
  print("playing time and rating are independent")
}
# They were dependent.