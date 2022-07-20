getwd()
rm(list=ls())
<<<<<<< HEAD

df=read.csv("data/hospital_rename.csv")
require(dplyr)
library(DataExplorer)
DataExplorer::plot_intro(df)

install.packages("dplyr", type = 'binary') 

install.packages("rlang")
require(rlang)
library(dplyr)
library(magrittr)

    ## Compute row and column sums for a matrix:
     x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
     x 
     rowSums(x); colSums(x)

     ?rowSums
     dimnames(x)[[1]] <- letters[1:8]
     rowSums(x); colSums(x); rowMeans(x); colMeans(x)
     x[] <- as.integer(x)
     rowSums(x); colSums(x)
     x[] <- x < 3
     rowSums(x); colSums(x)
     x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
     x[3, ] <- NA; x[4, 2] <- NA
     rowSums(x); colSums(x); rowMeans(x); colMeans(x)
     rowSums(x, na.rm = TRUE); colSums(x, na.rm = TRUE)
     rowMeans(x, na.rm = TRUE); colMeans(x, na.rm = TRUE)
     
     ## an array
     dim(UCBAdmissions)
     rowSums(UCBAdmissions); rowSums(UCBAdmissions, dims = 2)
     colSums(UCBAdmissions); colSums(UCBAdmissions, dims = 2)
     
     ## complex case
     x <- cbind(x1 = 3 + 2i, x2 = c(4:1, 2:5) - 5i)
     x[3, ] <- NA; x[4, 2] <- NA
     rowSums(x); colSums(x); rowMeans(x); colMeans(x)
     rowSums(x, na.rm = TRUE); colSums(x, na.rm = TRUE)
     rowMeans(x, na.rm = TRUE); colMeans(x, na.rm = TRUE)


############
#create data frame
df <- data.frame(day=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                 sales=c(7, 8, 8, 12, 10, 9, 13, 16, 11, 7))

                 df

#calculate difference between rows in 'sales' column
sales_diff <- diff(df$sales)
sales_diff
#append NA to beginning of differences vector
sales_diff <- c(NA, sales_diff)

#append differences vector as new column
df$sales_diff <- sales_diff

#view updated data frame
df


?diff
diff(1:10, 2)
diff(1:10, 2, 2)
x <- cumsum(cumsum(1:10))
diff(x, lag = 2)
diff(x, differences = 2)

diff(.leap.seconds)



x <- cumsum(cumsum(1:10))

?cumsum

?diff



# installing required libraries
library("dplyr")

# creating a data frame
data_frame <- data.frame(col1 = sample(6:9, 9 , replace = TRUE),
						col2 = letters[1:3],
						col3 = c(1,4,5,1,NA,NA,2,NA,2))

print ("Original DataFrame")
print (data_frame)

print ("Modified DataFrame")

# computing difference of each group
data_frame%>%
    group_by(col1)%>%
    mutate(diff=col3-lag(col3,default=first(col3)))

library(dplyr)
library(tidyverse)



#데이터 테이블(data.table) 자료형에 입력
> library(data.table)
> df <- data.table(group = rep(c("A", "B"), each = 3), value = c(10,20,25,5,10,15))
> class(df)
[1] "data.table" "data.frame"
> df
require(dplyr)


df <- data.frame(x = c(6, 4, 1, 10, 3, 1, 1))

df %>% top_n(2)  

# highest values
#> Selecting by x
#>    x
#> 1  6
#> 2 10
df %>% top_n(-2) # lowest values
#> Selecting by x
#>   x
#> 1 1
#> 2 1
#> 3 1
# now use


data <- data.frame(gr1 = rep(LETTERS[1:4],    # Create example data frame
                             each = 3),
                   gr2 = letters[1:2],
                   values = 101:112)
data                                          # Print example data frame
data_group <- data %>%                        # Group data
  group_by(gr1, gr2) %>%
  dplyr::summarise(gr_sum = sum(values))



  ###################################### 행계산 #########################################
  require(data.table)
  # on vectors, returns a vector as long as length(n) == 1, #1127
x = 1:5
# lag with n=1 and pad with NA (returns vector)
shift(x, n=1, fill=NA, type="lag")
# lag with n=1 and 2, and pad with 0 (returns list)
shift(x, n=1:2, fill=0, type="lag")
# getting a window by using positive and negative n:
shift(x, n = -1:1)
shift(x, n = -1:1, type = "shift", give.names = TRUE)

# on data.tables
DT = data.table(year=2010:2014, v1=runif(5), v2=1:5, v3=letters[1:5])
# lag columns 'v1,v2,v3' DT by 1 and fill with 0
cols = c("v1","v2","v3")
anscols = paste("lead", cols, sep="_")
DT[, (anscols) := shift(.SD, 1, 0, "lead"), .SDcols=cols]

# return a new data.table instead of updating
# with names automatically set
DT = data.table(year=2010:2014, v1=runif(5), v2=1:5, v3=letters[1:5])
DT[, shift(.SD, 1:2, NA, "lead", TRUE), .SDcols=2:4]

# lag/lead in the right order
DT = data.table(year=2010:2014, v1=runif(5), v2=1:5, v3=letters[1:5])
DT = DT[sample(nrow(DT))]
# add lag=1 for columns 'v1,v2,v3' in increasing order of 'year'
cols = c("v1","v2","v3")
anscols = paste("lag", cols, sep="_")
DT[order(year), (cols) := shift(.SD, 1, type="lag"), .SDcols=cols]
DT[order(year)]

# while grouping
DT = data.table(year=rep(2010:2011, each=3), v1=1:6)
DT[, c("lag1", "lag2") := shift(.SD, 1:2), by=year]

# on lists
ll = list(1:3, letters[4:1], runif(2))
shift(ll, 1, type="lead")
shift(ll, 1, type="lead", give.names=TRUE)
shift(ll, 1:2, type="lead")

# fill using first or last by group
DT = data.table(x=1:6, g=rep(1:2, each=3))

DT 
DT[ , new := shift(x, fill=NA), by=g]
DT
DT[ , new2 := shift(x, fill=x[1L]), by=g]
DT 

DT[ , new3 := shift(x, fill=x[.N]), by=g]
DT 




# on data.tables
DT = data.table(year=2010:2014, v1=runif(5), v2=1:5, v3=letters[1:5])
DT
# lag columns 'v1,v2,v3' DT by 1 and fill with 0
cols = c("v1","v2","v3")
anscols = paste("lead", cols, sep="_")
DT[, (anscols) := shift(.SD, 1, 0, "lead"), .SDcols=cols]
DT

#############################k-means##################
require(graphics)

# a 2-dimensional example
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
print(x)
colnames(x) <- c("x", "y")
(cl <- kmeans(x, 2))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex = 2)

# sum of squares
ss <- function(x) sum(scale(x, scale = FALSE)^2)

## cluster centers "fitted" to each obs.:
fitted.x <- fitted(cl);  head(fitted.x)
resid.x <- x - fitted(cl)

## Equalities : ----------------------------------
cbind(cl[c("betweenss", "tot.withinss", "totss")], # the same two columns
         c(ss(fitted.x), ss(resid.x),    ss(x)))
stopifnot(all.equal(cl$ totss,        ss(x)),
	  all.equal(cl$ tot.withinss, ss(resid.x)),
	  ## these three are the same:
	  all.equal(cl$ betweenss,    ss(fitted.x)),
	  all.equal(cl$ betweenss, cl$totss - cl$tot.withinss),
	  ## and hence also
	  all.equal(ss(x), ss(fitted.x) + ss(resid.x))
	  )

kmeans(x,1)$withinss # trivial one-cluster, (its W.SS == ss(x))

## random starts do help here with too many clusters
## (and are often recommended anyway!):
(cl <- kmeans(x, 5, nstart = 25))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)


=======
>>>>>>> parent of 99bb23a (220719)
