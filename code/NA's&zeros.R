#Septmeber 27 2016
#Calculates the proportion of NA's per each scale for each indicator
#'This was created to have an idea of which indicators at what scales had more than 25% NA's
#'in the data set and thus should not be inncluded in the correlation analysis
source(paste('D:/RProjects/GetIndi/R/amc helpers.R',sep=""))
#path <- file.path('D:',"RProjects","GetIndi")
path <- file.path('D:','RProjects','UseIndi')
cP = file.path(path,'data')

a = read.csv(file.path(cP,'stratsetq.csv'))
#a = read.csv(file.path(cP,'esswsssetq.csv'))
#a = read.csv(file.path(cP,'nafosetq.csv'))
#a = read.csv(file.path(cP,'shelfsetq.csv'))

cc = ncol(a)
b = names(a)
b = b[grep('_s',b,invert=T)]
lss = list()
m=0
for(i in b) {
  v = a[,c("YEAR",'ID',i)]
  vv = unique(v$ID)
  for(j in vv) {
    m = m+1
      vw = subset(v,ID==j)      
      op = sum(is.na(vw[,3])*1) / length(vw[,3])
  lss[[m]] = c(i,j,op)
    }
  }
op = as.data.frame(do.call(rbind,lss))
cX(op)
head(op)

sort.data.frame <- function(x, decreasing=FALSE, by=1, ... ){
  f <- function(...) order(...,decreasing=decreasing)
  i <- do.call(f,x[by])
  x[i,,drop=FALSE]
}
op_sorted <- sort.data.frame(op, by=c("V2", "V3"), decreasing=TRUE)
op_sorted <- op_sorted[op_sorted$V1 !=c('YEAR'),]
op_sorted <- op_sorted[op_sorted$V1 !=c('ID'),]
cX(op_sorted)






