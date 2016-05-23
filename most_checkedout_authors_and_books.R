library(reshape2)
library(plyr)

#original csv seems to have some issues. File has 159818 lines, however, upon import with read.csv(filename.csv, header=TRUE, sep = ';', encoding = 'Latin-1'), I only get 70343 lines.
#I fixed this by running in bash
#>csvclean -e Latin-1 -d ";" filename.csv
#and stayed only with clean rows: filename_out.csv (there were 5 problem rows which had to do with double double quotes?)

pa <- read.csv('bibliotecas_prestamos_201605_out.csv', header=TRUE)
#eliminate blank authors and black book titles
pa <- pa[pa$tiauto!="" & pa$tititu!="",]

##first, how many libraries are in data base?
u <- unique(pa$prprsu)
length(u)

##most checked-out author in all libraries
freq <- count(pa$tiauto)
colnames(freq)[1] <- 'author'
freq.sort <- freq[order(freq$freq,decreasing = T),]
head(freq.sort,20)

##most checked-out book in all libraries
freq <- count(pa$tititu)
colnames(freq)[1] <- 'book'
freq.sort <- freq[order(freq$freq,decreasing = T),]
head(freq.sort,20)
##most checked-out book by library
#stay only with library and author name
tmp <- pa[,c('tiauto','prprsu')]
#count number of authors checkout per library
tmp <- ddply(tmp, .(prprsu, tiauto), summarise, value=sum(!is.na(tiauto)))
#to speed up, we will only rank authors with more than 1 checkout
tmp <- tmp[tmp$value>1,]
#rank by check-outs
tmp <- ddply(tmp, .(prprsu), transform, rank=rank(-value, ties='random'))
#stay only with most checked-out author per library
author.rank <- tmp[tmp$rank==1,1:3]
#additionaly, lets sort to library/authors with most check outs
author.rank <- author.rank[order(author.rank$value,decreasing = T),]
author.rank

##most checked-out book per library (repeat previous but with book instead of author)
tmp <- pa[,c('tititu','prprsu')]
tmp <- ddply(tmp, .(prprsu, tititu), summarise, value=sum(!is.na(tititu)))
tmp <- tmp[tmp$value>1,]
tmp <- ddply(tmp, .(prprsu), transform, rank=rank(-value, ties='random'))
book.rank <- tmp[tmp$rank==1,1:3]
book.rank <- book.rank[order(book.rank$value,decreasing = T),]
book.rank
