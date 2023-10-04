invacost.import <- InvaCost_database_v4_1

install.packages(invacost)
library(invacost)
data("invacost")

str(invacost)
str(invacost.import)


install.packages("tidyr")
library(tidyr)

install.packages("dplyr")
library(dplyr)


length(unique(invacost$Official_country))
invacost.nested <- invacost %>% group_by(Official_country) %>%nest()
class(invacost.nested)

invacost.list <- list()
ii <- 0
for(i in unique(invacost$Official_country)){
  ii <- ii+1
  invacost.list[[ii]] <-
invacost[which(invacost$Official_country==i),]
}
names(invacost.list) <- unique(invacost$Official_country)


invacost2 <- invacost.nested %>% unnest(data)
class(invacost2)
invacost2 <- as.data.frame(invacost2)


#a <- 
#for(i in invacost.list) {
  
#}




invacost.red <-data.frame(invacost[,c("Official_country","Species")],Presence=1)
invacost.red <- invacost.red[!duplicated(invacost.red),]
site.species <- invacost.red %>%pivot_wider(names_from=Species,values_from=c(Presence))
site.species.mat <- as.matrix(site.species)

list0 <- as.list(rep(0,ncol(site.species)))
names(list0) <- names(site.species)
site.species <- site.species %>% replace_na(list0)

site.species.mat[which(is.na(site.species.mat),arr.ind = TRUE)] <- 0

invacost.red2 <- site.species %>%
  pivot_longer(cols=!Official_country,
      names_to = "Species",
      values_to = "Presence")

invacost.red2 <- invacost.red2[!is.na(invacost.red2$Presence),-3]
