install.packages("rgbif")
library("rgbif")

dat.messy <-
  read.csv("~/Downloads/doi_10/Species_list_full_2905.csv",sep=";")


spp <- unique(dat.messy$species)
spp <- spp[-which(is.na(spp))] 
spp <- spp[1:200] 
n_spp <- length(spp)

spp.check.ok <- name_backbone("Felis catus",verbose=T,strict=T)
spp.check.ok <- spp.check.ok[-1,]
spp.check.bad <- name_backbone("xxx",verbose=T,strict=T)
spp.check.bad <- spp.check.bad[-1,]

pb <- txtProgressBar(min=0, max=n_spp, initial=0,style = 3)
for(i in 1:n_spp){
  toto <- name_backbone(spp[i],verbose=T,strict=T)
}

for(i in 1:n_spp){
  toto <- name_backbone(spp[i],verbose=T,strict=T) 
  if(length(which(names(toto)=="acceptedUsageKey"))==1){ 
    toto <- toto[,-which(names(toto)=="acceptedUsageKey")]
  }
  if(ncol(toto)==ncol(spp.check.ok)){ 
    if(length(which(toto$status=="ACCEPTED"))>0){ 
      spp.check.ok <- rbind(spp.check.ok,toto[which(toto$status=="ACCEPTED"),]) 
    }else if(length(which(toto$status=="SYNONYM"))>0){ 
      warning(paste("Species",spp[i],"is a synonmy")) 
      spp.check.ok <- rbind(spp.check.ok,toto[which(toto$status=="SYNONYM")[1],])
    }else if(length(which(toto$status=="DOUBTFUL"))>0){ 
      warning(paste("Species",spp[i],"is doubtful"))
      spp.check.ok <- rbind(spp.check.ok,toto[which(toto$status=="DOUBTFUL")[1],]) 
    }else{
      stop("Status unknown") 
    }   
  }else if(ncol(toto)==ncol(spp.check.bad)){
    spp.check.bad <- rbind(spp.check.bad,toto)
  }
  else{
    stop("Unknown length") 
  }
  info <- sprintf("%d%% done", round((i/n_spp)*100))
  setTxtProgressBar(pb, i, label=info)
}
close(pb)


duplicated(spp.check.ok$canonicalName)


length(which(spp.check.ok$status=="SYNONYM")) 
length(which(spp.check.ok$status=="DOUBTFUL")) 
spp.check.ok.syn <- spp.check.ok[which(spp.check.ok$status=="SYNONYM"),] 

which(spp.check.ok$rank=="GENUS")

spp[which(spp.check.ok$rank=="GENUS")]
