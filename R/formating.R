library(plyr)

setwd('C://Users/Tony/Downloads/')
sptab=read.csv('/Species_search_Tony.csv', h=T) 

sp=data.frame(unique(sptab$Species)) #1576


write.table(sp[c(1:526),1], "C://Users/Tony/Documents/Phd Bielefeld/Data manip/Host-shift _cospeciation/AvianParasites/Species_Tony.csv")

write.table(sp[c(527:1051),1], "C://Users/Tony/Documents/Phd Bielefeld/Data manip/Host-shift _cospeciation/AvianParasites/Species_Anja.csv")

write.table(sp[c(1052:1576),1], "C://Users/Tony/Documents/Phd Bielefeld/Data manip/Host-shift _cospeciation/AvianParasites/Species_Meinolf.csv")

setwd("C://Users/Tony/Documents/Phd Bielefeld/Data manip/Host-shift _cospeciation/AvianParasites/")

tab=read.csv2("../MalAvi_host_and_site_coord.cs", h=T)

unknown.names=as.data.frame(unknown.names)

head(unknown.names)

mydb=read.csv2("dataset_Tony_UICNadds.csv", h=T)

mydb$movement=NA

### Getting the common name from UICN to mydb

for (i in 1:length(mydb$Common.name)){
  if(!mydb$Scientific.name[i] %in% redlist$scientific_name){next}
  if(mydb$Scientific.name[i] %in% redlist$scientific_name){
  mydb$Common.name2[i]=redlist$main_common_name[redlist$scientific_name==mydb$Scientific.name[i]]
  mydb$superficy[i]=redlist$eoo_km2[redlist$scientific_name==mydb$Scientific.name[i]]
  mydb$upperalt[i]=redlist$elevation_upper[redlist$scientific_name==mydb$Scientific.name[i]]
  mydb$loweralt[i]=redlist$elevation_lower[redlist$scientific_name==mydb$Scientific.name[i]]
}
}

### Getting the movement from UICN to mydb

subsetuicn=subset(habitats, names(habitats) %in% mydb$Scientific_name)

for (j in 1:length(subsetuicn)){
  i=as.numeric(rownames(mydb[mydb$Scientific_name==names(subsetuicn[j]),]))
  x=subsetuicn[[j]]$season
    if("Breeding Season" %in% x[1|2|3|4]){
       mydb$movement[i]="Migratory"
    }
    if("Resident" %in% x){  
       mydb$movement[i]="Resident"
    }
  cat(names(subsetuicn[j]),"-",mydb$movement[i],"\n",x,"\n")
    #else{mydb$movement[i]=NA
    #cat("Watch out! \n")
    #}
}
    
    
    
### Getting the habitat from UICN to mydb, with condition suitable and major importance==yes
    
for (j in 1:length(habitats)){
  if(names(habitats[j]) %in% mydb$Scientific.name){
      cat(names(habitats[j])," - ",subset(habitats[[j]]$habitat, habitats[[j]]$suitability=="Suitable" & habitats[[j]]$majorimportance=="Yes" & habitats[[j]]$season=="Breeding Season" | habitats[[j]]$season=="Resident"),"\n")
      i=as.numeric(rownames(mydb[mydb$Scientific.name==names(habitats[j]),]))
    if(habitats[[j]]$majorimportance=="Yes" & habitats[[j]]$suitability=="Suitable" & c(habitats[[j]]$season=="Breeding Season" | habitats[[j]]$season=="Resident")){
      x=subset(habitats[[j]]$habitat, habitats[[j]]$suitability=="Suitable" & habitats[[j]]$majorimportance=="Yes" & habitats[[j]]$season=="Breeding Season" | habitats[[j]]$season=="Resident")
      y=subset(habitats[[j]]$season, habitats[[j]]$suitability=="Suitable" & habitats[[j]]$majorimportance=="Yes" & habitats[[j]]$season=="Breeding Season" | habitats[[j]]$season=="Resident")       
      mydb$biome[i]=x[1]
      if(y[1&2]=="Breeding Season"){
             mydb$movement[i]="Migratory"
      }
      if(y=="Resident"){
            mydb$movement[i]=="Resident"
      }
    }
  }
}

write.csv2(mydb, file = "dataset_Tony_UICNadds3.csv")

### Elton traits: (http://www.esapubs.org/archive/ecol/E095/178/)

download.file(url = "http://www.esapubs.org/archive/ecol/E095/178/BirdFuncDat.txt",
              destfile = "data/BirdFuncDat.txt",
              quiet = T,
              cacheOK = T)

### Rechecking merged data

tab=read.table("C://Users/Tony/Documents/Phd Bielefeld/Data manip/Host-shift _cospeciation/dataset_combined.txt", sep="\t")






