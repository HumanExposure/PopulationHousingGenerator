# HEM housing generator
# Designed and written by WGG at ICF
# Last modified on Nov 17, 2016



read.ahs = function(filename=NULL) {
  if (is.null(filename)) filename <- paste0(files$inpath,files$HEMahs)
  x1 <- fread(filename,colClasses ="numeric")
  x1[x1<0] <- NA
  return(x1)
}



read.recs = function(filename=NULL) {
  if (is.null(filename)) filename <- paste0(files$inpath,files$HEMrecs)
  x2 <- fread(filename,colClasses ="numeric")
  x2[x2<0] <- NA
  return(x2)
}



match.pop.housing = function(pop,ahsname=NULL,recsname=NULL) {
  cat("\n Starting population-housing matching \n")
  ahs  <- read.ahs(ahsname)
  recs <- read.recs(recsname)
  apool  <- ahs$pool
  a      <- select(ahs,-pool)
  rpool  <- recs$pool
  r      <- select(recs,-pool)
  u.ahs  <- get.randoms("ahs",g$num.persons,g$seeds,g$var.list,0)
  u.recs <- get.randoms("recs",g$num.persons,g$seeds,g$var.list,0)
  ahsvars   <- as.data.table(matrix(0,nrow=nrow(pop),ncol=ncol(a)))
  recsvars  <- as.data.table(matrix(0,nrow=nrow(pop),ncol=ncol(r)))
  setnames(ahsvars,names(ahsvars),names(a))
  setnames(recsvars,names(recsvars),names(r))
  # mode(ahsvars$control)  <- "character"
  for (i in 1:288) {
    x <- pop$row[pop$pool==i]
    if (length(x)>0) {
      y <- a[apool==i]
      if (nrow(y)==0) y <- a[apool==(i-1)]
      if (nrow(y)==0) y <- a[apool==(i-2)]
      y.wt <- cumsum(y$pwt)
      cp.y <- y.wt/y.wt[nrow(y)]
      rows.y <- 1+findInterval(u.ahs[x],cp.y,rightmost.closed=TRUE)
      ahsvars[x] <- y[rows.y]
      z <- r[rpool==i]
      if (nrow(z)==0) z <- r[rpool==(i-1)]
      if (nrow(z)==0) z <- r[rpool==(i-2)]
      if (nrow(z)==0) z <- r[rpool==(i+32)]
      z.wt <- cumsum(z$nweight)
      cp.z <- z.wt/z.wt[nrow(z)]
      rows.z <- 1+findInterval(u.recs[x],cp.z,rightmost.closed=TRUE)
      recsvars[x] <- z[rows.z]
    } 
  }
  ph <- as.data.table(cbind(pop,ahsvars,recsvars))
  ph$family <- 1 + 5*ph$famcat + 4*(ph$famcat %%2)
  return(ph)
} 



perc = function(dt,x){
  m <- length(x)
  y <- order(x)
  z <- order(y)
  p <- (z-0.5)/m
  return(p)
}



prepare.ahs = function(filepath=NULL) {
  if (is.null(filepath)) filename <- paste0(files$inpath,files$rawAHSh)
  newhouse <- fread(filename)
  setnames(newhouse,names(newhouse),tolower(names(newhouse)))
  nh <- newhouse[newhouse$hhage>0,]
  rm(newhouse)
  nh[, `:=` (id,1:nrow(nh))]
  hs <- as.data.table(select(nh,id,region,metro3,nunit2,type,zinc2,control,pwt,baths,bedrms,built,cellar,lot,rooms,
                             sewdis,unitsf,water,waterd,afuel,hequip,cars))
  hs$r.u[hs$metro3=="'3'" | hs$metro3=="'5'"] <- 1
  hs$r.u[hs$metro3=="'1'" | hs$metro3=="'2'" | hs$metro3=="'4'"] <- 2
  hs$reg <- 0
  hs$reg[hs$region=="'1'"]<- 1
  hs$reg[hs$region=="'2'"]<- 2
  hs$reg[hs$region=="'3'"]<- 3
  hs$reg[hs$region=="'4'"]<- 4
  hs$loc <- 2*hs$reg + hs$r.u - 2
  hs$hse[hs$type==1 & hs$nunit2=="'1'"] <- 1
  hs$hse[hs$type==1 & hs$nunit2=="'2'"] <- 1
  hs$hse[hs$type==1 & hs$nunit2=="'3'"] <- 2
  hs$hse[hs$type>1] <- 3
  for (i in 1:8) {
    x <- hs[hs$loc==i]
    hs$pinc[hs$loc==i]<- perc(x,x$zinc2)
  }
  hs$inc  <- ceiling(3*hs$pinc)
  
  filename <- paste0(files$inpath,files$rawAHSp)
  person <- as.data.table(read.csv(filename)) 
  setnames(person,names(person),tolower(names(person)))
  p <- as.data.table(select(person,control,pline,age,sex))
  rm(person)
  p$child            <- 0
  p$child[p$age<18]  <- 1
  p$adult            <- 0
  p$adult[p$age>=18] <- 1
  pc <- ddply(p,"control",summarize,adults=sum(adult),children=sum(child))
  pc$fam[pc$adults<=1 & pc$children==0] <- 10
  pc$fam[pc$adults<=1 & pc$children>0 ] <- 11
  pc$fam[pc$adults>1  & pc$children==0] <- 20
  pc$fam[pc$adults>1  & pc$children>0 ] <- 21
  pc$famcat <- pc$fam %/% 5 + pc$fam %% 10 - 1 
  ahs <- join(hs,pc,by="control",type="inner",match="first")
  ahs$pool <- 36*(ahs$loc-1) + 12*(ahs$hse-1) + 3*(ahs$famcat-1) + ahs$inc
  z <- as.data.table(select(ahs,pool,control,pwt,baths,bedrms,built,cellar,lot,rooms,sewdis,unitsf,water,waterd,afuel,hequip,cars))
  setnames(z,c("sewdis","water","waterd","afuel","cars"),c("cs","cw","cwd","ca","cc"))
  sewdis <- rep(0,nrow(z))
  sewdis[z$cs=="'-6'"] <- -6
  sewdis[z$cs=="'1'"]  <- 1
  sewdis[z$cs=="'3'"]  <- 3
  sewdis[z$cs=="'4'"]  <- 4
  sewdis[z$cs=="'5'"]  <- 5
  water <- rep(0,nrow(z))
  water[z$cw=="'1'"]  <- 1
  water[z$cw=="'2'"]  <- 2
  water[z$cw=="'3'"]  <- 3
  water[z$cw=="'4'"]  <- 4
  water[z$cw=="'5'"]  <- 5
  water[z$cw=="'6'"]  <- 6
  water[z$cw=="'7'"]  <- 7
  waterd <- rep(0,nrow(z))
  waterd[z$cwd=="'1'"]  <- 1
  waterd[z$cwd=="'2'"]  <- 2
  waterd[z$cwd=="'3'"]  <- 3
  waterd[z$cwd=="'4'"]  <- 4
  waterd[z$cwd=="'5'"]  <- 5
  waterd[z$cwd=="'6'"]  <- 6
  waterd[z$cwd=="'7'"]  <- 7
  waterd[z$cwd=="'-6'"] <- -6
  waterd[z$cwd=="'-7'"] <- -7
  waterd[z$cwd=="'-8'"] <- -8
  waterd[z$cwd=="'-9'"] <- -9
  afuel <- rep(0,nrow(z))
  afuel[z$ca=="'-6'"] <- -6
  afuel[z$ca=="'1'"]  <- 1
  afuel[z$ca=="'2'"]  <- 2
  afuel[z$ca=="'3'"]  <- 3
  cars <- rep(0,nrow(z))
  cars[z$cc=="'0'"] <- 0
  cars[z$cc=="'1'"] <- 1
  cars[z$cc=="'2'"] <- 2
  cars[z$cc=="'3'"] <- 3
  cars[z$cc=="'4'"] <- 4
  cars[z$cc=="'5'"] <- 5
  id <- z$control
  substr(id,1,1)   <- " "
  substr(id,14,14) <- " "
  control   <- as.numeric(id)
  z$afuel   <- afuel
  z$cars    <- cars
  z$control <- control
  z$sewdis  <- sewdis
  z$water   <- water
  z$waterd  <- waterd
  
  hemahs <- as.data.table(select(z,pool,afuel,baths,bedrms,built,cars,cellar,hequip,lot,pwt,rooms,sewdis,unitsf,water,waterd,control))
  sahs <- setorder(hemahs,pool)  
  path <- getwd()
  outname <- c(path,"/input/HEMahs.csv")
  write.csv(sahs,outname,row.names=FALSE)
}



prepare.recs = function(filename=NULL) {
  if (is.null(filename)) filename <- paste0(files$inpath,files$rawRECS)
  allrecs <- fread(filename)  
  setnames(allrecs,names(allrecs),tolower(names(allrecs)))
  r <- select(allrecs,agehhmemcat2,agehhmemcat3,agehhmemcat4,agehhmemcat5,agehhmemcat6,agehhmemcat7,agehhmemcat8,
              agehhmemcat9,agehhmemcat10,agehhmemcat11,agehhmemcat12,agehhmemcat13,agehhmemcat14)
  ad2  <- ifelse(r$agehhmemcat2>4,1,0)
  ad3  <- ifelse(r$agehhmemcat3>4,1,0)
  ad4  <- ifelse(r$agehhmemcat4>4,1,0)
  ad5  <- ifelse(r$agehhmemcat5>4,1,0)
  ad6  <- ifelse(r$agehhmemcat6>4,1,0)
  ad7  <- ifelse(r$agehhmemcat7>4,1,0)
  ad8  <- ifelse(r$agehhmemcat8>4,1,0)
  ad9  <- ifelse(r$agehhmemcat9>4,1,0)
  ad10 <- ifelse(r$agehhmemcat10>4,1,0)
  ad11 <- ifelse(r$agehhmemcat11>4,1,0)
  ad12 <- ifelse(r$agehhmemcat12>4,1,0)
  ad13 <- ifelse(r$agehhmemcat13>4,1,0)
  ad14 <- ifelse(r$agehhmemcat14>4,1,0)
  adults <- 1+ad2+ad3+ad4+ad5+ad6+ad7+ad8+ad9+ad10+ad11+ad12+ad13+ad14
  rm(ad2,ad3,ad4,ad5,ad6,ad7,ad8,ad9,ad10,ad11,ad12,ad13,ad14)
  ch2  <- ifelse(r$agehhmemcat2>0  & r$agehhmemcat2 <5,1,0)
  ch3  <- ifelse(r$agehhmemcat3>0  & r$agehhmemcat3 <5,1,0)
  ch4  <- ifelse(r$agehhmemcat4>0  & r$agehhmemcat4 <5,1,0)
  ch5  <- ifelse(r$agehhmemcat5>0  & r$agehhmemcat5 <5,1,0)
  ch6  <- ifelse(r$agehhmemcat6>0  & r$agehhmemcat6 <5,1,0)
  ch7  <- ifelse(r$agehhmemcat7>0  & r$agehhmemcat7 <5,1,0)
  ch8  <- ifelse(r$agehhmemcat8>0  & r$agehhmemcat8 <5,1,0)
  ch9  <- ifelse(r$agehhmemcat9>0  & r$agehhmemcat9 <5,1,0)
  ch10 <- ifelse(r$agehhmemcat10>0 & r$agehhmemcat10<5,1,0)
  ch11 <- ifelse(r$agehhmemcat11>0 & r$agehhmemcat11<5,1,0)
  ch12 <- ifelse(r$agehhmemcat12>0 & r$agehhmemcat12<5,1,0)
  ch13 <- ifelse(r$agehhmemcat13>0 & r$agehhmemcat13<5,1,0)
  ch14 <- ifelse(r$agehhmemcat14>0 & r$agehhmemcat14<5,1,0)
  children <- ch2+ch3+ch4+ch5+ch6+ch7+ch8+ch9+ch10+ch11+ch12+ch13+ch14
  rm(ch2,ch3,ch4,ch5,ch6,ch7,ch8,ch9,ch10,ch11,ch12,ch13,ch14)
  
  recs <- as.data.table(select(allrecs,doeid,regionc,ur,typehuq,moneypy,swimpool,recbath))
  setnames(recs,"regionc","region")
  recs$adults   <- adults
  recs$children <- children
  recs$fam[adults==1 & children==0] <- 10
  recs$fam[adults==1 & children>0 ] <- 11
  recs$fam[adults>1  & children==0] <- 20
  recs$fam[adults>1  & children>0 ] <- 21
  recs$famcat <- recs$fam %/% 5 + recs$fam %% 10 - 1 
  recs$r.u[recs$ur=="R"]<- 1
  recs$r.u[recs$ur=="U"]<- 2
  recs$loc <- 2*recs$region + recs$r.u - 2
  recs$hse[recs$typehuq==2 | recs$typehuq==3] <- 1
  recs$hse[recs$typehuq==4 | recs$typehuq==5] <- 2
  recs$hse[recs$typehuq==1]                   <- 3
  for (i in 1:8) {
    x <- recs[recs$loc==i]
    recs$pinc[recs$loc==i]<- perc(x,x$moneypy)
  }
  allrecs$nweight <- round(1000*allrecs$nweight)
  recs$inc  <- ceiling(3*recs$pinc)
  recs$swim <- recs$recbath
  recs$swim[recs$swimpool==1] <- recs$swim[recs$swimpool==1]+2
  recs$pool <- 36*(recs$loc-1) + 12*(recs$hse-1) + 3*(recs$famcat-1) + recs$inc
  allrecs[, `:=` (pool,recs$pool)]
  allrecs[, `:=` (swim,recs$swim)]
  hemrecs <- as.data.table(select(allrecs,pool,doeid,nweight,hdd30yr,cdd30yr,kownrent,condcoop,naptflrs,stories,stoven,stovenfuel,
                                  stove,stovefuel,oven,ovenfuel,ovenuse,outgrill,dishwash,cwasher,washload,dryer,dryruse,tvcolor,
                                  computer,numpc,pcprint,moisture,prkgplc1,prkgplc2,cooltype,tempniteac,numberac,numcfan,notmoist,
                                  highceil,windows,adqinsul,drafty,swim))
  srecs <- setorder(hemrecs,pool)
  rm(allrecs,recs)
  path <- getwd()
  outname <- paste0(path,"/input/HEMrecs.csv")
  write.csv(srecs,outname,row.names=FALSE)
}


