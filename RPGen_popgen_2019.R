

# Source code for EPA's HEM population generator module
# Designed and written by WGG at ICF 
# Last modified on July 18, 2019


popgen = function (runfile=NULL) {
  cat("\n HEM population generator module")
  # HEM.setup()
  if(!is.null(runfile)) specs <- read.popfile(runfile)
  if(is.null(runfile))  specs <- read.console()
  specs$var.list <- c("pums","ahs","recs","months","nkde","hw1","hw2","norm",
                      "logn","flow","adip","gliv","hema","rfun1","rfun2")
  specs$seeds    <- get.seeds(specs$run.seed,length(specs$var.list))
  g       <<- specs
  cat("Saved run settings\n")
  pums1    <- read.pums()
  sel.pop  <- gen.pop(pums1)
  pop0     <- pums1[sel.pop]
  pop0$row <- 1:nrow(pop0)
  popx     <- httkvars(pop0)
  indiv_dt <- gen_height_weight(popx,g)
  indiv_dt <- tissue_masses_flows(tmf_dt = indiv_dt)
  indiv_dt <- estimate_hematocrit(hcttmp_dt = indiv_dt)
  indiv_dt <- estimate_gfr(gfrtmp.dt = indiv_dt)
  indiv_dt[, `:=` (bmi_adj, weight_adj/((height/100)^2))]
  indiv_dt[, `:=` (bmi,     weight/((height/100)^2))]
  indiv_dt[, `:=` (BSA, body_surface_area(weight, height, age_years))]
  indiv_dt[, `:=` (age_years,  age)]
  indiv_dt[, `:=` (age_months, temp_age_months)]  
  indiv_dt <- adjust_weight(y = indiv_dt)
  indiv_dt$q.gliv <- NULL
  setnames(indiv_dt,"vehicles","cars")
  indiv_dt <- setorder(indiv_dt,num)
  return(indiv_dt)
}



read.popfile = function(popfile){
  
  run.name       <- "test"
  num.persons    <- 10
  min.age        <- 3 
  max.age        <- 5
  gender         <- c("M","F")
  ethnicity      <- c("N","M","O")
  race           <- c("W","B","N","A","P","O")
  run.seed       <- 876144637
  filename       <- paste0(files$inpath,files$states)
  allstates      <- fread(filename,colClasses = "character")
  
  a <- read.table(paste0("input/",popfile),skip=1,sep="=",
                  stringsAsFactors=FALSE,strip.white=TRUE)
  a$V1 <- tolower(a$V1)
  a$V2 <- gsub(" ","",a$V2)
  for (i in 1:nrow(a)) {
    if (str_trim(a$V1[i])=="run.name")       run.name      <- str_trim(a$V2[i])
    if (str_trim(a$V1[i])=="num.persons")    num.persons   <- as.integer(a$V2[i])
    if (str_trim(a$V1[i])=="min.age")        min.age       <- as.integer(a$V2[i])
    if (str_trim(a$V1[i])=="max.age")        max.age       <- as.integer(a$V2[i])
    if (str_trim(a$V1[i])=="gender")         gender        <- c(str_trim(a$V2[i]))
    if (str_trim(a$V1[i])=="ethnicity")      ethnicity     <- c(str_trim(a$V2[i]))
    if (str_trim(a$V1[i])=="race")           race          <- c(str_trim(a$V2[i]))
    if (str_trim(a$V1[i])=="run.seed")       run.seed      <- as.integer(a$V2[i])
    if (str_trim(a$V1[i])=="regions")        regions       <- str_trim(a$V2[i])
    if (str_trim(a$V1[i])=="states")         states        <- splitpairs(a$V2[i])
  }
  
  dir  <- paste0("output/",run.name)
  if(!file.exists(dir)) dir.create(dir,recursive=TRUE)
  gender <- toupper(gender)
  b    <- ""
  if (str_detect(gender,"M"))  b <- c(b,"M")
  if (str_detect(gender,"F"))  b <- c(b,"F")
  gender.list <- b[2:length(b)]
  ethnicity <- toupper(ethnicity)
  b       <- ""
  if (str_detect(ethnicity,"N"))  b <- c(b,"N")
  if (str_detect(ethnicity,"M"))  b <- c(b,"M")
  if (str_detect(ethnicity,"O"))  b <- c(b,"O")
  ethnicity.list <- b[2:length(b)]
  race <- toupper(race)
  b <- ""
  if (str_detect(race,"W"))  b <- c(b,"W")
  if (str_detect(race,"B"))  b <- c(b,"B")
  if (str_detect(race,"N"))  b <- c(b,"N")
  if (str_detect(race,"A"))  b <- c(b,"A")
  if (str_detect(race,"P"))  b <- c(b,"P")
  if (str_detect(race,"O"))  b <- c(b,"O")
  race.list <- b[2:length(b)]
  if (regions[1]=="" & states[1]=="") states = c(allstates$FIPS)
  if (str_detect(regions,"1")) states <- c(states,allstates$FIPS[allstates$region=="1"])
  if (str_detect(regions,"2")) states <- c(states,allstates$FIPS[allstates$region=="2"])
  if (str_detect(regions,"3")) states <- c(states,allstates$FIPS[allstates$region=="3"])    
  if (str_detect(regions,"4")) states <- c(states,allstates$FIPS[allstates$region=="4"])
  states  <- unique(states[order(states)])
  states  <- states[states!=""]
  allregs <- unique(allstates[allstates$FIPS %in% states]$region)
  
  if (num.persons<=0)          stop("\n No persons to model \n")
  if (is.null(gender.list))    stop("\n No gender selected \n")
  if (is.null(ethnicity.list)) stop("\n No ethnicity selected \n")
  if (is.null(race.list))      stop("\n No race selected \n")
  if (max.age<min.age)         stop("\n Max age < min age \n")
  
  
  cat("\n")
  cat("run.name      =",run.name,"\n")
  cat("num.persons   =",num.persons,"\n")
  cat("min.age       =",min.age,"\n")
  cat("max.age       =",max.age,"\n")
  cat("gender        =",gender,"\n")
  cat("ethnicity     =",ethnicity,"\n")
  cat("race          =",race,"\n")
  cat("run.seed      =",run.seed,"\n")
  cat("states        =",states,"\n")
  cat("regions       =",regions,"\n")
  
  specs <- list(
    run.name       = run.name,
    num.persons    = num.persons,
    min.age        = min.age,
    max.age        = max.age,
    gender         = gender,
    ethnicity      = ethnicity,
    race           = race,
    run.seed       = run.seed,
    states         = states,
    regions        = regions, 
    allregs        = allregs)
  return(specs)
  
}



read.console = function() {
  if(exists("specs")) rm(specs,inherits=TRUE)
  run.name <- ""
  while (run.name=="") {
    run.name    <- readline("Name for this run: ")
    if (run.name=="") cat("\n Error: run name is blank ")
  }  
  num.persons <- 0
  while (is.na(num.persons) | num.persons<=0) {
    num.persons <- as.numeric(readline("Number of simulated persons: "))
    if (is.na(num.persons)) cat("\n Error: number of persons missing")
    else if (num.persons<=0) cat("\n Error: number of persons not > 0")
  }     
  ok <- 0
  while (ok==0) {
    min.age <- as.numeric(readline("Minimum age in this run (years): "))
    max.age <- as.numeric(readline("Maximum age in this run (years): "))
    if (min.age<0)  cat("\n Error: Minimum age must be 0-99 ")
    if (min.age>99) cat("\n Error: Maximum age must be 0-99 ")
    if (min.age>max.age) cat("\n Error: Minimum cannot exceed maximum: ")
    if (min.age>=0 & max.age<=99 & min.age <=max.age) ok<-1 else cat("\n Redo age limits")
  }
  ok <- 0
  while (ok==0) {
    yn <- readline("Include males (y/n): ")
    if (substr(tolower(yn),1,1)=="n") males<-"" else males<-"M"
    yn <- readline("Include females (y/n): ")
    if (substr(tolower(yn),1,1)=="n") females<-"" else females<-"F"
    if (males=="M" | females=="F") ok<-1 else cat ("\n Pick males, females, or both")
  }
  gender <- paste0(females,males)
  ok <- 0
  while (ok==0) {
    yn <- readline("Include non-Hispanic (y/n): ")
    if (substr(tolower(yn),1,1)=="n") nonh<-"" else nonh<-"N"
    yn <- readline("Include Mexican-American (y/n): ")
    if (substr(tolower(yn),1,1)=="n") mex<-"" else mex<-"M"
    yn <- readline("Include Other Hispanic (y/n): ")
    if (substr(tolower(yn),1,1)=="n") otherh<-"" else otherh<-"O"
    if (nonh=="N" | mex=="M" | otherh=="O") ok<-1 else cat ("\n Pick at least one ethnicity")
  }   
  ethnicity <- paste0(nonh,mex,otherh)
  ok <- 0
  while (ok==0) {
    yn <- readline("Include White persons (y/n): ")
    if (substr(tolower(yn),1,1)=="n") white<-"" else white<-"W"
    yn <- readline("Include African-Americans (y/n): ")
    if (substr(tolower(yn),1,1)=="n") black<-"" else black<-"B"
    yn <- readline("Include Native Americans (y/n): ")
    if (substr(tolower(yn),1,1)=="n") native<-"" else native<-"N"
    yn <- readline("Include Asian Americans (y/n): ")
    if (substr(tolower(yn),1,1)=="n") asian<-"" else asian<-"A"
    yn <- readline("Include Pacific Islanders (y/n): ")
    if (substr(tolower(yn),1,1)=="n") pacific<-"" else pacific<-"P"
    yn <- readline("Include multiple/other races (y/n): ")
    if (substr(tolower(yn),1,1)=="n") multrace<-"" else multrace<-"O"
    if (white=="W" | black=="B" | native=="N" | asian=="A" | pacific=="P" | multrace=="O") ok<-1 else 
      cat ("Pick at least one race")
  }  
  race <- paste0(white,black,native,asian,pacific,multrace)
  run.seed <- 0
  while (is.na(run.seed) | run.seed<=0 | run.seed > 2147483646 ) {
    run.seed <- as.numeric(readline("Initial random number seed (1-2147483646): "))
    if (is.na(run.seed)) cat("\n Error: Random number seed missing")
    else if (run.seed<=0) cat("\n Error: Random number seed not > 0")
    else if(run.seed > 2147483646) cat("\n Error: Run seed over maximum of 2147483646")
  }  
  filename  <- paste0(files$inpath,files$states)
  allstates <- fread(filename,colClasses = "character")
  rg  <- readline("Enter list of region codes:")
  rg2 <- str_replace_all(rg, "[ .,]", "")
  rg3 <- string.values(rg2)
  st  <- readline("Enter list of state FIPS codes:") 
  st2 <- str_replace_all(st, "[ .,]", "")
  st3 <- string.values(st2)
  if (rg3=="" & st3[1]=="") st3 <- c(allstates$FIPS) 
  if (str_detect(rg3,"1"))  st3 <- c(st3,allstates$FIPS[allstates$region=="1"])
  if (str_detect(rg3,"2"))  st3 <- c(st3,allstates$FIPS[allstates$region=="2"])
  if (str_detect(rg3,"3"))  st3 <- c(st3,allstates$FIPS[allstates$region=="3"])    
  if (str_detect(rg3,"4"))  st3 <- c(st3,allstates$FIPS[allstates$region=="4"])
  st4     <- unique(st3[order(st3)])
  states  <- st4[st4!=""]
  allregs <- unique(allstates[allstates$FIPS %in% states]$region)
  
  specs <- list(
    run.name       = run.name,
    num.persons    = num.persons,
    min.age        = min.age,
    max.age        = max.age,
    gender         = gender,
    ethnicity      = ethnicity,
    race           = race,
    run.seed       = run.seed,
    states         = states,
    regions        = rg3,
    allregs        = allregs)
  return(specs)
}



get.seeds = function(init.seed,num) {
  n     <- 2*num
  base  <- as.integer64(2147483647)
  mult  <- as.integer64(397204094)
  seeds <- vector("integer",n)
  x     <- as.integer64(init.seed)
  for (i in 1:n) {
    x <- (x * mult) %% base
    seeds[i] <- as.numeric(x)
  }
  return(seeds)
}



get.randoms = function(var,n,seeds,varlist,flag) {
  if(flag==0) off <- 0 else off <- 1
  b <- 2*match(var,varlist)-1+off
  if(off==0) off<- -1
  if(is.na(b)) stop(var," not found")
  set.seed(seeds[b:(b-off)],"Marsaglia-Multicarry")
  return(runif(n))
}



splitpairs = function(str) {
  n <- str_length(str)/2
  x <- substr(str,1,2)
  if (n>1) { 
    for (i in 2:n) {
      x <- c(x,substr(str,2*i-1,2*i))
    }
  }
  return(x)
}



string.values = function(x){
  y <- str_replace_all(x,","," ")
  w <- lapply(str_split(y," "),as.numeric)[[1]]
  v <- w[!is.na(w)]
  return(v)
}



cumul.prob = function(x) {
  if (min(x)<0) cat("Negative values in probability vector")
  y <- cumsum(x)
  return(y/max(y))
}



read.pums = function() {
  filename <- paste0(files$inpath,substr(files$HEMpums,1,str_length(files$HEMpums)-4))
  kept <- 0
  for (i in 1:4) {
    if (i %in% g$allregs) {
      x <- fread(paste0(filename,i,".csv"),sep=",",quote="")
      setnames(x,1,"pool")
      setnames(x,length(names(x)),"vehicles")
      x$compid <- x$compid + 10000000   # to force leading zero on state FIPS
      x[, `:=`(state, substr(compid,2,3))]
      x1 <- x[x$state %in% g$states]
      x2 <- x1[x1$age>=g$min.age & x1$age<=g$max.age]
      if (kept==0) y <- x2
      if (kept>0)  y <- rbind(y,x2)
      kept <- 1
    }
  }
  pums <- y[str_detect(g$gender,y$gender) & str_detect(g$race,y$race) & str_detect(g$ethnic,y$ethnicity)]
  if (nrow(pums)==0) stop("No suitable persons found in PUMS database")
  return(pums)
}  



gen.pop = function(pums) {
  u.pop   <- get.randoms("pums",g$num.persons,g$seeds,g$var.list,0)
  cp.pop  <- cumul.prob(pums$pwgtp)
  return( 1+findInterval(u.pop,cp.pop,rightmost.closed=TRUE))
}



httkvars = function(p) {
  reths <- c("Mexican American", "Other Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Other")
  q <- data.table::copy(p)
  nreth <- rep(5,nrow(q))
  nreth[q$ethnicity=="M"] <- 1
  nreth[p$ethnicity=="O"] <- 2
  nreth[p$ethnicity=="N" & p$race=="W"] <- 3
  nreth[p$ethnicity=="N" & p$race=="B"] <- 4
  q[, `:=` (reth, as.factor(reths[nreth]))]
  q.months <- get.randoms("months",nrow(q),g$seeds,g$var.list,0)
  q[, `:=` (age_months,12*q$age+floor(12*q.months))]
  q[, `:=` (temp_age_months,12*q$age+floor(12*q.months))]
  q[, `:=` (age_years, age)]
  q$gender[q$gender=="M"] <- "Male"
  q$gender[q$gender=="F"] <- "Female"
  q[, `:=` (num,1:nrow(q))]
  return(q)
}



bi_norm_cor = function(q,px=c(0,1),py=c(0,1),rho=0,means=c(0,0),sigma=NULL) {
  if (!is.null(sigma)) {
    px[1] <- means[1]
    py[1] <- means[2]
    px[2] <- sigma[1,1]^0.5
    py[2] <- sigma[2,2]^0.5
    rho   <- sigma[1,2]/(px[2]*py[2])
  }
  x   <- qnorm(q[,1],mean=px[1],sd=px[2])
  y   <- qnorm(q[,2],mean=py[1]+rho*py[2]*(x-px[1])/px[2],sd=py[2]*(1-rho^2)^0.5)
  return(cbind(x,y))
}







