# Control file for EPA's HEM model
# Designed and written for EPA by WGG of ICF
# Last modified September 26, 2017


HEM.setup = function(wd=NULL) {
  # WD is the working directory, assumed to be one level up from the R,
  # input, and output directories. File path names are relative to this.
  # The default value below should be adjusted for each installation.
  if(is.null(wd)) wd <- "C:/main/HEM/code/forGitHub"
  setwd(wd)
  inpath   <- paste0(wd,"/input/")
  outpath  <- paste0(wd,"/output/")
  run      <- "runfile.txt"
  HEMpums  <- "HEMpums.csv"
  HEMahs   <- "HEMahs.csv"
  HEMrecs  <- "HEMrecs.csv"
  states   <- "states.txt"
  rawAHSh  <- "AHSh.csv"
  rawAHSp  <- "AHSp.csv"
  rawRECS  <- "RECS.csv"
  outP     <- "HEMp.csv"
  outPH    <- "HEMph.csv"
  f        <- as.list(c(wd,inpath,outpath,run,HEMpums,HEMahs,HEMrecs,states,rawAHSh,rawAHSp,rawRECS,outP,outPH))
  names(f) <- c("wd","inpath","outpath","run","HEMpums","HEMahs","HEMrecs","states","rawAHSh","rawAHSp","rawRECS","outP","outPH")
  files   <<- f
  
  suppressPackageStartupMessages(TRUE)
  # Load required packages and source all code modules.
  library("data.table")
  library("stringr")
  library("plyr")
  library("dplyr")
  library("dtplyr")
  library("ggplot2")
  library("bit64")
  library("httk")
  library("msm")
  library("truncnorm")
  library("survey")
  source("HEMcontrol.R")
  source("HEMpopgen.R")
  source("httkpop2.R")
  source("HEMhousing.R")
}    


HEM.run = function(runfile=NULL) {
  pop  <<- popgen(runfile)
  dir  <- paste0(files$outpath,g$run.name)
  if(!file.exists(dir)) dir.create(dir,recursive=TRUE)
  filename <- paste0(files$outpath,g$run.name,"/pop.csv") 
  write.csv(pop,filename,row.names = FALSE)
  cat("Population generator completed: R object = 'pop', filename =",filename,"\n")
  pophouse <<- match.pop.housing(pop,NULL,NULL)
  filename <- paste0(files$outpath,g$run.name,"/pophouse.csv") 
  write.csv(pophouse,filename,row.names = FALSE)
  cat("Housing generator completed: R object = 'pophouse', filename =",filename,"\n")
}   


