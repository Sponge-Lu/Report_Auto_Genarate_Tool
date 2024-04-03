#remve all variables firstly
rm(list = ls()); 
gc();
batchsize <- 2000e6; #1/5 of availables memory is suggested #unit in Bytes (e.g. 1e6 = 1MB)
#n_cores <- detectCores() - 1;

require("rstudioapi");
require("doSNOW");
require("data.table");
require("ggplot2");
require("officer");
source('D:/Rscript/CODE_Base/loadallcsvs.r', local = FALSE);

productlist = c("BiCS6"); #input the key word of the folders that you want to process
teststeplist = c("BlockRD"); #input the key word of the test that you want to process
####################### self define file path ##############
filepath = "./0_BiCS6X3_512Gb/Initial_data_for_Ea1.1_BLKRD";
if(!dir.exists(filepath)){
  dir.create(filepath);
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); #set current work path
blocklist <- data.table();
blocklist <- fread(file = "./Block.csv", header = TRUE);
filterfile <- fread("filter.csv", header = TRUE);
filterfile <- filterfile[,c("filter_blocks"):= list(paste0(Channel,"_",CE,"_",LUN,"_",Block,"_",Product,"_",Ea))];
filterlist <- unique(filterfile$filter_blocks);

tryCatch({
  ###################### Load data ######################
  datadt <- data.table();
  datadt <- loadallcsvs(path=".", pattern="^(HS)+.*csv", productlist, teststeplist, full.name=TRUE, recursive=TRUE, header = TRUE);

  ###################### Initialize data ######################
  cat("...initialize data start... \n");
  datadt <- datadt[,-c("Id","SegmentIndex","Vcc","VccQ","Vpp","DataRate","Tag","WordLine")];
  datadt <- datadt[!(SegmentName=="Initialize" | SegmentName=="DeviceConfig" | SegmentName=="DQCalib" | Function=="TURN_OFF"),];
  datadt <- datadt[,c("filter_cond"):= list(paste0(Channel,"_",Ce,"_",Lun,"_",Block,"_",Product,"_",Ea))];
  datadt <- datadt[!(filter_cond %in% filterlist),]; #only filter the bad block data for tread and bitflip
  datadt <- datadt[,-c("filter_cond")];
  datadt <- datadt[,c("Block"):=list(as.integer(sapply(strsplit(as.character(Block),"-"),"[",1)))];
  datadt <- merge.data.table(datadt, blocklist, all=TRUE);
  datadt <- datadt[,c("Page","Counter") := list(as.integer(Page),as.integer(Counter))];
  datadt <- datadt[,c("Level","WordLine") := list(ifelse(Page%%3==0,"LSB",ifelse(Page%%3==1,"CSB","MSB")),as.integer(Page%/%3))];
  datadt <- datadt[,c("MaxString") := list(ifelse(grepl("BiCS4",Product,ignore.case=FALSE),4,
                                                  ifelse(grepl("BiCS5",Product,ignore.case=FALSE),4,
                                                         ifelse(grepl("BiCS6",Product,ignore.case=FALSE),5,
                                                                ifelse(grepl("BiCS8",Product,ignore.case=FALSE),5,
                                                                       ifelse(grepl("X39070",Product,ignore.case=FALSE),6,"NA"))))))]
  datadt <- datadt[,c("Layer","String") := list(as.integer(Page%/%(MaxString*3)),as.integer((Page%%(MaxString*3))%/%3))];
  datadt <- datadt[,-c("MaxString")];
  
  ## tERS
  cat("... tERS ... \n");
  erasedadadt <- datadt[Measurement=="tERS",];
  erasedadadt <- erasedadadt[,-c("Measurement","Page","Level")];
  setnames(erasedadadt, "Value", "tERASE");
  write.csv(erasedadadt,file=paste0(filepath,"Terase.csv"), row.names = FALSE);
  rm(erasedadadt);
  ## tPROG
  cat("... tPROG ... \n");
  progdatadt <- datadt[Measurement=="tPROG",];
  progdatadt <- progdatadt[,-c("Measurement","Page","Level")];
  setnames(progdatadt, "Value", "tPROG");
  write.csv(progdatadt,file=paste0(filepath,"Tprog.csv"), row.names = FALSE);
  rm(progdatadt);
  ## tREAD
  cat("... tREAD ... \n");
  readdatadt <- datadt[Measurement=="tR",];
  readdatadt <- readdatadt[,-c("Measurement")];
  setnames(readdatadt, "Value", "tREAD");
  write.csv(readdatadt,file=paste0(filepath,"Tread.csv"), row.names = FALSE);
  rm(readdatadt);
  ReadBitflip
  cat("... Bitflip ... \n");
  fbcdatadt <- datadt[Measurement=="FAILS4CHUNK",];
  rm(datadt);
  gc();
  fbcdatadt <- fbcdatadt[,-c("Measurement")];
  fbcdatadt <- fbcdatadt[,c("Layer","String","Level") := list(as.integer(Page%/%15),as.integer((Page%%15)%/%3),
                                                              ifelse(Page%%3==0,"LSB",ifelse(Page%%3==1,"CSB","MSB")))];
  fbcdatadt <- fbcdatadt[,c("Sector1","Sector2","Sector3","Sector4"):=list(as.integer(sapply(strsplit(Value,"@"),"[",1)),as.integer(sapply(strsplit(Value,"@"),"[",2)),
                                                                           as.integer(sapply(strsplit(Value,"@"),"[",3)),as.integer(sapply(strsplit(Value,"@"),"[",4)))];
  fbcdatadt <- fbcdatadt[,c("TotalFBC","AvgFBC"):= list(as.integer(Sector1+Sector2+Sector3+Sector4),as.numeric((Sector1+Sector2+Sector3+Sector4)/4))];
  fbcdatadt$MaxFBC <- as.integer(apply(fbcdatadt,1,function(fbcdatadt,c1=c("Sector1","Sector2","Sector3","Sector4")){max(fbcdatadt[c1])}));
  fbcdatadt <- fbcdatadt[,-c("Value","Sector1","Sector2","Sector3","Sector4")];
  write.csv(fbcdatadt,file=paste0(filepath,"ReadBitflip.csv"), row.names = FALSE);
  rm(fbcdatadt);
  
  cat("... Bitflip ... \n");
  datadt <- datadt[Measurement=="FAILS4CHUNK",];
  datadt <- datadt[,-c("Measurement")];
  datadt <- datadt[,c("Layer","String","Level") := list(as.integer(Page%/%15),as.integer((Page%%15)%/%3),
                                                              ifelse(Page%%3==0,"LSB",ifelse(Page%%3==1,"CSB","MSB")))];
  datadt <- datadt[,c("Sector1","Sector2","Sector3","Sector4"):=list(as.integer(sapply(strsplit(Value,"@"),"[",1)),as.integer(sapply(strsplit(Value,"@"),"[",2)),
                                                                           as.integer(sapply(strsplit(Value,"@"),"[",3)),as.integer(sapply(strsplit(Value,"@"),"[",4)))];
  datadt <- datadt[,c("TotalFBC","AvgFBC"):= list(as.integer(Sector1+Sector2+Sector3+Sector4),as.numeric((Sector1+Sector2+Sector3+Sector4)/4))];
  datadt$MaxFBC <- as.integer(apply(datadt,1,function(datadt,c1=c("Sector1","Sector2","Sector3","Sector4")){max(fbcdatadt[c1])}));
  datadt <- datadt[,-c("Value","Sector1","Sector2","Sector3","Sector4")];
  write.csv(datadt,file=paste0(filepath,"ReadBitflip.csv"), row.names = FALSE);
  rm(datadt);
  
  cat("...initialize data end... \n");
}, #end of data process
error = function(e){
  cat(toString(e));
}#end of error
)#end of tryCatch

gc()
