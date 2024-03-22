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
require("data.table");
require("readr");
source('D:/Rscript/CODE_Base/loadallcsvs.r', local = FALSE);
chunk_size <- 2000000; # 每块读取的行数
first_chunk_flag = 1;

productlist = c("BiCS6"); #input the key word of the folders that you want to process
teststeplist = c("BlockRD"); #input the key word of the test that you want to process
####################### self define file path ##############
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); #set current work path
filepath = "./0_BiCS6X3_512Gb/Initial_data_for_Ea1.1_BLKRD";
if(!dir.exists(filepath)){
  dir.create(filepath);
}

blocklist <- data.table();
blocklist <- fread(file = "./Block.csv", header = TRUE);
filterfile <- fread("filter.csv", header = TRUE);
filterfile <- filterfile[,c("filter_blocks"):= list(paste0(Channel,"_",CE,"_",LUN,"_",Block,"_",Product,"_",Ea))];
filterlist <- unique(filterfile$filter_blocks);

tryCatch({
  ###################### Load data ######################
  cat("...initialize data start... \n");
  datafiles <-list.files(path = ".", pattern = "^(HS)+.*csv", full.names = TRUE, recursive = TRUE);
  if (!is.null(datafiles)){
    for(datafile_index in datafiles){
      subfolderlist <- strsplit(datafile_index, "/");
      for(product_index in productlist){
        if(grepl(product_index, subfolderlist[[1]][2], ignore.case=TRUE))  #judge if current file in productlist
        {
          for(test_index in teststeplist){
            if(grepl(test_index, subfolderlist[[1]][3], ignore.case=TRUE)) #judge if current file in test_step folder
            {
              print(datafile_index);
              #New method for large size csv file: 分块读取csv,并转化成数据框并处理，处理结束写入csv，然后下一个块
              print(paste0("File size is ",round(file.size(datafile_index)/1e9,2),"GB, Use read_csv with chunk_size=",chunk_size," to read data"));
              skip <- 0;
              file_counter <- 1;

              while (TRUE)
              {
                print(paste0("-- read column: ", skip,"~",skip+chunk_size," --"));
                chunk <- read_csv(datafile_index, col_names = FALSE, n_max = chunk_size, show_col_types = FALSE, progress=TRUE, skip = skip);
                print(length(chunk));
                if(is.null(chunk) || nrow(chunk) == 0){break};
                datadt <- as.data.table(chunk);
                datadt <- datadt[,-c("X23")];
                
                if(first_chunk_flag == 1){
                  new_colnames <- datadt[1,];
                  datadt <- datadt[-1,];
                }
                colnames(datadt) <- as.character(new_colnames);
                
                datadt <- datadt[,c("Product","TestItem","Ea"):=list(strsplit(subfolderlist[[1]][2],"_")[[1]][2],strsplit(subfolderlist[[1]][3],"_")[[1]][2],subfolderlist[[1]][4])];
                datadt <- datadt[,-c("Id","SegmentIndex","Vcc","VccQ","Vpp","DataRate","Tag","WordLine")];
                datadt <- datadt[!(SegmentName=="Initialize" | SegmentName=="DeviceConfig" | SegmentName=="DQCalib" | Function=="TURN_OFF"),];
                datadt <- datadt[,c("filter_cond"):= list(paste0(Channel,"_",Ce,"_",Lun,"_",Block,"_",Product,"_",Ea))];
                datadt <- datadt[!(filter_cond %in% filterlist),]; #only filter the bad block data for tread and bitflip
                datadt <- datadt[,-c("filter_cond")];
                datadt <- datadt[,c("Block"):=list(as.integer(sapply(strsplit(as.character(Block),"-"),"[",1)))];
                datadt <- merge.data.table(datadt, blocklist, all=TRUE);
                datadt <- datadt[,c("Page","Counter") := list(as.integer(Page),as.integer(Counter))];
                datadt <- datadt[,c("Level","WordLine") := list(ifelse(Page%%3==0,"LSB",ifelse(Page%%3==1,"CSB","MSB")),as.integer(Page%/%3))];
                datadt <- datadt[,c("MaxString") := list(as.integer(ifelse(grepl("BiCS4",Product,ignore.case=FALSE),4,
                                                                          ifelse(grepl("BiCS5",Product,ignore.case=FALSE),4,
                                                                                 ifelse(grepl("BiCS6",Product,ignore.case=FALSE),5,
                                                                                        ifelse(grepl("BiCS8",Product,ignore.case=FALSE),5,
                                                                                               ifelse(grepl("X39070",Product,ignore.case=FALSE),6,"NA")))))))];
                datadt <- datadt[,c("Layer","String") := list(as.integer(Page%/%(MaxString*3)),as.integer((Page%%(MaxString*3))%/%3))];
                datadt <- datadt[,-c("MaxString")];
                
                datadt <- datadt[Measurement=="FAILS4CHUNK",];
                datadt <- datadt[,-c("Measurement")];
                datadt <- datadt[,c("Layer","String","Level") := list(as.integer(Page%/%15),as.integer((Page%%15)%/%3),
                                                                      ifelse(Page%%3==0,"LSB",ifelse(Page%%3==1,"CSB","MSB")))];
                datadt <- datadt[,c("Sector1","Sector2","Sector3","Sector4"):=list(as.integer(sapply(strsplit(Value,"@"),"[",1)),as.integer(sapply(strsplit(Value,"@"),"[",2)),
                                                                                   as.integer(sapply(strsplit(Value,"@"),"[",3)),as.integer(sapply(strsplit(Value,"@"),"[",4)))];
                datadt <- datadt[,c("TotalFBC","AvgFBC"):= list(as.integer(Sector1+Sector2+Sector3+Sector4),as.numeric((Sector1+Sector2+Sector3+Sector4)/4))];
                datadt$MaxFBC <- as.integer(apply(datadt,1,function(datadt,c1=c("Sector1","Sector2","Sector3","Sector4")){max(datadt[c1])}));
                datadt <- datadt[,-c("Value","Sector1","Sector2","Sector3","Sector4")];
                
                if(first_chunk_flag==1)
                {
                  write_csv(datadt,file=paste0(filepath,"/ReadBitflip.csv"), col_names = TRUE,append=FALSE);
                }
                else{
                  write_csv(datadt,file=paste0(filepath,"/ReadBitflip.csv"),col_names = FALSE, append=TRUE);
                }
                rm(chunk);
                rm(datadt);
                gc();
                first_chunk_flag = 0;
                skip <- skip + chunk_size;
                file_counter <- file_counter+1;
              }
            }
          }
        }
      }
    }
  }
  else{
    cat("...search csv complete...no returned \n");
  }
  cat("...initialize data end... \n");
}, #end of data process
error = function(e){
  cat(toString(e));
}#end of error
)#end of tryCatch

gc()
