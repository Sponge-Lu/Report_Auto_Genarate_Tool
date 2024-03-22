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

####################### self define file path ##############
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); #set current work path
filepath = "D:/0_Projects/NplusT/0_BiCS6X3_512Gb/1_BlockRD/Ea1.1/Vt_55C";
BlkRDlist = c("0K","15K","30K","100K");

tryCatch({
  ###################### Load data ######################
  cat("...initialize data start... \n");
  datafiles <-list.files(path = filepath, pattern = "^Vt+.*csv", full.names = TRUE, recursive = TRUE);
  if (!is.null(datafiles)){
    finaldatadt <- data.table();
    print(datafiles);
    for (datafile_index in datafiles){
      datadt <- fread(file = datafile_index, header = FALSE);
      datadtlist <- split(datadt, list(cumsum(grepl("CH",datadt$V1))));
      for(ii in seq_len(length(datadtlist))){
        data <- as.data.table(datadtlist[[ii]]);
        colnames(data) <- as.character(data[1,]);
        data <- data[-1,];
        data <- data[,c("BlkRD"):= list(BlkRDlist[ii])];
        data <- data[,c("Block","WordLine","Page","Vt","Count") := list(as.integer(Block),as.integer(WordLine),as.integer(Page),as.integer(Vt),as.integer(Count))];
        finaldatadt <- rbind(finaldatadt,data);
      }
    }
    write.csv(finaldatadt, file=paste0(filepath,"/Initialization_Vt.csv"), row.names = FALSE);
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
