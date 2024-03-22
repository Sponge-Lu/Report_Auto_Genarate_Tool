loadalltexts <- function(path=".", pattern=NULL, full.name=TRUE, recursive=FALSE, header = FALSE,...){
  datafiles <-list.files(path = path, pattern = pattern, full.names = full.name, recursive = recursive);
  if (!is.null(datafiles)){
    require("data.table");
    data <- data.table();
    for(datafileindex in datafiles){
      filename <- strsplit(datafileindex,"/")[[1]][3];
      filename <- strsplit(filename,".txt")[[1]][1];
      namecomponent <- strsplit(filename,"_")[[1]];
      data <- fread(file = datafileindex, header = header);
      data <- data[5,];
      data <- data.table(strsplit(data$V1[[1]]," ")[[1]]);
      setnames(data,"V1","Bitflip");
      data <- data[,c("ProgramType","slice","chip_die","block","PEC") := list(paste0(namecomponent[1],"_",namecomponent[2]),
                                                                                         rev(namecomponent)[9],
                                                                                         rev(namecomponent)[6],
                                                                                         rev(namecomponent)[3],
                                                                                         rev(namecomponent)[1])];
      datadt <- rbind(datadt, data);
      gc();
    }
    cat("...read text complete...data table returned \n");
    return(datadt);
  }
  else{
    cat("...search text complete...no returned \n");
  }
}
