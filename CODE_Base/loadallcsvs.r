loadallcsvs <- function(path=".", pattern=NULL, productlist, teststeplist, 
                        full.name=TRUE, recursive=TRUE, header = TRUE,...){
  cat("...searching csv files... \n");
  datafiles <-list.files(path = path, pattern = pattern, full.names = full.name, recursive = recursive);
  if (!is.null(datafiles)){
    cat("...loading files... \n");
    require("data.table");
    data <- list();
    for(datafileindex in datafiles){
      subfolderlist <- strsplit(datafileindex, "/");
      for(product_index in productlist){
        if(grepl(product_index, subfolderlist[[1]][2], ignore.case=TRUE))  #judge if current file in productlist
        {
          for(test_index in teststeplist){
            if(grepl(test_index, subfolderlist[[1]][3], ignore.case=TRUE)) #judge if current file in test_step folder
            {
              print(datafileindex);
              data <- fread(file = datafileindex, header = header);
              data <- data[,c("Product","TestItem","Ea"):=list(strsplit(subfolderlist[[1]][2],"_")[[1]][2],strsplit(subfolderlist[[1]][3],"_")[[1]][2],subfolderlist[[1]][4])];
              datadt <- rbind(datadt, data);
              rm(data);
              gc();
            }
          }
        }
      }
    }
    cat("...read csv complete...data table returned \n");
    return(datadt);
  }
  else{
    cat("...search csv complete...no returned \n");
  }
}
