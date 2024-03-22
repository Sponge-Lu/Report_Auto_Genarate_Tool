loadallcsvs <- function(path=".", pattern=NULL, productlist, teststeplist, 
                        full.name=TRUE, recursive=TRUE, header = TRUE,...){
  library(readr);
  chunk_size <- 100000; # 每块读取的行数

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
              
              #New method for large size csv file: 分块读取csv,并转化成数据框，最后合并在一起
              if(file.size(datafileindex)>4e9){
                print(paste0("File size is ",round(file.size(datafileindex)/1e9,2),"GB, Use read_csv with chunk_size=",chunk_size," to read data"));
                skip <- 0;
                while (TRUE)
                {
                  print(paste0("-- read column: ", skip,"~",skip+chunk_size," --"));
                  chunk <- read_csv(datafileindex, col_names = FALSE, n_max = chunk_size, show_col_types = FALSE, progress=TRUE, skip = skip);
                  if(is.null(chunk) || nrow(chunk) == 0){break};
                  chunkdt <- as.data.table(chunk);
                  data <- rbind(data,chunkdt);
                  skip <- skip + chunk_size;
                }
                rm(chunk);
                data <- data[,-c("X23")];
                new_colnames <- data[1,];
                data <- data[-1,];
                colnames(data) <- as.character(new_colnames);
              }# Old method for small size csv file
              else{
                print(paste0("File size is ",round(file.size(datafileindex)/1e9,2),"GB, Use fread to read data"));
                data <- fread(file = datafileindex, header = header);
              }
              
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
