# Not complete modification -- 2024/4/3
loadallcsvs <- function(path=".", pattern=NULL, productlist, teststeplist, 
                        full.name=TRUE, recursive=TRUE, header = TRUE,...){
  cat("...searching csv files... \n");
  datafiles <-list.files(path = '.', pattern = "^comb+.*(SFR.csv)$", full.names = TRUE, recursive = TRUE);
  datafilelists <- split(datafiles, dirname(datafiles));
  folders<- names(datafilelists);

  filesize <- sapply(datafilelists, function(x)(sum(file.size(x))));
  batchsize <- max(batchsize, filesize);
  workers <- max(min(parallel:::detectCores(),sum(cumsum(sort(filesize, decreasing = T))<batchsize)),1);
  cl <- makeCluster(workers);
  registerDoSNOW(cl);
  if(!dir.exists("./log")){
    dir.create("./log");
  }

  foreach(folderindex=seq_along(folders), .combine = rbind, .packages = c("data.table","officer"))%dopar%{
    datadt <- data.table();
    openlog <- file(paste0("./log/Log_",Sys.getpid(),"_", format(Sys.time(), "%b%d_%Hh%Mm%Ss"), ".txt"), "a");
    cat(paste0("Log Started on ", format(Sys.time(), "%b%d_%Hh%Mm%Ss"), "\n"), file=openlog);
    tryCatch({
      ###################Load data###################
      cat(paste0("Loading data started on", format(Sys.time(), "%b%d_%Hh%Mm%Ss"), "\n"), file=openlog);
      for (file_name in datafilelists[folderindex]){
        data <- fread(file = file_name, header = TRUE);
        datadt <- rbind(datadt, data);
        rm(data);
      }
      cat(paste0("Loading data ended on", format(Sys.time(), "%b%d_%Hh%Mm%Ss"), "\n"), file=openlog);
      
      ###################process data###################
      datadt <- datadt[];
      
      ###################Export data to pptx###################
      #write.csv(datadt, paste0(folders[folderindex],"/FBC_", format(Sys.time(), "%b%d_%Hh%Mm%Ss"), ".csv"), row.names = FALSE);
    },
    error = function(e){
      cat(paste0("[", folders[folderindex],"] Errored. \n"), file=openlog);
      cat(toString(e), file=openlog);
    },
    finally = {
      close(openlog);
    })#end of tryCatch
  }#end of foreach
  stopCluster(cl);
  cat(paste0("Program Ended on ", format(Sys.time(), "%b%d_%Hh%Mm%Ss"), "\n"))
}