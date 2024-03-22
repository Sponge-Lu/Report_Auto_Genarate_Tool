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
# source('D:/Rscript/CODE_Base/sumplot.r', local = FALSE);
# source('D:/Rscript/CODE_Base/sumplot2.r', local = FALSE);

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); #set current work path
templatefile = file.path('D:/Rscript/CODE_Base', 'Report_Template.pptx');
pptx <- read_pptx(templatefile); pptxtitle = "YMTCX39070 Endurance Test";
if(!file.exists(templatefile)) stop(paste0("Not existing template file: ", templatefile));

if(!dir.exists("./log")){
  dir.create("./log");
}
openlog <- file(paste0("./log/Log_",Sys.getpid(),"_", format(Sys.time(), "%b%d_%Hh%Mm%Ss"), ".txt"), "a");
cat(paste0("Log Started on ", format(Sys.time(), "%b%d_%Hh%Mm%Ss"), "\n"), file=openlog);

tryCatch({
  ###################### Load data ######################
  cat(paste0("Loading data started on", format(Sys.time(), "%b%d_%Hh%Mm%Ss"), "\n"), file=openlog);
  datadt <- data.table();
  datadt <- fread(file = "./Init_raw_data/ReadBitflip.csv", header = TRUE);
  cat(paste0("Loading data ended on", format(Sys.time(), "%b%d_%Hh%Mm%Ss"), "\n"), file=openlog);

  ###################### Initialize data ######################
  if(TRUE){
    cat("...initialize data start... \n");
    datadt[,c("SegmentName"):= list(gsub(".*Pre.*","BeforeEndurance",SegmentName,ignore.case = TRUE))];
    datadt[,c("SegmentName"):= list(gsub(".*Post.*","AfterEndurance",SegmentName,ignore.case = TRUE))];
    datadt <- datadt[,c("SegmentName") := factor(SegmentName, levels = c("BeforeEndurance","AfterEndurance"))];
    datadt <- datadt[,c("Level") := factor(Level, levels = c("LSB","CSB","MSB"))];
    datadt <- datadt[,c("Block") := list(factor(Block, levels=sort(unique(datadt$Block))))];
    datadt <- datadt[,c("ProdMaxWL"):= ave(WordLine, Product, FUN=function(x){max(x)})];
    datadt <- datadt[,c("Normalized_WL") := list(WordLine/ProdMaxWL)];
    datadt <- datadt[,-c("ProdMaxWL")];
    cat("...initialize data end... \n");
  }
  
  ###################### Process data ######################
  # get avg/max FBC by minor dimension
  if(TRUE){
    cat("...process data start... \n");
    fbc_minor_dim = "test"; #Input the data minor dimension
    if(length(grep("Step",fbc_minor_dim))>0){
      datadt <- datadt[,-c("CHP_CE_LUN","Block","Layer","Condition","Type","TestItem","Level")];
      datadt <- datadt[,c("FBC*Count") := list(FBC*Count)];
      datadt <- datadt[,c("SumFBC"):= ave(FBC*Count, Product,Ea,SegmentName,Read,Group,WordLine, FUN=function(x){sum(x)})]; #,Level
      datadt <- datadt[,c("SumCount"):= ave(Count, Product,Ea,SegmentName,Read,Group,WordLine, FUN=function(x){sum(x)})];#,Level
      datadt <- datadt[,c("AvgFBC"):= list(SumFBC/SumCount)];
      datadt <- datadt[,c("MaxFBC"):= ave(FBC, Product,Ea,SegmentName,Read,Group,WordLine, FUN=function(x){max(x)})];#,Level
      datadt <- datadt[,-c("FBC","Count","FBC*Count","SumFBC","SumCount")];
      datadt <- unique(datadt, by=c("Product","Ea","SegmentName","Read","Group","WordLine","AvgFBC","MaxFBC"));#,"Level"
      #获取pre/post delta FBC
      data2 <- dcast(datadt, Product+Group+WordLine+Read+Ea+Normalized_WL~SegmentName, value.var=c("AvgFBC","MaxFBC"), ); #+Level
      data2 <- data2[,c("Delta_AvgFBC", "Delta_MaxFBC") := list(AvgFBC_PostPEC-AvgFBC_PrePEC, MaxFBC_PostPEC-MaxFBC_PrePEC)];
    }
    else if(length(grep("Level",fbc_minor_dim))>0){
      #analysis wl
      datadt <- datadt[,-c("CHP_CE_LUN","Block","Layer","Condition","Type","TestItem")];
      datadt <- datadt[,c("FBC*Count") := list(FBC*Count)];
      datadt <- datadt[,c("SumFBC"):= ave(FBC*Count, Product,Ea,SegmentName,Read,Group,WordLine,Level, FUN=function(x){sum(x)})]; #,Level
      datadt <- datadt[,c("SumCount"):= ave(Count, Product,Ea,SegmentName,Read,Group,WordLine,Level, FUN=function(x){sum(x)})];#,Level
      datadt <- datadt[,c("AvgFBC"):= list(SumFBC/SumCount)];
      datadt <- datadt[,c("MaxFBC"):= ave(FBC, Product,Ea,SegmentName,Read,Group,WordLine,Level, FUN=function(x){max(x)})];#,Level
      datadt <- datadt[,-c("FBC","Count","FBC*Count","SumFBC","SumCount")];
      datadt <- unique(datadt, by=c("Product","Ea","SegmentName","Read","Group","WordLine","Level","AvgFBC","MaxFBC"));#,"Level"
      #获取pre/post delta FBC
      data2 <- dcast(datadt, Product+Ea+Read+Group+WordLine+Level+Normalized_WL~SegmentName, value.var=c("AvgFBC","MaxFBC"), ); #+Level
      data2 <- data2[,c("Delta_AvgFBC", "Delta_MaxFBC") := list(AvgFBC_PostPEC-AvgFBC_PrePEC, MaxFBC_PostPEC-MaxFBC_PrePEC)];
    }
    else if(length(grep("LUN",fbc_minor_dim))>0){
      datadt <- datadt[,-c("Block","Layer","Condition","Type")];
      datadt <- datadt[,c("FBC*Count") := list(FBC*Count)];
      datadt <- datadt[,c("SumFBC"):= ave(FBC*Count, Product,Ea,SegmentName,Read,CHP_CE_LUN,Group,WordLine,Level, FUN=function(x){sum(x)})];
      datadt <- datadt[,c("SumCount"):= ave(Count, Product,Ea,SegmentName,Read,CHP_CE_LUN,Group,WordLine,Level, FUN=function(x){sum(x)})];
      datadt <- datadt[,c("AvgFBC"):= list(SumFBC/SumCount)];
      datadt <- datadt[,c("MaxFBC"):= ave(FBC, Product,Ea,SegmentName,Read,CHP_CE_LUN,Group,WordLine,Level, FUN=function(x){max(x)})];
      datadt <- datadt[,-c("FBC","Count","FBC*Count","SumFBC","SumCount")];
      datadt <- unique(datadt, by=c("CHP_CE_LUN","Group","WordLine","Read","SegmentName","Ea","Level","AvgFBC","MaxFBC"));
      #获取pre/post delta FBC
      data2 <- dcast(datadt, CHP_CE_LUN+Group+WordLine+Read+Ea+Level~SegmentName, value.var=c("AvgFBC","MaxFBC"));
      data2 <- data2[,c("Delta_AvgFBC", "Delta_MaxFBC") := list(AvgFBC_Post_PEC-AvgFBC_Pre_PEC, MaxFBC_Post_PEC-MaxFBC_Pre_PEC)];
    }
    else if(length(grep("Block",fbc_minor_dim))>0){
      datadt <- datadt[,-c("Layer","Condition","Type")];
      #Select 1 wordline
      datadt <- datadt[WordLine==410,];
      datadt <- datadt[,c("FBC*Count") := list(FBC*Count)];
      datadt <- datadt[,c("SumFBC"):= ave(FBC*Count, Product,Ea,SegmentName,Read,CHP_CE_LUN,Group,Block,WordLine,Level, FUN=function(x){sum(x)})];
      datadt <- datadt[,c("SumCount"):= ave(Count, Product,Ea,SegmentName,Read,CHP_CE_LUN,Group,Block,WordLine,Level, FUN=function(x){sum(x)})];
      datadt <- datadt[,c("AvgFBC"):= list(SumFBC/SumCount)];
      datadt <- datadt[,c("MaxFBC"):= ave(FBC, Product,Ea,SegmentName,Read,CHP_CE_LUN,Group,Block,WordLine,Level, FUN=function(x){max(x)})];
      datadt <- datadt[,-c("FBC","FBC*Count","Count","SumFBC","SumCount")];
      datadt <- unique(datadt, by=c("Product","Ea","SegmentName","Read","CHP_CE_LUN","Group","Block","WordLine","Level","AvgFBC","MaxFBC"));
      datadt <- melt(datadt,id.vars=c("Ea","SegmentName","Read","Group","CHP_CE_LUN","Block","WordLine","Level"),measure.vars=c("AvgFBC","MaxFBC"),variable.name="fbcType",value.name="Bitflip");
    }
    else if(length(grep("test",fbc_minor_dim))>0){
      #all die/block combine
      datadt <- datadt[,-c("Sector1","Sector2","Sector3","Sector4")];
      # datadt <- datadt[,c("pString"):= list(ifelse(String==3,0,ifelse(String==0,1,ifelse(String==2,2,ifelse(String==1,3,4)))))];
      # datadt <- datadt[,c("SumFBC"):= ave(FBC*Count, Product,Ea,SegmentName,Read,Group,WordLine,pString, FUN=function(x){sum(x)})]; #,Level
      # datadt <- datadt[,c("SumCount"):= ave(Count, Product,Ea,SegmentName,Read,Group,WordLine,pString, FUN=function(x){sum(x)})];#,Level
      datadt <- datadt[,c("AvgFBC"):= ave(AvgFBC, Product,Ea,SegmentName,Group,WordLine,Level, FUN=function(x){mean(x)})];
      datadt <- datadt[,c("MaxFBC"):= ave(MaxFBC, Product,Ea,SegmentName,Group,WordLine,Level, FUN=function(x){max(x)})];
      datadt <- unique(datadt, by=c("Product","Ea","SegmentName","Group","WordLine","Level","AvgFBC","MaxFBC"));
      # write.csv(datadt,file="./datadt.csv", row.names = FALSE);
      #获取pre/post delta FBC
      data2 <- dcast(datadt, Product+Ea+Group+WordLine+Level+Normalized_WL~SegmentName, value.var=c("AvgFBC","MaxFBC"), );
      data2 <- data2[,c("Delta_AvgFBC", "Delta_MaxFBC") := list(AvgFBC_AfterEndurance-AvgFBC_BeforeEndurance, MaxFBC_AfterEndurance-MaxFBC_BeforeEndurance)];
      # write.csv(data2,file="./data2.csv", row.names = FALSE);
    }
    cat("...process data end... \n");
  }
  
  ###################### Plot image ######################
  cat("...plotting and plot to pptx start... \n");
  plotlist <- list();
  theme_set(theme_bw());
  theme_update(plot.margin = margin(20,5,20,20,"pt"), panel.grid.minor=element_line(color = "grey92"));
  themex <- theme_bw() + theme(plot.margin = margin(5,5,5,5,"pt"),
                               axis.title.y = element_text(margin = margin(3,3,3,3)),
                               axis.title.x = element_text(margin = margin(5,5,5,5)),
                               panel.grid.minor = element_line(color = "grey92"));
  # test
  if(FALSE){
    plottype = "line";
    ii = 1;
    if(length(grep("boxplot",plottype))>0){
      for(index in unique(datadt$SegmentName)){
        plotlist[[ii]] <- ggplot(datadt[SegmentName==index,], aes(x=Level, y=Bitflip,color=Ea), stat="identity")+
          geom_boxplot(outlier.shape=6 ,outlier.size = 0.5)+ 
          stat_summary(fun.y=median, geom="line")+ #geom="point", shape=23, size=1
          themex + theme(text=element_text(size=12)) +  #, legend.position = "none"
          theme(axis.title=element_text(size=14))+ #+ theme(axis.text.x = element_text(angle = 90))
          facet_grid(.~fbcType)+scale_color_brewer(palette = "Set1")+ #
          scale_x_discrete(breaks=sort(datadt[,Level])); #[seq(1,length(unique(datadt[,Block])),by=floor(length(unique(datadt[,Block]))/20))]
        
        pptx <- add_slide(pptx, layout = "Title Subtitle and Content", master = "PetaIO_Theme");
        pptx <- ph_with(pptx, value=plotlist[[ii]], location=ph_location_type(type="body"), res = 300);
        pptx <- ph_with(pptx, value="Pre-PEC FBC check for Ea(0.64/1.0eV)", location=ph_location_type(type="title"));
        pptx <- ph_with(pptx, value=index, location=ph_location_type(type="subTitle"));
        
        ii = ii+1;
      }
    }
    else if(length(grep("line",plottype))>0){
      
      plotlist[[ii]] <- ggplot(datadt, aes(x=Layer, y=AvgFBC,color=pString))+
        geom_line(stat = "identity", linewidth=0.5)+
        themex + theme(text=element_text(size=12)) +  #, legend.position = "none"
        theme(axis.title=element_text(size=14))+ theme(axis.text.x = element_text(angle = 90))+
        facet_grid(Group~pString); #
      pptx <- add_slide(pptx, layout = "Title Subtitle and Content", master = "PetaIO_Theme");
      pptx <- ph_with(pptx, value=plotlist[[ii]], location=ph_location_type(type="body"), res = 300);
      pptx <- ph_with(pptx, value=pptxtitle, location=ph_location_type(type="title"));
      #pptx <- ph_with(pptx, value="Zoom worse WL", location=ph_location_type(type="subTitle"));
    }
    print(pptx, target = paste0("./AutoReprot_", format(Sys.time(), "%b%d_%Hh%Mm%Ss"), ".pptx"));
  }
  
  if(TRUE){
    # same product, compare different Ea
    ii = 1;
    for(product in unique(datadt$Product)){
      # AvgFBC
      plotlist[[ii]] <- ggplot(datadt[Product==product,], aes(x=WordLine, y=AvgFBC, color = Level))+
        themex+geom_line(stat="identity", linewidth=0.5)+theme(text = element_text(size = 12))+theme(axis.title = element_text(size = 14))+
        facet_grid(Group~SegmentName+Level)+scale_color_brewer(palette = "Set1");
      pptx <- add_slide(pptx, layout = "Title Subtitle and Content", master = "PetaIO_Theme");
      pptx <- ph_with(pptx, value=plotlist[[ii]], location=ph_location_type(type="body"), res = 300);
      pptx <- ph_with(pptx, value=pptxtitle, location=ph_location_type(type="title"));
      pptx <- ph_with(pptx, value="Average FBC", location=ph_location_type(type="subTitle"));
      ii = ii+1;
      # MaxFBC
      plotlist[[ii]] <- ggplot(datadt[Product==product,], aes(x=WordLine, y=MaxFBC, color = Level))+
        themex+geom_line(stat="identity", linewidth=0.5)+theme(text = element_text(size = 12))+theme(axis.title = element_text(size = 14))+
        facet_grid(Group~SegmentName+Level)+scale_color_brewer(palette = "Set1");
      pptx <- add_slide(pptx, layout = "Title Subtitle and Content", master = "PetaIO_Theme");
      pptx <- ph_with(pptx, value=plotlist[[ii]], location=ph_location_type(type="body"), res = 300);
      pptx <- ph_with(pptx, value=pptxtitle, location=ph_location_type(type="title"));
      pptx <- ph_with(pptx, value="Max FBC", location=ph_location_type(type="subTitle"));
      ii = ii+1;
      # Delta AvgFBC
      plotlist[[ii]] <- ggplot(data2[Product==product,], aes(x=WordLine, y=Delta_AvgFBC, color = Level))+ labs(x="WordLine", y="ΔAvgFBC/4KB")+
        themex+geom_line(stat="identity", linewidth=0.5)+theme(text = element_text(size = 14))+theme(axis.title = element_text(size = 18))+
        theme(plot.margin = margin(0,4,0,4,"cm"))+
        facet_grid(Group~Level)+scale_color_brewer(palette = "Set1");
      pptx <- add_slide(pptx, layout = "Title Subtitle and Content", master = "PetaIO_Theme");
      pptx <- ph_with(pptx, value=plotlist[[ii]], location=ph_location_type(type="body"), res = 300);
      pptx <- ph_with(pptx, value=pptxtitle, location=ph_location_type(type="title"));
      pptx <- ph_with(pptx, value="ΔAverage FBC", location=ph_location_type(type="subTitle"));
      ii = ii+1;
      # Delta MaxFBC
      plotlist[[ii]] <- ggplot(data2[Product==product,], aes(x=WordLine, y=Delta_MaxFBC, color = Level))+ labs(x="WordLine", y="ΔMaxFBC/4KB")+
        themex+geom_line(stat="identity", linewidth=0.5)+theme(text = element_text(size = 14))+theme(axis.title = element_text(size = 18))+
        theme(plot.margin = margin(0,4,0,4,"cm"))+
        facet_grid(Group~Level)+scale_color_brewer(palette = "Set1");
      pptx <- add_slide(pptx, layout = "Title Subtitle and Content", master = "PetaIO_Theme");
      pptx <- ph_with(pptx, value=plotlist[[ii]], location=ph_location_type(type="body"), res = 300);
      pptx <- ph_with(pptx, value=pptxtitle, location=ph_location_type(type="title"));
      pptx <- ph_with(pptx, value="ΔMax FBC", location=ph_location_type(type="subTitle"));
      ii = ii+1;
    }
    # same Ea, compare different Product
    for(ea in unique(datadt$Ea)){
      # AvgFBC
      plotlist[[ii]] <- ggplot(datadt[Ea==ea,], aes(x=Normalized_WL, y=AvgFBC, color = Product))+ labs(x="Normalized_WL", y="AvgFBC/4KB")+
        themex+geom_line(stat="identity", linewidth=0.5)+theme(text = element_text(size = 12))+theme(axis.title = element_text(size = 14))+
        facet_grid(Group~SegmentName+Level)+scale_color_brewer(palette = "Set1")+
        scale_x_continuous(breaks=seq(0,1, by=0.5), labels=c("Top","Middle","Bottom"), limits=c(0,1))+
        theme(axis.text.x = element_text(angle = 90));
      pptx <- add_slide(pptx, layout = "Title Subtitle and Content", master = "PetaIO_Theme");
      pptx <- ph_with(pptx, value=plotlist[[ii]], location=ph_location_type(type="body"), res = 300);
      pptx <- ph_with(pptx, value=pptxtitle, location=ph_location_type(type="title"));
      pptx <- ph_with(pptx, value=ea, location=ph_location_type(type="subTitle"));
      ii = ii+1;
      # MaxFBC
      plotlist[[ii]] <- ggplot(datadt[Ea==ea,], aes(x=Normalized_WL, y=MaxFBC, color = Product))+ labs(x="Normalized_WL", y="MaxFBC/4KB")+
        themex+geom_line(stat="identity", linewidth=0.5)+theme(text = element_text(size = 12))+theme(axis.title = element_text(size = 14))+
        facet_grid(Group~SegmentName+Level)+scale_color_brewer(palette = "Set1")+
        scale_x_continuous(breaks=seq(0,1, by=0.5), labels=c("Top","Middle","Bottom"), limits=c(0,1))+
        theme(axis.text.x = element_text(angle = 90));
      pptx <- add_slide(pptx, layout = "Title Subtitle and Content", master = "PetaIO_Theme");
      pptx <- ph_with(pptx, value=plotlist[[ii]], location=ph_location_type(type="body"), res = 300);
      pptx <- ph_with(pptx, value=pptxtitle, location=ph_location_type(type="title"));
      pptx <- ph_with(pptx, value=ea, location=ph_location_type(type="subTitle"));
      ii = ii+1;
      # Delta AvgFBC
      plotlist[[ii]] <- ggplot(data2[Ea==ea,], aes(x=Normalized_WL, y=Delta_AvgFBC, color = Product))+ labs(x="Normalized_WL", y="ΔAvgFBC/4KB")+
        themex+geom_line(stat="identity", linewidth=0.5)+theme(text = element_text(size = 14))+theme(axis.title = element_text(size = 18))+
        theme(plot.margin = margin(0,4,0,4,"cm"))+
        facet_grid(Group~Level)+scale_color_brewer(palette = "Set1")+
        scale_x_continuous(breaks=seq(0,1, by=0.5), labels=c("Top","Middle","Bottom"), limits=c(0,1))+
        theme(axis.text.x = element_text(angle = 90));
      pptx <- add_slide(pptx, layout = "Title Subtitle and Content", master = "PetaIO_Theme");
      pptx <- ph_with(pptx, value=plotlist[[ii]], location=ph_location_type(type="body"), res = 300);
      pptx <- ph_with(pptx, value=pptxtitle, location=ph_location_type(type="title"));
      pptx <- ph_with(pptx, value=ea, location=ph_location_type(type="subTitle"));
      ii = ii+1;
      # Delta MaxFBC
      plotlist[[ii]] <- ggplot(data2[Ea==ea,], aes(x=Normalized_WL, y=Delta_MaxFBC, color = Product))+ labs(x="Normalized_WL", y="ΔMaxFBC/4KB")+
        themex+geom_line(stat="identity", linewidth=0.5)+theme(text = element_text(size = 14))+theme(axis.title = element_text(size = 18))+
        theme(plot.margin = margin(0,4,0,4,"cm"))+
        facet_grid(Group~Level)+scale_color_brewer(palette = "Set1")+
        scale_x_continuous(breaks=seq(0,1, by=0.5), labels=c("Top","Middle","Bottom"), limits=c(0,1))+
        theme(axis.text.x = element_text(angle = 90));
      pptx <- add_slide(pptx, layout = "Title Subtitle and Content", master = "PetaIO_Theme");
      pptx <- ph_with(pptx, value=plotlist[[ii]], location=ph_location_type(type="body"), res = 300);
      pptx <- ph_with(pptx, value=pptxtitle, location=ph_location_type(type="title"));
      pptx <- ph_with(pptx, value=ea, location=ph_location_type(type="subTitle"));
      ii = ii+1;
    }
  }
  cat("...plotting and plot to pptx end... \n");
  cat("...print pptx start... \n");
  print(pptx, target = paste0("./AutoReprot_", format(Sys.time(), "%b%d_%Hh%Mm%Ss"), ".pptx"));
  cat("...print pptx end... \n");
}, #end of data process
error = function(e){
  cat(toString(e));
  cat(toString(e), file=openlog);
},#end of error
finally = {
  close(openlog);
})#end of tryCatch
cat(paste0("Program Ended on ", format(Sys.time(), "%b%d_%Hh%Mm%Ss"), "\n"));
gc()
