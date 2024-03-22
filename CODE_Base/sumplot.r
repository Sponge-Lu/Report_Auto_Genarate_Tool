sumplot <- function(data, xcol=NULL,ycol=NULL,style="",option="",normfactor="",drop=T,FUN=sum,
                    separate="",leg="",group="",facetgridx="",facetgridy="",
                    xlab="",ylab="",xlim=NULL,ylim=NULL,logscale="",alignylim=F,
                    xticksrotate=FALSE,linex=NA_real_,liney=NA_real_,
                    xticks=waiver(),yticks=waiver(),xminorticks=waiver(),yminorticks=waiver(),
                    leg_display=TRUE, legpos="",pointsize=2,titleoff=FALSE, flip=F,returnable=F...){
  
  require(plyr);
  require(ggplot2);
  require(RColorBrewer);
  require(data.table);
  
  if(nrow(data)==0){
    return(list());
  }
  if(separate==""){
    data$nonexistcol <- T;
    separate<-"nonexistcol";
  }
  if(leg==""){
    data$nonexistcol <- T;
    leg<-"nonexistcol";
  }
  if(group==""){
    data$nonexistcol <- T;
    group<-"nonexistcol";
  }
  if(facetgridx==""){
    data$nonexistcol <- T;
    facetgridx<-"nonexistcol";
  }
  if(facetgridy==""){
    data$nonexistcol <- T;
    facetgridy<-"nonexistcol";
  }
  
  plotlist <- list();
  
  ################### Process data with option #################
  if(object.size(data)>2e-7 & "data.table" %in% rownames(installed.packages())){
    #larger than 20MB will use data.table
    data.dt <- data.table(data);
    setkeyv(data.dt, c(separate, leg, xcol, group, facetgridx, facetgridy));
    data <- as.data.frame(data.dt[,list(V1=FUN(get(ycol))), by=eval(unique(c(separate, leg, xcol, group, facetgridx, facetgridy)))]);
  }else{
    if(length(grep("boxplot",style))==0 & !identical(FUN, identity())){
      data<-ddply(data,c(separate, leg, xcol, group, facetgridx, facetgridy), function(x, f=FUN) f(x[,ycol]));
    }
  }
  colnames(data)[colnames(data)=="V1"] <- ycol;
  
  percentage = (length(grep("percentage",option))>0);
  cum = (length(grep("cum",option))>0);
  if(percentage){
    data$sum_y <- ave(data[,ycol],data[,separate],data[,leg],data[,group],data[facetgridx],data[,facetgridy], FUN=function(x){sum(as.numeric((x)))});
    if (is.null(ylab)) ylab = paste("Probability of", ycol);
    data[,c("ycol"):= list(ycol/sum_y)];
  }
  if(cum){
    data<-data[order(data[,separate],data[,leg],data[,group],data[facetgridx],data[,facetgridy],-data[,xcol]),];
    data&cum_y <- ave(data[,ycol],data[,separate],data[,leg],data[,group],data[facetgridx],data[,facetgridy], FUN=function(x){cumsum(as.numeric((x)))});
    data[,ycol]<-data$cum_y;
  }
  
  ################### Get separate list #################
  if(any(is.na(data[,separate]))){
    data <- data[, separate:= list(ifelse(is.na(separate),"NA", separate))];
  }
  seplist <- unique(data[,separate]);
  if (is.character(seplist)) {seplist <- spelist[order(nchar(seplist), seplist)];}
  
  ################### Set plot theme by separate list #################
  if(length(seplist)<=2){
    theme_set(theme_bw(base_size = 18));
    theme_update(plot.margin = margin(20,5,20,20,"pt"), panel.grid.minor=element_line(color = "grey92"));
    themex <- theme_bw(base_size = 18) + theme(plot.margin = margin(20,5,20,20,"pt"),
                                               axis.title.y = element_text(margin = margin(3,3,3,3)),
                                               axis.title.x = element_text(margin = margin(5,5,5,5)),
                                               panel.grid.minor = element_line(color = "grey92"));
  }else
  {
    theme_set(theme_bw(base_size = 12));
    theme_update(plot.margin = margin(2,2,2,2,"pt"), panel.grid.minor=element_line(color = "grey92"));
    themex <- theme_bw(base_size = 12) + theme(plot.margin = margin(5,5,5,5,"pt"),
                                               axis.title.y = element_text(margin = margin(0,0,0,0)),
                                               axis.title.x = element_text(margin = margin(3,0,0,0)),
                                               panel.grid.minor = element_line(color = "grey92"));
  }
  
  ################### Set leg as factor #################
  if(is.character(data[,leg])){
    data[,leg] <- as.factor(data[,leg]);
    data[,leg] <- factor(data[,leg], levels=levels(data[,leg])[order(nchar(levels(data[,leg])), levels(data[,leg]))]);
  }
  
  ################### Process data with normfactor #################
  if(is.numeric(normfactor)){
    data[,ycol] <- data[,ycol]/normfactor;
  }else{
    warning("Errored normfactor");
  }
  
  ################### aligh y limit #################
  if(alignylim)
  {
    if(is.null(ylim))
    {
      ylim=c(min(data[,ycol],liney,na.rm=T))
    }else
    {
      if(is.na(ylim[2]))
      {
        ylim[2]=max(data[,ycol], liney, na.rm=T);
      }else
      {
        ylim[1]=min(data[,ycol],liney,na.rm=T);
      }
    }
  }
  
  ################### Plot section #################
  colorCount <- list();
  for(ii in seq_along(seplist)){
    if(length(grep("point", style))>0){
      plotlist[[ii]] <- ggplot(data[data[,separate]==seplist[ii],], aes(x=xcol, y=ycol, group=paste("paste0(",group,",",leg,")"), color = leg))+
        geom_point(stat="identity", size=pointsize);
      if((length(grep("line", style)) | (style=="") & (is.numeric(data[,xcol])))>0){
        plotlist[[ii]] <- plotlist[[ii]] + geom_line(stat = "identity", size=1);
      }
    }else{
      if((length(grep("line", style)) | ((style=="") & (is.numeric(data[,xcol]))))>0){
        plotlist[[ii]] <- ggplot(data[data[,separate]==seplist[ii],], aes(x=xcol, y=ycol, group=paste("paste0(",group,",",leg,")"), color = leg))+
          geom_line(stat = "identity", size=1);
      }
    }
    
    if((length(grep("bar", style))>0) & (length(grep("dodge", style))>0)){
      plotlist[[ii]] <- ggplot(data[data[,separate]==seplist[ii],], aes(x=xcol, y=ycol, fill = leg))+
        geom_bar(stat="identity", position = "dodge", size=1);
    }
    if(((length(grep("bar", style))>0) & (length(grep("dodge", style))>0)) | ((style="") & (!is.numeric(data[,xcol])))){
      plotlist[[ii]] <- ggplot(data[data[,separate]==seplist[ii],], aes(x=xcol, y=ycol, fill = leg))+
        geom_bar(stat="identity", size=1);
    }
    
    if(length(grep("boxplot", style))>0){
      data[,leg] <- as.factor(data[,leg]);
      plotlist[[ii]] <- ggplot(data[data[,separate]==seplist[ii],], aes(x=xcol, y=ycol, color = leg))+
        geom_boxplot(position = "dodge");
    }
    
    # Color part #
    colorCount[[ii]] <- length(unique(data[data[,separate]==seplist[ii],leg]));
    getPalette <- colorRampPalette(c(brewer.pal(11, "Spectral")[c(1:3,8:11)]));
    if(!is.numeric(data[,leg]) | leg == "nonexistcol"){
      plotlist[[ii]] <- plotlist[[ii]] + scale_color_manual(values=rep(getPalette(min(colorCount[[ii]],25)), times=floor(colorCount[[ii]]/25)+1))+
        scale_fill_manual(values=rep(getPalette(min(colorCount[[ii]],25)), times=floor(colorCount[[ii]]/25)+1));
    }else{
      plotlist[[ii]] <- plotlist[[ii]] + scale_color_gradient(colors=rev(brewer.pal(11, "Spectral")[c(1:3,8:11)]))+
        scale_fill_gradient(colors=rev(brewer.pal(11, "Spectral")[c(1:3,8:11)]));
    }
    # Color part ends #
    
    # Guide part #
    if((!leg_display) | (leg=="")){
      plotlist[[ii]] <- plotlist[[ii]] + guides(colour=FALSE) + guides(fill=FALSE);
    }else{
      if((colorCount[[ii]]>10) & (!is.numeric(data[,leg]))){
        plotlist[[ii]] <- plotlist[[ii]] + guides(colour=guide_legend(keywidth = 0.5, keyheight = 0.5, label.theme = element_text(size = 10, angle = 0)));
      }
      if((colorCount[[ii]]>10) & (!is.numeric(data[,leg]))){
        plotlist[[ii]] <- plotlist[[ii]] + guides(colour=FALSE) + guides(fill=FALSE);
      }
    }
    # Guide part ends #
    
    # Y Scale and limit part #
    if(length(grep("y", logscale))>0){
      if(identical(yticks,waiver())){
        yticks <- function(y){as.numeric(format(scales::trans_breaks("log10", function(x) 10^x)(y), digits =2))};
      }
      if(all(data[data[,separate]==seplist, ycol]==0) & is.null(ylim)){
        plotlist[[ii]] <- plotlist[[ii]] + scale_y_log10(breaks=yticks, limits=c(1e-2,10)) + annotation_logticks(sides = "l");
      }else{
        plotlist[[ii]] <- plotlist[[ii]] + scale_y_log10(breaks=yticks, minor_breaks=yminorticks, limits=ylim) + annotation_logticks(sides = "l");
      }
    }else if(any(grepl("bar", style))){
      plotlist[[ii]] <- plotlist[[ii]] + coord_cartesian(ylim = ylim) + scale_y_continuous(breaks = yticks, minor_breaks = yminorticks);
    }else{
      plotlist[[ii]] <- plotlist[[ii]] + scale_y_continuous(breaks = yticks, minor_breaks = yminorticks, limits = ylim);
    }
    # Y Scale and limit part end #
    
    # X Scale and limit part #
    if(is.numeric(data[,xcol])){
      if(length(grep("x",logscale))>0){
        plotlist[[ii]] <- plotlist[[ii]] + scale_x_log10(breaks=xticks, limits=xlim) + annotation_logticks(sides = "b");
      }else{
        plotlist[[ii]] <- plotlist[[ii]] + scale_x_continuous(breaks = xticks, minor_breaks = xminorticks, limits = xlim);
      }
    }else{
      if(length(unique(data[data[,separate]==seplist,xcol]))>20){
        plotlist[[ii]] <- plotlist[[ii]] + 
          scale_x_discrete(breaks=sort(data[data[,separate]==seplist,xcol])[seq(1,length(data[data[,separate]==seplist,xcol]),
                                                                               by=floor(length(data[data[,separate]==seplist,xcol])/20))], drop=drop);
      }else{
        plotlist[[ii]] <- plotlist[[ii]] + scale_x_discrete(drop=drop);
      }
    }
    # X Scale and limit part end #
    
    # facetgrid part #
    if((facetgridx!="nonexistcol") & (facetgridy!="nonexistcol")) {
      plotlist[[ii]] <- plotlist[[ii]] + facet_grid(reformulate(facetgridx,facetgridy));
    }else{
      if(facetgridx!="nonexistcol") {plotlist[[ii]] <- plotlist[[ii]] + facet_grid(reformulate(facetgridx));}
      if(facetgridy!="nonexistcol") {plotlist[[ii]] <- plotlist[[ii]] + facet_grid(reformulate(".",facetgridx));}
    }
    # facetgrid part end #
    
    # lab part #
    if(!is.null(xlab) && (xlab!="")) {plotlist[[ii]] <- plotlist[[ii]] + xlab(xlab);}
    if(!is.null(ylab) && (ylab!="")) {plotlist[[ii]] <- plotlist[[ii]] + ylab(ylab);}
    # lab part end #
    
    #xticks rotate
    if(xticksrotate){
      plotlist[[ii]] <- plotlist[[ii]] + themex + theme(axis.text.x = element_text(angle = 90));
    }else{
      plotlist[[ii]] <- plotlist[[ii]] + themex;
    }
    
    #lengend position
    if(legpos!="") plotlist[[ii]] <- plotlist[[ii]] + theme(legend.position = legpos);
    #x,y spec line
    if(!is.null(linex) && !is.na(linex)) plotlist[[ii]] <- plotlist[[ii]] + geom_vline(xintercept = linex, colour = "red", linetype = "longdash");
    if(!is.null(liney) && !is.na(liney)) plotlist[[ii]] <- plotlist[[ii]] + geom_hline(yintercept = liney, colour = "red", linetype = "longdash");
    #title
    if(!titleoff) plotlist[[ii]] <- plotlist[[ii]] + annotate("text", label=ifelse(separate=="nonexistcol","",as.character(seplist[ii])),
                                                              x=Inf, y=Inf, hjust =1, vjust=1, size=7);
    #x-y flip
    if(filp) plotlist[[ii]] <- plotlist[[ii]] + coord_flip(xlim = NULL, ylim = NULL, expand = TRUE);
    
    temp <- ggplot_build(plotlist[[ii]]);
  }
  if(returnable) plotlist[[length(plotlist)+1]] <- data[,setdiff(unique(c(separate,leg,group,xcol,ycol,facetgridx,facetgridy)),"nonexistcol")];
  
  return(plotlist)
}