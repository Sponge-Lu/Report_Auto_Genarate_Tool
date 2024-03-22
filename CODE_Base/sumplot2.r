sumplot2 <- function(data, xcol=NULL,ycol=NULL,style="",separate=NULL,leg=NULL,group=NULL,facetgridx=NULL,facetgridy=NULL,...){
  theme_set(theme_bw());
  theme_update(plot.margin = margin(20,5,20,20,"pt"), panel.grid.minor=element_line(color = "grey92"));
  themex <- theme_bw() + theme(plot.margin = margin(20,5,20,20,"pt"),
                               axis.title.y = element_text(margin = margin(3,3,3,3)),
                               axis.title.x = element_text(margin = margin(5,5,5,5)),
                               panel.grid.minor = element_line(color = "grey92")); 
  
  plotlist <- ggplot(data, aes(x=xcol, y=ycol, color = leg))+
    themex+geom_line(stat="identity", size=0.5)+theme(text = element_text(size = 14))+theme(axis.title = element_text(size = 18))+
    facet_grid(reformulate(facetgridx,facetgridy))+scale_color_brewer(palette = "Set1");
  
  return(plotlist)
}