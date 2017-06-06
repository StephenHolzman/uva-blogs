#Draw comination tfr asfr frame
drawComboFrame <- function(t,path,title="",subtitle="",cite="Source: NCHS, U.S. Census, prepared by Human Fertility Database",author="@UVAdemographics & @StephenHolz",height=1080,width=1920){
  
  tfr_plot <- ggplot(tfr_df,aes(x=Year,y=TFR)) + 
    geom_line(size=2,colour=colpal[1]) +
    geom_point(data=filter(tfr_df,Year==t),
               aes(x=Year,y=TFR),
               size=10,
               colour=colpal[2]) +
    ylab("Total Fertility Rate") +
    scale_y_continuous(breaks=seq(0,4,by=.5),limits=c(0,4)) +
    uvatheme()
  
  asfr_plot <- ggplot(filter(usaFert,Year==t),aes(x=Age,y=ASFR)) +
    geom_bar(stat="identity",fill=colpal[2]) +
    ylab("Age-Specific Fertility Rate") + 
    scale_y_continuous(breaks=seq(0,.25,by=.05),limits=c(0,.28)) +
    uvatheme()
  
  
  png(path,width=width,height=height)
  grid.arrange(arrangeGrob(tfr_plot,asfr_plot,ncol=2,widths=c(width/2,width/2)),
               heights=c(height))
  
  grid.text(t,x=unit(3*width/4,"points"),y=unit(height/2,"points"),gp=gpar(fontsize=200,alpha=.3,fontfamily=styling$header$title$font$family,fontface=styling$header$title$font$face, col=styling$header$title$font$color))
  grid.text(cite, x=unit(width-8,"points"),y=unit(15,"points"),just="right",gp=gpar(fontsize=styling$footer$cite$font$size*1.5,fontfamily=styling$footer$cite$font$family,fontface=styling$footer$cite$font$face, col=styling$header$title$font$color))
  grid.text(author, x=unit(width-8,"points"),y=unit(35,"points"),just="right",gp=gpar(fontsize=styling$footer$author$font$size*1.5,fontfamily=styling$footer$author$font$family,fontface=styling$footer$author$font$face, col=styling$header$title$font$color))
  grid.text(title, x=unit(50,"points"),y=unit((height-50)/height,"npc"),just="left",gp=gpar(fontsize=styling$header$title$font$size*2,fontfamily=styling$header$title$font$family,fontface=styling$header$title$font$face, col=styling$header$title$font$color))
  grid.text(subtitle, x=unit(50,"points"),y=unit((height-114)/height,"npc"),just="left",gp=gpar(fontsize=styling$header$subtitle$font$size*2,fontfamily=styling$header$subtitle$font$family,fontface=styling$header$subtitle$font$face, col=styling$header$subtitle$font$color))
  if(file.exists(file.path("input","logos","WCCPPDRGlogoBlueText.svg"))){
    img <- rsvg(file.path("input","logos","WCCPPDRGlogoBlueText.svg"),height = 75)
    g <- rasterGrob(img,x=unit(235,"points"),y=unit(26/height,"npc"),height=unit(50,"points"))
    print(grid.draw(g), newpage = FALSE)  
  }
  
  dev.off()
  
}