#####Total Fertility Rate Explainer#####
library(HMDHFDplus)
library(tidyverse)
library(grid)
library(gridExtra)
library(rsvg)
library(demography)

for(file in list.files(file.path("analysis","R","utils"))){
  source(file.path("analysis","R","utils",file))
}

postname <- "2017-06-06-fertility-explainer"

styling <- loadStyling()

HFDcredentials <- read.csv("~/Documents/creds/HFD.csv", stringsAsFactors = FALSE)
HMDcredentials <- read.csv("~/Documents/creds/HMD.csv")

dir <- file.path("input","data","HFD","USA")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

if(!file.exists(file.path(dir,"asfrRR.csv"))){
  usaFert <- readHFDweb(CNTRY = "USA", item ="asfrRR", username=HFDcredentials$username, password = HFDcredentials$password)
  write_csv(usaFert,path = file.path(dir,"asfrRR.csv"))
}else{
  usaFert <- read_csv(file.path(dir,"asfrRR.csv"))
}

tfr_df <- group_by(usaFert,Year) %>% 
  summarise_each(funs(sum)) %>%
  as.data.frame()

names(tfr_df)[3] <- "TFR"

uvatheme <- function(...) theme( panel.grid.major.x = element_line(colour = styling$grid$lines$color),
                                 panel.grid.minor.x = element_blank(),
                                 panel.grid.minor.y = element_blank(),
                                 panel.grid.major.y = element_line(colour = styling$grid$lines$color),
                                 plot.margin = unit(c(200, 25, 100, 50), "points"),
                                 axis.text = element_text(size = 30, family = styling$axis$labels$font$family, colour = styling$axis$labels$font$color),
                                 axis.ticks = element_line(colour = NULL),
                                 axis.ticks.y = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.line = element_line(colour = "black", size = 1.5),
                                 axis.line.y = element_blank(),
                                 axis.line.x = element_blank(),
                                 axis.title.y = element_text(size = 40, angle = 90,colour=styling$axis$labels$font$color,margin=margin(0,20,0,0),family=styling$axis$labels$font$family,face="bold"),
                                 axis.title.x = element_text(size = 40,margin=margin(20,0,0,0),colour=styling$axis$labels$font$color,family=styling$axis$labels$font$family,face="bold"),
                                 panel.background = element_rect(fill = styling$plot$color, colour = styling$plot$color),
                                 legend.key.size = unit(.05,"npc"),
                                 legend.key = element_rect(fill = styling$legend$color, colour = styling$legend$color),
                                 legend.title = element_text(size = 40,colour = styling$legend$title$font$color,margin=margin(0,0,0,0),family=styling$legend$title$font$family,face=styling$legend$title$font$face),
                                 legend.position = 'right',
                                 legend.text = element_text(family=styling$legend$labels$font$family,size = 40,colour = styling$legend$labels$font$color),
                                 legend.background = element_rect(fill = styling$plot$color, colour = styling$plot$color),
                                 plot.background = element_rect(fill = styling$plot$color, colour = styling$plot$color),...)


colpal <- styling$colors$main

dir.create(file.path("output",postname,"tfr-asfr-combo-frames"), showWarnings = FALSE, recursive = TRUE)

for(i in 1933:2014){
  drawComboFrame(title="United States Fertility",
                 subtitle="Total Fertility is the sum of Age-Specific Rates",
                 t=i,
                 path=file.path("output",postname,"tfr-asfr-combo-frames",paste0("frame-",i,".png")))
}

dir <- file.path("input","data","HMD","USA")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

if(!file.exists(file.path(dir,"Population.csv"))){
  usaPop <- readHMDweb(CNTRY = "USA",item ="Population",username = HMDcredentials$username,password = HMDcredentials$password)
  write_csv(usaPop,path = file.path(dir,"Population.csv"))
}else{
  usaPop <- read_csv(file.path(dir,"Population.csv"))
}

topyear <- 2014
bottomyear <- 1933

usaFert.wide <- filter(usaFert,Year <= topyear & Year >= bottomyear) %>%
  group_by(Age,Year) %>%
  select(-OpenInterval) %>%
  spread(key=Year,value=ASFR) %>%
  as.matrix()

rownames(usaFert.wide) <- usaFert.wide[,1]

usaFert.wide <- usaFert.wide[,2:ncol(usaFert.wide)]


usaPop.wide <- filter(usaPop,Year <= topyear & Year >= bottomyear,Age > 11 & Age <56) %>%
  group_by(Age,Year) %>%
  select(Year,Age,Female1) %>%
  spread(key=Year,value=Female1) %>%
  as.matrix()

rownames(usaPop.wide) <- usaPop.wide[,1]

usaPop.wide <- usaPop.wide[,2:ncol(usaPop.wide)]

usaFert.demogdata <- demogdata(data=usaFert.wide,
                               pop=usaPop.wide,
                               ages=as.vector(12:55),
                               years=as.vector(bottomyear:topyear),
                               type="fertility",
                               label="USA",
                               name="female",
                               lambda=.4)

usaFert.smooth <- smooth.demogdata(usaFert.demogdata)

usaFert.fdm <- fdm(usaFert.smooth)

usaFert.forecast <- forecast(usaFert.fdm, h=200,level=95,max.d=1)

usaFert.forecast.tidy.point <- usaFert.forecast$rate$female %>%
  as.data.frame() %>%
  rownames_to_column(var = "Age") %>%
  gather_(key="Year",value="ASFR","2015":"2164") %>%
  mutate(bound = "Point", type = "forecast",Year = as.numeric(Year),Age = as.numeric(Age))

usaFert.forecast.tidy.lower <- usaFert.forecast$rate$lower %>%
  as.data.frame() %>%
  rownames_to_column(var = "Age") %>%
  gather_(key="Year",value="ASFR","2015":"2164") %>%
  mutate(bound = "Lower", type = "interval",Year = as.numeric(Year),Age = as.numeric(Age)) 

usaFert.forecast.tidy.upper <- usaFert.forecast$rate$upper %>%
  as.data.frame() %>%
  rownames_to_column(var = "Age") %>%
  gather_(key="Year",value="ASFR","2015":"2064") %>%
  mutate(bound = "Upper", type = "interval",Year = as.numeric(Year),Age = as.numeric(Age))

usaFert <- mutate(usaFert,bound = "Point",type = "Observed")
###Analysis
cohorts <- c("1930","1940","1950","1960","1970","1980","1990")



usaFert.forecast.tidy <- bind_rows(usaFert,usaFert.forecast.tidy.point,usaFert.forecast.tidy.lower,usaFert.forecast.tidy.upper) %>%
  mutate(cohort = as.numeric(Year) - as.numeric(Age)) %>%
  arrange(Age,bound) %>%
  select(-OpenInterval) %>%
  filter(cohort %in% cohorts)

usaFert.forecast.tidy$cohort <- factor(usaFert.forecast.tidy$cohort,levels=cohorts)

usaFert.forecast.tidy[is.na(usaFert.forecast.tidy)] <- 0

intervals <- filter(usaFert.forecast.tidy,bound=="Upper") %>%
  bind_rows(filter(usaFert.forecast.tidy,bound=="Lower") %>% arrange(desc(Year)),
            filter(usaFert.forecast.tidy,bound=="Point",Year=="2014")) 

intervals$cohort <- factor(intervals$cohort,levels=cohorts)



####ASFR observed graphic
height <- 1080
width <- 1920
p <- ggplot(data = usaFert.forecast.tidy,aes(x=Age,y=ASFR)) +
  geom_polygon(data=intervals,aes(color=cohort,fill=cohort),alpha=0,size=0,linetype=3) +
  geom_line(data = filter(usaFert.forecast.tidy,bound == "Point",type == "Observed"),aes(colour=as.character(cohort),linetype=type),size=2) +
  scale_colour_manual(values = colpal,drop=FALSE,guide=FALSE) +
  scale_fill_manual(values = colpal,drop=FALSE) +
  scale_linetype_manual(values = c(1,1),guide=FALSE) +
  ylab("Age-Specific Fertility Rates...") +
  xlab("...at age...") +
  guides(fill = guide_legend(title="...experienced by\nwomen born in...",override.aes = list(alpha=1))) +
  scale_y_continuous(breaks = seq(0,.25,by=.05),limits = c(0,.28)) + 
  uvatheme()
#geom_path(data = filter(usaFert.forecast.tidy,bound=="Point")) +
#geom_path(data = filter(usaFert.forecast.tidy,type=="Interval"))
png(file.path("output",postname,"asfr-observed.png"),width=1920,height=1080)

print(p)
grid.text("Source: NCHS, U.S. Census, prepared by Human Fertility Database", x=unit(width-8,"points"),y=unit(15,"points"),just="right",gp=gpar(fontsize=styling$footer$cite$font$size*1.5,fontfamily=styling$footer$cite$font$family,fontface=styling$footer$cite$font$face, col=styling$header$title$font$color))
grid.text("@UVAdemographics & @StephenHolz", x=unit(width-8,"points"),y=unit(35,"points"),just="right",gp=gpar(fontsize=styling$footer$author$font$size*1.5,fontfamily=styling$footer$author$font$family,fontface=styling$footer$author$font$face, col=styling$header$title$font$color))
grid.text("USA ASFRs Experienced by Select Cohorts", x=unit(50,"points"),y=unit((height-50)/height,"npc"),just="left",gp=gpar(fontsize=styling$header$title$font$size*2,fontfamily=styling$header$title$font$family,fontface=styling$header$title$font$face, col=styling$header$title$font$color))
grid.text("", x=unit(50,"points"),y=unit((height-114)/height,"npc"),just="left",gp=gpar(fontsize=styling$header$subtitle$font$size*2,fontfamily=styling$header$subtitle$font$family,fontface=styling$header$subtitle$font$face, col=styling$header$subtitle$font$color))
if(file.exists(file.path("input","logos","WCCPPDRGlogoBlueText.svg"))){
  img <- rsvg(file.path("input","logos","WCCPPDRGlogoBlueText.svg"),height = 75)
  g <- rasterGrob(img,x=unit(235,"points"),y=unit(26/height,"npc"),height=unit(50,"points"))
  print(grid.draw(g), newpage = FALSE)  
}
dev.off()

cohorts <- c("1990")

fert1990 <- filter(usaFert.forecast.tidy,
                   cohort=="1990")

fert1990$cohort <- factor(fert1990$cohort,levels = c("1990"))

intervals1990 <- filter(intervals,
                        cohort=="1990")

intervals1990$cohort <- factor(intervals1990$cohort,levels=c("1990"))
####Forecast graphic
p <- ggplot(data = fert1990,aes(x=Age,y=ASFR)) +
  geom_polygon(data=filter(intervals1990,cohort %in% cohorts),aes(color=cohort,fill=cohort),alpha=.2,size=0,linetype=3) +
  geom_line(data = filter(fert1990,bound == "Point",cohort %in% cohorts),aes(colour=as.character(cohort),linetype=type),size=2) +
  scale_colour_manual(values = "blue",drop=FALSE,guide=FALSE) +
  scale_fill_manual(values = c("blue"),drop=FALSE) +
  scale_linetype_manual(values = c(2,1),guide=FALSE) +
  ylab("Age-Specific Fertility Rates...") +
  xlab("...at age...") +
  guides(fill = guide_legend(title="...experienced by\nwomen born in...",override.aes = list(alpha=1))) +
  scale_y_continuous(breaks = seq(0,.25,by=.05),limits = c(0,.28)) + 
  uvatheme()
#geom_path(data = filter(usaFert.forecast.tidy,bound=="Point")) +
#geom_path(data = filter(usaFert.forecast.tidy,type=="Interval"))
png(file.path("output",postname,"asfr-forecast.png"),width=1920,height=1080)

print(p)
grid.text("Source: NCHS, U.S. Census, prepared by Human Fertility Database", x=unit(width-8,"points"),y=unit(15,"points"),just="right",gp=gpar(fontsize=styling$footer$cite$font$size*1.5,fontfamily=styling$footer$cite$font$family,fontface=styling$footer$cite$font$face, col=styling$header$title$font$color))
grid.text("@UVAdemographics & @StephenHolz", x=unit(width-8,"points"),y=unit(35,"points"),just="right",gp=gpar(fontsize=styling$footer$author$font$size*1.5,fontfamily=styling$footer$author$font$family,fontface=styling$footer$author$font$face, col=styling$header$title$font$color))
grid.text("USA ASFR Forecast for Mothers Born in 1990", x=unit(50,"points"),y=unit((height-50)/height,"npc"),just="left",gp=gpar(fontsize=styling$header$title$font$size*2,fontfamily=styling$header$title$font$family,fontface=styling$header$title$font$face, col=styling$header$title$font$color))
grid.text("Forecast (for demonstration purposes only) shown with 95% prediction interval.", x=unit(50,"points"),y=unit((height-114)/height,"npc"),just="left",gp=gpar(fontsize=styling$header$subtitle$font$size*2,fontfamily=styling$header$subtitle$font$family,fontface=styling$header$subtitle$font$face, col=styling$header$subtitle$font$color))
if(file.exists(file.path("input","logos","WCCPPDRGlogoBlueText.svg"))){
  img <- rsvg(file.path("input","logos","WCCPPDRGlogoBlueText.svg"),height = 75)
  g <- rasterGrob(img,x=unit(235,"points"),y=unit(26/height,"npc"),height=unit(50,"points"))
  print(grid.draw(g), newpage = FALSE)  
}
dev.off()