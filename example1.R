library(data.table)
library(ggplot2)
library(pwt10)
library(directlabels)
pwt <- pwt10.0
setDT(pwt)
names(pwt)
?pwt10.0
pwts <- pwt[,list(isocode,year, rgdpo,pop, emp, avh, hc, rgdpna,rkna)]
summary(pwts)
library(mice)
md.pattern(pwts)
clist <- c("TUR","USA","FRA","DEU","JPN","CHN","IND","ESP","GRC","SWE")
pwtc <- pwts [isocode %in% clist]
pwtc[, hca := mean(hc, na.rm = TRUE), by = isocode]

base <- ggplot(pwtc, aes(year, (rgdpo / pop) / 1000, colour = isocode )) +
  geom_line(show.legend = FALSE) +
  scale_y_log10() + 
  directlabels:: geom_dl(aes(label = isocode), method = "smart.grid")
base
  
labelled <- base + labs(x = "", 
                        y = "Real GDP per capita, PPP, 2011 prices",
                        title = "Real GDP per capita increased over the years
                        in all countries",
                        caption = "Source: Penn World Tables 10.0") +
  scale_color_brewer(type = "seq", palette = "Dark2")
  
labelled
theme_a <- theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
                 axis.ticks = element_line(colour = "grey70", size = 0.2),
                 panel.grid.major = element_line(colour = "grey70", size = 0.2),
                 panel.grid.minor = element_blank(),
        legend.position = "none"
                           )
labelled + theme_a

base2 <- ggplot(pwtc, aes(hc, rgdpna, colour = isocode)) + 
  geom_point(show.legend = FALSE) +
  scale_y_log10() + geom_smooth(method = "lm")+
  directlabels:: geom_dl(aes(label = isocode), method = "smart.grid")
base2

labelled2 <- base2 + labs(x = "Human Capital Index",
       y = "Real GDP at constant 2017 national prices (in million USD)",
       title = "There is a positive relationship between Real GDP & HCI",
       caption = "Source: Penn World Tables 10.0 ") 

labelled2 + theme_a



base3 <-ggplot(pwtc, aes(isocode, hca, fill = "red"))+
  geom_bar(stat = "summary_bin",na.rm = TRUE) + 
  theme(legend.position = "none")
base3
labelled3 <- base3 + labs(x = "",
                         y = "Human Capital Index",
                         title = "Human capital index across countries",
                         caption = "Source: Penn World Tables 10.0")
labelled3 + theme_a
