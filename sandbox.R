rm(list=ls())
library(data.table)
library(ggplot2)

# Theme
t <- theme(plot.title = element_text(face="bold", margin=margin(t = 15, r = 0, b = 15, l = 0, unit = "pt")),
           axis.text.x = element_text(size=10,color='#000000',angle=45,hjust=1),
           axis.text.y = element_text(size=10,color='#000000'),
           axis.title.x = element_text(face="bold", size=10,color='#000000',margin=margin(t = 10, r = 0, b = 0, l = 0, unit = "pt")),
           axis.title.y = element_text(face="bold", size=10,color='#000000',margin=margin(t = 0, r = 10, b = 0, l = 0, unit = "pt")),
           panel.background = element_rect(fill='#ffffff', color='#a5a5a5',size=0.5),
           panel.ontop = F,
           panel.grid.major = element_line(color='#a5a5a5', linetype='dashed',size=0.2),
           panel.grid.minor = element_line(color='#a5a5a5', linetype='dashed', size=0),
           legend.text = element_text(size=10,color='#000000'),
           legend.title = element_text(face='bold',size=10,color='#000000'),
           legend.box.background = element_rect(fill='#ffffff', color='#ffffff', size=1.5),
           strip.text = element_text(size=10,color='#000000', face='bold'),
           strip.background = element_rect(colour = NA, fill = '#ffffff'))

pal <- c('#056585','#851934','#191919','#D11341','#1E7D9E','#333745','#000000')

# Finals
dt <- fread('WorldCupMatches.csv', encoding='Latin-1')
dt <- dt[!is.na(Year)]
colnames(dt) <- gsub(' ', '_', colnames(dt))
dt <- dt[,Home_Team_Name := gsub('IR Iran','Iran',Home_Team_Name)]
dt <- dt[,Away_Team_Name := gsub('IR Iran','Iran',Away_Team_Name)]
dt <- dt[,Home_Team_Name := gsub('rn"">Republic of Ireland','Republic of Ireland',Home_Team_Name, fixed=T)]
dt <- dt[,Away_Team_Name := gsub('rn"">Republic of Ireland','Republic of Ireland',Away_Team_Name, fixed=T)]

dt <- dt[!duplicated(dt),] # For some reason there are duplicate rows in the data set
dt <- dt[Year >= 1998]
finals <- dt[!grepl('Group',Stage)]
rm(dt)

# Group stage
groups <- fread('WorldCupGroupWinners.csv')

# Count amount of matches played per team per year in finals
finals <- finals[!grepl('Third',Stage,ignore.case=T)] # Filter out small final
finals <- rbind(finals[,.(Year,Home_Team_Name)],finals[,.(Year,Away_Team_Name)], use.names=F,fill=F)

colnames(finals) <- c('Year','Team')
finals <- finals[order(Year)]
finals <- finals[,.(matches = .N),by=.(Year,Team)]

dt <- merge(groups,finals,by=c('Year','Team'),all.x=T)
dt <- dt[order(Year,Group,Position)]
dt <- dt[,Position := paste('Position', Position)]

dt_g1 <- dt[,.(mean_matches = mean(matches)),by=Position]
g1 <- ggplot(dt_g1,aes(x=as.factor(Position),y=mean_matches, fill=Position)) + 
    geom_bar(stat='identity') + 
    labs(x='position in group phase', y='avg. matches played after group phase', title='Plot 1: Average matches played after group phase per group phase position') +
    t +
    scale_fill_manual(values=pal,name='position')
g1

dt_g2 <- dt[,.(prob = .N / 40 * 100),by=.(Position, matches)]
dt_g2$matches <- gsub('1','1. Round of 16',dt_g2$matches)
dt_g2$matches <- gsub('2','2. Quarter Finals',dt_g2$matches)
dt_g2$matches <- gsub('3','3. Semi Finals',dt_g2$matches)
dt_g2$matches <- gsub('4','4. Final',dt_g2$matches)

# dt_g2 <- dt[,.(prob = .N),by=.(Position, matches)]
g2 <- ggplot(dt_g2,aes(x=matches, y=prob, fill=Position, label=paste0(prob,'%'))) + 
    geom_bar(stat='identity') + 
    geom_text(vjust=-1.5,size=2.5) +
    labs(x='matches played after group phase', y='probability (%)', title='Plot 2: Probability to end up in each elimination round per group phase position') +
    facet_wrap(~ Position) + 
    t +
    scale_fill_manual(values=pal,name=NULL) +
    theme(legend.position='none')
g2

dt_g3 <- dt[Position == 'Position 2',.(prob = .N / 8 * 100),by=.(Position, matches, Year)]
dt_g3$matches <- gsub('1','1. Round of 16',dt_g3$matches)
dt_g3$matches <- gsub('2','2. Quarter Finals',dt_g3$matches)
dt_g3$matches <- gsub('3','3. Semi Finals',dt_g3$matches)
dt_g3$matches <- gsub('4','4. Final',dt_g3$matches)
g3 <- ggplot(dt_g3,aes(x=matches, y=prob, fill=as.factor(Year), label=paste0(prob,'%'))) + 
    geom_bar(stat='identity') + 
    geom_text(vjust=-1.5,size=2.5) +
    labs(x='matches played after group phase', y='probability (%)',title='Plot 3: Probability for teams in position 2 to reach each elimination round per year') +
    facet_grid(.~ Year) + 
    t +
    scale_fill_manual(values=pal,name=NULL) +
    theme(legend.position='none')
g3

dt_g4 <- dt[,.(mean_matches = mean(matches)),by=.(Position, Year)]
g4 <- ggplot(dt_g4,aes(x=as.factor(Year),y=mean_matches, col=Position, fill=Position, label=mean_matches, group=Position)) + 
    geom_line(size=1) + 
    geom_point(size=3) + 
    geom_text(vjust=3.5,hjust=0.5,size=3) +
    t +
    scale_fill_manual(values=pal) +
    scale_color_manual(values=pal) +
    labs(y='avg. matches played after group phase', x='year', title='Plot 4: Average matches played after group phase per year')
g4
