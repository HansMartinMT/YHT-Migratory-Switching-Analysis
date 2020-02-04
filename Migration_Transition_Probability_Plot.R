library(dplyr)
library(igraph)
library(diagram)

results<-read.csv("3MigrationTransitionProb.csv")
trans_rate <- results %>%  
  group_by(migr,migrto) %>%  
  summarize(meanrate=mean(mean)) %>% ungroup() %>% 
  mutate(migrfrom_num=case_when(
    migr=="resident" ~ 1,
    migr=="east" ~ 2,
    migr=="west" ~ 3
  ),
  migrto_num=case_when(
    migrto=="resident" ~ 1,
    migrto=="east" ~ 2,
    migrto=="west" ~ 3
  )) %>% select(migrfrom_num,migrto_num,meanrate)%>% as.matrix()

#Build transition matrix
trans_matrix_link<-matrix(nrow = 3,ncol=3,NA)
trans_matrix_import<-matrix(nrow = 3,ncol=3,NA)
#Fill Transition matrix
##NOTE: rows are migration TO and columns are migration FROM

trans_matrix_import[trans_rate[,2:1]]<-round(trans_rate[,3],2)
trans_matrix_link[trans_rate[,2:1]]<-as.integer(trans_rate[,3]>0)

#Build names of each box
names <- c("Resident", "East", "West")


plotmat(trans_matrix_import, 
        pos = c(1,2), #number of columns and rows you want each node within
        curve = 0, 
        name = names, #names of the nodes
        lwd = 2, 
        box.lwd = 2, 
        box.col = c("Red","blue","green"), #color of nodes
        cex.txt = 0.8, #relative size of text
        box.type = "circle", 
        box.prop = 1.0,
        shadow.size=0,
        arr.type = "triangle",
        #specify arrow dimensions keeping them proportional to the values in the transition matrix
        arr.width=trans_matrix_import*2.5, 
        arr.length=trans_matrix_import*3,
        arr.pos=0.45,#Location of the arrow along the line segment
        endhead=T,#the line segment ends at the arrow
        #location of the lines to self for the nodes
        self.shiftx=0.15,
        self.shifty=-0.03
)
###################################################################################
###Yearly transition ratios
mig_res_ratio<-read.csv("mig_res_east_west_ratio.csv")
results<-read.csv("3MigrationTransitionProb.csv")
trans_rate <- results %>%  
  #group_by(migr,migrto) %>%  
  #summarize(meanrate=mean(mean)) %>% ungroup() %>% 
  mutate(migrfrom_num=case_when(
    migr=="resident" ~ 1,
    migr=="east" ~ 2,
    migr=="west" ~ 3
  ),
  migrto_num=case_when(
    migrto=="resident" ~ 1,
    migrto=="east" ~ 2,
    migrto=="west" ~ 3)
  ) %>% 
  select(year,migrfrom_num,migrto_num,mean) %>%
  tidyr::nest(-year) %>% 
  mutate(
    yr_trans_mtrx=purrr::map(data,
                             function(x){
                               x1<-as.matrix(x)
                               x2<-matrix(nrow=3,ncol=3,0)
                               x2[x1[,2:1]]<-round(x1[,3],2)
                               x2
                               }
                             )
    ) %>% left_join(mig_res_ratio,by=c("year"="Bio_year")) %>% 
    mutate(
      plots=purrr::map(yr_trans_mtrx,
                     function(x){
                       plotmat(x, 
                  pos = c(1,2), #number of columns and rows you want each node within
                  curve = 0, 
                  name = c("Resident", "East", "West"), #names of the nodes
                  lwd = 2, 
                  box.lwd = 2, 
                  box.col = c("Red","blue","green"), #color of nodes
                  cex.txt = 0.8, #relative size of text
                  box.type = "circle", 
                  box.prop = 1.0,
                  shadow.size=0,
                  arr.type = "triangle",
                  #specify arrow dimensions keeping them proportional to the values in the transition matrix
                  arr.width=x*1.5, 
                  arr.length=x*2,
                  arr.pos=0.45,#Location of the arrow along the line segment
                  endhead=T,#the line segment ends at the arrow
                  #location of the lines to self for the nodes
                  self.shiftx=0.15,
                  self.shifty=-0.03
                  )
               
               }
               )
    )
    


##########################################################

trans_rate <- results %>%  
  #group_by(migr,migrto) %>%  
  #summarize(meanrate=mean(mean)) %>% ungroup() %>% 
  mutate(migrfrom_num=case_when(
    migr=="resident" ~ 1,
    migr=="east" ~ 2,
    migr=="west" ~ 3
  ),
  migrto_num=case_when(
    migrto=="resident" ~ 1,
    migrto=="east" ~ 2,
    migrto=="west" ~ 3)
  ) %>% 
  select(year,migrfrom_num,migrto_num,mean) %>%
  tidyr::nest(-year) %>% 
  mutate(
    yr_trans_mtrx=purrr::map(data,
                             function(x){
                               x1<-as.matrix(x)
                               x2<-matrix(nrow=3,ncol=3,0)
                               x2[x1[,2:1]]<-round(x1[,3],2)
                               x2
                             }
    )
  ) %>% left_join(mig_res_ratio,by=c("year"="Bio_year")) %>% 
  mutate(
    plots=purrr::map(yr_trans_mtrx,
                     function(x){
                       plotmat(x, 
                               pos = c(1,2), #number of columns and rows you want each node within
                               curve = 0, 
                               name = c("Resident", "East", "West"), #names of the nodes
                               lwd = 2, 
                               box.lwd = 2, 
                               box.col = c("Red","blue","green"), #color of nodes
                               cex.txt = 0.8, #relative size of text
                               box.type = "circle", 
                               box.prop = 1.0,
                               shadow.size=0,
                               arr.type = "triangle",
                               #specify arrow dimensions keeping them proportional to the values in the transition matrix
                               arr.width=x*1.5, 
                               arr.length=x*2,
                               arr.pos=0.45,#Location of the arrow along the line segment
                               endhead=T,#the line segment ends at the arrow
                               #location of the lines to self for the nodes
                               self.shiftx=0.15,
                               self.shifty=-0.03
                       )
                       
                     }
    )
  )

##############################################

plot(trans_rate$plots)


  plotmat(trans_rate$yr_trans_mtrx, 
                  pos = c(1,2), #number of columns and rows you want each node within
                  curve = 0, 
                  name = c("Resident", "East", "West"), #names of the nodes
                  lwd = 2, 
                  box.lwd = 2, 
                  box.col = c("Red","blue","green"), #color of nodes
                  cex.txt = 0.8, #relative size of text
                  box.type = "circle", 
                  box.prop = 1.0,
                  shadow.size=0,
                  arr.type = "triangle",
                  #specify arrow dimensions keeping them proportional to the values in the transition matrix
                  arr.width=trans_matrix_import*2.5, 
                  arr.length=trans_matrix_import*3,
                  arr.pos=0.45,#Location of the arrow along the line segment
                  endhead=T,#the line segment ends at the arrow
                  #location of the lines to self for the nodes
                  self.shiftx=0.15,
                  self.shifty=-0.03
    )




#Build transition matrix
trans_matrix_link<-matrix(nrow = 3,ncol=3,NA)
trans_matrix_import<-matrix(nrow = 3,ncol=3,NA)
#Fill Transition matrix

trans_matrix_import[trans_rate[,1:2]]<-round(trans_rate[,3],2)
trans_matrix_link[trans_rate[,1:2]]<-as.integer(trans_rate[,3]>0)

#Build names of each box
names <- c("Resident", "East", "West")


plotmat(trans_matrix_import, 
        pos = c(1,2), #number of columns and rows you want each node within
        curve = 0, 
        name = names, #names of the nodes
        lwd = 2, 
        box.lwd = 2, 
        box.col = c("Red","blue","green"), #color of nodes
        cex.txt = 0.8, #relative size of text
        box.type = "circle", 
        box.prop = 1.0,
        shadow.size=0,
        arr.type = "triangle",
        #specify arrow dimensions keeping them proportional to the values in the transition matrix
        arr.width=trans_matrix_import*2.5, 
        arr.length=trans_matrix_import*3,
        arr.pos=0.45,#Location of the arrow along the line segment
        endhead=T,#the line segment ends at the arrow
        #location of the lines to self for the nodes
        self.shiftx=0.15,
        self.shifty=-0.03
)

