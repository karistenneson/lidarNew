setwd('C:\\Users\\krtenneson\\Desktop\\lidarModel\\VersionControl\\lidar')
setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidar')

################################################################################
## Kaibab info
Kai <- read.csv('./Data/NKaibab07192017.csv')
Kai <- Kai[, c('DOM_TYPE','ForestType')]

################################################################################
## dominant species info
folder2 <- '.\\Data\\OriginalFiles\\VegComposition\\'
spp <- read.table(paste0(folder2, 'unique_dom_types.txt'), sep = '\t', header = T)
colnames(spp)

## merge species info
data2<- merge(spp, Kai, by.x = 'DOM_TYPE', by.y = 'DOM_TYPE', all.x = T)
dim(Kai); dim(spp); dim(data2)
head(data2)

out_table = unique(data2)
head(out_table)
dim(out_table)

write.table(out_table, paste0(folder2, 'unique_dom_types2.txt'), quote = FALSE, sep='\t', row.names = F)

