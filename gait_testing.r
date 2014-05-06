require(ggplot2)

data_titles <- c('Frame','Subframe','Chest_x','Chest_y','Chest_z','MMS_x','MMS_y','MMS_z','RMH1_x','RMH1_y','RMH1_z','RCA_x','RCA_y','RCA_z','RMH5_x','RMH5_y','RMH5_z')

t3<-read.csv('test_data/Trial3.CSV',skip=5)
t4<-read.csv('test_data/Trial4.CSV',skip=5)
t5<-read.csv('test_data/Trial5.CSV',skip=5)

colnames(t3) <- data_titles
colnames(t4) <- data_titles
colnames(t5) <- data_titles

bf <- butter(2,10/100)
fd <- filtfilt(bf,t3[,'MMS_z'])

locs <- diff(sign(diff(fd)))

len = min(length(locs[locs<0]),length(locs[locs>0]))
df = data.frame(1:len,fd[locs>0][1:len],fd[locs<0][1:len])

ggplot(data=df)+geom_line()+geom_line()

