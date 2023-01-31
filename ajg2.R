#!/home/pikam04/anaconda3/envs/egg/bin Rscript
# setwd('/home/pikam04/egg/model_weight')
setwd('Z:/99_Personal/eggy/plot_reforecast_4model')
rm(list = ls())
source('func.R')

bln <- c(1:7)
blns <- month.abb[bln]
blns1 <- rep(blns, each=3)
das <- c('I','II','III')
id_das <- paste(blns1,das)
dir.create(path = paste0('out/xlsx'), recursive = T)

#thn <- c(2000:2020)
thn <- c(2011:2020)
# thn <- c(2009:2020) ### error di 2009
for (th in 1:length(thn)) { #### error di 2009
# for (nz in 305:length(nzom)) { #### error di Kaltara_06
  th=1 ##############
  thns=thn[th]
  # arima <- read.csv(list.files(paste0(list.files(paste0('REFORECAST_HYBMG/',thns,'/'), pattern = as.character(thns-1), full.names = T),'/perzom'),full.names = T)[1]) 
  arima <- read.csv(list.files(paste0(list.files(paste0('../../../16_HYBMG_OUTLOOK/REFORECAST_HYBMG/',thns,'/'), pattern = as.character(thns-1), full.names = T),'/perzom'),full.names = T)[1])
  colnames(arima) <- gsub('X','',colnames(arima))
  arima$NOZOM <- ubah_nzom(arima$NOZOM);
  arima <- arima[c(1:22)]
  warima <- read.csv(list.files(paste0(list.files(paste0('../../../16_HYBMG_OUTLOOK/REFORECAST_HYBMG/',as.character(thns)), pattern = as.character(thns-1), full.names = T),'/perzom'),full.names = T)[2])
  # warima <- read.csv(list.files(paste0(list.files(paste0('REFORECAST_HYBMG/',as.character(thns)), pattern = as.character(thns-1), full.names = T),'/perzom'),full.names = T)[2]) 
  colnames(warima) <- gsub('X','',colnames(warima)) 
  warima$NOZOM <- ubah_nzom(warima$NOZOM);
  warima <- warima[c(1:22)] 
  ecraw <- readxl::read_excel(list.files('../../../01_MODEL_ECMWF/REFORECAST/PERZOM9120/RAW', pattern = as.character(thns), full.names = T)[1], sheet = 'EnsMean')
  # ecraw <- readxl::read_excel(list.files('REFORECAST_ECMWF/RAW', pattern = as.character(thns), full.names = T)[1], sheet = 'EnsMean')
  ecraw$NOZOM <- ubah_nzom(ecraw$NOZOM)
  ecraw <- ecraw[c(1:22)]
  eccor <- readxl::read_excel(list.files('../../../01_MODEL_ECMWF/REFORECAST/PERZOM9120/COR', pattern = as.character(thns), full.names = T)[1], sheet = 'EnsMean')
  # eccor <- readxl::read_excel(list.files('REFORECAST_ECMWF/COR', pattern = as.character(thns), full.names = T)[1], sheet = 'EnsMean')
  eccor$NOZOM <- ubah_nzom(eccor$NOZOM)
  eccor <- eccor[c(1:22)]
  mme1 <- readxl::read_excel(list.files(paste0('refo_mme1_raw/InaMME1_PERZOM9120/'), pattern = as.character(thns), full.names = T)[1], sheet = 'EnsMean')
  mme1$NOZOM <- ubah_nzom(mme1$NOZOM);
  mme1 <- mme1[c(1:22)]
  cfs <- readxl::read_excel(list.files(paste0('refo_cfs_cor/COR/'), pattern = as.character(thns), full.names = T)[1], sheet = 'EnsMean')
  cfs$NOZOM <- ubah_nzom(cfs$NOZOM)
  cfs <- cfs[c(1:22)]
  obs <- read.csv('../../../16_HYBMG_OUTLOOK/Update_CH_Blending_ZOM9120.csv') 
  # obs <- read.csv('OBS/Update_CH_Blending_ZOM9120.csv')
  obs$NOZOM <- ubah_nzom(obs$NOZOM); colnames(obs) <- gsub('X','',colnames(obs));
  obs <- obs[,which(colnames(obs) %in% colnames(arima))]
  #arima <- arima[c(1:ncol(ecraw))]; warima <- warima[c(1:ncol(ecraw))]
  obs <- obs[c(1:22)]
  #obs <- obs[c(1:ncol(ecraw))]
  # colnames(ecraw) <- colnames(arima); colnames(eccor) <- colnames(arima)
  
  colnames(obs)[2:ncol(obs)] <- id_das
  colnames(ecraw)[2:ncol(ecraw)] <- id_das
  colnames(eccor)[2:ncol(eccor)] <- id_das
  colnames(arima)[2:ncol(arima)] <- id_das
  colnames(warima)[2:ncol(warima)] <- id_das
  
  gabung <- c()
  for (nz in 1:length(nzom)) { #### error di Kaltara_06
    # nz=2 ##########
    nzoms <- nzom[nz]
    print(paste0(nzoms,' ',thns))
    arima1 <- arima[arima$NOZOM==nzoms,]; 
    warima1 <- warima[warima$NOZOM==nzoms,]; 
    ecraw1 <- ecraw[ecraw$NOZOM==nzoms,]
    eccor1 <- eccor[eccor$NOZOM==nzoms,]
    mme11 <- mme1[mme1$NOZOM==nzoms,]
    cfs1 <- cfs[cfs$NOZOM==nzoms,]
    obs1 <- obs[obs$NOZOM==nzoms,]
    
    model <- c('OBS','ECMWF_RAW','ECMWF_COR','ARIMA','WARIMA','MME1','CFSv2')
    dt <- rbind(obs1,ecraw1,eccor1,arima1,warima1,mme11,cfs1)
    colnames(dt)[1] <- 'MODEL'; dt$MODEL <- model
    
    dt1 <- pivot_longer(data = dt, cols = colnames(dt[!colnames(dt) %in% c('MODEL')]), names_to = 'DAS', values_to = 'CH')
    dt1$MODEL <- factor(dt1$MODEL, levels = unique(dt1$MODEL))
    dt1$DAS <- factor(dt1$DAS, levels = unique(dt1$DAS))
    t_dt <- as.data.frame(t(dt)); nms <- t_dt[1,]; t_dt <- t_dt[2:nrow(t_dt),]; colnames(t_dt) <- nms; t_dt$DAS <- rownames(t_dt)
    t_dt[!colnames(t_dt) %in% c('DAS')] <- as.data.frame(sapply(t_dt[!colnames(t_dt) %in% c('DAS')], as.numeric))
    t_dt2 <- t_dt
    t_dt1 <- as.data.frame(pivot_longer(data = t_dt, cols = colnames(t_dt[!colnames(t_dt) %in% c('DAS','OBS')]), names_to = 'MODEL', values_to = 'CH'))
    t_dt1[!colnames(t_dt1) %in% c('MODEL','DAS')] <- sapply(t_dt1[!colnames(t_dt1) %in% c('MODEL','DAS')], as.numeric)
    t_dt1$MODEL <- factor(t_dt1$MODEL, levels = unique(t_dt1$MODEL))
    
    warna <- c('black','cornflowerblue','blue','deeppink','deeppink4','orange2','red3')
    lty <- c(rep("solid", 7))
    p1 <- ggplot(dt1, aes(x=DAS)) + geom_line(aes(y = CH, color = MODEL, group=MODEL,linetype=MODEL),
          stat="identity", linewidth=1.3) + theme_linedraw() + labs(x='Dasarian', y='CH (mm/Dasarian)') + ggtitle(paste0("OBS vs MODEL ",nzoms)) +
          annotate("text",  x=Inf, y = Inf, label = paste0('Tahun ',thns), vjust=1, hjust=1) + scale_linetype_manual(values = lty) + 
          scale_color_manual(values = warna) + theme(legend.title = element_blank(),legend.position="bottom", plot.title = element_text(hjust = 0.5))
    p2 <- TaylorDiagram(t_dt1, obs = "OBS", mod = "CH", group = "MODEL", normalise = T, cols = c('cornflowerblue','blue','deeppink','deeppink4','orange2','red3'),
          text.obs = 'Observasi', key = F, main=paste0('Taylor Diagram'))
    
    png(paste0('out/png/',nzoms,'_',thns,'.png'), width = 2000, height = 600, res = 160)
    gridExtra::grid.arrange(p1,p2$plot, nrow = 1, widths=c(3,1))
    dev.off()
    
    rmse1 <- sapply(t_dt[!colnames(t_dt) %in% c('OBS','DAS')], function(x){rmse(actual = t_dt$OBS, model = x)})
    R <- as.numeric(t(p2$data)[rownames(t(p2$data))=='R',])
    sd_norm <- as.numeric(t(p2$data)[rownames(t(p2$data))=='sd.mod',])
    ss <- (4*((1+R)^4))/(((sd_norm+(1/sd_norm))^2)*((1+max(R))^4))
    w <- ss/(sum(ss))
    dtax <- as.data.frame(rbind(rmse1,R,sd_norm,ss,w))
    # print('DTAX')
    gabung[[nz]] <- dtax
  } 
  names(gabung) <- nzom
  
  gab_rmse <- c()
  for (ll in 1:length(gabung)) {
    # ll <- 1 #########
    rms <- gabung[[ll]][1,]
    gab_rmse[[ll]] <- rms
  }
  names(gab_rmse) <- nzom
  gab_rmse <- do.call(rbind, gab_rmse)
  gab_R <- c()
  for (ll in 1:length(gabung)) {
    # ll <- 1 #########
    R <- gabung[[ll]][2,]
    gab_R[[ll]] <- R
  }
  names(gab_R) <- nzom
  gab_R <- do.call(rbind, gab_R)
  gab_sdn <- c()
  for (ll in 1:length(gabung)) {
    # ll <- 1 #########
    sdn <- gabung[[ll]][3,]
    gab_sdn[[ll]] <- sdn
  }
  names(gab_sdn) <- nzom
  gab_sdn <- do.call(rbind, gab_sdn)
  gab_ss <- c()
  for (ll in 1:length(gabung)) {
    # ll <- 1 #########
    ss1 <- gabung[[ll]][4,]
    gab_ss[[ll]] <- ss1
  }
  names(gab_ss) <- nzom
  gab_ss <- do.call(rbind, gab_ss)
  gab_w <- c()
  for (ll in 1:length(gabung)) {
    # ll <- 1 #########
    w <- gabung[[ll]][5,]
    gab_w[[ll]] <- w
  }
  names(gab_w) <- nzom
  gab_w <- do.call(rbind, gab_w)
  
  par_gab <- list(gab_rmse,gab_R,gab_sdn,gab_ss,gab_w)
  names(par_gab) <- c('RMSE','R','SD_NORM','SS','W')
  
  openxlsx::write.xlsx(x = par_gab, file = paste0('out/xlsx/',thns,'.xlsx'), rowNames=T, colNames=T)
}

# rmse(actual = tes$OBS, model = tes$ECMWF_RAW)
# sapply(tes, function(x) {rmse(actual = tes$OBS, predicted = x)})
# sqrt(mean((tes$OBS - tes$ECMWF_RAW)^2))



