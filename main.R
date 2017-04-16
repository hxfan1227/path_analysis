source("header.R")
library(agricolae)
library(hrbrthemes)
library(cowplot)
source("./functions/sen_slope.R")
source("./functions/trendMK.R")
source("./functions/ggally_cor.R")
xjrb.monthly.dt <- fread("./data/Xinjiang.csv")
xjrb.monthly.dt$Season <- factor(xjrb.monthly.dt$Season, levels = c("Spring (MAM)", "Summer (JJA)", "Autumn (SON)", "Winter (DJF)"))
pam <- function(dt, x, y){
  y.cor <- correlation(dt[, y, with = F], dt[, x, with = F])$correlation
  x.cor <- correlation(dt[, x, with = F])$correlation
  path.analysis(x.cor, y.cor)
}
xjrb.monthly.dt[, print(pam(.SD, x = 4:5, y = 6)), by = Season]
trendtest_result <- data.table(start = rep(NA, sum(1:21)), end = NA, trend = NA, slope = NA)
n <- 21
for (i in 1:21){
  if (n>=21){
    sum1 <- 1
  }
  else{
    sum1 <- 1+sum(21:(n+1))
  }
  n <- n-1
  trendtest_result$start[(sum1):sum(21:n)] <- 1977+i
  trendtest_result$end[(sum1):sum(21:n)] <-  1977+i+rep(9:(30-i))
}

trend.fun <- function(x){
  for (i in 1:sum(1:21)){
    trendtest_result$trend[i] <- trendMK(x[(Year <= trendtest_result$end[i] & Year >= trendtest_result$start[i]), value])
    trendtest_result$slope[i] <- sen_slope(x[(Year <= trendtest_result$end[i] & Year >= trendtest_result$start[i]), value], dt=1)$slope
  }
  trendtest_result$variable <- unique(x$variable)
  trendtest_result$season <- unique(x$Season)
  data.table(trendtest_result)
}


xjrb.monthly.dt[, .(PRCP = sum(Prcp), SM = sum(SM), Runoff = sum(Runoff)), by = .(Year, Season)] %>%
  split(.$Season) %>% map(~melt(.x, id.vars = 1:2)) %>% 
  map(~split(.x, .x$variable)) %>% flatten() %>% 
  map(trend.fun) -> xjrb.sesaon.list

xjrb.sesaon.list %>% map(~ggplot(.x, aes(x = start, y = end, fill = slope)) +
                           geom_raster(interpolate = F) +
                           geom_contour(aes(z=abs(trend),colour=..level..),size=1,breaks=c(0,1.96,2.56),lineend = "round") +
                           scale_fill_gradient2(name = "mm/yr\n", low = muted("blue"), high = "red") +
                           scale_color_continuous(guide = F,high = "blue",low = "black")+labs(x="Start year",y="End year") +
                           scale_x_continuous(expand = c(0, 0)) + 
                           scale_y_continuous(expand = c(0, 0)) +
                           ggtitle(label = unique(.x$season)) +
                           theme_bw(base_family = "serif", base_size = 25) +
                           theme(legend.key.height = unit(0.6, "in"))
                           ) -> xjrb.plot.list
# plot seasnoal PRCP trend 
xjrb.season.prcp.plot <- cowplot::plot_grid(xjrb.plot.list[[1]], xjrb.plot.list[[4]], xjrb.plot.list[[7]], xjrb.plot.list[[10]], labels = "AUTO", label_size = 30)
ggsave(filename = "./figures/xjrb_season_prcp.wmf", xjrb.season.prcp.plot, width = 14, height = 12, units = 'in', family = "serif")
ggsave(filename = "./figures/xjrb_season_prcp.eps", xjrb.season.prcp.plot, width = 14, height = 12, units = 'in', family = "serif", device = cairo_ps)
ggsave(filename = "./figures/xjrb_season_prcp.tiff", xjrb.season.prcp.plot, width = 14, height = 12, units = 'in', family = "serif", dpi = 500, compression = "lzw+p")
# plot seasnoal SM trend 
xjrb.season.sm.plot <- cowplot::plot_grid(xjrb.plot.list[[2]], xjrb.plot.list[[5]], xjrb.plot.list[[8]], xjrb.plot.list[[11]], labels = "AUTO", label_size = 30)
ggsave(filename = "./figures/xjrb_season_sm.wmf", xjrb.season.sm.plot, width = 14, height = 12, units = 'in', family = "serif")
ggsave(filename = "./figures/xjrb_season_sm.eps", xjrb.season.sm.plot, width = 14, height = 12, units = 'in', family = "serif", device = cairo_ps)
ggsave(filename = "./figures/xjrb_season_sm.tiff", xjrb.season.sm.plot, width = 14, height = 12, units = 'in', family = "serif", dpi = 500, compression = "lzw+p")
# plot seasnoal Runoff trend 
xjrb.season.runoff.plot <- cowplot::plot_grid(xjrb.plot.list[[3]], xjrb.plot.list[[6]], xjrb.plot.list[[9]], xjrb.plot.list[[12]], labels = "AUTO", label_size = 30)
ggsave(filename = "./figures/xjrb_season_runoff.wmf", xjrb.season.runoff.plot, width = 14, height = 12, units = 'in', family = "serif")
ggsave(filename = "./figures/xjrb_season_runoff.eps", xjrb.season.runoff.plot, width = 14, height = 12, units = 'in', family = "serif", device = cairo_ps)
ggsave(filename = "./figures/xjrb_season_runoff.tiff", xjrb.season.runoff.plot, width = 14, height = 12, units = 'in', family = "serif", dpi = 500, compression = "lzw+p")

assignInNamespace("ggally_cor", ggally_cor, "GGally")
xjrb.montly.cor.plot <- ggpairs(xjrb.monthly.dt[,5:7],
                                lower = list(continuous = my_smooth),
                                diag = list(continuous = wrap("barDiag", fill = "blue", colour = "black")),
                                upper = list(continuous = wrap("cor",colour="black")), 
                                columnLabels = c("Precipitation (mm)", "Soil water storage (mm)", "Runoff (mm)"), 
                                axisLabels = "show") +
  theme_bw(base_size = 25,base_family = "serif") +
  theme(strip.background = element_blank()) 
ggsave(filename = "./figures/xjrb_montly_cor.wmf", xjrb.montly.cor.plot, width = 14, height = 14, units = 'in', family = "serif")
ggsave(filename = "./figures/xjrb_montly_cor.eps", xjrb.montly.cor.plot, width = 14, height = 14, units = 'in', family = "serif", device = cairo_ps)
ggsave(filename = "./figures/xjrb_montly_cor.tiff", xjrb.montly.cor.plot, width = 14, height = 14, units = 'in', family = "serif", dpi = 500, compression = "lzw+p")

my_smooth <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(..., size = 2, colour = "blue") +
    geom_smooth(..., se = F, method = "lm", colour = "red")
  p
}

xjrb.sp.cor.plot <- ggpairs(xjrb.monthly.dt[Season == "Spring (MAM)",5:7],
                                lower = list(continuous = my_smooth),
                                diag = list(continuous = wrap("barDiag", fill = "blue", colour = "black")),
                                upper = list(continuous = wrap("cor",colour="black")), 
                                columnLabels = c("Precipitation (mm)", "SWS (mm)", "Runoff (mm)"), 
                                axisLabels = "show") +
  theme_bw(base_size = 40,base_family = "serif") +
  theme(strip.background = element_blank()) 
ggsave(filename = "./figures/xjrb_sp_cor.wmf", xjrb.sp.cor.plot, width = 14, height = 14, units = 'in', family = "serif")
ggsave(filename = "./figures/xjrb_sp_cor.eps", xjrb.sp.cor.plot, width = 14, height = 14, units = 'in', family = "serif", device = cairo_ps)
ggsave(filename = "./figures/xjrb_sp_cor.tiff", xjrb.sp.cor.plot, width = 14, height = 14, units = 'in', family = "serif", dpi = 500, compression = "lzw+p")

xjrb.su.cor.plot <- ggpairs(xjrb.monthly.dt[Season == "Summer (JJA)",5:7],
                            lower = list(continuous = my_smooth),
                            diag = list(continuous = wrap("barDiag", fill = "blue", colour = "black")),
                            upper = list(continuous = wrap("cor",colour="black")), 
                            columnLabels = c("Precipitation (mm)", "SWS (mm)", "Runoff (mm)"), 
                            axisLabels = "show") +
  theme_bw(base_size = 40,base_family = "serif") +
  theme(strip.background = element_blank()) 
ggsave(filename = "./figures/xjrb_su_cor.wmf", xjrb.su.cor.plot, width = 14, height = 14, units = 'in', family = "serif")
ggsave(filename = "./figures/xjrb_su_cor.eps", xjrb.su.cor.plot, width = 14, height = 14, units = 'in', family = "serif", device = cairo_ps)
ggsave(filename = "./figures/xjrb_su_cor.tiff", xjrb.su.cor.plot, width = 14, height = 14, units = 'in', family = "serif", dpi = 500, compression = "lzw+p")

xjrb.au.cor.plot <- ggpairs(xjrb.monthly.dt[Season == "Autumn (SON)",5:7],
                            lower = list(continuous = my_smooth),
                            diag = list(continuous = wrap("barDiag", fill = "blue", colour = "black")),
                            upper = list(continuous = wrap("cor",colour="black")), 
                            columnLabels = c("Precipitation (mm)", "SWS (mm)", "Runoff (mm)"), 
                            axisLabels = "show") +
  theme_bw(base_size = 40,base_family = "serif") +
  theme(strip.background = element_blank()) 
ggsave(filename = "./figures/xjrb_au_cor.wmf", xjrb.au.cor.plot, width = 14, height = 14, units = 'in', family = "serif")
ggsave(filename = "./figures/xjrb_au_cor.eps", xjrb.au.cor.plot, width = 14, height = 14, units = 'in', family = "serif", device = cairo_ps)
ggsave(filename = "./figures/xjrb_au_cor.tiff", xjrb.au.cor.plot, width = 14, height = 14, units = 'in', family = "serif", dpi = 500, compression = "lzw+p")

xjrb.wi.cor.plot <- ggpairs(xjrb.monthly.dt[Season == "Winter (DJF)",5:7],
                            lower = list(continuous = my_smooth),
                            diag = list(continuous = wrap("barDiag", fill = "blue", colour = "black")),
                            upper = list(continuous = wrap("cor",colour="black")), 
                            columnLabels = c("Precipitation (mm)", "SWS (mm)", "Runoff (mm)"), 
                            axisLabels = "show") +
  theme_bw(base_size = 40,base_family = "serif") +
  theme(strip.background = element_blank()) 
ggsave(filename = "./figures/xjrb_wi_cor.wmf", xjrb.wi.cor.plot, width = 14, height = 14, units = 'in', family = "serif")
ggsave(filename = "./figures/xjrb_wi_cor.eps", xjrb.wi.cor.plot, width = 14, height = 14, units = 'in', family = "serif", device = cairo_ps)
ggsave(filename = "./figures/xjrb_wi_cor.tiff", xjrb.wi.cor.plot, width = 14, height = 14, units = 'in', family = "serif", dpi = 500, compression = "lzw+p")

trend.fun2 <- function(x){
  for (i in 1:sum(1:21)){
    # trendtest_result$trend[i] <- trendMK(x[(Year <= trendtest_result$end[i] & Year >= trendtest_result$start[i]), value])
    trendtest_result$slope[i] <- sen_slope(x[(Year <= trendtest_result$end[i] & Year >= trendtest_result$start[i]), value], dt=1)$slope / 
      mean(x[(Year <= trendtest_result$end[i] & Year >= trendtest_result$start[i]), value]) * 100
  }
  trendtest_result$variable <- unique(x$variable)
  trendtest_result$season <- unique(x$Season)
  data.table(trendtest_result)
}


xjrb.monthly.dt[, .(PRCP = sum(Prcp), SM = sum(SM), Runoff = sum(Runoff)), by = .(Year, Season)] %>%
  split(.$Season) %>% map(~melt(.x, id.vars = 1:2)) %>% 
  map(~split(.x, .x$variable)) %>% flatten() %>% 
  map(trend.fun2) -> xjrb.sesaon.trend2.list

xjrb.sesaon.trend2.dt <- data.table(do.call(rbind.fill, xjrb.sesaon.trend2.list))[, trend := NULL] %>%
  dcast(start + end + season ~ variable, value.var = "slope") %>% data.table()

xjrb.elasticity.raster <- raster(extent(c(-20, 20, -20, 20)), ncols = 100, nrows = 100)
xjrb.elasticity.raster[] <- 1:ncell(xjrb.elasticity.raster)
projection(xjrb.elasticity.raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

my_inter <- function(df, df.proj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"){
  # cat(paste(unique(df$Year), "_", unique(df$variable), "_", unique(df$group), "\n", sep = ""))
  coordinates(df) <- ~PRCP+SM
  projection(df) <- df.proj
  # df.trans <- spTransform(df, CRSobj = CRS(df.trans))
  vgm <- variogram(Runoff ~ 1, df)
  m <- fit.variogram(vgm,  vgm("Sph"))
  gOK <- gstat(NULL, "model", Runoff ~ 1, df, model = m)
  OK <- interpolate(xjrb.elasticity.raster, gOK)
  OK.dt <- as.data.frame(OK, xy = T, na.rm = T)
}

elasticity.result.list <- dlply(xjrb.sesaon.trend2.dt, .(season), .fun = my_inter)

elasticity.result.dt <- data.table(do.call(rbind.fill, elasticity.result.list))
elasticity.result.dt[, season := rep(unique(xjrb.sesaon.trend2.dt$season), each = 100 * 100)]

test <- ggplot(elasticity.result.dt, aes(x = x, y = y, fill = model.pred)) +
  geom_tile() +
  geom_contour(aes(z = model.pred), colour = "black") +
  labs(x = "Change in Precipitation (%/yr)", y = "Change in Soil water storage (%/yr)") +
  facet_wrap(~season) +
  scale_fill_gradient2(name = "Change in Runoff (%/yr)\n", low = muted("blue"), high = "red") +
  theme_ipsum(base_family = "serif", base_size = 20, axis_title_just = "c", ticks = T, grid = F, 
              strip_text_size = 20, axis_title_size = 20, axis_title_family = "serif") +
  theme(legend.position = "bottom", 
        legend.key.width = unit(1, "in"), 
        axis.text = element_text(colour = "black"))
ggsave(filename = "./figures/test.wmf", test, width = 14, height = 14, units = 'in', family = "serif")
ggsave(filename = "./figures/xjrb_au_cor.eps", xjrb.au.cor.plot, width = 14, height = 14, units = 'in', family = "serif", device = cairo_ps)
ggsave(filename = "./figures/xjrb_au_cor.tiff", xjrb.au.cor.plot, width = 14, height = 14, units = 'in', family = "serif", dpi = 500, compression = "lzw+p")

xjrb.year.dt <- xjrb.monthly.dt[, llply(.SD, sum), .SDcols = c("Prcp", "SM", "Runoff"), by = Year]
xjrb.year.dt.melt <- melt(xjrb.year.dt, id.vars = 1)
annual.trend.list <- dlply(xjrb.year.dt.melt, .(variable), .fun = function(x) trend.fun(data.table(x)))
annual.trend.dt <- data.table(do.call(rbind.fill, annual.trend.list))


summary(lm(data = xjrb.year.dt, Prcp~Year))
p3 <- ggplot(xjrb.year.dt, aes(x = Year, y = Prcp))  +
  geom_line() + geom_smooth(method = "lm", colour = "red", se = F) +
  scale_x_continuous(breaks = seq(1970, 2010, by = 5)) +
  theme_bw(base_size = 20, base_family = "serif") +
  geom_text(x = 1985, y = 2500, label = eqn(-13622.10, 7.78, 0.04), parse = T, check_overlap = T, size = 6) +
  labs(y = expression(paste("Annual Precipitation (mm/yr)"))) 
p4 <- ggplot(annual.trend.dt[variable == "Prcp"],aes(x=start,y=end,fill=slope))+geom_tile()+
  geom_contour(aes(z=abs(trend),colour=..level..),size=1,breaks=c(0,1.96,2.56),lineend = "round") +
  scale_fill_gradient2(name = "mm/yr\n", low = muted("blue"), high = "red") +
  scale_color_continuous(guide = F,high = "blue",low = "black")+labs(x="Start year",y="End year") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw(base_size = 20,base_family = "serif")+
  theme(legend.key.height = unit(1,"in"),
        legend.background = element_blank(),
        plot.background=element_blank()) 
p34 <- plot_grid(p3, p4, align = "h", labels = "AUTO", label_size = 20)
ggsave(filename = "./figures/annualP.wmf", p34, width = 16, height = 8, units = 'in', family = "serif")
ggsave(filename = "./figures/annualP.eps", p34, width = 16, height = 8, units = 'in', family = "serif", device = cairo_ps)
ggsave(filename = "./figures/annualP.tiff", p34, width = 16, height = 8, units = 'in', family = "serif", dpi = 500, compression = "lzw+p")


summary(lm(data = xjrb.year.dt, SM~Year))
p5 <- ggplot(xjrb.year.dt, aes(x = Year, y = SM))  +
  geom_line() + geom_smooth(method = "lm", colour = "red", se = F) +
  scale_x_continuous(breaks = seq(1970, 2010, by = 5)) +
  theme_bw(base_size = 20, base_family = "serif") +
  geom_text(x = 1985, y = 950, label = eqn(1193.21, -0.25, 0.01), parse = T, check_overlap = T, size = 6) +
  labs(y = expression(paste("Annual soil water storage (mm/yr)"))) 
p6 <- ggplot(annual.trend.dt[variable == "SM"],aes(x=start,y=end,fill=slope))+geom_tile()+
  geom_contour(aes(z=abs(trend),colour=..level..),size=1,breaks=c(0,1.96,2.56),lineend = "round") +
  scale_fill_gradient2(name = "mm/yr\n", low = muted("blue"), high = "red") +
  scale_color_continuous(guide = F,high = "blue",low = "black")+labs(x="Start year",y="End year") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw(base_size = 20,base_family = "serif")+
  theme(legend.key.height = unit(1,"in"),
        legend.background = element_blank(),
        plot.background=element_blank()) 
p56 <- plot_grid(p5, p6, align = "h", labels = "AUTO", label_size = 20)
ggsave(filename = "./figures/annualSM.wmf", p56, width = 16, height = 8, units = 'in', family = "serif")
ggsave(filename = "./figures/annualSM.eps", p56, width = 16, height = 8, units = 'in', family = "serif", device = cairo_ps)
ggsave(filename = "./figures/annualSM.tiff", p56, width = 16, height = 8, units = 'in', family = "serif", dpi = 500, compression = "lzw+p")

summary(lm(data = xjrb.year.dt, Runoff~Year))
p7 <- ggplot(xjrb.year.dt, aes(x = Year, y = Runoff))  +
  geom_line() + geom_smooth(method = "lm", colour = "red", se = F) +
  scale_x_continuous(breaks = seq(1970, 2010, by = 5)) +
  theme_bw(base_size = 20, base_family = "serif") +
  geom_text(x = 1985, y = 1750, label = eqn(-16604.36, 9.91, 0.05), parse = T, check_overlap = T, size = 6) +
  labs(y = expression(paste("Annual runoff depth (mm/yr)"))) 
p8 <- ggplot(annual.trend.dt[variable == "Runoff"],aes(x=start,y=end,fill=slope))+geom_tile()+
  geom_contour(aes(z=abs(trend),colour=..level..),size=1,breaks=c(0,1.96,2.56),lineend = "round") +
  scale_fill_gradient2(name = "mm/yr\n", low = muted("blue"), high = "red") +
  scale_color_continuous(guide = F,high = "blue",low = "black")+labs(x="Start year",y="End year") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw(base_size = 20,base_family = "serif")+
  theme(legend.key.height = unit(1,"in"),
        legend.background = element_blank(),
        plot.background=element_blank()) 
p78 <- plot_grid(p7, p8, align = "h", labels = "AUTO", label_size = 20)
ggsave(filename = "./figures/annualR.wmf", p78, width = 16, height = 8, units = 'in', family = "serif")
ggsave(filename = "./figures/annualR.eps", p78, width = 16, height = 8, units = 'in', family = "serif", device = cairo_ps)
ggsave(filename = "./figures/annualR.tiff", p78, width = 16, height = 8, units = 'in', family = "serif", dpi = 500, compression = "lzw+p")




  
