# require(rgdal)
# require(sp)
require(raster)

setwd('/Users/pohle/switchdrive/SGG.00273/MODIS/sydney_region/')

dem = raster('DEM_sydney_region_resampled.tif')
plot(dem)

landcover_2018 = raster('MCD12Q1.A2019001.006.LC_Prop2_Assessment.reproj.tif')
plot(landcover_2018)


# julian date 'j':
as.Date('2017033', format='%Y%j')

# checking the julian date from standard format
date2transform = as.Date('2017-03-01', format='%Y-%m-%d')
format(date2transform, '%j')

# Selecting dates based on filename:
# 1) list filenames:
filenames = list.files(pattern = 'MOD13Q1')  # .. only with specific pattern in the file names
# 2) split the filenames - principle
strsplit(filenames, '\\.')       # the output is not convenient
strsplit(filenames, '\\.')[[1]]  # the output is a list. At element 1 it looks like this

# 3) go through each file name and extract the date
for(f in filenames){
  
}




require(raster)

setwd('/Users/pohle/switchdrive/SGG.00273/MODIS/sydney_region/')
scale_factor = 10000

filenames = list.files(pattern = 'MOD13Q1')  # .. only with specific pattern in the file names

# each file name and extract the date
dates_ = c()
for(f in filenames){
  date_string_ = strsplit(f, '\\.')[[1]][2] 
  date_string = gsub(pattern = 'A', replacement = '',x = date_string_)  # remove the "A"
  dates_ = c(dates_, date_string)
}
dates = as.Date(dates_, format = '%Y%j')  # convert the date string to a date object

rst = stack(filenames)
rst = rst/scale_factor

rst_mean = mean(rst)
# rst_range = range(rst)
rst_sd = raster::calc(rst, fun = sd)

# plot(rst_sd)
sd_high_ind = which(rst_sd[] > 1800)

# get pixels of each scene wherre the SD is higher than a certain value, e.g. 1800 (not scaled values)
sd_values = list()
for(layer_i in seq(nlayers(rst))){
  values = rst[[layer_i]][sd_high_ind]
  values[values<0] = NA
  sd_values[[layer_i]] = values
}

boxplot(sd_values[40:98], axes=F)
axis(2)
axis(1, labels = dates[40:98], at = seq(length(dates[40:98])), las=2)


n2020_feb_ind = which(dates== as.Date('2020-02-02'))
n2020_feb = rst[[n2020_feb_ind]]

plot(n2020_feb - rst_mean)

div_colors = colorRampPalette(colors = c('brown','white','darkgreen'))(20)  
# the number in the "()" is the number of colors

# dates[[5]]
plot(rst[[c(11:22)]] - rst_mean,
     col= div_colors,
     zlim=c(-.5,.5),
     legend.args = list(text = 'NDVI [-]',
                        side=4,
                        line = 2.6)
)

plot(rst[[c(23:44)]] - rst_mean,
     col= div_colors,
     zlim=c(-.5,.5),
     legend.args = list(text = 'NDVI [-]',
                        side=4,
                        line = 2.6)
)


