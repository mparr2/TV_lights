
library(imager)


light_dim <- c(x = 24, y = 12)
padding <- 50
img_path <- "./Pics/TronAres.jpg"
img <- load.image(img_path)

# to be set once later
img_ras <- as.raster(img)
x <- dim(img_ras)[2]
y <- dim(img_ras)[1]

x.win <- unname(x/light_dim[1])
y.win <- unname(y/light_dim[2])

# init
light_bar_left <- data.frame(num = 1:light_dim[2], pos = "L", 
                             x0 = 0, x1 = padding, 
                             y0 = round((0:(light_dim[2]-1))*y.win,0),
                             y1 = round(1:(light_dim[2])*y.win,0),
                             R = NA, G = NA, B = NA)

light_bar_top <- data.frame(num = 1:light_dim[1], pos = "T", 
                            x0 = round((0:(light_dim[1]-1))*x.win,0),
                            x1 = round(1:(light_dim[1])*x.win,0),
                            y0 = y - padding, y1 = y, 
                            R = NA, G = NA, B = NA)

light_bar_right <- data.frame(num = light_dim[2]:1, pos = "R", 
                              x0 =  x - padding, x1 = x, 
                              y0 = round(((light_dim[2] - 1):0) * y.win,0), 
                              y1 = round((light_dim[2]:1) *y.win,0), 
                              R = NA, G = NA, B = NA)

lights_all <- rbind(light_bar_left, light_bar_top, light_bar_right)

# start
lights <- lights_all
sapply(lights, function(light){
  get_avgRGB(img_ras[light[5]:light[6], light[3]:light[4]])
})




get_avgRGB <- function(x = as.raster(img)[1:100,1:100]){
  rgb <- col2rgb(x)
  rgb.df <- as.data.frame(rgb)
  out <- round(rowMeans(rgb.df),0)
  return(out)
}

val <- get_avgRGB(img_ras)







