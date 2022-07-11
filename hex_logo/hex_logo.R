library(hexSticker)
library(magick)

base_logo <- image_read('hex_logo/base_logo.png')
file_name <- "expowo"
sticker(subplot = base_logo, # Center image
        package = "expowo",  # Package name to be displayed in the logo
        p_size = 21, # package's name font size
        p_color = "#FFFFFF", # package's name font color
        p_x = 1, p_y = 0.6, # package's name position, xy
        s_x = 1, s_y = 1.2, # image's position, xy
        s_width = 1.1, # image size
        s_height = 1.1,
        h_size = 1.2,
        h_fill = "#000000", # background
        h_color = "#91beb1", # border color (collie!)
        spotlight = TRUE,
        l_x = 0.75, l_y = 0.8, # spotlight's position
        l_width = 2,
        l_height = 2,
        l_alpha = 0.2,
        url = "",
        filename = paste0(file_name, ".png"),
        dpi = 300)

