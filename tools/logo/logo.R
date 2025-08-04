library(magick)
library(hexSticker)
library(showtext)


img <- "tools/logo/pigeon_sign.png"

sticker(
  subplot = img,
  package = "SiGN",
  s_x = 0.85, s_y = .9, s_width = .9,
  p_size = 25,
  p_x = 1.35, p_y = 1.25,
  p_color = "#2E3A59",
  h_fill = "#e0e6ed",
  h_color = "#2E3A59",
  url = "https://sign-r.github.io/SiGN/",
  u_x = .225,
  u_y = .55,
  u_angle = -30,
  u_size = 7,
  dpi = 400,
  filename = "tools/logo/hex_sticker.png"
)

sticker(
  subplot = img,
  package = "SiGN",
  s_x = 0.85, s_y = .9, s_width = .9,
  p_size = 7,
  p_x = 1.35, p_y = 1.25,
  p_color = "#2E3A59",
  h_fill = "#e0e6ed",
  h_color = "#2E3A59",
  url = "https://sign-r.github.io/SiGN/",
  u_x = .2,
  u_y = .55,
  u_angle = -30,
  u_size = 1.75,
  #dpi = 400,
  filename = "tools/logo/hex_sticker.svg"
)

# use_logo("tools/logo/hex_sticker.svg")


# GitHub Org Logo
#img <- image_read("tools/logo/hex_sticker.png")
#img <- image_resize(img, "150x")
#image_write(img, "tools/logo/hex_sticker_200x200.png")

