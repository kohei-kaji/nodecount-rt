#!/bin/bash

magick -density 600 ndccg.pdf ndccg.png
magick -density 600 ndccg_right.pdf ndccg_right.png
magick -density 600 nddep.pdf nddep.png
magick -density 600 ndtop.pdf ndtop.png
magick -density 600 ndbottom.pdf ndbottom.png
magick -density 600 ndleft.pdf ndleft.png

HEIGHT=$(identify -format "%h" ndtop.png)
magick -size 3x${HEIGHT} canvas:black vline.png

magick ./ndtop.png ./vline.png ./ndbottom.png ./vline.png ./ndleft.png +append ./ndcons.png
