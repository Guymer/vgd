#!/usr/bin/python3

# Import standard modules ...
import glob
import os

# Import special modules ...
try:
    import PIL
    import PIL.Image
except:
    raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

# Import my modules ...
try:
    import pyguymer3
    import pyguymer3.image
except:
    raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

# Configure PIL to open images up to 1 GiP ...
PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                             # [px]

# Loop over PGMs ...
for pgm in sorted(glob.glob("data/scale=??km/elev=????m.pgm")):
    print(f"Converting \"{pgm}\" ...")

    # Deduce PNG name ...
    png = f"{pgm[:-4]}.png"

    # Open PGM and save it as a PNG ...
    PIL.Image.open(pgm).convert("RGB").save(png, optimize = True)

    # Remove PGM ...
    os.remove(pgm)

    # Optimize PNG ...
    pyguymer3.image.optimize_image(png, strip = True)
