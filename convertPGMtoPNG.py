#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.10/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import glob
    import os

    # Import special modules ...
    try:
        import PIL
        import PIL.Image
        PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                         # [px]
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

    # Loop over PGMs ...
    for pgm in sorted(glob.glob("data/scale=??km/elev=????m.pgm")):
        print(f"Converting \"{pgm}\" ...")

        # Deduce PNG name ...
        png = f'{pgm.removesuffix(".pgm")}.png'

        # Open image as RGB (even if it is paletted) ...
        with PIL.Image.open(pgm) as iObj:
            im = iObj.convert("RGB")

        # Save image as a PNG ...
        im.save(png, optimize = True)

        # Remove PGM ...
        os.remove(pgm)

        # Optimize PNG ...
        pyguymer3.image.optimize_image(png, strip = True)
