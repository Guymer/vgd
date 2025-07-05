#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import glob
    import os

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
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
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Convert PGM images to PNG images.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    args = parser.parse_args()

    # **************************************************************************

    # Loop over PGMs ...
    for pgm in sorted(glob.glob("data/scale=??km/elev=????m.pgm")):
        # Create short-hand and skip this PGM if the PNG already exists ...
        png = f'{pgm.removesuffix(".pgm")}.png'
        if os.path.exists(png):
            continue

        print(f"Converting \"{pgm}\" ...")

        # Open PGM and convert to NumPy array ...
        with PIL.Image.open(pgm) as iObj:
            img = numpy.array(iObj)

        # Save NumPy array as a PNG ...
        src = pyguymer3.image.makePng(
            img.reshape((img.shape[0], img.shape[1], 1)),
            calcAdaptive = True,
             calcAverage = True,
                calcNone = True,
               calcPaeth = True,
                 calcSub = True,
                  calcUp = True,
                 choices = "all",
                   debug = args.debug,
                     dpi = None,
                  levels = [9,],
               memLevels = [9,],
                 modTime = None,
                palUint8 = None,
              strategies = None,
                  wbitss = [15,],
        )
        with open(png, "wb") as fObj:
            fObj.write(src)
