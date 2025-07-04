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
    parser.add_argument(
        "--timeout",
        default = 60.0,
           help = "the timeout for any requests/subprocess calls (in seconds)",
           type = float,
    )
    args = parser.parse_args()

    # **************************************************************************

    # Loop over PGMs ...
    for pgm in sorted(glob.glob("data/scale=??km/elev=????m.pgm")):
        print(f"Converting \"{pgm}\" ...")

        # Deduce PNG name ...
        png = f'{pgm.removesuffix(".pgm")}.png'

        # Open image as RGB (even if it is paletted) ...
        with PIL.Image.open(pgm) as iObj:
            im = iObj.convert("RGB")

        # Save image as a PNG ...
        im.save(png, optimise = True)

        # Remove PGM ...
        os.remove(pgm)

        # Optimize PNG ...
        pyguymer3.image.optimise_image(
            png,
              debug = args.debug,
              strip = True,
            timeout = args.timeout,
        )
