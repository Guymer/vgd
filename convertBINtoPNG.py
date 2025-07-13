#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import json
    import os

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

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
            description = "Convert BIN files to PNG images.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    parser.add_argument(
        "--maximum-size",
        default = 250.0,
           dest = "maxSize",
           help = "the maximum size of image to make a PNG for (in mega-pixels)",
           type = float,
    )
    args = parser.parse_args()

    # **************************************************************************

    # Load colour tables and create short-hand ...
    with open(f"{pyguymer3.__path__[0]}/data/json/colourTables.json", "rt", encoding = "utf-8") as fObj:
        colourTables = json.load(fObj)
    turbo = numpy.array(colourTables["turbo"]).astype(numpy.uint8)

    # Define the size of the dataset ...
    nx = 43200                                                                  # [px]
    ny = 21600                                                                  # [px]

    # Loop over scales ...
    for iscale in range(6):
        # Determine scale ...
        scale = pow(2, iscale)                                                  # [km]

        # Determine file names ...
        bName = f"data/scale={scale:02d}km.bin"
        pName = f"data/scale={scale:02d}km.png"

        # Skip this scale if the BIN does not exist ...
        if not os.path.exists(bName):
            print(f"Skipping \"{bName}\" (the BIN is missing).")
            continue

        # Skip this scale if the PNG already exists ...
        if os.path.exists(pName):
            print(f"Skipping \"{bName}\" (the PNG already exists).")
            continue

        # Find out how many mega-pixels there are at this scale and skip this
        # scale if the PNG would be too big ...
        mega = float((nx // scale) * (ny // scale)) / 1.0e6                     # [Mpx]
        if mega > args.maxSize:
            print(f"Skipping \"{bName}\" (the PNG would be {mega:,.1f} Mpx).")
            continue

        print(f"Making \"{pName}\" ...")

        # Load dataset as 64-bit floats ...
        elev = numpy.fromfile(
            bName,
            dtype = numpy.int16,
        ).astype(numpy.float64).reshape(ny // scale, nx // scale, 1)            # [m]

        # Scale data from 0 to 255, mapping it from 0 m to 2,000 m ...
        elev = 255.0 * (elev / 2000.0)
        numpy.place(elev, elev <   0.0,   0.0)
        numpy.place(elev, elev > 255.0, 255.0)
        elev = elev.astype(numpy.uint8)

        # Make PNG ...
        src = pyguymer3.image.makePng(
            elev,
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
                palUint8 = turbo,
              strategies = None,
                  wbitss = [15,],
        )
        with open(pName, "wb") as fObj:
            fObj.write(src)
