#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
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
        "--timeout",
        default = 60.0,
           help = "the timeout for any requests/subprocess calls (in seconds)",
           type = float,
    )
    args = parser.parse_args()

    # **************************************************************************

    # Define the size of the dataset ...
    nx = 43200                                                                  # [px]
    ny = 21600                                                                  # [px]

    # Loop over scales ...
    for iscale in range(6):
        # Determine scale ...
        scale = pow(2, iscale)                                                  # [km]

        # Determine file names ...
        bname = f"data/scale={scale:02d}km.bin"
        iname = f"data/scale={scale:02d}km.png"

        # Skip this scale if the BIN does not exist ...
        if not os.path.exists(bname):
            print(f"Skipping \"{iname}\" (BIN is missing).")
            continue

        # Skip this scale if the PNG already exists ...
        if os.path.exists(iname):
            print(f"Skipping \"{iname}\" (PNG already exists).")
            continue

        # Find out how many mega-pixels there are at this scale ...
        mega = float((nx // scale) * (ny // scale)) / 1.0e6                     # [Mpx]

        # Skip this scale if the PNG would be too big ...
        if mega > 250.0:
            print(f"Skipping \"{iname}\" (the PNG would be {mega:.1f} Mpx).")
            continue

        print(f"Making \"{iname}\" ...")

        # Load dataset as 64-bit floats ...
        elev = numpy.fromfile(
            bname,
            dtype = numpy.int16,
        ).astype(numpy.float64).reshape(ny // scale, nx // scale)               # [m]

        # Scale data from 0 to 255, mapping it from 0m to 2000m ...
        elev = 255.0 * elev / 2000.0
        numpy.place(elev, elev <   0.0,   0.0)
        numpy.place(elev, elev > 255.0, 255.0)

        # Make PNG ...
        pyguymer3.image.save_array_as_image(
            elev.astype(numpy.uint8),
            iname,
                 ct = "turbo",
              debug = args.debug,
               form = "png",
            timeout = args.timeout,
        )
