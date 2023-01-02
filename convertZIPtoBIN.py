#!/usr/bin/env python3

# Import standard modules ...
import os
import zipfile

# Import special modules ...
try:
    import numpy
except:
    raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

# ******************************************************************************

# Check if the BIN file does not exist yet ...
if not os.path.exists("data/scale=01km.bin"):
    print("Making \"data/scale=01km.bin\" ...")

    # Define constants ...
    bins = [
        "all10/a11g",
        "all10/b10g",
        "all10/c10g",
        "all10/d10g",
        "all10/e10g",
        "all10/f10g",
        "all10/g10g",
        "all10/h10g",
        "all10/i10g",
        "all10/j10g",
        "all10/k10g",
        "all10/l10g",
        "all10/m10g",
        "all10/n10g",
        "all10/o10g",
        "all10/p10g",
    ]
    nx = 43200                                                                  # [px]
    ny = 21600                                                                  # [px]

    # Make map ...
    elev = numpy.zeros((ny, nx), dtype = numpy.int16)                           # [m]

    # Load dataset ...
    with zipfile.ZipFile("all10g.zip", "r") as fObj:
        # Initialize index ...
        iy = 0                                                                  # [px]

        # Loop over y-axis ...
        for i in range(4):
            # Initialize index ...
            ix = 0                                                              # [px]

            # Loop over x-axis ...
            for j in range(4):
                # Define tile size ...
                if i in [0, 3]:
                    nrows = 4800                                                # [px]
                else:
                    nrows = 6000                                                # [px]
                ncols = 10800                                                   # [px]

                # Load tile ...
                tile = numpy.frombuffer(
                    fObj.read(bins[j + i * 4]),
                    dtype = numpy.int16
                ).reshape(nrows, ncols)                                         # [m]

                # Fill map ...
                elev[iy:iy + tile.shape[0], ix:ix + tile.shape[1]] = tile[:, :] # [m]

                # Increment index ...
                ix += ncols                                                     # [px]

            # Increment index ...
            iy += nrows                                                         # [px]

    # Save BIN ...
    elev.tofile("data/scale=01km.bin")
