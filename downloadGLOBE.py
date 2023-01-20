#!/usr/bin/env python3

# Import standard modules ...
import os

# Import my modules ...
try:
    import pyguymer3
except:
    raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

# ******************************************************************************

# Check if the ZIP file does not exist yet ...
if not os.path.exists("all10g.zip"):
    print("Downloading \"all10g.zip\" ...")

    # Start session ...
    with pyguymer3.start_session() as sess:
        # Download the ZIP file ...
        if not pyguymer3.download_file(sess, "https://www.ngdc.noaa.gov/mgg/topo/DATATILES/elev/all10g.zip", "all10g.zip"):
            raise Exception("download failed", "https://www.ngdc.noaa.gov/mgg/topo/DATATILES/elev/all10g.zip") from None
