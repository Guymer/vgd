#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import glob
    import json
    import os

    # Import special modules ...
    try:
        import h5py
    except:
        raise Exception("\"h5py\" is not installed; run \"pip install --user h5py\"") from None
    try:
        import geojson
    except:
        raise Exception("\"geojson\" is not installed; run \"pip install --user geojson\"") from None
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.geo
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Convert H5 files to GeoJSON files.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--indiv",
        action = "store_true",
          help = "save each Polygon individually too",
    )
    args = parser.parse_args()

    # **************************************************************************

    print(f"The resolution of Earth is {0.001 * pyguymer3.RESOLUTION_OF_EARTH:,.1f} km/°.")
    print(f"The finest GLOBE scale is {1.0:,.1f} km.")
    print(f"The finest GLOBE resolution is {1000.0 / pyguymer3.RESOLUTION_OF_EARTH:,.4f} °.")

    # **************************************************************************

    # Loop over HDF5 files ...
    for hName in sorted(glob.glob("data/scale=??km/elev=????m.h5"))[::-1]:
        # Deduce GeoJSON name and skip this HDF5 file if it already exists ...
        jName = f'{hName.removesuffix(".h5")}.geojson'
        if os.path.exists(jName):
            print(f"Skipping \"{jName}\" (it already exists).")
            continue

        # Extract scale and skip if it is too detailed ...
        scale = hName.split("/")[1]
        if scale in [
            "scale=01km",
            "scale=02km",
            "scale=04km",
        ]:
            print(f"Skipping \"{jName}\" (it is too detailed).")
            continue

        print(f"Making \"{jName}\" ...")

        # **********************************************************************

        # Initialize list ...
        polys = []

        print(f"  Loading \"{hName}\" ...")

        # Open HDF5 file ...
        with h5py.File(hName, "r") as hObj:
            # Loop over rings ...
            for iRing in range(hObj.attrs["nRings"]):
                # Create short-hand ...
                key = f"ring={iRing:06d}"

                # Extract a dirty copy of the data ...
                dirtyLats = hObj[key]["lats"][:]                                # [°]
                dirtyLons = hObj[key]["lons"][:]                                # [°]

                # Create a clean list of the coordinates ...
                # NOTE: This is required as some of the Polygons touch
                #       themselves. According to the Shapely documentation, a
                #       Polygon can only touch itself once. See:
                #         * https://shapely.readthedocs.io/en/stable/manual.html#polygons
                coords = []                                                     # [°], [°]
                for iCoord in range(dirtyLats.size):
                    if ((dirtyLats == dirtyLats[iCoord]) * (dirtyLons == dirtyLons[iCoord])).sum() == 1:
                        coords.append((dirtyLons[iCoord], dirtyLats[iCoord]))   # [°], [°]

                # Create a Polygon from the list of coordinates ...
                poly = shapely.geometry.polygon.Polygon(coords)
                pyguymer3.geo.check(poly)

                # Append Polygon to list ...
                polys.append(poly)

        # Skip this collection if there aren't any Polygons ...
        if len(polys) == 0:
            print("  Skipping (there aren't any Polygons).")
            continue

        # **********************************************************************

        print("  Correcting holes ...")

        # Start infinite loop ...
        while True:
            # Initialize flag and progress ...
            foundHole = False
            progress = 0.0                                                      # [%]

            # Loop over Polygons ...
            for iOuter, outerPoly in enumerate(polys):
                # Loop over Polygons ...
                for iInner, innerPoly in enumerate(polys):
                    # Skip this inner Polygon if it is the same one as in the
                    # outer loop ...
                    if iInner == iOuter:
                        continue

                    # Skip this inner Polygon if it is not contained in the
                    # outer Polygon ...
                    if not outerPoly.contains(innerPoly):
                        continue

                    # Set flag and progress ...
                    foundHole = True
                    progress = 100.0 * float(iOuter) / float(len(polys) - 1)    # [%]

                    # Make a new Polygon containing this inner Polygon as a new
                    # hole in the outer Polygon ...
                    exterior = outerPoly.exterior
                    interiors = []
                    for interior in outerPoly.interiors:
                        interiors.append(interior)
                    interiors.append(innerPoly.exterior)
                    poly = shapely.geometry.polygon.Polygon(exterior, holes = interiors)
                    pyguymer3.geo.check(poly)

                    # Overwrite the outer Polygon with this new Polygon ...
                    polys[iOuter] = poly

                    # Remove this inner Polygon from the list ...
                    del polys[iInner]

                    # Stop looping ...
                    break

                # Check if a new hole was found ...
                if foundHole:
                    # Stop looping ...
                    break

            # Check if a new hole was not found ...
            if not foundHole:
                # Stop looping ...
                break

            print(f"   {progress:7.3f}%")

        # **********************************************************************

        print("  Saving GeoJSON ...")

        # Make a GeometryCollection of these Polygons ...
        # NOTE: Some of these Polygons may touch each other. According to the
        #       Shapely documentation, a MultiPolygon can only touch itself
        #       once. Therefore, I make a GeometryCollection instead. See:
        #         * https://shapely.readthedocs.io/en/stable/manual.html#collections
        #         * https://shapely.readthedocs.io/en/stable/manual.html#collections-of-polygons
        coll = shapely.geometry.collection.GeometryCollection(polys)

        # Save GeometryCollection as a GeoJSON ...
        # NOTE: As of 4/Aug/2025, the Python module "geojson" just converts the
        #       object to a Python dictionary and then it just calls the
        #       standard "json.dump()" function to format the Python dictionary
        #       as text. There is no way to specify the precision of the written
        #       string. Fortunately, if you have no shame, then you can load and
        #       then dump the string again, see:
        #         * https://stackoverflow.com/a/29066406
        with open(jName, "wt", encoding = "utf-8") as fObj:
            json.dump(
                json.loads(
                    geojson.dumps(
                        coll,
                        ensure_ascii = False,
                              indent = 4,
                           sort_keys = True,
                    ),
                    parse_float = lambda x: round(float(x), 4),                 # NOTE: 0.0001° is approximately 11.1 m.
                ),
                fObj,
                ensure_ascii = False,
                      indent = 4,
                   sort_keys = True,
            )

        # **********************************************************************

        # Check if the user wants to save each Polygon individually ...
        if args.indiv:
            # Loop over Polygons ...
            for iPoly, poly in enumerate(polys):
                # Deduce GeoJSON name ...
                jName = f'{hName.removesuffix(".h5")}.poly={iPoly:06d}.geojson'

                print(f"Making \"{jName}\" ...")

                # Save Polygon as a GeoJSON ...
                # NOTE: As of 4/Aug/2025, the Python module "geojson" just
                #       converts the object to a Python dictionary and then it
                #       just calls the standard "json.dump()" function to format
                #       the Python dictionary as text. There is no way to
                #       specify the precision of the written string.
                #       Fortunately, if you have no shame, then you can load and
                #       then dump the string again, see:
                #         * https://stackoverflow.com/a/29066406
                with open(jName, "wt", encoding = "utf-8") as fObj:
                    json.dump(
                        json.loads(
                            geojson.dumps(
                                poly,
                                ensure_ascii = False,
                                      indent = 4,
                                   sort_keys = True,
                            ),
                            parse_float = lambda x: round(float(x), 4),         # NOTE: 0.0001° is approximately 11.1 m.
                        ),
                        fObj,
                        ensure_ascii = False,
                              indent = 4,
                           sort_keys = True,
                    )
