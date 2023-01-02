#!/usr/bin/env python3

# Import standard modules ...
import glob
import os

# Import special modules ...
try:
    import geojson
    geojson.geometry.Geometry.__init__.__defaults__ = (None, False, 12)     # NOTE: See https://github.com/jazzband/geojson/issues/135#issuecomment-596509669
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
    raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

# Loop over collections ...
for dname in sorted(glob.glob("data/scale=??km/elev=????m")):
    # Deduce GeoJSON name and skip this collection if it already exists ...
    jname = f"{dname}.geojson"
    if os.path.exists(jname):
        continue

    # Extract scale and skip if it is too detailed ...
    scale = dname.split("/")[1]
    if scale in ["scale=01km", "scale=02km", "scale=04km"]:
        print(f"Skipping \"{jname}\" (too detailed).")
        continue

    print(f"Making \"{jname}\" ...")

    # **************************************************************************

    print(" > Loading CSVs ...")

    # Initialize list ...
    polys = []

    # Loop over LinearRings ...
    for cname in sorted(glob.glob(f"{dname}/ring=??????.csv")):
        # Load lines of CSV ...
        with open(cname, "rt", encoding = "utf-8") as fObj:
            dirtyLines = fObj.readlines()[1:]

        # Remove duplicate lines ...
        # NOTE: This is required as some of the Polygons touch themselves.
        #       According to the Shapely documentation, a Polygon can only touch
        #       itself once. See:
        #         * https://shapely.readthedocs.io/en/stable/manual.html#polygons
        cleanLines = []
        for i, dirtyLine in enumerate(dirtyLines):
            if i in [0, len(dirtyLines) - 1]:
                cleanLines.append(dirtyLine)
                continue
            if dirtyLines.count(dirtyLine) == 1:
                cleanLines.append(dirtyLine)
                continue
        del dirtyLines

        # Create a list of coordinates ...
        coords = []                                                             # [°], [°]
        for cleanLine in cleanLines:
            lon, lat = cleanLine.split(",")
            coords.append((float(lon), float(lat)))                             # [°], [°]
        del cleanLines

        # Create a Polygon from the list of coordinates ...
        poly = shapely.geometry.polygon.Polygon(coords)
        pyguymer3.geo.check(poly)
        del coords

        # Append Polygon to list ...
        polys.append(poly)

    # Skip this collection if there aren't any Polygons ...
    if len(polys) == 0:
        continue

    # **************************************************************************

    print(" > Correcting holes ...")

    # Start infinite loop ...
    while True:
        # Initialize flag and progress ...
        foundHole = False
        progress = 0.0                                                          # [%]

        # Loop over Polygons ...
        for iOuter, outerPoly in enumerate(polys):
            # Loop over Polygons ...
            for iInner, innerPoly in enumerate(polys):
                # Skip this inner Polygon if it is the same one as in the outer
                # loop ...
                if iInner == iOuter:
                    continue

                # Skip this inner Polygon if it is not contained in the outer
                # Polygon ...
                if not outerPoly.contains(innerPoly):
                    continue

                # Set flag and progress ...
                foundHole = True
                progress = 100.0 * float(iOuter) / float(len(polys) - 1)        # [%]

                # Make a new Polygon containing this inner Polygon as a new hole
                # in the outer Polygon ...
                exterior = outerPoly.exterior
                interiors = []
                for interior in outerPoly.interiors:
                    interiors.append(interior)
                interiors.append(innerPoly.exterior)
                poly = shapely.geometry.polygon.Polygon(exterior, holes = interiors)
                pyguymer3.geo.check(poly)
                del exterior, interiors

                # Overwrite the outer Polygon with this new Polygon ...
                polys[iOuter] = poly
                del poly

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

    # **************************************************************************

    print(" > Saving GeoJSON ...")

    # Make a GeometryCollection of these Polygons ...
    # NOTE: Some of these Polygons may touch each other. According to the
    #       Shapely documentation, a MultiPolygon can only touch itself once.
    #       Therefore, I make a GeometryCollection instead. See:
    #         * https://shapely.readthedocs.io/en/stable/manual.html#collections
    #         * https://shapely.readthedocs.io/en/stable/manual.html#collections-of-polygons
    coll = shapely.geometry.collection.GeometryCollection(polys)
    del polys

    # Save GeometryCollection as a GeoJSON ...
    with open(jname, "wt", encoding = "utf-8") as fObj:
        geojson.dump(
            coll,
            fObj,
               indent = 4,
            sort_keys = True,
        )
    del coll
