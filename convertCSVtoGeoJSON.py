#!/usr/bin/env python3

# Import standard modules ...
import glob

# Import special modules ...
try:
    import geojson
    geojson.geometry.Geometry.__init__.__defaults__ = (None, False, 12)     # NOTE: See https://github.com/jazzband/geojson/issues/135#issuecomment-596509669
except:
    raise Exception("\"geojson\" is not installed; run \"pip install --user geojson\"") from None
try:
    import shapely
    import shapely.geometry
    import shapely.validation
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
    print(f"Processing \"{dname}\" ...")

    # **************************************************************************

    # Initialize list ...
    polys = []

    # Loop over LinearRings ...
    for cname in sorted(glob.glob(f"{dname}/ring=??????.csv")):
        print(f" > Loading \"{cname}\" ...")

        # Load lines of CSV ...
        with open(cname, "rt", encoding = "utf-8") as fobj:
            dirtyLines = fobj.readlines()[1:]

        # Remove duplicate lines ...
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
        if not isinstance(poly, shapely.geometry.polygon.Polygon):
            raise Exception("\"poly\" is not a Polygon") from None
        if not poly.is_valid:
            # pyguymer3.geo._debug(poly)
            raise Exception(f"\"poly\" is not a valid Polygon ({shapely.validation.explain_validity(poly)})") from None
        if poly.is_empty:
            raise Exception("\"poly\" is an empty Polygon") from None
        del coords

        # Append Polygon to list ...
        polys.append(poly)

    # **************************************************************************

    # print(polys)
    # exit()
