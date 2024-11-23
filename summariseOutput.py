#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import glob

    # Import my modules ...
    try:
        import pyguymer3
    except:
        raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

    # Loop over output folders ...
    for dName in sorted(glob.glob("data/scale=??km/elev=????m")):
        # Find CSV files in output folder and skip if there aren't any ...
        cNames = sorted(glob.glob(f"{dName}/ring=??????.csv"))
        if len(cNames) == 0:
            continue

        # Find the maximum number of steps in any CSV file in output folder ...
        maxSteps = 0                                                            # [#]
        for cName in cNames:
            maxSteps = max(maxSteps, pyguymer3.nlines(cName) - 1)               # [#]

        print(dName, len(cNames), maxSteps)
