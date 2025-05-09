# Vectorise GLOBE Dataset (VGD)

!["gmake" GitHub Action Status](https://github.com/Guymer/vgd/actions/workflows/gmake.yaml/badge.svg) !["mypy" GitHub Action Status](https://github.com/Guymer/vgd/actions/workflows/mypy.yaml/badge.svg) !["pylint" GitHub Action Status](https://github.com/Guymer/vgd/actions/workflows/pylint.yaml/badge.svg)

This collection of FORTRAN programs and Python 3.x scripts downloads the [GLOBE](https://www.ngdc.noaa.gov/mgg/topo/globe.html) dataset and (eventually) creates a collection of GeoJSON files containing Polygons that describe Earth's elevation.

## Workflow

1. Download the [GLOBE](https://www.ngdc.noaa.gov/mgg/topo/globe.html) dataset (by running [downloadGLOBE.py](downloadGLOBE.py))
2. Convert the ZIP file of the [GLOBE](https://www.ngdc.noaa.gov/mgg/topo/globe.html) dataset to a BIN file (by running [convertZIPtoBIN.py](convertZIPtoBIN.py))
3. Compile the FORTRAN programs (by running [Makefile](src/Makefile))
4. Downscale the [GLOBE](https://www.ngdc.noaa.gov/mgg/topo/globe.html) dataset to different resolutions (by running [downscale](src/downscale.F90))
5. Convert the [GLOBE](https://www.ngdc.noaa.gov/mgg/topo/globe.html) dataset(s) to PNG files (by running [convertBINtoPNG.py](convertBINtoPNG.py))
6. Vectorise the [GLOBE](https://www.ngdc.noaa.gov/mgg/topo/globe.html) dataset(s) to rings stored in H5 files (by running [vectorise](src/vectorise.F90))
7. Convert the masks to PNG files (by running [convertPGMtoPNG.py](convertPGMtoPNG.py))
8. Convert the rings stored in H5 files to Polygons stored in GeoJSON files (by running [convertH5toGeoJSON.py](convertH5toGeoJSON.py))

## Dependencies

VGD requires the following Python modules to be installed and available in your `PYTHONPATH`.

* [geojson](https://pypi.org/project/geojson/)
* [h5py](https://pypi.org/project/h5py/)
* [numpy](https://pypi.org/project/numpy/)
* [PIL](https://pypi.org/project/Pillow/)
* [pyguymer3](https://github.com/Guymer/PyGuymer3)
* [shapely](https://pypi.org/project/Shapely/)

Additionally, due to the dependency on [my FORTRAN library](https://github.com/Guymer/fortranlib), you will also require the following Python modules to be installed and available in your `PYTHONPATH`:

* [matplotlib](https://pypi.org/project/matplotlib/)
* [scipy](https://pypi.org/project/scipy/)

## Pathfinding Logic

When FORTRAN loads up the binary data of elevation, the origin is in the top-left corner of Earth. The pathfinding algorithm then follows this logic (recognising that there are `nx * ny` pixels and `(nx + 1) * (ny + 1)` pixel edges):

![photo of my doodle](IMG_2875.jpg)
