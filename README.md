# Vectorise GLOBE Dataset

## Workflow

1. Download the [GLOBE](https://www.ngdc.noaa.gov/mgg/topo/globe.html) dataset (by running [downloadGLOBE.py](downloadGLOBE.py))
2. Convert the ZIP file of the [GLOBE](https://www.ngdc.noaa.gov/mgg/topo/globe.html) dataset to a BIN file (by running [convertZIPtoBIN.py](convertZIPtoBIN.py))
3. Compile the FORTRAN programs (by running [Makefile](src/Makefile))
4. Downscale the [GLOBE](https://www.ngdc.noaa.gov/mgg/topo/globe.html) dataset to different resolutions (by running [downscale](src/downscale.F90))
5. Convert the [GLOBE](https://www.ngdc.noaa.gov/mgg/topo/globe.html) dataset(s) to PNG files (by running [convertBINtoPNG.py](convertBINtoPNG.py))
6. Vectorise the [GLOBE](https://www.ngdc.noaa.gov/mgg/topo/globe.html) dataset(s) to GeoJSON files (by running [vectorise](src/vectorise.F90))
