NETCDF_INC_DIR=/usr/local/netcdf-4.2_hdf5_parallel/include
NETCDF_LIB_DIR=/usr/local/netcdf-4.2_hdf5_parallel/lib

ifort -O3 -xAVX p_interp.F90 -L$NETCDF_LIB_DIR -lnetcdff -lnetcdf -I$NETCDF_INC_DIR -o p_interp.curiethin.serial.exe
mpif90 -O3 -xAVX -D_MPI p_interp.F90 -L$NETCDF_LIB_DIR -lnetcdff -lnetcdf -I$NETCDF_INC_DIR -o p_interp.curiethin.dmpar.exe
ifort -O3 -xSSE4.2 p_interp.F90 -L$NETCDF_LIB_DIR -lnetcdff -lnetcdf -I$NETCDF_INC_DIR -o p_interp.curiefat.serial.exe
mpif90 -O3 -xSSE4.2 -D_MPI p_interp.F90 -L$NETCDF_LIB_DIR -lnetcdff -lnetcdf -I$NETCDF_INC_DIR -o p_interp.curiefat.dmpar.exe
