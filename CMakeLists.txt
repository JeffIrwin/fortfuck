
cmake_minimum_required(VERSION 3.7)

set(PROJECT fbc)

set(CMAKE_Fortran_FLAGS "-Wall -Wextra -fopenmp -Wno-tabs")
set(CMAKE_Fortran_FLAGS_DEBUG "-pedantic -fbounds-check -fbacktrace")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

if(APPLE)
	set(CMAKE_EXE_LINKER_FLAGS "-openmp")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
else()
	set(CMAKE_EXE_LINKER_FLAGS "-fopenmp")
endif()

set(CMAKE_Fortran_COMPILER "gfortran")

project(${PROJECT} Fortran)

add_executable(${PROJECT} src/fbc.f90)

