cmake_minimum_required(VERSION 2.6)
enable_language(Fortran)
set(PROJECT "CommonModules")
project(${PROJECT})
enable_testing()

set(CMAKE_Fortran_FLAGS "")
set(CMAKE_Fortran_FLAGS_DEBUG "-g2 -Wall -Wtabs -fcheck=all -ffpe-trap=invalid,zero,overflow -fbacktrace -finit-real=snan")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -mtune=native -march=native")

include(FindPkgConfig)
pkg_search_module(PLPLOT REQUIRED plplot-f95 plplotd-f95)
include_directories(${PLPLOT_INCLUDE_DIRS})
link_directories(${PLPLOT_LIBRARY_DIRS})

find_library(NETCDF_LIBRARY netcdff "/usr/lib")
find_path(NETCDF_INCLUDE "netcdf.mod" "/usr/include")

find_library(LAPACK_LIBRARY lapack "/usr/lib")
find_library(UMFPACK_LIBRARY umfpack "/usr/lib")
include_directories(${NETCDF_INCLUDE})

add_custom_target(symlink
	COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/results" "${CMAKE_BINARY_DIR}/results"
	COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/config" "${CMAKE_BINARY_DIR}/config"
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

set(FORD_EXECUTABLE "ford")
add_custom_target(ford
	COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_SOURCE_DIR}/pages" "${CMAKE_BINARY_DIR}/pages"
	COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_SOURCE_DIR}/src" "${CMAKE_BINARY_DIR}/src"
	COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_SOURCE_DIR}/project.md" "${CMAKE_BINARY_DIR}"
	COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_SOURCE_DIR}/README.md" "${CMAKE_BINARY_DIR}"
	COMMAND ${FORD_EXECUTABLE} "project.md"
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

set(testKinds_src
	"src/kinds.f90"
	"src/test/testKinds.f90")

add_executable(testKinds ${testKinds_src})
add_dependencies(testKinds symlink)

# target_link_libraries(main "gomp")
# target_link_libraries(main ${NETCDF_LIBRARY})
# target_link_libraries(main ${LAPACK_LIBRARY})
# target_link_libraries(main ${UMFPACK_LIBRARY})
# target_link_libraries(main ${PLPLOT_LIBRARIES})

add_test(testKinds testKinds)