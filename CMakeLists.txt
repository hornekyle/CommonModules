cmake_minimum_required(VERSION 2.6)
enable_language(Fortran)
set(PROJECT "CommonModules")
project(${PROJECT})
enable_testing()

#=====================#
#= Set Default Flags =#
#=====================#

if(CMAKE_FORTRAN_COMPILER_VERSION VERSION_GREATER 5.0)
	set(WTABS "-Wno-tabs")
else(CMAKE_FORTRAN_COMPILER_VERSION VERSION_GREATER 5.0)
	set(WTABS "-Wtabs")
endif(CMAKE_FORTRAN_COMPILER_VERSION VERSION_GREATER 5.0)

set(CMAKE_Fortran_FLAGS "")
set(CMAKE_Fortran_FLAGS_DEBUG "-g2 -Wall ${WTABS} -fcheck=all -ffpe-trap=invalid,zero,overflow -fbacktrace -finit-real=snan")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -mtune=native -march=native")

#===============================#
#= Find Packages and Libraries =#
#===============================#

function(finder name lib)
	set(file ${ARGV2})
	
	find_library(finder_LIBRARY "${lib}" "/usr/lib")
	
	if(${file})
		find_path(finder_INCLUDE "${file}" "/usr/include")
	else(${file})
		set(finder_INCLUDE "/usr/include" CACHE PATH "")
	endif(${file})
	include_directories(${finder_INCLUDE})
	
	if(finder_LIBRARY AND finder_INCLUDE)
		set(${name}_FOUND 1 PARENT_SCOPE)
		
		set(${name}_LIBRARY ${finder_LIBRARY} CACHE FILEPATH "Path to ${name} library")
		set(${name}_INCLUDE ${finder_INCLUDE} CACHE PATH "Include path for ${name} library")
	else(finder_LIBRARY AND finder_INCLUDE)
		set(${name}_FOUND 0 PARENT_SCOPE)
		
		set(${name}_LIBRARY "" CACHE FILEPATH "Path to ${name} library")
		set(${name}_INCLUDE "" CACHE PATH "Include path for ${name} library")
	endif(finder_LIBRARY AND finder_INCLUDE)
	
	unset(finder_LIBRARY CACHE)
	unset(finder_INCLUDE CACHE)
endfunction(finder)

include(FindPkgConfig)
pkg_search_module(PLPLOT QUIET plplot-f95 plplotd-f95)
include_directories(${PLPLOT_INCLUDE_DIRS})
link_directories(${PLPLOT_LIBRARY_DIRS})

finder(NETCDF "netcdff" "netcdf.mod")
finder(FFTW3 "fftw3" "fftw3.f03")
finder(LAPACK "lapack")
finder(UMFPACK "umfpack")

#=========================#
#= Create Symlink Target =#
#=========================#

add_custom_target(symlink
	COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/input" "${CMAKE_BINARY_DIR}/input"
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

#======================#
#= Create FORD Target =#
#======================#

find_program(FORD_EXECUTABLE "ford")
add_custom_target(ford
	COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/pages" "${CMAKE_BINARY_DIR}/pages"
	COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/src" "${CMAKE_BINARY_DIR}/src"
	COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/project.md" "${CMAKE_BINARY_DIR}/project.md"
	COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/README.md" "${CMAKE_BINARY_DIR}/README.md"
	COMMAND "plplotlib/basic_prg"
	COMMAND "plplotlib/examples_prg"
	COMMAND ${FORD_EXECUTABLE} "project.md"
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

#=============#
#= Find Gmsh =#
#=============#

find_program(GMSH_EXECUTABLE "gmsh")

#=======================#
#= Create Test Targets =#
#=======================#

set(testKinds_src
	"src/kinds.f90"
	"src/test/testKinds.f90")
add_executable(testKinds ${testKinds_src})
set_target_properties(testKinds PROPERTIES RUNTIME_OUTPUT_DIRECTORY "tests")
add_test(testKinds "tests/testKinds")

set(testTime_src
	"src/kinds.f90"
	"src/time.f90"
	"src/test/testTime.f90")
add_executable(testTime ${testTime_src})
set_target_properties(testTime PROPERTIES RUNTIME_OUTPUT_DIRECTORY "tests")
add_test(testTime "tests/testTime")

set(testStats_src
	"src/kinds.f90"
	"src/stats.f90"
	"src/test/testStats.f90")
add_executable(testStats ${testStats_src})
set_target_properties(testStats PROPERTIES RUNTIME_OUTPUT_DIRECTORY "tests")
add_test(testStats "tests/testStats")

set(testArray_src
	"src/array.f90"
	"src/kinds.f90"
	"src/test/testArray.f90")
add_executable(testArray ${testArray_src})
set_target_properties(testArray PROPERTIES RUNTIME_OUTPUT_DIRECTORY "tests")
add_test(testArray "tests/testArray")

set(testMatIO_src
	"src/kinds.f90"
	"src/IO/matIO.f90"
	"src/test/testMatIO.f90")
add_executable(testMatIO ${testMatIO_src})
set_target_properties(testMatIO PROPERTIES RUNTIME_OUTPUT_DIRECTORY "tests")
add_test(testMatIO "tests/testMatIO")

if(GMSH_EXECUTABLE)
	set(testMesh_src
		"src/kinds.f90"
		"src/IO/mesh.f90"
		"src/test/testMesh.f90")
	add_executable(testMesh ${testMesh_src})
	set_target_properties(testMesh PROPERTIES RUNTIME_OUTPUT_DIRECTORY "tests")
	add_test(testMesh "tests/testMesh")
endif(GMSH_EXECUTABLE)

set(testAutoDiff_src
	"src/autoDiff.f90"
	"src/kinds.f90"
	"src/test/testAutoDiff.f90")
add_executable(testAutoDiff ${testAutoDiff_src})
set_target_properties(testAutoDiff PROPERTIES RUNTIME_OUTPUT_DIRECTORY "tests")
add_test(testAutoDiff "tests/testAutoDiff")

set(testTensor_src
	"src/kinds.f90"
	"src/matrix/tensor.f90"
	"src/test/testTensor.f90")
add_executable(testTensor ${testTensor_src})
set_target_properties(testTensor PROPERTIES RUNTIME_OUTPUT_DIRECTORY "tests")
add_test(testTensor "tests/testTensor")

if(PLPLOT_FOUND)
	set(testSparse_src
		"src/array.f90"
		"src/kinds.f90"
		"src/IO/text.f90"
		"src/time.f90"
		"src/plplotlib/plplotlib.f90"
		"src/matrix/solver.f90"
		"src/matrix/sparse.f90"
		"src/test/testSparse.f90")
	add_executable(testSparse ${testSparse_src})
	set_target_properties(testSparse PROPERTIES RUNTIME_OUTPUT_DIRECTORY "tests")
	add_test(testSparse "tests/testSparse")
	target_link_libraries(testSparse ${PLPLOT_LIBRARIES})
endif(PLPLOT_FOUND)

set(testIterate_src
	"src/iterate.f90"
	"src/kinds.f90"
	"src/IO/text.f90"
	"src/time.f90"
	"src/test/testIterate.f90")
add_executable(testIterate ${testIterate_src})
set_target_properties(testIterate PROPERTIES RUNTIME_OUTPUT_DIRECTORY "tests")
add_test(testIterate "tests/testIterate")

set(testVtkIO_src
	"src/array.f90"
	"src/kinds.f90"
	"src/IO/vtkIO.f90"
	"src/test/testVtkIO.f90")
add_executable(testVtkIO ${testVtkIO_src})
set_target_properties(testVtkIO PROPERTIES RUNTIME_OUTPUT_DIRECTORY "tests")
add_test(testVtkIO "tests/testVtkIO")

if(NETCDF_FOUND)
	set(testNetCDF_src
		"src/array.f90"
		"src/kinds.f90"
		"src/IO/netCDF.f90"
		"src/test/testNetCDF.f90")
	add_executable(testNetCDF ${testNetCDF_src})
	target_link_libraries(testNetCDF ${NETCDF_LIBRARY})
	set_target_properties(testNetCDF PROPERTIES RUNTIME_OUTPUT_DIRECTORY "tests")
	add_test(testNetCDF "tests/testNetCDF")
endif(NETCDF_FOUND)

if(FFTW3_FOUND)
	set(testFourier_src
		"src/kinds.f90"
		"src/fftw3.f90"
		"src/fourier.f90"
		"src/test/testFourier.f90")
	add_executable(testFourier ${testFourier_src})
	target_link_libraries(testFourier ${FFTW3_LIBRARY})
	set_target_properties(testFourier PROPERTIES RUNTIME_OUTPUT_DIRECTORY "tests")
	add_test(testFourier "tests/testFourier")
endif(FFTW3_FOUND)

set(testText_src
	"src/kinds.f90"
	"src/IO/text.f90"
	"src/time.f90"
	"src/test/testText.f90")
add_executable(testText ${testText_src})
set_target_properties(testText PROPERTIES RUNTIME_OUTPUT_DIRECTORY "tests")
add_test(testText "tests/testText")

set(testConfig_src
	"src/IO/config.f90"
	"src/kinds.f90"
	"src/IO/text.f90"
	"src/time.f90"
	"src/test/testConfig.f90")
add_executable(testConfig ${testConfig_src})
add_test(testConfig "tests/testConfig")
set_target_properties(testConfig PROPERTIES RUNTIME_OUTPUT_DIRECTORY "tests")
add_dependencies(testConfig symlink)

if(PLPLOT_FOUND)
	set(testEval_src
		"src/array.f90"
		"src/eval.f90"
		"src/kinds.f90"
		"src/plplotlib/plplotlib.f90"
		"src/IO/text.f90"
		"src/time.f90"
		"src/test/testEval.f90")
	add_executable(testEval ${testEval_src})
	target_link_libraries(testEval ${PLPLOT_LIBRARIES})
	add_test(testEval "tests/testEval")
	set_target_properties(testEval PROPERTIES RUNTIME_OUTPUT_DIRECTORY "tests")
endif(PLPLOT_FOUND)

#============================#
#= Create PlPlotLib Targets =#
#============================#

if(PLPLOT_FOUND)

	set(plplotlib_src
		"src/array.f90"
		"src/kinds.f90"
		"src/plplotlib/plplotlib.f90"
		"src/IO/text.f90"
		"src/time.f90")

	add_executable(logo_prg ${plplotlib_src} "src/plplotlib/logo.f90")
	target_link_libraries(logo_prg ${PLPLOT_LIBRARIES})

	add_executable(basic_prg ${plplotlib_src} "src/plplotlib/basic.f90")
	target_link_libraries(basic_prg ${PLPLOT_LIBRARIES})
	add_custom_command(TARGET basic_prg PRE_BUILD
		COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_BINARY_DIR}/examples"
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

	add_executable(animate_prg ${plplotlib_src} "src/plplotlib/animate.f90")
	target_link_libraries(animate_prg ${PLPLOT_LIBRARIES})

	add_executable(examples_prg ${plplotlib_src} "src/plplotlib/examples.f90")
	target_link_libraries(examples_prg ${PLPLOT_LIBRARIES})
	add_custom_command(TARGET examples_prg PRE_BUILD
		COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_BINARY_DIR}/examples"
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

	set_target_properties(logo_prg basic_prg animate_prg examples_prg
		PROPERTIES
		RUNTIME_OUTPUT_DIRECTORY "plplotlib")

endif(PLPLOT_FOUND)