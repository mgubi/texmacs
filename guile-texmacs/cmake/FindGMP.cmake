# FindGMP.cmake
# Find the GNU Multi-Precision (GMP) library
#
# Output targets:
#   GMP::GMP
# Output variables:
#   GMP_FOUND
#   GMP_INCLUDE_DIRS
#   GMP_LIBRARIES

find_path(GMP_INCLUDE_DIR
    NAMES gmp.h
    PATHS
        "${CMAKE_CURRENT_SOURCE_DIR}/../local/include"
        "C:/msys64/windows-qt6/local/include"
        "C:/msys64/mingw64/include"
        "/mingw64/include"
        "/usr/local/include"
        "/usr/include"
)

find_library(GMP_LIBRARY
    NAMES gmp libgmp
    PATHS
        "${CMAKE_CURRENT_SOURCE_DIR}/../local/lib"
        "C:/msys64/windows-qt6/local/lib"
        "C:/msys64/mingw64/lib"
        "/mingw64/lib"
        "/usr/local/lib"
        "/usr/lib"
)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(GMP
    REQUIRED_VARS GMP_LIBRARY GMP_INCLUDE_DIR
)

if(GMP_FOUND)
    set(GMP_INCLUDE_DIRS ${GMP_INCLUDE_DIR})
    set(GMP_LIBRARIES ${GMP_LIBRARY})

    if(NOT TARGET GMP::GMP)
        add_library(GMP::GMP UNKNOWN IMPORTED)
        set_target_properties(GMP::GMP PROPERTIES
            IMPORTED_LOCATION "${GMP_LIBRARY}"
            INTERFACE_INCLUDE_DIRECTORIES "${GMP_INCLUDE_DIR}"
        )
    endif()
endif()

