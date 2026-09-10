# FindLTDL.cmake
# Find GNU Libtool Dynamic Loading (ltdl) library
#
# Output targets:
#   LTDL::LTDL
# Output variables:
#   LTDL_FOUND
#   LTDL_INCLUDE_DIRS
#   LTDL_LIBRARIES

find_path(LTDL_INCLUDE_DIR
    NAMES ltdl.h
    PATHS
        "${CMAKE_CURRENT_SOURCE_DIR}/../local/include"
        "C:/msys64/windows-qt6/local/include"
        "C:/msys64/mingw64/include"
        "/mingw64/include"
        "/usr/local/include"
        "/usr/include"
)

find_library(LTDL_LIBRARY
    NAMES ltdl libltdl
    PATHS
        "${CMAKE_CURRENT_SOURCE_DIR}/../local/lib"
        "C:/msys64/windows-qt6/local/lib"
        "C:/msys64/mingw64/lib"
        "/mingw64/lib"
        "/usr/local/lib"
        "/usr/lib"
)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LTDL
    REQUIRED_VARS LTDL_LIBRARY LTDL_INCLUDE_DIR
)

if(LTDL_FOUND)
    set(LTDL_INCLUDE_DIRS ${LTDL_INCLUDE_DIR})
    set(LTDL_LIBRARIES ${LTDL_LIBRARY})

    if(NOT TARGET LTDL::LTDL)
        add_library(LTDL::LTDL UNKNOWN IMPORTED)
        set_target_properties(LTDL::LTDL PROPERTIES
            IMPORTED_LOCATION "${LTDL_LIBRARY}"
            INTERFACE_INCLUDE_DIRECTORIES "${LTDL_INCLUDE_DIR}"
        )
    endif()
endif()

