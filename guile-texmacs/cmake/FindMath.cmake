# FindMath.cmake
# Find standard C math library (m)

include(FindPackageHandleStandardArgs)

if(MSVC)
    set(MATH_FOUND TRUE)
    set(Math_FOUND TRUE)
    set(MATH_LIBRARIES "")
else()
    find_library(MATH_LIBRARY m)
    if(MATH_LIBRARY)
        set(MATH_FOUND TRUE)
        set(Math_FOUND TRUE)
        set(MATH_LIBRARIES ${MATH_LIBRARY})
    else()
        # Some systems (e.g. Haiku, or MinGW in some setups) have math built into libc
        set(MATH_FOUND TRUE)
        set(Math_FOUND TRUE)
        set(MATH_LIBRARIES "")
    endif()
endif()

find_package_handle_standard_args(Math DEFAULT_MSG MATH_FOUND)

if(Math_FOUND AND NOT TARGET Math::Math)
    add_library(Math::Math INTERFACE IMPORTED)
    if(MATH_LIBRARIES)
        set_target_properties(Math::Math PROPERTIES
            INTERFACE_LINK_LIBRARIES "${MATH_LIBRARIES}"
        )
    endif()
endif()

