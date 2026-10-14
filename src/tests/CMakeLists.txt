find_package (Qt${AC_QT_MAJOR_VERSION} COMPONENTS Test REQUIRED)

file (GLOB_RECURSE TEST_SRC_FILES "*.cpp")
# the mac_images API only exists on macOS with Qt < 6 (see mac_images.h)
if (NOT APPLE OR AC_QT_MAJOR_VERSION GREATER_EQUAL 6)
  list (FILTER TEST_SRC_FILES EXCLUDE REGEX "/Plugins/MacOS/")
endif ()
# xml_test.cpp includes xml.cpp, whose symbols clash with texmacs_body
list (FILTER TEST_SRC_FILES EXCLUDE REGEX "/xml_test\\.cpp$")

# from list of files we'll create tests test_name.cpp -> test_name
foreach (_test_file ${TEST_SRC_FILES})
  get_filename_component (_test_name ${_test_file} NAME_WE)
  add_executable (${_test_name}
    ${_test_file}
  )
  target_compile_options (${_test_name} PRIVATE
    -include ${CMAKE_BINARY_DIR}/src/System/config.h
  )
  target_link_libraries (${_test_name}
    texmacs_body
    ${TeXmacs_Libraries}
    Qt${AC_QT_MAJOR_VERSION}::Test
  )
  set_target_properties (${_test_name} PROPERTIES AUTOMOC ON)
  add_test (${_test_name} ${_test_name})
  set_tests_properties (${_test_name} PROPERTIES TIMEOUT 5)
  set_tests_properties (${_test_name} PROPERTIES ENVIRONMENT "TEXMACS_PATH=${TEXMACS_SOURCE_DIR}/TeXmacs")
endforeach ()
