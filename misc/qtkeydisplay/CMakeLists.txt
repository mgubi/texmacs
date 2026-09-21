cmake_minimum_required(VERSION 3.16)

project(QtKeyDisplay LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Qt6 REQUIRED COMPONENTS Widgets)

add_executable(QtKeyDisplay main.cpp)

target_link_libraries(QtKeyDisplay PRIVATE Qt6::Widgets)

set_target_properties(QtKeyDisplay PROPERTIES
    WIN32_EXECUTABLE ON
    MACOSX_BUNDLE ON
)
