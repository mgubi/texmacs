cmake_minimum_required(VERSION 3.16)
project(guile-texmacs VERSION 1.8.8 LANGUAGES C)

# Version variables matching GUILE-VERSION
set(GUILE_MAJOR_VERSION 1)
set(GUILE_MINOR_VERSION 8)
set(GUILE_MICRO_VERSION 8)
set(GUILE_EFFECTIVE_VERSION "${GUILE_MAJOR_VERSION}.${GUILE_MINOR_VERSION}")
set(GUILE_VERSION "${GUILE_EFFECTIVE_VERSION}.${GUILE_MICRO_VERSION}")
set(VERSION "${GUILE_VERSION}")
set(PACKAGE "guile-texmacs")

set(LIBGUILE_INTERFACE_CURRENT 21)
set(LIBGUILE_INTERFACE_REVISION 0)
set(LIBGUILE_INTERFACE_AGE 4)
set(LIBGUILE_INTERFACE "${LIBGUILE_INTERFACE_CURRENT}:${LIBGUILE_INTERFACE_REVISION}:${LIBGUILE_INTERFACE_AGE}")

set(LIBGUILE_SRFI_SRFI_1_MAJOR 3)
set(LIBGUILE_SRFI_SRFI_4_MAJOR 3)
set(LIBGUILE_SRFI_SRFI_13_14_MAJOR 3)
set(LIBGUILE_SRFI_SRFI_60_MAJOR 2)

# Standards and options
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)

option(BUILD_SHARED_LIBS "Build shared libraries instead of static" OFF)
option(GUILE_ENABLE_NETWORKING "Enable networking support" ON)
option(GUILE_ENABLE_POSIX "Enable POSIX support" ON)
option(GUILE_ENABLE_REGEX "Enable regex support" ON)

if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    add_compile_options(-fno-strict-aliasing -fwrapv)
endif()

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(GNUInstallDirs)

# Autotool checks (probes system types, headers, functions)
include(cmake/AutotoolCheck.cmake)

# Dependencies
find_package(GMP REQUIRED)
find_package(Math REQUIRED)
find_package(Threads REQUIRED)

find_package(LTDL QUIET)
if(LTDL_FOUND)
    set(HAVE_LIBLTDL 1)
endif()

if(WIN32)
    set(HAVE_WINSOCK2_H 1)
    set(HAVE_LIBWS2_32 1)
endif()

# Installation directories & libpath.h configuration
set(GUILE_BINDIR "${CMAKE_INSTALL_FULL_BINDIR}")
set(GUILE_SBINDIR "${CMAKE_INSTALL_FULL_SBINDIR}")
set(GUILE_LIBEXECDIR "${CMAKE_INSTALL_FULL_LIBEXECDIR}")
set(GUILE_DATADIR "${CMAKE_INSTALL_FULL_DATADIR}")
set(GUILE_SYSCONFDIR "${CMAKE_INSTALL_FULL_SYSCONFDIR}")
set(GUILE_SHAREDSTATEDIR "${CMAKE_INSTALL_FULL_SHAREDSTATEDIR}")
set(GUILE_LOCALSTATEDIR "${CMAKE_INSTALL_FULL_LOCALSTATEDIR}")
set(GUILE_LIBDIR "${CMAKE_INSTALL_FULL_LIBDIR}")
set(GUILE_INFODIR "${CMAKE_INSTALL_FULL_INFODIR}")
set(GUILE_MANDIR "${CMAKE_INSTALL_FULL_MANDIR}")
set(GUILE_INCLUDEDIR "${CMAKE_INSTALL_FULL_INCLUDEDIR}")
set(GUILE_PKGDATA_DIR "${CMAKE_INSTALL_FULL_DATADIR}/guile-texmacs")
set(GUILE_PKGLIB_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/guile-texmacs")
set(GUILE_PKGINCLUDE_DIR "${CMAKE_INSTALL_FULL_INCLUDEDIR}/guile-texmacs")
set(GUILE_LIBRARY_DIR "${GUILE_PKGDATA_DIR}/${GUILE_EFFECTIVE_VERSION}")
set(GUILE_SITE_DIR "${GUILE_PKGDATA_DIR}/site")
set(GUILE_BUILDSTAMP "CMake build")
set(GUILE_LIBS "")
if(NOT BUILD_SHARED_LIBS)
    set(GUILE_LIBS "-lgmp")
    if(LTDL_FOUND)
        string(APPEND GUILE_LIBS " -lltdl")
    endif()
    if(WIN32)
        string(APPEND GUILE_LIBS " -lws2_32")
    endif()
    if(MATH_LIBRARIES)
        string(APPEND GUILE_LIBS " -lm")
    endif()
    if(CMAKE_USE_PTHREADS_INIT)
        string(APPEND GUILE_LIBS " -lpthread")
    endif()
endif()
set(GUILE_CFLAGS "")

# Variables for guile-1.8.pc
set(prefix "${CMAKE_INSTALL_PREFIX}")
set(exec_prefix "${CMAKE_INSTALL_PREFIX}")
set(libdir "${CMAKE_INSTALL_FULL_LIBDIR}")
set(includedir "${CMAKE_INSTALL_FULL_INCLUDEDIR}")
set(datarootdir "${CMAKE_INSTALL_FULL_DATADIR}")
set(datadir "${CMAKE_INSTALL_FULL_DATADIR}")
set(sitedir "${GUILE_SITE_DIR}")

# Generate configuration headers
configure_file(cmake/config.h.cmake
               ${CMAKE_CURRENT_BINARY_DIR}/config.h)

configure_file(libguile/scmconfig.h.in
               ${CMAKE_CURRENT_BINARY_DIR}/libguile/scmconfig.h
               @ONLY)

configure_file(cmake/version.h.cmake
               ${CMAKE_CURRENT_BINARY_DIR}/libguile/version.h
               @ONLY)

configure_file(cmake/libpath.h.cmake
               ${CMAKE_CURRENT_BINARY_DIR}/libguile/libpath.h
               @ONLY)

configure_file(guile-1.8.pc.in
               ${CMAKE_CURRENT_BINARY_DIR}/guile-1.8.pc
               @ONLY)

# Include paths
include_directories(
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}/libguile
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/libguile
    ${CMAKE_CURRENT_SOURCE_DIR}/srfi
)

add_definitions(-DHAVE_CONFIG_H)

# Core libguile sources
set(LIBGUILE_SOURCES
    libguile/alist.c
    libguile/arbiters.c
    libguile/async.c
    libguile/backtrace.c
    libguile/boolean.c
    libguile/chars.c
    libguile/continuations.c
    libguile/convert.c
    libguile/cpp_err_symbols.c
    libguile/cpp_errno.c
    libguile/cpp_sig_symbols.c
    libguile/cpp_signal.c
    libguile/debug.c
    libguile/deprecation.c
    libguile/deprecated.c
    libguile/discouraged.c
    libguile/dynl.c
    libguile/dynwind.c
    libguile/environments.c
    libguile/eq.c
    libguile/error.c
    libguile/eval.c
    libguile/evalext.c
    libguile/extensions.c
    libguile/feature.c
    libguile/fluids.c
    libguile/fports.c
    libguile/futures.c
    libguile/gc.c
    libguile/gc-mark.c
    libguile/gc-segment.c
    libguile/gc-malloc.c
    libguile/gc-card.c
    libguile/gc-freelist.c
    libguile/gc_os_dep.c
    libguile/gdbint.c
    libguile/gh_data.c
    libguile/gh_eval.c
    libguile/gh_funcs.c
    libguile/gh_init.c
    libguile/gh_io.c
    libguile/gh_list.c
    libguile/gh_predicates.c
    libguile/goops.c
    libguile/gsubr.c
    libguile/guardians.c
    libguile/hash.c
    libguile/hashtab.c
    libguile/hooks.c
    libguile/i18n.c
    libguile/init.c
    libguile/inline.c
    libguile/ioext.c
    libguile/keywords.c
    libguile/lang.c
    libguile/list.c
    libguile/load.c
    libguile/macros.c
    libguile/mallocs.c
    libguile/modules.c
    libguile/numbers.c
    libguile/objects.c
    libguile/objprop.c
    libguile/options.c
    libguile/pairs.c
    libguile/ports.c
    libguile/print.c
    libguile/procprop.c
    libguile/procs.c
    libguile/properties.c
    libguile/random.c
    libguile/rdelim.c
    libguile/read.c
    libguile/root.c
    libguile/rw.c
    libguile/scmsigs.c
    libguile/script.c
    libguile/simpos.c
    libguile/smob.c
    libguile/sort.c
    libguile/srcprop.c
    libguile/stackchk.c
    libguile/stacks.c
    libguile/stime.c
    libguile/strings.c
    libguile/srfi-4.c
    libguile/srfi-13.c
    libguile/srfi-14.c
    libguile/strorder.c
    libguile/strports.c
    libguile/struct.c
    libguile/symbols.c
    libguile/threads.c
    libguile/null-threads.c
    libguile/throw.c
    libguile/values.c
    libguile/variable.c
    libguile/vectors.c
    libguile/version.c
    libguile/vports.c
    libguile/weaks.c
    libguile/ramap.c
    libguile/unif.c
    libguile/system.c
)

if(GUILE_ENABLE_POSIX)
    list(APPEND LIBGUILE_SOURCES libguile/filesys.c libguile/posix.c)
endif()

if(GUILE_ENABLE_NETWORKING)
    list(APPEND LIBGUILE_SOURCES libguile/net_db.c libguile/socket.c)
endif()

if(GUILE_ENABLE_REGEX AND HAVE_REGCOMP)
    list(APPEND LIBGUILE_SOURCES libguile/regex-posix.c)
endif()

if(WIN32)
    list(APPEND LIBGUILE_SOURCES libguile/win32-uname.c)
    if(GUILE_ENABLE_NETWORKING)
        list(APPEND LIBGUILE_SOURCES libguile/win32-socket.c)
    endif()
endif()

# Fallback functions if missing in libc
if(NOT HAVE_INET_ATON)
    list(APPEND LIBGUILE_SOURCES libguile/inet_aton.c)
endif()
if(NOT HAVE_MEMMOVE)
    list(APPEND LIBGUILE_SOURCES libguile/memmove.c)
endif()
if(NOT HAVE_PUTENV)
    list(APPEND LIBGUILE_SOURCES libguile/putenv.c)
endif()
if(NOT HAVE_STRERROR)
    list(APPEND LIBGUILE_SOURCES libguile/strerror.c)
endif()
if(NOT HAVE_MKSTEMP)
    list(APPEND LIBGUILE_SOURCES libguile/mkstemp.c)
endif()

# Target: libguile
add_library(guile ${LIBGUILE_SOURCES})
target_link_libraries(guile PUBLIC GMP::GMP Math::Math Threads::Threads)
if(LTDL_FOUND)
    target_link_libraries(guile PUBLIC LTDL::LTDL)
endif()
if(WIN32)
    target_link_libraries(guile PUBLIC ws2_32)
endif()
if(REGEX_LIBRARIES)
    target_link_libraries(guile PUBLIC ${REGEX_LIBRARIES})
endif()

# Target: guile (executable)
add_executable(guile-bin libguile/guile.c)
set_target_properties(guile-bin PROPERTIES OUTPUT_NAME "guile")
target_link_libraries(guile-bin PRIVATE guile)

# SRFI extension libraries
add_library(guile-srfi-srfi-1 srfi/srfi-1.c)
target_link_libraries(guile-srfi-srfi-1 PUBLIC guile)
set_target_properties(guile-srfi-srfi-1 PROPERTIES OUTPUT_NAME "guile-srfi-srfi-1-v-${LIBGUILE_SRFI_SRFI_1_MAJOR}")

add_library(guile-srfi-srfi-4 srfi/srfi-4.c)
target_link_libraries(guile-srfi-srfi-4 PUBLIC guile)
set_target_properties(guile-srfi-srfi-4 PROPERTIES OUTPUT_NAME "guile-srfi-srfi-4-v-${LIBGUILE_SRFI_SRFI_4_MAJOR}")

add_library(guile-srfi-srfi-13-14 srfi/srfi-13.c srfi/srfi-14.c)
target_link_libraries(guile-srfi-srfi-13-14 PUBLIC guile)
set_target_properties(guile-srfi-srfi-13-14 PROPERTIES OUTPUT_NAME "guile-srfi-srfi-13-14-v-${LIBGUILE_SRFI_SRFI_13_14_MAJOR}")

add_library(guile-srfi-srfi-60 srfi/srfi-60.c)
target_link_libraries(guile-srfi-srfi-60 PUBLIC guile)
set_target_properties(guile-srfi-srfi-60 PROPERTIES OUTPUT_NAME "guile-srfi-srfi-60-v-${LIBGUILE_SRFI_SRFI_60_MAJOR}")

# Installation rules
install(TARGETS guile guile-srfi-srfi-1 guile-srfi-srfi-4 guile-srfi-srfi-13-14 guile-srfi-srfi-60
    EXPORT guileTargets
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

install(TARGETS guile-bin
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

# Headers
install(FILES libguile.h
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

install(DIRECTORY libguile/
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/libguile
    FILES_MATCHING PATTERN "*.h"
    PATTERN "gen-scmconfig.h" EXCLUDE
)

install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/libguile/scmconfig.h
    ${CMAKE_CURRENT_BINARY_DIR}/libguile/version.h
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/libguile
)

install(DIRECTORY srfi/
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/guile-texmacs/srfi
    FILES_MATCHING PATTERN "*.h"
)

# Scheme library files
install(DIRECTORY ice-9/
    DESTINATION ${CMAKE_INSTALL_DATADIR}/guile-texmacs/${GUILE_EFFECTIVE_VERSION}/ice-9
    FILES_MATCHING PATTERN "*.scm"
)

install(DIRECTORY oop/
    DESTINATION ${CMAKE_INSTALL_DATADIR}/guile-texmacs/${GUILE_EFFECTIVE_VERSION}/oop
    FILES_MATCHING PATTERN "*.scm"
)

install(DIRECTORY srfi/
    DESTINATION ${CMAKE_INSTALL_DATADIR}/guile-texmacs/${GUILE_EFFECTIVE_VERSION}/srfi
    FILES_MATCHING PATTERN "*.scm"
)

install(DIRECTORY lang/
    DESTINATION ${CMAKE_INSTALL_DATADIR}/guile-texmacs/${GUILE_EFFECTIVE_VERSION}/lang
    FILES_MATCHING PATTERN "*.scm"
)

install(FILES libguile/guile-procedures.txt
    DESTINATION ${CMAKE_INSTALL_DATADIR}/guile-texmacs/${GUILE_EFFECTIVE_VERSION}
)

# Pkg-config
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/guile-1.8.pc
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig
)

# CMake export configuration
install(EXPORT guileTargets
    FILE guileTargets.cmake
    NAMESPACE guile::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/guile
)

include(CMakePackageConfigHelpers)
configure_package_config_file(
    cmake/guileConfig.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/guileConfig.cmake
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/guile
)
write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/guileConfigVersion.cmake
    VERSION ${GUILE_VERSION}
    COMPATIBILITY AnyNewerVersion
)

install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/guileConfig.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/guileConfigVersion.cmake
    cmake/FindGMP.cmake
    cmake/FindLTDL.cmake
    cmake/FindMath.cmake
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/guile
)

