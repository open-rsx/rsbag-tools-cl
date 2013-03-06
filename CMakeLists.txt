# CMakeLists.txt --- CMake build description file.
#
# Copyright (C) 2012, 2013 Jan Moringen
#
# Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
#
# This file may be licensed under the terms of of the GNU Lesser
# General Public License Version 3 (the ``LGPL''), or (at your option)
# any later version.
#
# Software distributed under the License is distributed on an ``AS
# IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
# implied. See the LGPL for the specific language governing rights and
# limitations.
#
# You should have received a copy of the LGPL along with this
# program. If not, go to http://www.gnu.org/licenses/lgpl.html or
# write to the Free Software Foundation, Inc., 51 Franklin Street,
# Fifth Floor, Boston, MA 02110-1301, USA.
#
# The development of this software was supported by:
#   CoR-Lab, Research Institute for Cognition and Robotics
#     Bielefeld University

CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

# Extract version components from an approximately "git describe"-like
# formatted version string.
FUNCTION(GIT_VERSION_COMPONENTS INPUT OUT_REVISION)
    SET(REGEX "^[-.a-z0-9]*([0-9]+)\\.([0-9]+)[-.]([0-9]+)-?(g[a-f0-9]+(:?-dirty)?|)$")
    STRING(REGEX REPLACE ${REGEX} "\\3" ${OUT_REVISION} "${INPUT}")
    SET(${OUT_REVISION} ${${OUT_REVISION}} PARENT_SCOPE)
ENDFUNCTION()

# Git-based version: run git describe, parse the output and store the
# "revision" and "commit" components into version.sexp.
FIND_PROGRAM(GIT_EXECUTABLE "git")
IF(GIT_EXECUTABLE)
    FUNCTION(GIT_BASED_VERSION MAJOR MINOR OUTPUT)
        MATH(EXPR MINOR_MINUS_ONE "${MINOR} - 1")
        SET(RELEASE_TAG "release-${MAJOR}.${MINOR_MINUS_ONE}")
        EXECUTE_PROCESS(COMMAND ${GIT_EXECUTABLE} describe
                                                  --long --dirty=-dirty
                                                  --match "${RELEASE_TAG}"
                        RESULT_VARIABLE RESULT
                        OUTPUT_VARIABLE GIT_DESCRIBE_OUTPUT
                        OUTPUT_STRIP_TRAILING_WHITESPACE
                        ERROR_QUIET)
        IF(RESULT EQUAL 0)
            SET(${OUTPUT} ${GIT_DESCRIBE_OUTPUT} PARENT_SCOPE)
        ELSE()
            MESSAGE(STATUS "git describe failed (not a git repository?) in ${CMAKE_SOURCE_DIR}")
        ENDIF()
    ENDFUNCTION()
ELSE()
    MESSAGE(STATUS "git executable not found")
ENDIF()

# Project definition (version is grovelled from asd system definition,
# possibly in combination with git describe).
PROJECT(cl-rsbag-tools)
SET(SYSTEM_NAME "cl-rsbag-tools-info")

# Determine major and minor version components from asd file. Run git
# describe to determine remaining patch component.
FUNCTION(EXTRACT_COMPONENT NAME OUTPUT)
    FILE(STRINGS "${CMAKE_SOURCE_DIR}/${SYSTEM_NAME}.asd" TEMP
         REGEX   "def[a-z]+ +\\+version-${NAME}\\+")
    STRING(REGEX REPLACE ".*\\+version-${NAME}\\+ +([0-9]+).*" "\\1"
           ${OUTPUT} ${TEMP})
    SET(${OUTPUT} ${${OUTPUT}} PARENT_SCOPE)
ENDFUNCTION()
EXTRACT_COMPONENT(major CL_RSBAG_TOOLS_VERSION_MAJOR)
EXTRACT_COMPONENT(minor CL_RSBAG_TOOLS_VERSION_MINOR)
GIT_BASED_VERSION(${CL_RSBAG_TOOLS_VERSION_MAJOR}
                  ${CL_RSBAG_TOOLS_VERSION_MINOR}
                  GIT_DESCRIBE_OUTPUT)
GIT_VERSION_COMPONENTS(${GIT_DESCRIBE_OUTPUT} CL_RSBAG_TOOLS_VERSION_PATCH)

MESSAGE(STATUS "Project is ${CMAKE_PROJECT_NAME} (${CL_RSBAG_TOOLS_VERSION_MAJOR}.${CL_RSBAG_TOOLS_VERSION_MINOR}.${CL_RSBAG_TOOLS_VERSION_PATCH})")

# Find and check SBCL installation.
SET(SBCL_HOME $ENV{SBCL_HOME})
IF(NOT SBCL_HOME)
    MESSAGE(FATAL_ERROR "SBCL_HOME is not set. Set it.")
ENDIF()
FIND_PROGRAM(SBCL_EXECUTABLE sbcl
             PATHS "${SBCL_HOME}/../../bin"
             NO_DEFAULT_PATH)

# Configure Lisp environment.
SET(LISP_RUNTIME_OPTIONS "" CACHE STRING   "Runtime options to pass to the Lisp system.")
SET(LISP_INIT_FILE       "" CACHE FILEPATH "File to load as Lisp init file.")
SET(LISP_INIT_CODE       "" CACHE STRING   "Lisp code to be evaulated during initialization.")
SET(LISP_INIT "")
IF(LISP_INIT_FILE)
    SET(LISP_INIT "${LISP_INIT} --userinit ${LISP_INIT_FILE}")
ENDIF()
IF(LISP_INIT_CODE)
    SET(LISP_INIT "${LISP_INIT} --eval \"${LISP_INIT_CODE}\"")
ENDIF()

SET(CL_SOURCE_REGISTRY       "(:source-registry (:tree \\\"${CMAKE_CURRENT_SOURCE_DIR}\\\") :inherit-configuration)")
SET(ASDF_OUTPUT_TRANSLATIONS "(:output-translations (t (\\\"${CMAKE_CURRENT_BINARY_DIR}/fasl-cache\\\" :implementation)) :ignore-inherited-configuration)")

# Create binary.
SET(SCRIPT    "${CMAKE_CURRENT_SOURCE_DIR}/main/dump.lisp")
SET(MAIN_NAME "bag")
IF(NOT CMAKE_SYSTEM_NAME STREQUAL "Windows")
    SET(REDIRECTIONS "OUTPUT_FILE \"${CMAKE_CURRENT_BINARY_DIR}/${NAME}.log\"
                      ERROR_FILE  \"${CMAKE_CURRENT_BINARY_DIR}/${NAME}.log\"")
ENDIF()
CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/sbcl.cmake.in"
               "${CMAKE_CURRENT_BINARY_DIR}/sbcl.cmake"
               @ONLY)
ADD_CUSTOM_COMMAND(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}"
                   COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.cmake"
                   DEPENDS "${SCRIPT}"
                   COMMENT "Creating Lisp image ${MAIN_NAME} (this can take a long time)")
ADD_CUSTOM_TARGET(binary ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}")

# Create symlinks.
IF(NOT CMAKE_SYSTEM_NAME STREQUAL "Windows")
    FILE(GLOB     TOOLS
         RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
         "bag-*")
    FOREACH(TOOL ${TOOLS})
        MESSAGE(STATUS "Configuring tool ${TOOL}")
        ADD_CUSTOM_COMMAND(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}"
                           COMMAND "${CMAKE_COMMAND}" -E create_symlink "./${MAIN_NAME}" "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}"
                           DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}"
                           COMMENT "Creating symlink ${TOOL} -> ./${MAIN_NAME}")
    ENDFOREACH()
    ADD_CUSTOM_TARGET(links ALL DEPENDS ${TOOLS})
ENDIF()

# Tests
SET(DATA_DIR "${cl-rsbag-tools_SOURCE_DIR}/test/data/")

ENABLE_TESTING()

FOREACH(TOOL ${TOOLS})
    ADD_TEST(NAME    "${TOOL}-help"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}" --help)
    ADD_TEST(NAME    "${TOOL}-help-all"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}" --help-for=all)
    ADD_TEST(NAME    "${TOOL}-version"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}" --version)
ENDFOREACH()

FILE(GLOB TIDE_FILES "${DATA_DIR}/?.?/*.tide")
FILE(GLOB VERSIONS   RELATIVE "${DATA_DIR}" "${DATA_DIR}/?.?")
FILE(GLOB ELAN_FILES "${DATA_DIR}/elan/*.eaf")

# bag-{info,cat,play} tests
FOREACH(FILE ${TIDE_FILES} ${ELAN_FILES})
    FILE(RELATIVE_PATH NAME "${DATA_DIR}" "${FILE}")
    ADD_TEST(NAME    "bag-info${NAME}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-info"
                     --debug "${FILE}")
    FOREACH(RANGE_OPTION ""
                         "--start-index=0" "--start-time=0" "--start-time=-1e-9"
                         "--end-index=0"   "--end-time=0"   "--end-time=-1e-9")
        STRING(REGEX REPLACE "--([-a-z]+)[ =]+(.*)" "\\1/\\2"
               TAG "${RANGE_OPTION}")
        ADD_TEST(NAME    "bag-cat${NAME}/${TAG}"
                 COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-cat"
                         --debug
                         ${RANGE_OPTION}
                         "${FILE}")
        IF(FILE MATCHES "(.*\\.eaf$|empty\\.tide|empty-channel\\.tide|single-event\\.tide)")
            ADD_TEST(NAME    "bag-play${NAME}/${TAG}"
                     COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-play"
                             --debug
                             ${RANGE_OPTION}
                             --replay-strategy as-fast-as-possible
                             "${FILE}" inprocess:)
        ENDIF()
    ENDFOREACH()
ENDFOREACH()

# Installation
SET(BINARY_PREFIX    "")
SET(VERSION_SUFFIX   "cl${CL_RSBAG_TOOLS_VERSION_MAJOR}.${CL_RSBAG_TOOLS_VERSION_MINOR}")
SET(MAIN_BINARY_NAME ${BINARY_PREFIX}${MAIN_NAME}${VERSION_SUFFIX})
INSTALL(PROGRAMS    "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}"
        DESTINATION "bin"
        RENAME      "${MAIN_BINARY_NAME}")
# Fake binary for symlink creation
IF(NOT CMAKE_SYSTEM_NAME STREQUAL "Windows")
    ADD_CUSTOM_COMMAND(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_BINARY_NAME}"
                       COMMAND "${CMAKE_COMMAND}" -E touch "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_BINARY_NAME}"
                       COMMENT "Creating fake link target ${MAIN_BINARY_NAME}")
    SET(TOOLS_VERSIONED "")
    FOREACH(TOOL ${TOOLS})
        SET(TOOL_NAME "${BINARY_PREFIX}${TOOL}${VERSION_SUFFIX}")
        SET(TOOLS_VERSIONED "${CMAKE_CURRENT_BINARY_DIR}/${TOOL_NAME};${TOOLS_VERSIONED}")
        INSTALL(CODE "MESSAGE(\"Creating symlink ${TOOL_NAME} -> ${MAIN_BINARY_NAME}\")")
        ADD_CUSTOM_COMMAND(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/${TOOL_NAME}"
                           COMMAND "${CMAKE_COMMAND}" -E create_symlink "./${MAIN_BINARY_NAME}" "${CMAKE_CURRENT_BINARY_DIR}/${TOOL_NAME}"
                           DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}"
                                   "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_BINARY_NAME}"
                           COMMENT "Creating symlink ${TOOL_NAME} -> ./${MAIN_BINARY_NAME}")
        INSTALL(FILES       "${CMAKE_CURRENT_BINARY_DIR}/${TOOL_NAME}"
                DESTINATION "bin")
    ENDFOREACH()
    ADD_CUSTOM_TARGET(versioned_links ALL DEPENDS ${TOOLS_VERSIONED})
ENDIF()
# Install scripts
INSTALL(DIRECTORY   "scripts"
        DESTINATION "share/rsbag/"
        PATTERN     ".svn" EXCLUDE)

# Packaging
SET(PACKAGE_BASE_NAME           "rsbag-bin-${VERSION_SUFFIX}")
SET(CPACK_PACKAGE_VERSION_MAJOR ${CL_RSBAG_TOOLS_VERSION_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${CL_RSBAG_TOOLS_VERSION_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${CL_RSBAG_TOOLS_VERSION_PATCH})
SET(CPACK_PACKAGE_VERSION       "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
SET(CPACK_PACKAGE_VENDOR        "CoR-Lab, Bielefeld University")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")

INCLUDE("${CMAKE_SOURCE_DIR}/cpack/CPackInclude.cmake")
