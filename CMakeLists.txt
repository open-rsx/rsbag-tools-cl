# CMakeLists.txt --- CMake build description file.
#
# Copyright (C) 2011, 2012 Jan Moringen
#
# Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
#
# This Program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This Program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses>.

CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

# Extract version components from an approximately "git describe"-like
# formatted version string.
MACRO(GIT_VERSION_COMPONENTS INPUT OUT_MAJOR OUT_MINOR OUT_REVISION OUT_COMMIT)
    SET(REGEX "^[-.a-z0-9]*([0-9]+)\\.([0-9]+)[-.]([0-9]+)-?(g[a-f0-9]+(:?-dirty)?|)$")
    STRING(REGEX REPLACE ${REGEX} "\\1" ${OUT_MAJOR}    "${INPUT}")
    STRING(REGEX REPLACE ${REGEX} "\\2" ${OUT_MINOR}    "${INPUT}")
    STRING(REGEX REPLACE ${REGEX} "\\3" ${OUT_REVISION} "${INPUT}")
    STRING(REGEX REPLACE ${REGEX} "\\4" ${OUT_COMMIT}   "${INPUT}")
ENDMACRO()

# Git-based version.
FIND_PROGRAM(GIT_EXECUTABLE "git")
IF(GIT_EXECUTABLE)
    EXECUTE_PROCESS(COMMAND ${GIT_EXECUTABLE} describe --long --dirty=-dirty
                    RESULT_VARIABLE RESULT
                    OUTPUT_VARIABLE GIT_DESCRIBE_OUTPUT
                    OUTPUT_STRIP_TRAILING_WHITESPACE
                    ERROR_QUIET)
    IF(RESULT EQUAL 0)
        GIT_VERSION_COMPONENTS(${GIT_DESCRIBE_OUTPUT} IGN1 IGN2 REVISION COMMIT)
        FILE(WRITE "${CMAKE_SOURCE_DIR}/version.sexp" "${REVISION} \"${COMMIT}\"")
    ELSE()
        MESSAGE(STATUS "Not a git repository: ${CMAKE_SOURCE_DIR}")
    ENDIF()
ELSE()
    MESSAGE(STATUS "git executable not found")
ENDIF()

# Find and check SBCL installation.
SET(SBCL_HOME $ENV{SBCL_HOME})
IF(NOT SBCL_HOME)
    MESSAGE(STATUS "SBCL_HOME is not set. Trying to find SBCL on default path.")
    FIND_PROGRAM(SBCL_EXECUTABLE sbcl)
ELSE()
    MESSAGE(STATUS "SBCL_HOME is set to ${SBCL_HOME}. Trying to find SBCL there.")
    FIND_PROGRAM(SBCL_EXECUTABLE sbcl
                 PATHS "${SBCL_HOME}/../../bin" ${SBCL_HOME}
                 NO_DEFAULT_PATH)
ENDIF()
MESSAGE(STATUS "SBCL executable: ${SBCL_EXECUTABLE}")

# Configure Lisp environment.
SET(LISP_RUNTIME_OPTIONS "" CACHE STRING
    "Runtime options to pass to the Lisp system.")
SET(LISP_INIT_FILE       "" CACHE FILEPATH
    "File to load as Lisp init file.")
SET(LISP_INIT_CODE       "" CACHE STRING
    "Lisp code to be evaulated during initialization.")
SET(LISP_INIT "")
IF(LISP_INIT_FILE)
    SET(LISP_INIT "${LISP_INIT} --userinit ${LISP_INIT_FILE}")
ENDIF()
IF(LISP_INIT_CODE)
    SET(LISP_INIT "${LISP_INIT} --eval \"${LISP_INIT_CODE}\"")
ENDIF()

SET(CL_SOURCE_REGISTRY       "(:source-registry (:tree \\\"${CMAKE_CURRENT_SOURCE_DIR}\\\") :inherit-configuration)")
SET(ASDF_OUTPUT_TRANSLATIONS "(:output-translations (t (\\\"${CMAKE_CURRENT_BINARY_DIR}/fasl-cache\\\" :implementation)) :ignore-inherited-configuration)")

# Execute Lisp process
MACRO(CONFIGURE_LISP NAME CODE)
    SET(NAME "${NAME}")
    SET(DO   "${CODE} --quit")
    CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/sbcl.cmake.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                   @ONLY)
    SET(REDIRECTIONS)
ENDMACRO()
MACRO(CONFIGURE_LISP_REDIRECT NAME CODE)
    SET(REDIRECTIONS "OUTPUT_FILE \"${CMAKE_CURRENT_BINARY_DIR}/${NAME}.out\"
                      ERROR_FILE  \"${CMAKE_CURRENT_BINARY_DIR}/${NAME}.err\"")
    CONFIGURE_LISP(${NAME} ${CODE})
ENDMACRO()
MACRO(EXECUTE_LISP NAME CODE OUTPUT_VARIABLE)
    CONFIGURE_LISP(${NAME} "${CODE}")
    MESSAGE(STATUS "Executing Lisp process \"${NAME}\"")
    EXECUTE_PROCESS(COMMAND cmake -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                    OUTPUT_VARIABLE ${OUTPUT_VARIABLE}
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
ENDMACRO()

# Project definition (version is grovelled from asd system definition
# and, possibly in combination with version.sexp file written above).
PROJECT(cl-rsbag-tools)
SET(SYSTEM_NAME "cl-rsbag-tools-info")
SET(REDIRECTIONS "ERROR_QUIET")
EXECUTE_LISP("version"
             "--eval \"(asdf:find-system :${SYSTEM_NAME})\"
              --eval \"(princ (${SYSTEM_NAME}-system:version/string :commit? t))\""
             VERSION_FROM_LISP)
GIT_VERSION_COMPONENTS(${VERSION_FROM_LISP} RSBAG_TOOLS_VERSION_MAJOR
                                            RSBAG_TOOLS_VERSION_MINOR
                                            RSBAG_TOOLS_VERSION_PATCH
                                            RSBAG_TOOLS_VERSION_COMMIT)
MESSAGE(STATUS "Project is ${CMAKE_PROJECT_NAME} (${VERSION_FROM_LISP})")

# Create binary.
SET(MAIN_NAME "bag")
CONFIGURE_LISP_REDIRECT("${MAIN_NAME}"
                        "--load \"${CMAKE_CURRENT_SOURCE_DIR}/main/dump.lisp\"")
ADD_CUSTOM_COMMAND(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}"
                   COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${MAIN_NAME}.cmake"
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
                           COMMAND "${CMAKE_COMMAND}" -E create_symlink "${MAIN_NAME}" "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}"
                           DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}"
                           COMMENT "Creating symlink ${TOOL} -> ./${MAIN_NAME}")
    ENDFOREACH()
    ADD_CUSTOM_TARGET(links ALL DEPENDS ${TOOLS})
ENDIF()

# Tests
SET(DATA_DIR "${cl-rsbag-tools_SOURCE_DIR}/test/data/")

ENABLE_TESTING()

# --help, --help-all and --version for all binaries.
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

# All on-error policies for bag-{info,cat,play,record}
SET(FILE "${DATA_DIR}/${RSBAG_TOOLS_VERSION_MAJOR}.${RSBAG_TOOLS_VERSION_MINOR}/empty.tide")
FOREACH(POLICY continue abort)
    ADD_TEST(NAME    "bag-info/on-error=${POLICY}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-info"
                     --debug --on-error "${POLICY}" "${FILE}")
    # bag-cat
    ADD_TEST(NAME    "bag-cat/on-error=${POLICY}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-cat"
                     --debug --on-error "${POLICY}"
                     "${FILE}")
    # bag-play
    ADD_TEST(NAME    "bag-play/on-error=${POLICY}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-play"
                     --debug --on-error "${POLICY}"
                     --replay-strategy as-fast-as-possible
                     "${FILE}" inprocess:)
    # bag-record
    ADD_TEST(NAME    "bag-record/on-error=${POLICY}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-record"
                     --eval "(let ((timer (sb-ext:make-timer (lambda () (sb-ext:exit))))) (sb-ext:schedule-timer timer 3))"
                     --debug --on-error "${POLICY}" --force
                     -o "bag-record-on-error=${POLICY}.tide" inprocess:)
ENDFOREACH()

# bag-{info,cat,play} tests
FOREACH(FILE ${TIDE_FILES} ${ELAN_FILES})
    FILE(RELATIVE_PATH NAME "${DATA_DIR}" "${FILE}")
    IF(FILE MATCHES "(empty-channel\\.tide|unsupported-type\\.tide)")
        SET(CONTINUE --on-error continue)
    ELSE()
        SET(CONTINUE)
    ENDIF()

    # bag-info
    ADD_TEST(NAME    "bag-info${NAME}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-info"
                     --debug ${CONTINUE} "${FILE}")
    FOREACH(RANGE_OPTION ""
                         "--start-index=0" "--start-time=0" "--start-time=-1e-9"
                         "--end-index=0"   "--end-time=0"   "--end-time=-1e-9")
        STRING(REGEX REPLACE "--([-a-z]+)[ =]+(.*)" "\\1/\\2"
               TAG "${RANGE_OPTION}")

        # bag-cat
        ADD_TEST(NAME    "bag-cat${NAME}/${TAG}"
                 COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-cat"
                         --debug ${CONTINUE}
                         ${RANGE_OPTION}
                         "${FILE}")

        # bag-play
        IF(FILE MATCHES "(.*\\.eaf$|empty\\.tide|empty-channel\\.tide|single-event\\.tide)")
            ADD_TEST(NAME    "bag-play${NAME}/${TAG}"
                     COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-play"
                             --debug ${CONTINUE}
                             ${RANGE_OPTION}
                             --replay-strategy as-fast-as-possible
                             "${FILE}" inprocess:)
        ENDIF()
    ENDFOREACH()
ENDFOREACH()

# Installation
SET(BINARY_PREFIX    "")
SET(VERSION_SUFFIX   "cl${RSBAG_TOOLS_VERSION_MAJOR}.${RSBAG_TOOLS_VERSION_MINOR}")
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
                           COMMAND "${CMAKE_COMMAND}" -E create_symlink "${MAIN_BINARY_NAME}" "${CMAKE_CURRENT_BINARY_DIR}/${TOOL_NAME}"
                           DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}"
                                   "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_BINARY_NAME}"
                           COMMENT "Creating symlink ${TOOL_NAME} -> ${MAIN_BINARY_NAME}")
        INSTALL(FILES       "${CMAKE_CURRENT_BINARY_DIR}/${TOOL_NAME}"
                DESTINATION "bin")
    ENDFOREACH()
    ADD_CUSTOM_TARGET(versioned_links ALL DEPENDS ${TOOLS_VERSIONED})
ENDIF()
# Install scripts
INSTALL(DIRECTORY   "scripts"
        DESTINATION "share/rsbag/")

# Packaging
SET(PACKAGE_BASE_NAME           "rsbag-bin-${VERSION_SUFFIX}")
SET(CPACK_PACKAGE_VERSION_MAJOR ${RSBAG_TOOLS_VERSION_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${RSBAG_TOOLS_VERSION_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${RSBAG_TOOLS_VERSION_PATCH})
SET(CPACK_PACKAGE_VERSION       "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
SET(CPACK_PACKAGE_VENDOR        "CoR-Lab, Bielefeld University")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")

INCLUDE("${CMAKE_SOURCE_DIR}/cpack/CPackInclude.cmake")
