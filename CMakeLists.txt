# CMakeLists.txt --- CMake build description file.
#
# Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
#
# Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

cmake_minimum_required(VERSION 2.8)

# Extract version components from an approximately "git describe"-like
# formatted version string.
function(git_version_components INPUT OUT_MAJOR OUT_MINOR OUT_REVISION OUT_COMMIT)
    set(REGEX "^[-.a-z0-9]*([0-9]+)\\.([0-9]+)[-.]([0-9]+)-?(g[a-f0-9]+(:?-dirty)?|)$")
    string(REGEX REPLACE ${REGEX} "\\1" ${OUT_MAJOR}    "${INPUT}")
    string(REGEX REPLACE ${REGEX} "\\2" ${OUT_MINOR}    "${INPUT}")
    string(REGEX REPLACE ${REGEX} "\\3" ${OUT_REVISION} "${INPUT}")
    string(REGEX REPLACE ${REGEX} "\\4" ${OUT_COMMIT}   "${INPUT}")
    set(${OUT_MAJOR}    ${${OUT_MAJOR}}    PARENT_SCOPE)
    set(${OUT_MINOR}    ${${OUT_MINOR}}    PARENT_SCOPE)
    set(${OUT_REVISION} ${${OUT_REVISION}} PARENT_SCOPE)
    set(${OUT_COMMIT}   ${${OUT_COMMIT}}   PARENT_SCOPE)
endfunction()

# Git-based version: run git describe, parse the output and store the
# "revision" and "commit" components into version.sexp.
find_program(GIT_EXECUTABLE "git")
if(GIT_EXECUTABLE)
    function(git_based_version MAJOR MINOR)
        math(EXPR MINOR_MINUS_ONE "${MINOR} - 1")
        set(RELEASE_TAG "release-${MAJOR}.${MINOR_MINUS_ONE}")
        execute_process(COMMAND ${GIT_EXECUTABLE} describe
                                                  --long --dirty=-dirty
                                                  --tags --match "${RELEASE_TAG}"
                        RESULT_VARIABLE RESULT
                        OUTPUT_VARIABLE GIT_DESCRIBE_OUTPUT
                        OUTPUT_STRIP_TRAILING_WHITESPACE
                        ERROR_QUIET)
        if(RESULT EQUAL 0)
            git_version_components(${GIT_DESCRIBE_OUTPUT} IGN1 IGN2 REVISION COMMIT)
            file(WRITE "${CMAKE_SOURCE_DIR}/version.sexp" "${REVISION} \"${COMMIT}\"")
        else()
            message(STATUS "git describe failed (not a git repository?) in ${CMAKE_SOURCE_DIR}")
        endif()
    endfunction()
else()
    message(STATUS "git executable not found")
endif()

# Find and check SBCL installation.
set(SBCL_HOME $ENV{SBCL_HOME})
if(NOT SBCL_HOME)
    message(STATUS "SBCL_HOME is not set. Trying to find SBCL on default path.")
    find_program(SBCL_EXECUTABLE sbcl)
    file(TO_CMAKE_PATH "${SBCL_EXECUTABLE}" SBCL_EXECUTABLE)
else()
    file(TO_CMAKE_PATH "${SBCL_HOME}" SBCL_HOME)
    message(STATUS "SBCL_HOME is set to ${SBCL_HOME}. Trying to find SBCL there.")
    find_program(SBCL_EXECUTABLE sbcl
                 PATHS "${SBCL_HOME}/../../bin" ${SBCL_HOME}
                 NO_DEFAULT_PATH)
endif()
file(TO_CMAKE_PATH "${SBCL_EXECUTABLE}" SBCL_EXECUTABLE)
message(STATUS "SBCL executable: ${SBCL_EXECUTABLE}")

# Configure Lisp environment.
set(LISP_RUNTIME_OPTIONS "" CACHE STRING
    "Runtime options to pass to the Lisp system.")
set(LISP_SOURCE_REGISTRY_ENTRIES "" CACHE STRING
    "Additional entries to place on ASDF source registry.")
set(LISP_FASL_CACHE_DIR "${CMAKE_CURRENT_BINARY_DIR}/fasl-cache"
    CACHE PATH
    "Directory in which the fasl cache should be created.")
set(LISP_INIT_FILE       "" CACHE FILEPATH
    "File to load as Lisp init file.")
set(LISP_INIT_CODE       "" CACHE STRING
    "Lisp code to be evaulated during initialization.")
set(LISP_INIT "")
if(LISP_INIT_FILE)
    set(LISP_INIT "${LISP_INIT} --userinit ${LISP_INIT_FILE}")
endif()
if(LISP_INIT_CODE)
    set(LISP_INIT "${LISP_INIT} --eval \"${LISP_INIT_CODE}\"")
endif()

function(add_silent_load SYSTEM)
    set(LISP_INIT "${LISP_INIT} --eval \"(let* ((*standard-output* (make-broadcast-stream)) (*trace-output* *standard-output*)) (ql:quickload :${SYSTEM}))\"" PARENT_SCOPE)
endfunction()

option(WITH_SSL_SUPPORT "Enable support for HTTPS in client and server." OFF)
if(NOT WITH_SSL_SUPPORT)
    set(LISP_INIT "--eval \"(progn (pushnew :drakma-no-ssl *features*) (pushnew :hunchentoot-no-ssl *features*))\" ${LISP_INIT}")
endif()

option(WITH_ROS_TRANSPORT "Enable support for ROS transport." ON)
if(WITH_ROS_TRANSPORT)
    add_silent_load(rsb-ros)
endif()
option(WITH_YARP_TRANSPORT "Enable support for ROS transport." ON)
if(WITH_YARP_TRANSPORT)
    add_silent_load(rsb-yarp)
endif()
option(WITH_PNG_SUPPORT "Enable support for PNG image payloads." ON)
if(WITH_PNG_SUPPORT)
    add_silent_load(cl-rsb-formatting-png)
endif()

option(WITH_TIDELOG_BACKEND "Enable TIDELog backend." ON)
if(WITH_TIDELOG_BACKEND)
    add_silent_load(rsbag-tidelog)
endif()
option(WITH_ELAN_BACKEND "Enable ELAN backend." ON)
if(WITH_ELAN_BACKEND)
    add_silent_load(rsbag-elan)
endif()

file(TO_CMAKE_PATH "${CMAKE_CURRENT_SOURCE_DIR}" SOURCE_DIR)
set(CL_SOURCE_REGISTRY       "(:source-registry
                                (:tree \\\"${SOURCE_DIR}\\\")
                                ${LISP_SOURCE_REGISTRY_ENTRIES}
                                :inherit-configuration)")
file(TO_CMAKE_PATH "${LISP_FASL_CACHE_DIR}" LISP_FASL_CACHE_DIR)
set(ASDF_OUTPUT_TRANSLATIONS "(:output-translations
                                (t (\\\"${LISP_FASL_CACHE_DIR}\\\" :implementation))
                                :ignore-inherited-configuration)")

# Execute Lisp processes, potentially with output redirection.
function(configure_lisp NAME CODE)
    set(NAME "${NAME}")
    set(DO   "${CODE} --quit")
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/sbcl.cmake.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                   @ONLY)
    set(REDIRECTIONS)
endfunction()
function(configure_lisp_redirect NAME CODE)
    set(REDIRECTIONS "OUTPUT_FILE \"${CMAKE_CURRENT_BINARY_DIR}/${NAME}.out\"
                      ERROR_FILE  \"${CMAKE_CURRENT_BINARY_DIR}/${NAME}.err\"")
    configure_lisp(${NAME} ${CODE})
endfunction()
function(execute_lisp NAME CODE OUTPUT_VARIABLE)
    configure_lisp(${NAME} "${CODE}")
    message(STATUS "Executing Lisp process \"${NAME}\"")
    execute_process(COMMAND cmake -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                    OUTPUT_VARIABLE ${OUTPUT_VARIABLE}
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
    set(${OUTPUT_VARIABLE} ${${OUTPUT_VARIABLE}} PARENT_SCOPE)
endfunction()

# Project definition (version is grovelled from asd system definition,
# possibly in combination with version.sexp file written below).
project(cl-rsbag-tools NONE)
set(SYSTEM_NAME "cl-rsbag-tools-info")

# Determine at least major and minor version components.
set(REDIRECTIONS "ERROR_FILE \"${CMAKE_CURRENT_BINARY_DIR}/version.1.err\"")
execute_lisp("version"
             "--eval \"(asdf:find-system :${SYSTEM_NAME})\"
              --eval \"(princ (${SYSTEM_NAME}-system:version/string :commit? t))\""
             VERSION_FROM_LISP)
git_version_components(${VERSION_FROM_LISP} RSBAG_TOOLS_VERSION_MAJOR
                                            RSBAG_TOOLS_VERSION_MINOR
                                            RSBAG_TOOLS_VERSION_PATCH
                                            RSBAG_TOOLS_VERSION_COMMIT)

# If necessary, try to run git describe and determine remaining
# version components.
if(GIT_EXECUTABLE AND (NOT RSBAG_TOOLS_VERSION_PATCH))
    git_based_version(${RSBAG_TOOLS_VERSION_MAJOR} ${RSBAG_TOOLS_VERSION_MINOR})
    set(REDIRECTIONS "ERROR_FILE \"${CMAKE_CURRENT_BINARY_DIR}/version.2.err\"")
    execute_lisp("version"
                 "--eval \"(asdf:find-system :${SYSTEM_NAME})\"
                  --eval \"(princ (${SYSTEM_NAME}-system:version/string :commit? t))\""
                 VERSION_FROM_LISP)
    git_version_components(${VERSION_FROM_LISP} RSBAG_TOOLS_VERSION_MAJOR
                                                RSBAG_TOOLS_VERSION_MINOR
                                                RSBAG_TOOLS_VERSION_PATCH
                                                RSBAG_TOOLS_VERSION_COMMIT)
endif()
set(VERSION_SUFFIX "${RSBAG_TOOLS_VERSION_MAJOR}.${RSBAG_TOOLS_VERSION_MINOR}")

message(STATUS "Project is ${CMAKE_PROJECT_NAME} (${VERSION_FROM_LISP})")

# Create binary.
set(MAIN_NAME "bag")
set(MAIN_EXECUTABLE "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}${CMAKE_EXECUTABLE_SUFFIX}")
file(GLOB     TOOLS
     RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
     "bag-*")
configure_lisp_redirect("${MAIN_NAME}"
                        "--load \"${CMAKE_CURRENT_SOURCE_DIR}/main/dump.lisp\"")
add_custom_command(OUTPUT  "${MAIN_EXECUTABLE}"
                   COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${MAIN_NAME}.cmake"
                   COMMENT "Creating Lisp image ${MAIN_NAME} (this can take a long time)")
add_custom_target(binary ALL DEPENDS "${MAIN_EXECUTABLE}")

# Create symlinks.
if(NOT CMAKE_SYSTEM_NAME STREQUAL "Windows")
    message(STATUS "Configuring tool ${TOOLS}")
    add_custom_command(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/bag-info"
                       COMMAND "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}" create-links
                       DEPENDS "${MAIN_EXECUTABLE}"
                       COMMENT "Creating symlinks")
    add_custom_target(links ALL DEPENDS bag-info)
endif()

# Tests
set(DATA_DIR "${cl-rsbag-tools_SOURCE_DIR}/test/data/")

enable_testing()

# --help, --help-all and --version for all binaries.
foreach(TOOL ${TOOLS})
    add_test(NAME    "${TOOL}-help"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}" --help)
    add_test(NAME    "${TOOL}-help-all"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}" --help-for=all)
    add_test(NAME    "${TOOL}-version"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}" --version)
endforeach()

file(GLOB VALID_TIDE_FILES       "${DATA_DIR}/?.?/valid-*.tide")
file(GLOB RECOVERABLE_TIDE_FILES "${DATA_DIR}/?.?/recoverable-*.tide")
set(TIDE_FILES ${VALID_TIDE_FILES} ${RECOVERABLE_TIDE_FILES})
file(GLOB VERSIONS               RELATIVE "${DATA_DIR}" "${DATA_DIR}/?.?")
file(GLOB ELAN_FILES             "${DATA_DIR}/elan/*.eaf")
list(SORT VERSIONS)
list(GET VERSIONS -1 CURRENT_SERIALIZATION_VERSION)

# All on-error policies for bag-{info,cat,play,record}
set(FILE "${DATA_DIR}/${CURRENT_SERIALIZATION_VERSION}/valid-empty.tide")
foreach(POLICY continue abort)
    add_test(NAME    "bag-info/on-error=${POLICY}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-info"
                     --debug --on-error "${POLICY}" "${FILE}")
    # bag-cat
    add_test(NAME    "bag-cat/on-error=${POLICY}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-cat"
                     --debug --on-error "${POLICY}"
                     "${FILE}")
    # bag-play
    add_test(NAME    "bag-play/on-error=${POLICY}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-play"
                     --debug --on-error "${POLICY}"
                     --replay-strategy as-fast-as-possible
                     "${FILE}" inprocess:)
    # bag-record
    add_test(NAME    "bag-record/on-error=${POLICY}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-record"
                     --load "${CMAKE_SOURCE_DIR}/test/send-test-events.lisp"
                     --debug --on-error "${POLICY}" --force
                     -o "bag-record-on-error=${POLICY}.tide" inprocess:)
endforeach()

foreach(FLUSH_STRATEGY_OPTION "property-limit :property :length/entries :limit 10"
                              "property-limit :property :length/bytes :limit 10000000"
                              "property-limit :property :time-to-last-write :limit 10.5")
    string(REGEX REPLACE ".*:property ([^ ]*).*" "\\1"
           TAG ${FLUSH_STRATEGY_OPTION})
    string(REGEX REPLACE "[:/]" ""
           TAG ${TAG})
    add_test(NAME    "bag-record/${TAG}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-record"
                     --debug
                     --load "${CMAKE_SOURCE_DIR}/test/send-test-events.lisp"
                     --flush-strategy ${FLUSH_STRATEGY_OPTION}
                     --force
                     -o "output-${TAG}.tide" inprocess:)
endforeach()

foreach(TIMESTAMP "CREATE" "SEND" "RECEIVE" "DELIVER")
    add_test(NAME    "bag-record/timestamp=${TIMESTAMP}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-record"
                     --debug
                     --load "${CMAKE_SOURCE_DIR}/test/send-test-events.lisp"
                     --index-timestamp ${TIMESTAMP}
                     --force
                     -o "output-timetamp=${TIMESTAMP}.tide" inprocess:)
endforeach()

# bag-{info,cat,play} tests
foreach(FILE ${TIDE_FILES} ${ELAN_FILES})
    file(RELATIVE_PATH NAME "${DATA_DIR}" "${FILE}")
    list(FIND VALID_TIDE_FILES ${FILE} TEMP)
    if(TEMP EQUAL -1)
        set(CONTINUE --on-error continue)
    else()
        set(CONTINUE)
    endif()

    # bag-info
    add_test(NAME    "bag-info${NAME}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-info"
                     --debug ${CONTINUE} "${FILE}")
    add_test(NAME    "bag-info${NAME}/compute-size"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-info"
                     --debug ${CONTINUE} --compute-sizes "${FILE}")

    # bag-merge
    string(REGEX REPLACE "/" "" OUTPUT "${NAME}")
    add_test(NAME    "bag-merge${NAME}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-merge"
                     --debug ${CONTINUE} --force -o "${OUTPUT}"
                     "${FILE}")

    # bag-{cat,play}: range options
    foreach(RANGE_OPTION ""
                         "--start-index=0" "--start-time=0"
                         "--end-index=0"   "--end-time=0")
        string(REGEX REPLACE "--([-a-z]+)[ =]+(.*)" "\\1=\\2"
               TAG "range-option:${RANGE_OPTION}")

        # bag-cat
        add_test(NAME    "bag-cat${NAME}/${TAG}"
                 COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-cat"
                         --debug ${CONTINUE}
                         ${RANGE_OPTION}
                         "${FILE}")

        # bag-play
        add_test(NAME    "bag-play${NAME}/${TAG}"
                 COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-play"
                         --debug ${CONTINUE}
                         ${RANGE_OPTION}
                         --replay-strategy as-fast-as-possible
                         "${FILE}" inprocess:)
    endforeach()

    if(FILE MATCHES two-events)
        foreach(RANGE_OPTION "--start-index=-1" "--start-time=-1"
                             "--end-index=-1"   "--end-time=-1")
            string(REGEX REPLACE "--([-a-z]+)[ =]+(.*)" "\\1=\\2"
                   TAG "range-option:${RANGE_OPTION}")

            # bag-cat
            add_test(NAME    "bag-cat${NAME}/${TAG}"
                     COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-cat"
                             --debug ${CONTINUE}
                             ${RANGE_OPTION}
                             "${FILE}")

            # bag-play
            add_test(NAME    "bag-play${NAME}/${TAG}"
                     COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-play"
                             --debug ${CONTINUE}
                             ${RANGE_OPTION}
                             --replay-strategy as-fast-as-possible
                             "${FILE}" inprocess:)
        endforeach()
    endif()

    # bag-{cat,play}: replay strategies
    foreach(STRATEGY_OPTION ""
                            "--replay-strategy=recorded-timing :speed 100"
                            "--replay-strategy=recorded-timing :max-delay .0001"
                            "--replay-strategy=as-fast-as-possible"
                            "--replay-strategy=fixed-rate :rate 1000"
                            "--replay-strategy=fixed-rate :delay .0001")
        string(REGEX REPLACE "--([-a-z]+)[ =]+(.*)" "\\2"
               TEMP "replay-strategy:${STRATEGY_OPTION}")
        string(REGEX REPLACE "[ ]" "_" TAG "${TEMP}")

        # bag-cat
        add_test(NAME    "bag-cat${NAME}/${TAG}"
                 COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-cat"
                         --debug ${CONTINUE}
                         ${STRATEGY_OPTION}
                         "${FILE}")

        # bag-play
        if(NOT (FILE MATCHES "(simple\\.eaf$|recoverable-missing-index\\.tide$)")
           OR STRATEGY_OPTION)
            add_test(NAME    "bag-play${NAME}/${TAG}"
                     COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-play"
                             --debug ${CONTINUE}
                             ${STRATEGY_OPTION}
                             "${FILE}" inprocess:)
        endif()
    endforeach()
endforeach()

# bag-cat formatting styles tests
foreach(STYLE discard meta-data payload detailed
              compact)
    add_test(NAME    "bag-cat/style/${STYLE}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-cat"
                     --debug --style "${STYLE}"
                     "${DATA_DIR}/${CURRENT_SERIALIZATION_VERSION}/valid-single-event.tide")
endforeach()

# Installation
install(CODE "message(STATUS \"Compressing Lisp image ${MAIN_NAME} (this can take a long time)\")
              execute_process(COMMAND \"${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}\"
                                      redump \"${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}\" compress)")

set(BINARY_PREFIX    "")
set(BINARY_SUFFIX    "cl${VERSION_SUFFIX}")
set(MAIN_BINARY_NAME "${BINARY_PREFIX}${MAIN_NAME}${BINARY_SUFFIX}${CMAKE_EXECUTABLE_SUFFIX}")
install(PROGRAMS    "${MAIN_EXECUTABLE}"
        DESTINATION "bin"
        RENAME      "${MAIN_BINARY_NAME}")
# Install scripts
install(DIRECTORY   "scripts"
        DESTINATION "share/rsbag${VERSION_SUFFIX}/")

# Packaging
set(PACKAGE_BASE_NAME           "rsbag-bin-${BINARY_SUFFIX}")
set(CPACK_PACKAGE_VERSION_MAJOR ${RSBAG_TOOLS_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${RSBAG_TOOLS_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${RSBAG_TOOLS_VERSION_PATCH})
set(CPACK_PACKAGE_VERSION       "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
set(CPACK_PACKAGE_VENDOR        "CoR-Lab, Bielefeld University")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")

include("${CMAKE_SOURCE_DIR}/cpack/CPackInclude.cmake")
