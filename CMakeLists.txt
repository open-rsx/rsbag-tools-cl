CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

# Project definition (version is grovelled from an asd system definition)
PROJECT(cl-rsbag-tools)
SET(SYSTEM_NAME "cl-rsbag-tools-info")
MACRO(EXTRACT_COMPONENT NAME OUTPUT)
    FILE(STRINGS "${CMAKE_SOURCE_DIR}/${SYSTEM_NAME}.asd" TEMP
         REGEX   "defconstant +\\+version-${NAME}\\+")
    STRING(REGEX REPLACE ".*\\+version-${NAME}\\+ +([0-9]+).*" "\\1"
           CL_RSBAG_TOOLS_VERSION_${OUTPUT} ${TEMP})
ENDMACRO()
EXTRACT_COMPONENT(major    MAJOR)
EXTRACT_COMPONENT(minor    MINOR)
EXTRACT_COMPONENT(revision PATCH)

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
    SET(LISP_INIT "${LISP_INIT} --eval '${LISP_INIT_CODE}'")
ENDIF()

SET(CL_SOURCE_REGISTRY "${CMAKE_CURRENT_SOURCE_DIR}//:")
SET(ASDF_OUTPUT_TRANSLATIONS "(:output-translations (t (\\\"${CMAKE_CURRENT_BINARY_DIR}/fasl-cache\\\" :implementation)) :ignore-inherited-configuration)")

# Create binary.
SET(SCRIPT    "${CMAKE_CURRENT_SOURCE_DIR}/main/dump.lisp")
SET(MAIN_NAME "bag")
CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/sbcl.cmake.in"
               "${CMAKE_CURRENT_BINARY_DIR}/sbcl.cmake"
               @ONLY)
ADD_CUSTOM_COMMAND(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}"
                   COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.cmake"
                   DEPENDS "${SCRIPT}"
                   COMMENT "Creating Lisp image ${MAIN_NAME} (this can take a long time)")
ADD_CUSTOM_TARGET(binary ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}")

# Create symlinks.
FILE(GLOB     TOOLS
     RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
     "bag-*")
FOREACH(TOOL ${TOOLS})
    ADD_CUSTOM_COMMAND(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}"
                       COMMAND "${CMAKE_COMMAND}" -E create_symlink "./${MAIN_NAME}" "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}"
                       DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}"
                       COMMENT "Creating symlink ${TOOL} -> ./${MAIN_NAME}")
ENDFOREACH()
ADD_CUSTOM_TARGET(links ALL DEPENDS ${TOOLS})

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

FILE(GLOB TIDE_FILES
     "${DATA_DIR}/*.tide")

FOREACH(TIDE_FILE ${TIDE_FILES})
    FILE(RELATIVE_PATH NAME "${DATA_DIR}" "${TIDE_FILE}")
    ADD_TEST(NAME    "bag-info${NAME}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-info"
                     "${TIDE_FILE}")
ENDFOREACH()

FOREACH(TIDE_FILE ${TIDE_FILES})
    FILE(RELATIVE_PATH NAME "${DATA_DIR}" "${TIDE_FILE}")
    ADD_TEST(NAME    "bag-cat${NAME}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-cat"
                     "${TIDE_FILE}")
ENDFOREACH()

FOREACH(TIDE_FILE "${DATA_DIR}/empty.tide"
                  "${DATA_DIR}/empty-channel.tide"
                  "${DATA_DIR}/single-event.tide")
    FILE(RELATIVE_PATH NAME "${DATA_DIR}" "${TIDE_FILE}")
    ADD_TEST(NAME    "bag-play${NAME}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/bag-play"
                     "${TIDE_FILE}" inprocess:)
ENDFOREACH()

# Installation
SET(BINARY_PREFIX    "")
SET(VERSION_SUFFIX   "cl${CL_RSBAG_TOOLS_VERSION_MAJOR}.${CL_RSBAG_TOOLS_VERSION_MINOR}")
SET(MAIN_BINARY_NAME ${BINARY_PREFIX}${MAIN_NAME}${VERSION_SUFFIX})
INSTALL(PROGRAMS    "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}"
        DESTINATION "bin"
        RENAME      "${MAIN_BINARY_NAME}")
# Fake binary for symlink creation
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

# Packaging
SET(CPACK_PACKAGE_VERSION_MAJOR ${CL_RSBAG_TOOLS_VERSION_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${CL_RSBAG_TOOLS_VERSION_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${CL_RSBAG_TOOLS_VERSION_PATCH})
SET(PACKAGE_BASE_NAME           "rsbag-bin-${VERSION_SUFFIX}")
SET(CPACK_PACKAGE_VERSION       "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
SET(CPACK_PACKAGE_VENDOR        "CoR-Lab, Bielefeld University")
SET(PACKAGE_ALT_PRIORITY        "100")

IF(NOT CPACK_GENERATOR)
    IF(CMAKE_SYSTEM_NAME STREQUAL "Linux")
        LIST(APPEND CPACK_GENERATOR "DEB")
    ENDIF()
ENDIF()

IF(CMAKE_SYSTEM_NAME STREQUAL "Linux")

    FIND_PROGRAM(LSB_RELEASE_EXECUTABLE lsb_release)
    SET(LSB_PROCESSOR_ARCH ${CMAKE_SYSTEM_PROCESSOR})
    EXECUTE_PROCESS(COMMAND         ${LSB_RELEASE_EXECUTABLE} -s -c
                    OUTPUT_VARIABLE TMP_LSB_CODENAME
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
    STRING(TOLOWER ${TMP_LSB_CODENAME} LSB_CODENAME)
    EXECUTE_PROCESS(COMMAND         ${LSB_RELEASE_EXECUTABLE} -s -r
                    OUTPUT_VARIABLE TMP_LSB_RELEASE
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
    STRING(TOLOWER ${TMP_LSB_RELEASE} LSB_RELEASE)
    EXECUTE_PROCESS(COMMAND         ${LSB_RELEASE_EXECUTABLE} -s -i
                    OUTPUT_VARIABLE TMP_LSB_DISTRIBUTOR_ID
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
    STRING(TOLOWER ${TMP_LSB_DISTRIBUTOR_ID} LSB_DISTRIBUTOR_ID)

    SET(CPACK_PACKAGE_FILE_NAME     "${PACKAGE_BASE_NAME}-${CPACK_PACKAGE_VERSION}_${LSB_CODENAME}_${LSB_PROCESSOR_ARCH}")
    SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")

    IF(LSB_DISTRIBUTOR_ID STREQUAL "ubuntu" OR LSB_DISTRIBUTOR_ID STREQUAL "debian")
        # Generate postinst and prerm hooks
        SET(POSTINST_SCRIPT "${CMAKE_CURRENT_BINARY_DIR}/postinst")
        SET(PRERM_SCRIPT    "${CMAKE_CURRENT_BINARY_DIR}/prerm")
        FILE(WRITE "${POSTINST_SCRIPT}" "#!/bin/sh\n\n")
        FILE(WRITE "${PRERM_SCRIPT}"    "#!/bin/sh\n\n")
        FOREACH(TOOL ${TOOLS})
            FILE(APPEND "${POSTINST_SCRIPT}"
                        "update-alternatives --install                       \\
                           /usr/bin/${BINARY_PREFIX}${TOOL}                  \\
                           ${BINARY_PREFIX}${TOOL}                           \\
                           /usr/bin/${BINARY_PREFIX}${TOOL}${VERSION_SUFFIX} \\
                           ${PACKAGE_ALT_PRIORITY}\n\n")
            FILE(APPEND "${PRERM_SCRIPT}"
                        "update-alternatives --remove                            \\
                           ${BINARY_PREFIX}${TOOL}                               \\
                           /usr/bin/${BINARY_PREFIX}${TOOL}${VERSION_SUFFIX}\n\n")
        ENDFOREACH()
        EXECUTE_PROCESS(COMMAND chmod +x "${POSTINST_SCRIPT}" "${PRERM_SCRIPT}")

        SET(CPACK_DEBIAN_PACKAGE_NAME        "${PACKAGE_BASE_NAME}")
        SET(CPACK_DEBIAN_PACKAGE_VERSION     "${CPACK_PACKAGE_VERSION}")
        SET(CPACK_DEBIAN_PACKAGE_MAINTAINER  "Jan Moringen (jmoringe@techfak.uni-bielefeld.de)")
        SET(CPACK_DEBIAN_PACKAGE_DESCRIPTION "Tools for recording, manipulating and replaying RSB events (Common Lisp implementation)")
        SET(CPACK_DEBIAN_PACKAGE_PRIORITY    "optional")
        SET(CPACK_DEBIAN_PACKAGE_SECTION     "net")
        SET(CPACK_DEBIAN_ARCHITECTURE        "${CMAKE_SYSTEM_PROCESSOR}")
        SET(CPACK_DEBIAN_PACKAGE_DEPENDS     "libc6")
        SET(CPACK_DEBIAN_PACKAGE_RECOMMENDS  "spread (>= 4.0)")

        SET(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA "${POSTINST_SCRIPT};${PRERM_SCRIPT}")

        MESSAGE(STATUS "Debian Package: ${CPACK_DEBIAN_PACKAGE_NAME} (${CPACK_DEBIAN_PACKAGE_VERSION}) [${CPACK_PACKAGE_FILE_NAME}.deb]")
    ENDIF()

    INCLUDE(CPack)
ENDIF()
