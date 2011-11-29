CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

# Project definition (version is grovelled from an asd system definition)
PROJECT(cl-rsbag-tools)
SET(VERSION_SYSTEM_NAME cl-rsbag-tools-info)
EXECUTE_PROCESS(COMMAND         sh -c "cat ${CMAKE_SOURCE_DIR}/${VERSION_SYSTEM_NAME}.asd   \\
                                         | grep -e 'defconstant +\\+version-major\\+'       \\
                                         | sed -re 's/.*\\+version-major\\+ +([0-9]+)/\\1/'"
                OUTPUT_VARIABLE CL_RSBAG_TOOLS_VERSION_MAJOR
		OUTPUT_STRIP_TRAILING_WHITESPACE)
EXECUTE_PROCESS(COMMAND         sh -c "cat ${CMAKE_SOURCE_DIR}/${VERSION_SYSTEM_NAME}.asd   \\
                                         | grep -e 'defconstant +\\+version-minor\\+'       \\
                                         | sed -re 's/.*\\+version-minor\\+ +([0-9]+)/\\1/'"
                OUTPUT_VARIABLE CL_RSBAG_TOOLS_VERSION_MINOR
		OUTPUT_STRIP_TRAILING_WHITESPACE)
EXECUTE_PROCESS(COMMAND         sh -c "cat ${CMAKE_SOURCE_DIR}/${VERSION_SYSTEM_NAME}.asd      \\
                                         | grep -e 'defconstant +\\+version-revision\\+'       \\
                                         | sed -re 's/.*\\+version-revision\\+ +([0-9]+)/\\1/'"
                OUTPUT_VARIABLE CL_RSBAG_TOOLS_VERSION_PATCH
		OUTPUT_STRIP_TRAILING_WHITESPACE)

# Find and check SBCL installation.
SET(SBCL_HOME $ENV{SBCL_HOME})
IF(NOT SBCL_HOME)
    MESSAGE(FATAL_ERROR "SBCL_HOME is not set. Set it.")
ENDIF()
FIND_PROGRAM(SBCL_EXECUTABLE sbcl
             PATHS "${SBCL_HOME}/../../bin"
	     NO_DEFAULT_PATH)

# Find gzexe
IF(NOT CMAKE_HOST_APPLE)
    FIND_PROGRAM(GZEXE_EXECUTABLE gzexe)
ENDIF()

# Configure Lisp environment.
SET(LISP_INIT_FILE "" CACHE FILEPATH "File to load as Lisp init file.")
SET(LISP_INIT "")
IF(LISP_INIT_FILE)
    SET(LISP_INIT "${LISP_INIT} --userinit ${LISP_INIT_FILE}")
ENDIF()

SET(CL_SOURCE_REGISTRY "${CMAKE_CURRENT_SOURCE_DIR}//:")
SET(ASDF_OUTPUT_TRANSLATIONS "(:output-translations (t (\\\"${CMAKE_CURRENT_BINARY_DIR}/fasl-cache\\\" :implementation)) :ignore-inherited-configuration)")

# Create images.
FILE(GLOB SCRIPTS "*/dump.lisp")
SET(IMAGES)
FOREACH(SCRIPT ${SCRIPTS})
    STRING(REPLACE "//dump.lisp" "" IMAGE ${SCRIPT})
    GET_FILENAME_COMPONENT(NAME ${IMAGE} NAME)
    CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/sbcl.cmake.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                   @ONLY)
    ADD_CUSTOM_COMMAND(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/${NAME}"
                       COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                       DEPENDS ${SCRIPT}
                       COMMENT "Creating Lisp image ${NAME} (this can take a long time)")
    LIST(APPEND IMAGES "${CMAKE_CURRENT_BINARY_DIR}/${NAME}")
ENDFOREACH()
ADD_CUSTOM_TARGET(scripts ALL DEPENDS ${IMAGES})

# Tests
SET(DATA_DIR "${cl-rsbag-tools_SOURCE_DIR}/test/data/")

ENABLE_TESTING()

FOREACH(IMAGE ${IMAGES})
    FILE(RELATIVE_PATH NAME "${CMAKE_CURRENT_BINARY_DIR}" "${IMAGE}")
    ADD_TEST(NAME    "${NAME}-help"
             COMMAND "${IMAGE}" --help)
    ADD_TEST(NAME    "${NAME}-help-all"
             COMMAND "${IMAGE}" --help-for=all)
    ADD_TEST(NAME    "${NAME}-version"
             COMMAND "${IMAGE}" --version)
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
SET(BINARY_PREFIX  "")
SET(VERSION_SUFFIX "cl${CL_RSBAG_TOOLS_VERSION_MAJOR}.${CL_RSBAG_TOOLS_VERSION_MINOR}")
FOREACH(IMAGE ${IMAGES})
    FILE(RELATIVE_PATH NAME "${CMAKE_CURRENT_BINARY_DIR}" "${IMAGE}")
    INSTALL(PROGRAMS    "${IMAGE}"
            DESTINATION "bin"
	    RENAME      "${BINARY_PREFIX}${NAME}${VERSION_SUFFIX}")
ENDFOREACH()

# Packaging
SET(CPACK_PACKAGE_VERSION_MAJOR ${CL_RSBAG_TOOLS_VERSION_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${CL_RSBAG_TOOLS_VERSION_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${CL_RSBAG_TOOLS_VERSION_PATCH})
SET(PACKAGE_BASE_NAME      "rsbag-bin-${VERSION_SUFFIX}")
SET(CPACK_PACKAGE_VERSION  "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
SET(CPACK_PACKAGE_VENDOR   "CoR-Lab, Bielefeld University")
SET(PACKAGE_ALT_PRIORITY   "90")

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
        FOREACH(IMAGE ${IMAGES})
            FILE(RELATIVE_PATH NAME "${CMAKE_CURRENT_BINARY_DIR}" "${IMAGE}")
            FILE(APPEND "${POSTINST_SCRIPT}"
                        "update-alternatives --install                       \\
                           /usr/bin/${BINARY_PREFIX}${NAME}                  \\
                           ${BINARY_PREFIX}${NAME}                           \\
                           /usr/bin/${BINARY_PREFIX}${NAME}${VERSION_SUFFIX} \\
                           ${PACKAGE_ALT_PRIORITY}\n\n")
            FILE(APPEND "${PRERM_SCRIPT}"
                        "update-alternatives --remove                            \\
                           ${BINARY_PREFIX}${NAME}                               \\
                           /usr/bin/${BINARY_PREFIX}${NAME}${VERSION_SUFFIX}\n\n")
        ENDFOREACH()
        EXECUTE_PROCESS(COMMAND chmod +x "${POSTINST_SCRIPT}" "${PRERM_SCRIPT}")

        SET(CPACK_GENERATOR                  "DEB")
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
