CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

PROJECT(cl-rsbag-tools)

# Find and check SBCL installation.
SET(SBCL_HOME $ENV{SBCL_HOME})
IF(NOT SBCL_HOME)
    MESSAGE(FATAL_ERROR "SBCL_HOME is not set. Set it.")
ENDIF()
FIND_PROGRAM(SBCL_EXECUTABLE sbcl
             PATHS "${SBCL_HOME}/../../bin"
	     NO_DEFAULT_PATH)

# Find gzexe
FIND_PROGRAM(GZEXE_EXECUTABLE gzexe)

# Configure ASDF.
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
    ADD_CUSTOM_COMMAND(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${NAME}"
                       COMMAND ${CMAKE_COMMAND} -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
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
