CMAKE_MINIMUM_REQUIRED(VERSION 2.6)

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
