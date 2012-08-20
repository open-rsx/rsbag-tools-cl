SET(CPACK_GENERATOR "DEB")

SET(CPACK_PACKAGE_FILE_NAME "${PACKAGE_BASE_NAME}-${CPACK_PACKAGE_VERSION}${CPACK_PACKAGE_REVISION}_${CMAKE_SYSTEM_PROCESSOR}")

SET(CPACK_DEBIAN_PACKAGE_NAME        "${PACKAGE_BASE_NAME}")
SET(CPACK_DEBIAN_PACKAGE_VERSION     "${CPACK_PACKAGE_VERSION}${CPACK_PACKAGE_REVISION}")
SET(CPACK_DEBIAN_PACKAGE_MAINTAINER  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>")
SET(CPACK_DEBIAN_PACKAGE_DESCRIPTION "Tools for recording, manipulating and replaying RSB events (Common Lisp implementation)
 Currently, the following tools are available
  * bag-record
  * bag-info
  * bag-play
  * bag-cat
  * bag-merge")
SET(CPACK_DEBIAN_PACKAGE_PRIORITY    "optional")
SET(CPACK_DEBIAN_PACKAGE_SECTION     "net")
SET(CPACK_DEBIAN_ARCHITECTURE        "${CMAKE_SYSTEM_PROCESSOR}")
SET(CPACK_DEBIAN_PACKAGE_DEPENDS     "libc6")
SET(CPACK_DEBIAN_PACKAGE_RECOMMENDS  "spread (>= 4.0)")

# Generate postinst and prerm hooks
SET(PACKAGE_ALT_PRIORITY "100")

SET(POSTINST_SCRIPT      "${CMAKE_CURRENT_BINARY_DIR}/postinst")
SET(PRERM_SCRIPT         "${CMAKE_CURRENT_BINARY_DIR}/prerm")
FILE(WRITE "${POSTINST_SCRIPT}" "#!/bin/sh\n\nset -e\n")
FILE(WRITE "${PRERM_SCRIPT}"    "#!/bin/sh\n\nset -e\n")
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
EXECUTE_PROCESS(COMMAND chmod 755 "${POSTINST_SCRIPT}" "${PRERM_SCRIPT}")
SET(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA "${POSTINST_SCRIPT};${PRERM_SCRIPT}")

MESSAGE(STATUS "Debian Package: ${CPACK_DEBIAN_PACKAGE_NAME} (${CPACK_DEBIAN_PACKAGE_VERSION}) [${CPACK_PACKAGE_FILE_NAME}.deb]")
