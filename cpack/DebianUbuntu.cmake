set(CPACK_GENERATOR "DEB")

set(CPACK_DEBIAN_PACKAGE_NAME        "${PACKAGE_BASE_NAME}")
set(CPACK_DEBIAN_PACKAGE_VERSION     "${CPACK_PACKAGE_VERSION}${CPACK_PACKAGE_REVISION}")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>")
set(CPACK_DEBIAN_PACKAGE_DESCRIPTION "Tools for recording and replaying RSB events (Common Lisp implementation)
 Currently, the following tools are available
  * record
  * info
  * play
  * cat
  * transform")
set(CPACK_DEBIAN_PACKAGE_PRIORITY    "optional")
set(CPACK_DEBIAN_PACKAGE_SECTION     "net")
#SET(CPACK_DEBIAN_ARCHITECTURE        "${CMAKE_SYSTEM_PROCESSOR}") # Debian uses different names here
set(CPACK_DEBIAN_PACKAGE_DEPENDS     "libc6")
set(CPACK_DEBIAN_PACKAGE_SUGGESTS    "spread (>= 4.0)")

# Generate postinst and prerm hooks
math(EXPR PACKAGE_ALT_PRIORITY "100
                                + (100 * ${RSBAG_TOOLS_VERSION_MAJOR})
                                + (  1 * ${RSBAG_TOOLS_VERSION_MINOR})")

set(POSTINST_SCRIPT      "${CMAKE_CURRENT_BINARY_DIR}/postinst")
set(PRERM_SCRIPT         "${CMAKE_CURRENT_BINARY_DIR}/prerm")
file(WRITE "${POSTINST_SCRIPT}" "#!/bin/sh\n\nset -e\n")
file(WRITE "${PRERM_SCRIPT}"    "#!/bin/sh\n\nset -e\n")

# Uncompress binary. Create symbolic links.
file(APPEND "${POSTINST_SCRIPT}"
            "(                                 \\
               cd /usr/bin/                    \\
               && ./${MAIN_BINARY_NAME} redump \\
             )\n\n
             (                                                      \\
               cd /usr/bin                                          \\
               && ./${MAIN_BINARY_NAME}                             \\
                    create-links \"${BINARY_PREFIX}\" \"${BINARY_SUFFIX}\" \\
             )\n\n")

# Update alternatives.
file(APPEND "${POSTINST_SCRIPT}"
            "update-alternatives --install    \\
               /usr/bin/rsbag                 \\
               rsbag                          \\
               /usr/bin/rsbag${BINARY_SUFFIX} \\
               ${PACKAGE_ALT_PRIORITY}\n\n")
file(APPEND "${PRERM_SCRIPT}"
            "update-alternatives --remove           \\
               rsbag                                \\
               /usr/bin/rsbag${VERSION_SUFFIX}\n\n")
foreach(TOOL ${TOOLS})
    string(REGEX REPLACE "^bag-(.*)$" "\\1" TOOL "${TOOL}")
    file(APPEND "${POSTINST_SCRIPT}"
                "update-alternatives --install                       \\
                   /usr/bin/${BINARY_PREFIX}${TOOL}                  \\
                   ${BINARY_PREFIX}${TOOL}                           \\
                   /usr/bin/${BINARY_PREFIX}${TOOL}${BINARY_SUFFIX} \\
                   ${PACKAGE_ALT_PRIORITY}\n\n")
    file(APPEND "${PRERM_SCRIPT}"
                "update-alternatives --remove                            \\
                   ${BINARY_PREFIX}${TOOL}                               \\
                   /usr/bin/${BINARY_PREFIX}${TOOL}${VERSION_SUFFIX}\n\n")
endforeach()
execute_process(COMMAND chmod 755 "${POSTINST_SCRIPT}" "${PRERM_SCRIPT}")
set(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA "${POSTINST_SCRIPT};${PRERM_SCRIPT}")

# Generate required change log files.
find_program(LSB_EXECUTABLE "lsb_release")
execute_process(COMMAND ${LSB_EXECUTABLE} --short --codename
                OUTPUT_VARIABLE LSB_CODENAME
                OUTPUT_STRIP_TRAILING_WHITESPACE)

execute_process(COMMAND ${GIT_EXECUTABLE}
                        log "--format=%ad  %an  <%ae>%n%n%w(76,8,10)%s%w(76,8,8)%n%n%b%n"
                        --date=short
                COMMAND gzip -9
                OUTPUT_FILE "${CMAKE_BINARY_DIR}/changelog.gz")
execute_process(COMMAND sh -c "echo -n \"sed -e '\" ; for c in $(${GIT_EXECUTABLE} rev-list --all -- \"${CMAKE_CURRENT_LIST_FILE}\") ; do echo -n \"s/$c/$(${GIT_EXECUTABLE} describe --tags $c | sed -re s/[^0-9]*\\([0-9]+\\)\\.\\([0-9]+\\)-\\([0-9]+\\)-.*/\\\\1.\\'\\$\\(\\(\\\\2+1\\)\\)\\'.\\\\3/)/\\;\" ; done ; echo \"'\""
                OUTPUT_VARIABLE COMMIT_TO_VERSION_SED_RULES)
execute_process(COMMAND ${GIT_EXECUTABLE}
                        log "--format=${CPACK_DEBIAN_PACKAGE_NAME} (%H) ${LSB_CODENAME}; urgency=low%n%n%w(76,8,10)%s%w(76,8,8)%n%n%b%n%n%w(200,1,1)-- %an <%ae>  %ad%n"
                        --date=rfc
                        -- "${CMAKE_CURRENT_LIST_FILE}"
                COMMAND sh -c ${COMMIT_TO_VERSION_SED_RULES}
                COMMAND gzip -9
                OUTPUT_FILE "${CMAKE_BINARY_DIR}/changelog.Debian.gz")
install(FILES "${CMAKE_BINARY_DIR}/changelog.gz"
              "${CMAKE_BINARY_DIR}/changelog.Debian.gz"
        DESTINATION "share/doc/${CPACK_DEBIAN_PACKAGE_NAME}")

# Write license file
file(WRITE "${CMAKE_BINARY_DIR}/copyright"
     "Copyright (C) 2011-2012 ${CPACK_DEBIAN_PACKAGE_MAINTAINER}

   This software may be licensed under the terms of the GNU General
   Public License Version 3 (the ``GPL''), or (at your option) any
   later version.

   Software distributed under the License is distributed on an ``AS
   IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
   implied. See the GPL for the specific language governing rights and
   limitations.

   You should have received a copy of the GPL along with this
   program. If not, go to http://www.gnu.org/licenses/gpl.html
   or write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

On Debian systems, the complete text of the GNU General Public License
can be found in `/usr/share/common-licenses/GPL-3'.")
install(FILES "${CMAKE_BINARY_DIR}/copyright"
        DESTINATION "share/doc/${CPACK_DEBIAN_PACKAGE_NAME}")

set(CPACK_PACKAGE_FILE_NAME "${CPACK_DEBIAN_PACKAGE_NAME}-${CPACK_DEBIAN_PACKAGE_VERSION}_${CMAKE_SYSTEM_PROCESSOR}")

message(STATUS "Debian Package: ${CPACK_DEBIAN_PACKAGE_NAME} (${CPACK_DEBIAN_PACKAGE_VERSION}) [${CPACK_PACKAGE_FILE_NAME}.deb]")
