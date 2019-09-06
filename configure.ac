AC_INIT([ocaml-ogg],[0.6.0],[savonet-users@lists.sourceforge.net])

VERSION=$PACKAGE_VERSION
AC_MSG_RESULT([configuring $PACKAGE_STRING])

AC_BASE_CHECKS()

PKG_PROG_PKG_CONFIG()
PKG_CONFIG_CHECK_MODULE([ogg])
AC_CHECK_LIB([ogg],[ogg_stream_pageout_fill],[CFLAGS="$CFLAGS -DHAVE_PAGEOUT_FILL"])

AC_CHECK_OCAML_BINDING([bytes],[],[],[1])
INC="$INC `$OCAMLFIND query bytes`"

# substitutions to perform
AC_SUBST(VERSION)
AC_SUBST(INC)

# Finally create the Makefile and samples
AC_CONFIG_FILES([Makefile],[chmod a-w Makefile])
AC_CONFIG_FILES([src/META])
AC_CONFIG_FILES([src/Makefile])
AC_OUTPUT
