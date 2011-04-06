
# check for one particular file of the sources 
AC_INIT(ocaml-ogg, 0.4.1, savonet-users@lists.sourceforge.net)

VERSION=$PACKAGE_VERSION
AC_MSG_RESULT([configuring $PACKAGE_STRING])

# Add prefix to compilation variables
# if passed
if test "x$prefix" != "xNONE"; then
	CFLAGS="$CFLAGS -I$prefix/include"
	LDFLAGS="$LDFLAGS -L$prefix/lib"
	CPPFLAGS="$CPPFLAGS -I$prefix/include"
fi

AC_CHECK_OCAML_COMPILERS()

AC_PROG_CC()
AC_CHECK_TOOL([AR],[ar],no)
PKG_PROG_PKG_CONFIG()
#PKG_CHECK_MODULES(libogg, ogg, , [AC_MSG_ERROR(libogg not found.)])

# PKG_CHECK_MODULES loses when you need --libs-only-[lL]
if ! $PKG_CONFIG --exists ogg; then
        AC_MSG_ERROR([libogg not found])
fi
libogg_CFLAGS=`$PKG_CONFIG --cflags ogg`
AC_SUBST([libogg_CFLAGS])
libogg_LIBS=`$PKG_CONFIG --libs-only-l ogg`
AC_SUBST([libogg_LIBS])
libogg_LDFLAGS=`$PKG_CONFIG --libs-only-L ogg`
AC_SUBST([libogg_LDFLAGS])

# substitutions to perform
AC_SUBST(VERSION)

# Finally create the Makefile and samples
AC_CONFIG_FILES([Makefile],[chmod a-w Makefile])
AC_CONFIG_FILES([src/META])
AC_CONFIG_FILES([src/Makefile])
AC_OUTPUT
