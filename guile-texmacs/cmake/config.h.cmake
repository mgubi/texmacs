/* config.h.in.  Generated from configure.ac by autoheader.  */


/*GUILE_CONFIGURE_COPYRIGHT*/

/*
  TeXmacs may include another generated config header before this one,
  which can predefine autoconf PACKAGE_* names. Undefine them here so
  the defines emitted by this config header do not trigger redefinition
  warnings.
*/
#ifdef PACKAGE_BUGREPORT
# undef PACKAGE_BUGREPORT
#endif
#ifdef PACKAGE_NAME
# undef PACKAGE_NAME
#endif
#ifdef PACKAGE_STRING
# undef PACKAGE_STRING
#endif
#ifdef PACKAGE_TARNAME
# undef PACKAGE_TARNAME
#endif


/* Define if building universal (internal helper macro) */
#undef AC_APPLE_UNIVERSAL_BUILD

/* Define to 1 if using 'alloca.c'. */
#undef C_ALLOCA

/* Define to the type of elements in the array argument to 'getgroups'.
   Usually this is either 'int' or 'gid_t'. */
#define GETGROUPS_T int

/* Define this if you want to debug scm_must_malloc/realloc/free calls. */
#undef GUILE_DEBUG_MALLOC

/* The imaginary unit (positive square root of -1). */
#define GUILE_I _Complex_I

/* Define to 1 in order to try to use "64" versions of system and library
   calls. */
#cmakedefine GUILE_USE_64_CALLS 1

/* Define to 1 if you have the 'acosh' function. */
#cmakedefine HAVE_ACOSH 1

/* Define to 1 if you have 'alloca', as a function or macro. */
#cmakedefine HAVE_ALLOCA 1

/* Define to 1 if <alloca.h> works. */
#cmakedefine HAVE_ALLOCA_H 1

/* Define to 1 if you have the 'asinh' function. */
#cmakedefine HAVE_ASINH 1

/* Define to 1 if you have the <assert.h> header file. */
#cmakedefine HAVE_ASSERT_H 1

/* Define to 1 if you have the 'atanh' function. */
#cmakedefine HAVE_ATANH 1

/* Define to 1 if you have the 'atexit' function. */
#cmakedefine HAVE_ATEXIT 1

/* Define to 1 if you have the 'bcopy' function. */
#cmakedefine HAVE_BCOPY 1

/* Define to 1 if you have the 'cexp' function. */
#cmakedefine HAVE_CEXP 1

/* Define to 1 if you have the 'chown' function. */
#cmakedefine HAVE_CHOWN 1

/* Define to 1 if you have the 'chroot' function. */
#cmakedefine HAVE_CHROOT 1

/* Define to 1 if you have the 'chsize' function. */
#cmakedefine HAVE_CHSIZE 1

/* Define to 1 if you have the 'clog' function. */
#cmakedefine HAVE_CLOG 1

/* Define to 1 if you have the 'clog10' function. */
#cmakedefine HAVE_CLOG10 1

/* Define to 1 if the system has the type 'complex double'. */
#cmakedefine HAVE_COMPLEX_DOUBLE 1

/* Define to 1 if you have the <complex.h> header file. */
#cmakedefine HAVE_COMPLEX_H 1

/* Define to 1 if you have the 'connect' function. */
#cmakedefine HAVE_CONNECT 1

/* Define to 1 if you have the 'copysign' function. */
#cmakedefine HAVE_COPYSIGN 1

/* Define to 1 if you have the <crt_externs.h> header file. */
#cmakedefine HAVE_CRT_EXTERNS_H 1

/* Define to 1 if you have the `crypt' function. */
#cmakedefine HAVE_CRYPT 1

/* Define to 1 if you have the <crypt.h> header file. */
#cmakedefine HAVE_CRYPT_H 1

/* Define to 1 if you have the 'ctermid' function. */
#cmakedefine HAVE_CTERMID 1

/* Define to 1 if you have the 'cuserid' function. */
#cmakedefine HAVE_CUSERID 1

/* Define to 1 if you have the declaration of 'cuserid', and to 0 if you
   don't. */
#cmakedefine HAVE_DECL_CUSERID 1

/* Define to 1 if you have the declaration of 'flock', and to 0 if you don't.
   */
#cmakedefine HAVE_DECL_FLOCK 1

/* Define to 1 if you have the declaration of 'hstrerror', and to 0 if you
   don't. */
#cmakedefine HAVE_DECL_HSTRERROR 1

/* Define to 1 if you have the declaration of 'sethostname', and to 0 if you
   don't. */
#cmakedefine HAVE_DECL_SETHOSTNAME 1

/* Define to 1 if you have the declaration of 'strncasecmp', and to 0 if you
   don't. */
#cmakedefine HAVE_DECL_STRNCASECMP 1

/* Define to 1 if you have the declaration of 'strptime', and to 0 if you
   don't. */
#cmakedefine HAVE_DECL_STRPTIME 1

/* Define to 1 if you have the declaration of 'tzname', and to 0 if you don't.
   */
#cmakedefine HAVE_DECL_TZNAME 1

/* Define to 1 if you have the declaration of 'unsetenv', and to 0 if you
   don't. */
#cmakedefine HAVE_DECL_UNSETENV 1

/* Define to 1 if you have the declaration of 'vsnprintf', and to 0 if you
   don't. */
#cmakedefine HAVE_DECL_VSNPRINTF 1

/* Define to 1 if you have the 'DINFINITY' function. */
#cmakedefine HAVE_DINFINITY 1

/* Define to 1 if you have the <direct.h> header file. */
#cmakedefine HAVE_DIRECT_H 1

/* Define to 1 if you have the <dirent.h> header file, and it defines 'DIR'.
   */
#cmakedefine HAVE_DIRENT_H 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#cmakedefine HAVE_DLFCN_H 1

/* Define to 1 if you have the 'DQNAN' function. */
#cmakedefine HAVE_DQNAN 1

/* Define to 1 if you have the 'endhostent' function. */
#cmakedefine HAVE_ENDHOSTENT 1

/* Define to 1 if you have the 'endnetent' function. */
#cmakedefine HAVE_ENDNETENT 1

/* Define to 1 if you have the 'endprotoent' function. */
#cmakedefine HAVE_ENDPROTOENT 1

/* Define to 1 if you have the 'endservent' function. */
#cmakedefine HAVE_ENDSERVENT 1

/* Define to 1 if you have the 'fchown' function. */
#cmakedefine HAVE_FCHOWN 1

/* Define to 1 if you have the 'fcntl' function. */
#cmakedefine HAVE_FCNTL 1

/* Define to 1 if you have the <fenv.h> header file. */
#cmakedefine HAVE_FENV_H 1

/* Define to 1 if you have the 'fesetround' function. */
#cmakedefine HAVE_FESETROUND 1

/* Define to 1 if you have the 'finite' function. */
#cmakedefine HAVE_FINITE 1

/* Define to 1 if you have the <floatingpoint.h> header file. */
#cmakedefine HAVE_FLOATINGPOINT_H 1

/* Define to 1 if you have the 'flock' function. */
#cmakedefine HAVE_FLOCK 1

/* Define to 1 if you have the 'fork' function. */
#cmakedefine HAVE_FORK 1

/* Define to 1 if you have the 'ftime' function. */
#cmakedefine HAVE_FTIME 1

/* Define to 1 if you have the 'ftruncate' function. */
#cmakedefine HAVE_FTRUNCATE 1

/* Define to 1 if you have the 'getcwd' function. */
#cmakedefine HAVE_GETCWD 1

/* Define to 1 if you have the 'geteuid' function. */
#cmakedefine HAVE_GETEUID 1

/* Define to 1 if you have the 'getgrent' function. */
#cmakedefine HAVE_GETGRENT 1

/* Define to 1 if you have the 'getgroups' function. */
#cmakedefine HAVE_GETGROUPS 1

/* Define to 1 if you have the 'gethostbyname' function. */
#cmakedefine HAVE_GETHOSTBYNAME 1

/* Define to 1 if you have the 'gethostent' function. */
#cmakedefine HAVE_GETHOSTENT 1

/* Define to 1 if you have the 'gethostname' function. */
#cmakedefine HAVE_GETHOSTNAME 1

/* Define to 1 if you have the 'getitimer' function. */
#cmakedefine HAVE_GETITIMER 1

/* Define to 1 if you have the 'getlogin' function. */
#cmakedefine HAVE_GETLOGIN 1

/* Define to 1 if you have the 'getnetbyaddr' function. */
#cmakedefine HAVE_GETNETBYADDR 1

/* Define to 1 if you have the 'getnetbyname' function. */
#cmakedefine HAVE_GETNETBYNAME 1

/* Define to 1 if you have the 'getnetent' function. */
#cmakedefine HAVE_GETNETENT 1

/* Define to 1 if you have the 'getpass' function. */
#cmakedefine HAVE_GETPASS 1

/* Define to 1 if you have the 'getpgrp' function. */
#cmakedefine HAVE_GETPGRP 1

/* Define to 1 if you have the 'getppid' function. */
#cmakedefine HAVE_GETPPID 1

/* Define to 1 if you have the 'getpriority' function. */
#cmakedefine HAVE_GETPRIORITY 1

/* Define to 1 if you have the 'getprotoent' function. */
#cmakedefine HAVE_GETPROTOENT 1

/* Define to 1 if you have the 'getpwent' function. */
#cmakedefine HAVE_GETPWENT 1

/* Define to 1 if you have the 'getservent' function. */
#cmakedefine HAVE_GETSERVENT 1

/* Define to 1 if you have the 'gettimeofday' function. */
#cmakedefine HAVE_GETTIMEOFDAY 1

/* Define to 1 if you have the 'gmtime_r' function. */
#cmakedefine HAVE_GMTIME_R 1

/* Define to 1 if you have the <grp.h> header file. */
#cmakedefine HAVE_GRP_H 1

/* Define to 1 if you have the 'hstrerror' function. */
#cmakedefine HAVE_HSTRERROR 1

/* Define if h_errno is declared in netdb.h. */
#cmakedefine HAVE_H_ERRNO 1

/* Define to 1 if you have the <ieeefp.h> header file. */
#cmakedefine HAVE_IEEEFP_H 1

/* Define to 1 if you have the 'index' function. */
#cmakedefine HAVE_INDEX 1

/* Define to 1 if you have the 'inet_aton' function. */
#cmakedefine HAVE_INET_ATON 1

/* Define to 1 if you have the 'inet_lnaof' function. */
#cmakedefine HAVE_INET_LNAOF 1

/* Define to 1 if you have the 'inet_makeaddr' function. */
#cmakedefine HAVE_INET_MAKEADDR 1

/* Define to 1 if you have the 'inet_netof' function. */
#cmakedefine HAVE_INET_NETOF 1

/* Define to 1 if you have the 'inet_ntop' function. */
#cmakedefine HAVE_INET_NTOP 1

/* Define to 1 if you have the 'inet_pton' function. */
#cmakedefine HAVE_INET_PTON 1

/* Define to 1 if you have the <inttypes.h> header file. */
#cmakedefine HAVE_INTTYPES_H 1

/* Define to 1 if you have the 'ioctl' function. */
#cmakedefine HAVE_IOCTL 1

/* Define to 1 if you have the <io.h> header file. */
#cmakedefine HAVE_IO_H 1

/* Define if you want support for IPv6. */
#cmakedefine HAVE_IPV6 1

/* Define to 1 if you have the 'isblank' function. */
#cmakedefine HAVE_ISBLANK 1

/* Define to 1 if you have the `isinf' macro or function. */
#cmakedefine HAVE_ISINF 1

/* Define to 1 if you have the `isnan' macro or function. */
#cmakedefine HAVE_ISNAN 1

/* Define to 1 if you have the 'kill' function. */
#cmakedefine HAVE_KILL 1

/* Define to 1 if you have the <libc.h> header file. */
#cmakedefine HAVE_LIBC_H 1

/* Define if you have the __libc_stack_end variable. */
#cmakedefine HAVE_LIBC_STACK_END 1

/* Define to 1 if you have the 'gmp' library (-lgmp). */
#cmakedefine HAVE_LIBGMP 1

/* Define to 1 if you have the 'ltdl' library (-lltdl). */
#cmakedefine HAVE_LIBLTDL 1

/* Define to 1 if you have the 'm' library (-lm). */
#cmakedefine HAVE_LIBM 1

/* Define to 1 if you have the 'nsl' library (-lnsl). */
#cmakedefine HAVE_LIBNSL 1

/* Define to 1 if you have the 'rx' library (-lrx). */
#cmakedefine HAVE_LIBRX 1

/* Define to 1 if you have the 'socket' library (-lsocket). */
#cmakedefine HAVE_LIBSOCKET 1

/* Define to 1 if you have the 'systre' library (-lsystre). */
#cmakedefine HAVE_LIBSYSTRE 1

/* Define to 1 if you have the 'uca' library (-luca). */
#cmakedefine HAVE_LIBUCA 1

/* Define to 1 if you have the 'ws2_32' library (-lws2_32). */
#cmakedefine HAVE_LIBWS2_32 1

/* Define to 1 if you have the <limits.h> header file. */
#cmakedefine HAVE_LIMITS_H 1

/* Define to 1 if you have the 'link' function. */
#cmakedefine HAVE_LINK 1

/* Define to 1 if you have the 'lstat' function. */
#cmakedefine HAVE_LSTAT 1

/* Define to 1 if you have the <machine/fpu.h> header file. */
#cmakedefine HAVE_MACHINE_FPU_H 1

/* Define to 1 if you have the <malloc.h> header file. */
#cmakedefine HAVE_MALLOC_H 1

/* Define to 1 if you have the 'memcpy' function. */
#cmakedefine HAVE_MEMCPY 1

/* Define to 1 if you have the 'memmove' function. */
#cmakedefine HAVE_MEMMOVE 1

/* Define to 1 if you have the <memory.h> header file. */
#cmakedefine HAVE_MEMORY_H 1

/* Define if mingw64 uses ucrt. */
#cmakedefine HAVE_MINGW64_UCRT64 1

/* Define to 1 if you have the <minix/config.h> header file. */
#cmakedefine HAVE_MINIX_CONFIG_H 1

/* Define to 1 if you have the 'mkdir' function. */
#cmakedefine HAVE_MKDIR 1

/* Define to 1 if you have the 'mknod' function. */
#cmakedefine HAVE_MKNOD 1

/* Define to 1 if you have the 'mkstemp' function. */
#cmakedefine HAVE_MKSTEMP 1

/* Define to 1 if you have the <nan.h> header file. */
#cmakedefine HAVE_NAN_H 1

/* Define to 1 if you have the <ndir.h> header file, and it defines 'DIR'. */
#cmakedefine HAVE_NDIR_H 1

/* Define to 1 if you have the <netdb.h> header file. */
#cmakedefine HAVE_NETDB_H 1

/* Define this if you want support for networking in Guile. */
#cmakedefine HAVE_NETWORKING 1

/* Define to 1 if you have the 'nice' function. */
#cmakedefine HAVE_NICE 1

/* Define to 1 if you have the 'on_exit' function. */
#cmakedefine HAVE_ON_EXIT 1

/* Define to 1 if you have the 'pause' function. */
#cmakedefine HAVE_PAUSE 1

/* Define to 1 if you have the 'pipe' function. */
#cmakedefine HAVE_PIPE 1

/* Define this if you want support for POSIX system calls in Guile. */
#cmakedefine HAVE_POSIX 1

/* Define to 1 if you have the <process.h> header file. */
#cmakedefine HAVE_PROCESS_H 1

/* Define if you have POSIX threads libraries and header files. */
#cmakedefine HAVE_PTHREAD 1

/* Define to 1 if you have the 'pthread_attr_getstack' function. */
#cmakedefine HAVE_PTHREAD_ATTR_GETSTACK 1

/* Define to 1 if you have the 'pthread_getattr_np' function. */
#cmakedefine HAVE_PTHREAD_GETATTR_NP 1

/* Define to 1 if you have the 'pthread_get_stackaddr_np' function. */
#cmakedefine HAVE_PTHREAD_GET_STACKADDR_NP 1

/* Define to 1 if you have the <pthread.h> header file. */
#cmakedefine HAVE_PTHREAD_H 1

/* Define to 1 if you have the 'pthread_sigmask' function. */
#cmakedefine HAVE_PTHREAD_SIGMASK 1

/* Define to 1 if you have the 'putenv' function. */
#cmakedefine HAVE_PUTENV 1

/* Define to 1 if you have the <pwd.h> header file. */
#cmakedefine HAVE_PWD_H 1

/* Define to 1 if you have the 'readdir64_r' function. */
#cmakedefine HAVE_READDIR64_R 1

/* Define to 1 if you have the 'readdir_r' function. */
#cmakedefine HAVE_READDIR_R 1

/* Define to 1 if you have the 'readlink' function. */
#cmakedefine HAVE_READLINK 1

/* This is included as part of a workaround for a autoheader bug. */
#cmakedefine HAVE_REGCOMP 1

/* Define to 1 if you have the <regex.h> header file. */
#cmakedefine HAVE_REGEX_H 1

/* Define to 1 if you have the 'rename' function. */
#cmakedefine HAVE_RENAME 1

/* Define to 1 if you have the 'rindex' function. */
#cmakedefine HAVE_RINDEX 1

/* Define to 1 if you have the 'rmdir' function. */
#cmakedefine HAVE_RMDIR 1

/* Define to 1 if you have the <rxposix.h> header file. */
#cmakedefine HAVE_RXPOSIX_H 1

/* Define to 1 if you have the <rx/rxposix.h> header file. */
#cmakedefine HAVE_RX_RXPOSIX_H 1

/* Define to 1 if you have the 'select' function. */
#cmakedefine HAVE_SELECT 1

/* Define to 1 if you have the 'setegid' function. */
#cmakedefine HAVE_SETEGID 1

/* Define to 1 if you have the 'seteuid' function. */
#cmakedefine HAVE_SETEUID 1

/* Define to 1 if you have the 'setgroups' function. */
#cmakedefine HAVE_SETGROUPS 1

/* Define to 1 if you have the 'sethostent' function. */
#cmakedefine HAVE_SETHOSTENT 1

/* Define to 1 if you have the 'sethostname' function. */
#cmakedefine HAVE_SETHOSTNAME 1

/* Define to 1 if you have the 'setitimer' function. */
#cmakedefine HAVE_SETITIMER 1

/* Define to 1 if you have the 'setlocale' function. */
#cmakedefine HAVE_SETLOCALE 1

/* Define to 1 if you have the 'setnetent' function. */
#cmakedefine HAVE_SETNETENT 1

/* Define to 1 if you have the 'setpgid' function. */
#cmakedefine HAVE_SETPGID 1

/* Define to 1 if you have the 'setpriority' function. */
#cmakedefine HAVE_SETPRIORITY 1

/* Define to 1 if you have the 'setprotoent' function. */
#cmakedefine HAVE_SETPROTOENT 1

/* Define to 1 if you have the 'setpwent' function. */
#cmakedefine HAVE_SETPWENT 1

/* Define to 1 if you have the 'setservent' function. */
#cmakedefine HAVE_SETSERVENT 1

/* Define to 1 if you have the 'setsid' function. */
#cmakedefine HAVE_SETSID 1

/* Define to 1 if you have the 'sigaction' function. */
#cmakedefine HAVE_SIGACTION 1

/* Define to 1 if you have the 'siginterrupt' function. */
#cmakedefine HAVE_SIGINTERRUPT 1

/* Define this if your IPv6 has sin6_scope_id in sockaddr_in6 struct. */
#cmakedefine HAVE_SIN6_SCOPE_ID 1

/* Define to 1 if you have the 'sincos' function. */
#cmakedefine HAVE_SINCOS 1

/* Define to 1 if you have the 'socketpair' function. */
#cmakedefine HAVE_SOCKETPAIR 1

/* Define to 1 if you have the 'stat64' function. */
#cmakedefine HAVE_STAT64 1

/* Define to 1 if you have the <stdint.h> header file. */
#cmakedefine HAVE_STDINT_H 1

/* Define to 1 if you have the <stdio.h> header file. */
#cmakedefine HAVE_STDIO_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#cmakedefine HAVE_STDLIB_H 1

/* Define to 1 if you have the 'strchr' function. */
#cmakedefine HAVE_STRCHR 1

/* Define to 1 if you have the 'strcmp' function. */
#cmakedefine HAVE_STRCMP 1

/* Define to 1 if you have the 'strdup' function. */
#cmakedefine HAVE_STRDUP 1

/* Define to 1 if you have the 'strerror' function. */
#cmakedefine HAVE_STRERROR 1

/* Define to 1 if you have the 'strftime' function. */
#cmakedefine HAVE_STRFTIME 1

/* Define to 1 if you have the <strings.h> header file. */
#cmakedefine HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#cmakedefine HAVE_STRING_H 1

/* Define to 1 if you have the 'strncasecmp' function. */
#cmakedefine HAVE_STRNCASECMP 1

/* Define to 1 if you have the 'strptime' function. */
#cmakedefine HAVE_STRPTIME 1

/* Define this if your system defines struct linger, for use with the
   getsockopt and setsockopt system calls. */
#cmakedefine HAVE_STRUCT_LINGER 1

/* Define to 1 if 'sin6_len' is a member of 'struct sockaddr_in6'. */
#cmakedefine HAVE_STRUCT_SOCKADDR_IN6_SIN6_LEN 1

/* Define to 1 if 'sin_len' is a member of 'struct sockaddr'. */
#cmakedefine HAVE_STRUCT_SOCKADDR_SIN_LEN 1

/* Define to 1 if 'st_blksize' is a member of 'struct stat'. */
#cmakedefine HAVE_STRUCT_STAT_ST_BLKSIZE 1

/* Define to 1 if 'st_blocks' is a member of 'struct stat'. */
#cmakedefine HAVE_STRUCT_STAT_ST_BLOCKS 1

/* Define to 1 if 'st_rdev' is a member of 'struct stat'. */
#cmakedefine HAVE_STRUCT_STAT_ST_RDEV 1

/* Define this if your system defines struct timespec via either <time.h> or
   <pthread.h>. */
#cmakedefine HAVE_STRUCT_TIMESPEC 1

/* Define to 1 if 'tm_gmtoff' is a member of 'struct tm'. */
#cmakedefine HAVE_STRUCT_TM_TM_GMTOFF 1

/* Define to 1 if 'tm_zone' is a member of 'struct tm'. */
#cmakedefine HAVE_STRUCT_TM_TM_ZONE 1

/* Define to 1 if you have the 'symlink' function. */
#cmakedefine HAVE_SYMLINK 1

/* Define to 1 if you have the 'sync' function. */
#cmakedefine HAVE_SYNC 1

/* Define to 1 if you have the 'sysconf' function. */
#cmakedefine HAVE_SYSCONF 1

/* Define to 1 if you have the 'system' function. */
#cmakedefine HAVE_SYSTEM 1

/* Define to 1 if you have the <sys/dir.h> header file. */
#cmakedefine HAVE_SYS_DIR_H 1

/* Define to 1 if you have the <sys/file.h> header file. */
#cmakedefine HAVE_SYS_FILE_H 1

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#cmakedefine HAVE_SYS_IOCTL_H 1

/* Define to 1 if you have the <sys/ndir.h> header file, and it defines 'DIR'.
   */
#cmakedefine HAVE_SYS_NDIR_H 1

/* Define to 1 if you have the <sys/param.h> header file. */
#cmakedefine HAVE_SYS_PARAM_H 1

/* Define to 1 if you have the <sys/resource.h> header file. */
#cmakedefine HAVE_SYS_RESOURCE_H 1

/* Define to 1 if you have the <sys/select.h> header file. */
#cmakedefine HAVE_SYS_SELECT_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#cmakedefine HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/stdtypes.h> header file. */
#cmakedefine HAVE_SYS_STDTYPES_H 1

/* Define to 1 if you have the <sys/timeb.h> header file. */
#cmakedefine HAVE_SYS_TIMEB_H 1

/* Define to 1 if you have the <sys/times.h> header file. */
#cmakedefine HAVE_SYS_TIMES_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#cmakedefine HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#cmakedefine HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <sys/utime.h> header file. */
#cmakedefine HAVE_SYS_UTIME_H 1

/* Define to 1 if you have the <sys/utsname.h> header file. */
#cmakedefine HAVE_SYS_UTSNAME_H 1

/* Define to 1 if you have <sys/wait.h> that is POSIX.1 compatible. */
#cmakedefine HAVE_SYS_WAIT_H 1

/* Define to 1 if you have the 'tcgetpgrp' function. */
#cmakedefine HAVE_TCGETPGRP 1

/* Define to 1 if you have the 'tcsetpgrp' function. */
#cmakedefine HAVE_TCSETPGRP 1

/* Define to 1 if you have the 'times' function. */
#cmakedefine HAVE_TIMES 1

/* Define to 1 if you have the <time.h> header file. */
#cmakedefine HAVE_TIME_H 1

/* Define to 1 if your 'struct tm' has 'tm_zone'. Deprecated, use
   'HAVE_STRUCT_TM_TM_ZONE' instead. */
#cmakedefine HAVE_TM_ZONE 1

/* Define to 1 if you have the 'trunc' function. */
#cmakedefine HAVE_TRUNC 1

/* Define to 1 if you have the 'truncate' function. */
#cmakedefine HAVE_TRUNCATE 1

/* Define to 1 if you have the 'ttyname' function. */
#cmakedefine HAVE_TTYNAME 1

/* Define to 1 if you don't have 'tm_zone' but do have the external array
   'tzname'. */
#cmakedefine HAVE_TZNAME 1

/* Define to 1 if you have the 'tzset' function. */
#cmakedefine HAVE_TZSET 1

/* Define if uint32_t typedef is defined when netdb.h is include. */
#cmakedefine HAVE_UINT32_T 1

/* Define to 1 if you have the 'uname' function. */
#cmakedefine HAVE_UNAME 1

/* Define to 1 if you have the <unistd.h> header file. */
#cmakedefine HAVE_UNISTD_H 1

/* Define if the system supports Unix-domain (file-domain) sockets. */
#cmakedefine HAVE_UNIX_DOMAIN_SOCKETS 1

/* Define to 1 if you have the 'unsetenv' function. */
#cmakedefine HAVE_UNSETENV 1

/* Define to 1 if csqrt is bug-free */
#cmakedefine HAVE_USABLE_CSQRT 1

/* Define to 1 if you have the 'usleep' function. */
#cmakedefine HAVE_USLEEP 1

/* Define to 1 if you have the <utime.h> header file. */
#cmakedefine HAVE_UTIME_H 1

/* Define to 1 if you have the 'waitpid' function. */
#cmakedefine HAVE_WAITPID 1

/* Define to 1 if you have the <wchar.h> header file. */
#cmakedefine HAVE_WCHAR_H 1

/* Define if you have the <winsock2.h> header file. */
#cmakedefine HAVE_WINSOCK2_H 1

/* Define to 1 if you have the '_NSGetEnviron' function. */
#cmakedefine HAVE__NSGETENVIRON 1

/* Define to 1 if you have the '_pipe' function. */
#cmakedefine HAVE__PIPE 1

/* Define if the compiler supports __builtin_smulll_overflow */
#cmakedefine HAVE___BUILTIN_SMULLL_OVERFLOW 1

/* Define if the compiler supports __builtin_smull_overflow */
#cmakedefine HAVE___BUILTIN_SMULL_OVERFLOW 1

/* Define this if we should include <libc.h> when we've already included
   <unistd.h>. On some systems, they conflict, and libc.h should be omitted.
   See GUILE_HEADER_LIBC_WITH_UNISTD in aclocal.m4. */
#undef LIBC_H_WITH_UNISTD_H

/* Define if localtime caches the TZ setting. */
#cmakedefine LOCALTIME_CACHE 1

/* Define to the sub-directory where libtool stores uninstalled libraries. */
#undef LT_OBJDIR

/* Define if the operating system supplies sleep without declaring it. */
#undef MISSING_SLEEP_DECL

/* Define if the operating system supplies usleep without declaring it. */
#undef MISSING_USLEEP_DECL

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "@PACKAGE_BUGREPORT@"

/* Define to the full name of this package. */
#define PACKAGE_NAME "@PACKAGE_NAME@"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "@PACKAGE_STRING@"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "@PACKAGE_TARNAME@"

/* Define to the home page for this package. */
#define PACKAGE_URL "@PACKAGE_URL@"

/* Define to the version of this package. */
#define PACKAGE_VERSION "@PACKAGE_VERSION@"

/* Define when pthread_att_get_stack works for the main thread */
#undef PTHREAD_ATTR_GETSTACK_WORKS

/* Define to necessary symbol if this constant uses a non-standard name on
   your system. */
#undef PTHREAD_CREATE_JOINABLE

/* Define as the return type of signal handlers (`int' or `void'). */
#define RETSIGTYPE void

/* Define this if floats are the same size as longs. */
#define SCM_SINGLES 1

/* Define this to control the default warning level for deprecated features.
   */
#define SCM_WARN_DEPRECATED_DEFAULT "summary"

/* The size of 'char', as computed by sizeof. */
#define SIZEOF_CHAR @SIZEOF_CHAR@

/* The size of 'float', as computed by sizeof. */
#define SIZEOF_FLOAT @SIZEOF_FLOAT@

/* The size of 'int', as computed by sizeof. */
#define SIZEOF_INT @SIZEOF_INT@

/* The size of 'intmax_t', as computed by sizeof. */
#define SIZEOF_INTMAX_T @SIZEOF_INTMAX_T@

/* The size of 'intptr_t', as computed by sizeof. */
#define SIZEOF_INTPTR_T @SIZEOF_INTPTR_T@

/* The size of 'long', as computed by sizeof. */
#define SIZEOF_LONG @SIZEOF_LONG@

/* The size of 'long long', as computed by sizeof. */
#define SIZEOF_LONG_LONG @SIZEOF_LONG_LONG@

/* The size of 'off_t', as computed by sizeof. */
#define SIZEOF_OFF_T @SIZEOF_OFF_T@

/* The size of 'ptrdiff_t', as computed by sizeof. */
#define SIZEOF_PTRDIFF_T @SIZEOF_PTRDIFF_T@

/* The size of 'short', as computed by sizeof. */
#define SIZEOF_SHORT @SIZEOF_SHORT@

/* The size of 'size_t', as computed by sizeof. */
#define SIZEOF_SIZE_T @SIZEOF_SIZE_T@

/* The size of 'uintptr_t', as computed by sizeof. */
#define SIZEOF_UINTPTR_T @SIZEOF_UINTPTR_T@

/* The size of 'unsigned char', as computed by sizeof. */
#define SIZEOF_UNSIGNED_CHAR @SIZEOF_UNSIGNED_CHAR@

/* The size of 'unsigned int', as computed by sizeof. */
#define SIZEOF_UNSIGNED_INT @SIZEOF_UNSIGNED_INT@

/* The size of 'unsigned long', as computed by sizeof. */
#define SIZEOF_UNSIGNED_LONG @SIZEOF_UNSIGNED_LONG@

/* The size of 'unsigned long long', as computed by sizeof. */
#define SIZEOF_UNSIGNED_LONG_LONG @SIZEOF_UNSIGNED_LONG_LONG@

/* The size of 'unsigned short', as computed by sizeof. */
#define SIZEOF_UNSIGNED_SHORT @SIZEOF_UNSIGNED_SHORT@

/* The size of 'unsigned __int64', as computed by sizeof. */
#define SIZEOF_UNSIGNED___INT64 @SIZEOF_UNSIGNED___INT64@

/* The size of 'void *', as computed by sizeof. */
#define SIZEOF_VOID_P @SIZEOF_VOID_P@

/* The size of '__int64', as computed by sizeof. */
#define SIZEOF___INT64 @SIZEOF___INT64@

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at runtime.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown */
#define STACK_DIRECTION @STACK_DIRECTION@

/* Define to 1 if all of the C89 standard headers exist (not just the ones
   required in a freestanding environment). This macro is provided for
   backward compatibility; new code need not use it. */
#cmakedefine STDC_HEADERS 1

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. This
   macro is obsolete. */
#cmakedefine TIME_WITH_SYS_TIME 1

/* Define to 1 if your <sys/time.h> declares 'struct tm'. */
#cmakedefine TM_IN_SYS_TIME 1

/* Define if you need additional CPP macros on Win32 platforms. */
#cmakedefine USE_DLL_IMPORT 1

/* Enable extensions on AIX, Interix, z/OS.  */
#ifndef _ALL_SOURCE
# undef _ALL_SOURCE
#endif
/* Enable general extensions on macOS.  */
#ifndef _DARWIN_C_SOURCE
# undef _DARWIN_C_SOURCE
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# undef __EXTENSIONS__
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# undef _GNU_SOURCE
#endif
/* Enable X/Open compliant socket functions that do not require linking
   with -lxnet on HP-UX 11.11.  */
#ifndef _HPUX_ALT_XOPEN_SOCKET_API
# undef _HPUX_ALT_XOPEN_SOCKET_API
#endif
/* Identify the host operating system as Minix.
   This macro does not affect the system headers' behavior.
   A future release of Autoconf may stop defining this macro.  */
#ifndef _MINIX
# undef _MINIX
#endif
/* Enable general extensions on NetBSD.
   Enable NetBSD compatibility extensions on Minix.  */
#ifndef _NETBSD_SOURCE
# undef _NETBSD_SOURCE
#endif
/* Enable OpenBSD compatibility extensions on NetBSD.
   Oddly enough, this does nothing on OpenBSD.  */
#ifndef _OPENBSD_SOURCE
# undef _OPENBSD_SOURCE
#endif
/* Define to 1 if needed for POSIX-compatible behavior.  */
#ifndef _POSIX_SOURCE
# undef _POSIX_SOURCE
#endif
/* Define to 2 if needed for POSIX-compatible behavior.  */
#ifndef _POSIX_1_SOURCE
# undef _POSIX_1_SOURCE
#endif
/* Enable POSIX-compatible threading on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# undef _POSIX_PTHREAD_SEMANTICS
#endif
/* Enable extensions specified by ISO/IEC TS 18661-5:2014.  */
#ifndef __STDC_WANT_IEC_60559_ATTRIBS_EXT__
# undef __STDC_WANT_IEC_60559_ATTRIBS_EXT__
#endif
/* Enable extensions specified by ISO/IEC TS 18661-1:2014.  */
#ifndef __STDC_WANT_IEC_60559_BFP_EXT__
# undef __STDC_WANT_IEC_60559_BFP_EXT__
#endif
/* Enable extensions specified by ISO/IEC TS 18661-2:2015.  */
#ifndef __STDC_WANT_IEC_60559_DFP_EXT__
# undef __STDC_WANT_IEC_60559_DFP_EXT__
#endif
/* Enable extensions specified by C23 Annex F.  */
#ifndef __STDC_WANT_IEC_60559_EXT__
# undef __STDC_WANT_IEC_60559_EXT__
#endif
/* Enable extensions specified by ISO/IEC TS 18661-4:2015.  */
#ifndef __STDC_WANT_IEC_60559_FUNCS_EXT__
# undef __STDC_WANT_IEC_60559_FUNCS_EXT__
#endif
/* Enable extensions specified by C23 Annex H and ISO/IEC TS 18661-3:2015.  */
#ifndef __STDC_WANT_IEC_60559_TYPES_EXT__
# undef __STDC_WANT_IEC_60559_TYPES_EXT__
#endif
/* Enable extensions specified by ISO/IEC TR 24731-2:2010.  */
#ifndef __STDC_WANT_LIB_EXT2__
# undef __STDC_WANT_LIB_EXT2__
#endif
/* Enable extensions specified by ISO/IEC 24747:2009.  */
#ifndef __STDC_WANT_MATH_SPEC_FUNCS__
# undef __STDC_WANT_MATH_SPEC_FUNCS__
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# undef _TANDEM_SOURCE
#endif
/* Enable X/Open extensions.  Define to 500 only if necessary
   to make mbstate_t available.  */
#ifndef _XOPEN_SOURCE
# undef _XOPEN_SOURCE
#endif


/* Define if the system headers declare usleep to return void. */
#undef USLEEP_RETURNS_VOID

/* Define this if <utime.h> doesn't define struct utimbuf unless _POSIX_SOURCE
   is defined. See GUILE_STRUCT_UTIMBUF in aclocal.m4. */
#undef UTIMBUF_NEEDS_POSIX

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
#  undef WORDS_BIGENDIAN
# endif
#endif

/* Define to empty if 'const' does not conform to ANSI C. */
#undef const

/* Define as 'int' if <sys/types.h> doesn't define. */
#cmakedefine gid_t @gid_t@

/* Define to '__inline__' or '__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
#undef inline
#endif

/* Define to 'int' if <sys/types.h> does not define. */
#undef mode_t

/* Define as 'unsigned int' if <stddef.h> doesn't define. */
#undef size_t

/* Define to `int' if <sys/socket.h> does not define. */
#cmakedefine socklen_t @socklen_t@

/* Define as 'int' if <sys/types.h> doesn't define. */
#cmakedefine uid_t @uid_t@

/* Define to empty if the keyword 'volatile' does not work. Warning: valid
   code using 'volatile' can become incorrect without. Disable with care. */
#undef volatile


