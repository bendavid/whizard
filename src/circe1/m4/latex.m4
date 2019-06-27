dnl latex.m4 -- checks for LaTeX programs
dnl

AC_DEFUN([AC_PROG_TEX], [dnl
AC_CHECK_PROGS(PLAINTEX,[tex],no)
AM_CONDITIONAL([TEX_AVAILABLE], [test "$PLAINTEX" != "no"])
AC_SUBST(PLAINTEX)
])

AC_DEFUN([AC_PROG_LATEX], [dnl
AC_CHECK_PROGS(LATEX,[latex elatex lambda],no)
AM_CONDITIONAL([LATEX_AVAILABLE], [test "$LATEX" != "no"])
AC_SUBST(LATEX)
])


AC_DEFUN([_AC_LATEX_TEST], [dnl
AC_REQUIRE([AC_PROG_LATEX])
rm -rf .tmps_latex
mkdir .tmps_latex
cd .tmps_latex
ifelse($#,2,[
$2="no"; export $2;
cat &gt; testconf.tex &lt;&lt; \EOF
$1
EOF
],$#,3,[
echo "\\documentclass{$3}" &gt; testconf.tex
cat &gt;&gt; testconf.tex &lt;&lt; \EOF
$1
EOF
],$#,4,[
echo "\\documentclass{$3}" &gt; testconf.tex
echo "\\usepackage{$4}" &gt; testconf.tex
cat &gt;&gt; testconf.tex &lt;&lt; \EOF
$1
])
cat testconf.tex | $latex 2&gt;&amp;1 1&gt;/dev/null &amp;&amp; $2=yes; export $2;
cd ..
rm -rf .tmps_latex
])

dnl AC_LATEX_CLASSES([book],book)
dnl should set $book="yes"
dnl 
dnl AC_LATEX_CLASSES(allo,book)
dnl should set $book="no"


AC_DEFUN([AC_LATEX_CLASS], [dnl
AC_CACHE_CHECK([for class $1],[ac_cv_latex_class_]translit($1,[-],[_]),[
_AC_LATEX_TEST([
\begin{document}
\end{document}
],[ac_cv_latex_class_]translit($1,[-],[_]),$1)
])
$2=$[ac_cv_latex_class_]translit($1,[-],[_]) ; export $2;
AC_SUBST($2)
])

dnl Checking for dvips

AC_DEFUN([AC_PROG_DVIPS], [dnl
AC_CHECK_PROGS(DVIPS,dvips,no)
AM_CONDITIONAL([DVIPS_AVAILABLE], [test "$DVIPS" != "no"])
AC_SUBST(DVIPS)
])


dnl Checking for ps2pdf and friends

AC_DEFUN([AC_PROG_PS2PDF], [dnl
AC_CHECK_PROGS(PS2PDF,[ps2pdf14 ps2pdf13 ps2pdf12 ps2pdf],no)
AM_CONDITIONAL([PS2PDF_AVAILABLE], [test "$PS2PDF" != "no"])
AC_SUBST(PS2PDF)
])

dnl Checking for epstopdf 

AC_DEFUN([AC_PROG_EPSTOPDF], [dnl
AC_CHECK_PROGS(EPSTOPDF,[epstopdf],no)
AM_CONDITIONAL([EPSTOPDF_AVAILABLE], [test "$EPSTOPDF" != "no"])
AC_SUBST(EPSTOPDF)
])


dnl Checking for supp-pdf.tex (auxiliary for PDF output)

AC_DEFUN([AC_PROG_SUPP_PDF], [dnl
AC_REQUIRE([AC_PROG_TEX])
AC_CACHE_CHECK([for supp-pdf.tex],
[wo_cv_supp_pdf_exists],
[dnl
wo_cv_supp_pdf_exists="no"
if test "$PLAINTEX" != "no"; then
  wo_cmd='echo \\input supp-pdf.tex \\end > conftest.tex'
  eval "$wo_cmd"
  wo_cmd='$PLAINTEX conftest.tex >&5'
  (eval "$wo_cmd") 2>&5 && wo_cv_supp_pdf_exists="yes" 
fi])
AM_CONDITIONAL([SUPP_PDF_AVAILABLE], [test "$wo_cv_supp_pdf_exists" != "no"])
])

dnl Checking for pdflatex

AC_DEFUN([AC_PROG_PDFLATEX], [dnl
AC_CHECK_PROGS(PDFLATEX,[pdflatex],no)
AM_CONDITIONAL([PDFLATEX_AVAILABLE], [test "$PDFLATEX" != "no"])
AC_SUBST(PDFLATEX)
])

dnl Checking for Metapost

AC_DEFUN([AC_PROG_MPOST], [dnl
AC_CHECK_PROGS(MPOST,[mpost metapost],no)
AM_CONDITIONAL([MPOST_AVAILABLE], [test "$MPOST" != "no"])
AC_SUBST(MPOST)
])

dnl Putting the above together, check possibilities for online event analysis
AC_DEFUN([WO_CHECK_EVENT_ANALYSIS_METHODS], [dnl
AC_REQUIRE([AC_PROG_LATEX])
AC_REQUIRE([AC_PROG_MPOST])
AC_REQUIRE([AC_PROG_DVIPS])
AC_REQUIRE([AC_PROG_PS2PDF])

AC_CACHE_CHECK([whether we can display event analysis in PostScript format],
[wo_cv_event_analysis_ps],
[dnl
if test "$LATEX" != "no" -a "$MPOST" != "no" -a "$DVIPS" != "no"; then
  wo_cv_event_analysis_ps="yes"
else
  wo_cv_event_analysis_ps="no"
fi])
EVENT_ANALYSIS_PS="$wo_cv_event_analysis_ps"
AC_SUBST([EVENT_ANALYSIS_PS])

AC_CACHE_CHECK([whether we can display event analysis in PDF format],
[wo_cv_event_analysis_pdf],
[dnl
if test "$EVENT_ANALYSIS_PS" != "no" -a "$PS2PDF" != "no"; then
  wo_cv_event_analysis_pdf="yes"
else
  wo_cv_event_analysis_pdf="no"
fi])
EVENT_ANALYSIS_PDF="$wo_cv_event_analysis_pdf"
AC_SUBST([EVENT_ANALYSIS_PDF])
])


dnl Checking for gzip (putting this together with the LaTeX part)

AC_DEFUN([AC_PROG_GZIP],[
AC_CHECK_PROGS(GZIP,[gzip],no)
AM_CONDITIONAL([GZIP_AVAILABLE], [test "$GZIP" != "no"])
AC_SUBST(GZIP)
])
