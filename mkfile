#TODO: to port to plan9 you need to handle ppx [@@interactive]!
# maybe simpler to begin with consider them as attribute
# and generate a big file from Linux that store all the code
# by those interactive and use this file under plan9


#############################################################################
# Configuration section
#############################################################################
</$objtype/mkfile

#############################################################################
# Variables
#############################################################################

BACKENDDIR=graphics/libdraw

SRC=\
 commons/common.ml commons/file_type.ml commons/simple_color.ml \
 \
 commons/utils.ml commons/str2.ml\
 commons/log.ml\
 commons/options.ml\
 commons/local.ml\
 commons/concur.ml\
 \
 graphics/xtypes.ml\
 graphics/xdraw.ml\
 graphics/xK.ml\
 \
 core/text.ml\
 core/efuns.ml\
  core/globals.ml\
  core/var.ml\
  core/attr.ml\
  core/action.ml\
  core/hook.ml\
 core/keymap.ml\
 core/ebuffer.ml\
 core/window.ml\
 core/frame.ml\
 core/top_window.ml\
 \
 features/simple.ml\
  features/mouse.ml\
  features/highlight.ml\
  features/parameter.ml\
  features/indent.ml\
  features/structure.ml\
 features/minibuffer.ml\
 features/multi_frames.ml\
 features/select.ml\
 features/interactive.ml\
 features/multi_buffers.ml\
 features/complexe.ml\
 features/abbrevs.ml\
 features/system.ml\
 features/dircolors.ml\
 features/compil.ml\
 features/search.ml\
 \
 minor_modes/minor_mode_sample.ml\
 minor_modes/paren_mode.ml\
 minor_modes/abbrevs_mode.ml\
 minor_modes/fill_mode.ml\
 minor_modes/tab_mode.ml\
 \
 major_modes/dired.ml\
 major_modes/buffer_menu.ml\
 major_modes/shell.ml\
 major_modes/outline_mode.ml\
 \
 prog_modes/pl_colors.ml\
 prog_modes/makefile_mode.ml\
 prog_modes/ocaml_mode.ml\
 prog_modes/lisp_mode.ml\
 prog_modes/c_mode.ml\
 \
 text_modes/org_mode.ml\
 text_modes/tex_mode.ml\
 text_modes/html_mode.ml\
 \
 ipc/server.ml \
 \
 std_efunsrc.ml\
 pad.ml\
 graphics/libdraw/draw.ml \
 graphics/libdraw/graphics_efuns.ml \
 main.ml


COBJS=commons/realpath.$O graphics/libdraw/draw.$O

SYSLIBS=str.cma unix.cma  threads.cma

#INCLUDEDIRS=commons core features graphics \
# major_modes minor_modes prog_modes text_modes pfff_modes ipc
#INCLUDES=${INCLUDEDIRS:%=-I  %} does not work :(
INCLUDES=\
 -I commons \
 -I core -I features -I ipc \
 -I major_modes -I minor_modes -I prog_modes -I text_modes \
 -I graphics -I $BACKENDDIR

##############################################################################
# Generic variables
##############################################################################

BINDIR=/usr/local/bin
LIBDIR=/usr/local/lib/ocaml

OCAMLC=$BINDIR/ocamlrun $BINDIR/ocamlc -g -thread $INCLUDES
OCAMLLEX=$BINDIR/ocamlrun $BINDIR/ocamllex
OCAMLDEP=$BINDIR/ocamlrun $BINDIR/ocamldep

OBJS=${SRC:%.ml=%.cmo}
CMIS=${OBJS:%.cmo=%.cmi}
SYSCLIBS=${SYSLIBS:%.cma=$LIBDIR/lib%.a}

CC=pcc
LD=pcc
CINCLUDES= -I$LIBDIR
# -B to disable the check for missing return, which is flagged
# because of CAMLReturn
CFLAGS=-FVB -D_POSIX_SOURCE -D_BSD_EXTENSION -D_PLAN9_SOURCE $CINCLUDES

##############################################################################
# Top rules
##############################################################################

all:V: efuns.byte

# currently pcc does not accept -L so I replaced -cclib -unix by
# the more explicit /usr/local/lib/ocaml/libunix.a
#old:$OCAMLC str.cma unix.cma threads.cma  -custom -cclib -lstr -cclib -lunix -cclib -lthreads $COBJS  $OBJS -o $target

efuns.byte: $OBJS $COBJS
	$OCAMLC -verbose -custom $SYSLIBS $SYSCLIBS $COBJS  $OBJS -o $target

clean:V:
	rm -f $OBJS $CMIS $COBJS
    rm -f *.[58] *.byte


MODES= \
 prog_modes/ocaml_mode.ml prog_modes/c_mode.ml prog_modes/lisp_mode.ml \
 text_modes/tex_mode.ml text_modes/html_mode.ml

beforedepend: $MODES
prog_modes/ocaml_mode.ml: prog_modes/ocaml_mode.mll
	$OCAMLLEX $prereq
prog_modes/c_mode.ml: prog_modes/c_mode.mll
	$OCAMLLEX $prereq
prog_modes/lisp_mode.ml: prog_modes/lisp_mode.mll
	$OCAMLLEX $prereq
text_modes/tex_mode.ml: text_modes/tex_mode.mll
	$OCAMLLEX $prereq
text_modes/html_mode.ml: text_modes/html_mode.mll
	$OCAMLLEX $prereq


MLIS=${SRC:%.ml=%.mli}

depend:V: $MODES
	$OCAMLDEP $INCLUDES $SRC $MLIS  | grep -v -e '.* :$' > .depend2

##############################################################################
# Generic rules
##############################################################################

# do not use prereq or it will include also the .cmi in the command line
# because of the .depend file that also define some rules
%.cmo: %.ml
	$OCAMLC -c $stem.ml

%.cmi: %.mli
	$OCAMLC -c $stem.mli

%.$O: %.c
	$CC $CFLAGS -v -c $stem.c -o $stem.$O


<.depend2
