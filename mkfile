
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
  core/globals.ml\
  core/var.ml\
  core/attr.ml\
  core/action.ml\
  core/hook.ml\
 core/efuns.ml\
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
 \
 text_modes/org_mode.ml\
 \
 ipc/server.ml \
 \
 std_efunsrc.ml\
 pad.ml\
 graphics/libdraw/graphics_efuns.ml \
 main.ml

# syntax error special chars
# prog_modes/ocaml_mode.ml\
# prog_modes/lisp_mode.ml\
# prog_modes/c_mode.ml\
# text_modes/tex_mode.ml\
# text_modes/html_mode.ml\

OBJS=${SRC:%.ml=%.cmo}
CMIS=${OBJS:%.cmo=%.cmi}

#COBJS=commons/realpath.o
CFLAGS=-I/usr/local/lib/ocaml/

#SYSLIBS=str.cma unix.cma  threads.cma

#INCLUDEDIRS=commons core features graphics \
# major_modes minor_modes prog_modes text_modes pfff_modes ipc
#INCLUDES=${INCLUDEDIRS:%=-I  %} does not work :(
INCLUDES=-I commons -I core -I features -I graphics \
 -I major_modes -I minor_modes -I prog_modes -I text_modes -I ipc -I $BACKENDDIR

OCAMLC=/usr/local/bin/ocamlrun /usr/local/bin/ocamlc -thread $INCLUDES
OCAMLLEX=/usr/local/bin/ocamlrun /usr/local/bin/ocamllex

all:V: efuns.byte

# currently pcc does not accept -L so I replaced -cclib -unix by
# the more explicit /usr/local/lib/ocaml/libunix.a
#old:$OCAMLC str.cma unix.cma threads.cma  -custom -cclib -lstr -cclib -lunix -cclib -lthreads $COBJS  $OBJS -o $target

efuns.byte: $OBJS $COBJS
	$OCAMLC str.cma unix.cma threads.cma  -custom /usr/local/lib/ocaml/libstr.a /usr/local/lib/ocaml/libunix.a /usr/local/lib/ocaml/libthreads.a $COBJS  $OBJS -o $target

link:V:
	$OCAMLC str.cma unix.cma threads.cma  -custom /usr/local/lib/ocaml/libstr.a /usr/local/lib/ocaml/libunix.a /usr/local/lib/ocaml/libthreads.a $COBJS  $OBJS -o $target


# do not use prereq or it will include also the .cmi in the command line
%.cmo: %.ml
	$OCAMLC -c $stem.ml

%.cmi: %.mli
	$OCAMLC -c $stem.mli

%.o: %.c
	cc $CFLAGS -c $stem.c -o $stem.o


clean:V:
	rm -f $OBJS $CMIS


MODES= \
 prog_modes/ocaml_mode.ml prog_modes/c_mode.ml prog_modes/lisp_mode.ml \
 text_modes/tex_mode.ml text_modes/html_mode.ml

beforedepend:: $MODES
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

#$MODES

depend:V: 
	ocamldep $INCLUDES *.ml* */*.ml* | grep -v -e '.* :$' > .depend2

<.depend2

