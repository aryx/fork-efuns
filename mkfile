

SRC=\
 commons/common.ml commons/simple_color.ml \
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
 prog_modes/ocaml_mode.ml\
 prog_modes/c_mode.ml\
 prog_modes/lisp_mode.ml\
 \
 text_modes/tex_mode.ml\
 text_modes/html_mode.ml\
 text_modes/org_mode.ml\
 \
 ipc/server.ml \
 \
 std_efunsrc.ml\
 pad.ml\
 graphics/libdraw/graphics_efuns.ml \

# main.ml \

OBJS=${SRC:%.ml=%.cmo}

#SYSLIBS=unix.cma str.cma threads.cma nums.cma bigarray.cma


INCLUDEDIRS==commons core features graphics \
 major_modes minor_modes prog_modes text_modes pfff_modes ipc
#TODO: INCLUDES=${INCLUDEDIRS:%=-I  %}
INCLUDES=-I commons -I core -I features -I graphics \
 -I major_modes -I minor_modes -I prog_modes -I text_modes -I ipc

OCAMLC=ocamlc -thread $INCLUDES

all:V: efuns.byte

efuns.byte: $OBJS
	$OCAMLC  $prereq -o $target

# do not use prereq or it will include also the .cmi in the command line
%.cmo: %.ml
	$OCAMLC -c $stem.ml

%.cmi: %.mli
	$OCAMLC -c $stem.mli

clean:V:
	rm $OBJS



MODES= \
 prog_modes/ocaml_mode.ml prog_modes/c_mode.ml prog_modes/lisp_mode.ml \
 text_modes/tex_mode.ml text_modes/html_mode.ml

beforedepend:: $MODES
prog_modes/ocaml_mode.ml: prog_modes/ocaml_mode.mll
	ocamllex $prereq
prog_modes/c_mode.ml: prog_modes/c_mode.mll
	ocamllex $prereq
prog_modes/lisp_mode.ml: prog_modes/lisp_mode.mll
	ocamllex $prereq
text_modes/tex_mode.ml: text_modes/tex_mode.mll
	ocamllex $prereq
text_modes/html_mode.ml: text_modes/html_mode.mll
	ocamllex $prereq

depend:V: $MODES
	ocamldep $INCLUDES *.ml* */*.ml* > .depend

<.depend

