#############################################################################
# Configuration section
#############################################################################
-include Makefile.config

#############################################################################
# Variables
#############################################################################
TOP=$(shell pwd)

TARGET=efuns
PROGS=efuns efuns_client

#------------------------------------------------------------------------------
#package dependencies
#------------------------------------------------------------------------------

EXTERNAL_LIBS=h_visualization

EXTERNALDIRS=external/commons $(EXTERNAL_LIBS:%=external/pfff-%)
EXTERNALCMAS=external/commons/commons.cma $(EXTERNAL_LIBS:%=external/pfff-%/lib.cma)


LIBROOT=$(shell ocamlc -where)/..
#/Users/pad/.opam/4.01.0/lib/

# pfff
ifeq ($(USE_PFFF),1)
PFFF_MODES=\
 pfff_modes/pfff_modes.ml\
 pfff_modes/caml_mode.ml\
 pfff_modes/cpp_mode.ml\
 pfff_modes/noweb_mode.ml\

# many dirs are here just because of -linkall
PFFF_LIBS=\
 config\
 external-jsonwheel\
 h_files-format\
 h_program-lang \
 commons-graph \
 graph_code \
 matcher\
 lang_ml lang_ml-visual \
 lang_cpp lang_cpp-analyze \
 lang_nw lang_nw-analyze \

PFFFDIRS=$(PFFF_LIBS:%=$(LIBROOT)/pfff-%/)
PFFFCMAS=$(LIBROOT)/ocamlgraph/ocamlgraph.cma $(PFFFDIRS:%=%/lib.cma) 
endif

# gtk/cairo is actually the only working backend available right now
ifeq ($(USE_GTKCAIRO), 1)
BACKENDDIR=graphics/gtk_cairo
GRAPHICSDIRS=$(LIBROOT)/lablgtk2 $(LIBROOT)/cairo
#$(shell ocamlfind query cairo)
GRAPHICSLIBS=lablgtk.cma cairo.cma   cairo_lablgtk.cma pango_cairo.cma
GTKLOOP=gtkThread.cmo
# because of -linkall and the use of libcairo, but should not be required
EXTRA=-cclib -lfontconfig
endif

#alt:
#BACKENDDIR=graphics/ocamlgraphics
#OTHERSYSLIBS=graphics.cma

#------------------------------------------------------------------------------
# Main variables
#------------------------------------------------------------------------------

SRC=\
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
 prog_modes/c_mode.ml\
 prog_modes/lisp_mode.ml\
 \
 $(PFFF_MODES) \
 \
 text_modes/tex_mode.ml\
 text_modes/html_mode.ml\
 text_modes/org_mode.ml\
 \
 ipc/server.ml \
 \
 std_efunsrc.ml\
 pad.ml\
 $(BACKENDDIR)/graphics_efuns.ml \
 main.ml \

# minor_modes/accents_mode.ml\
# prog_modes/*.mll
# ipc/efuns_client.ml ipc/server.ml
# dynamic/eval.ml
# misc/efuns_xxx.ml



# bigarray is used by cairo
SYSLIBS=unix.cma str.cma threads.cma bigarray.cma

LIBS=$(SYSLIBS) $(EXTERNALCMAS) $(PFFFCMAS) $(GRAPHICSLIBS)

DIRS=\
  commons\
  core features\
  graphics $(BACKENDDIR)\
  major_modes minor_modes  prog_modes text_modes pfff_modes\
  ipc

INCLUDEDIRS=\
  $(EXTERNALDIRS) \
  $(PFFFDIRS) \
  $(GRAPHICSDIRS) \
  $(DIRS)

OCAMLDEPS=$(DIRS:%=-I %)

##############################################################################
# Generic variables
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
.PHONY:: all all.opt opt top clean distclean

all:: $(PROGS)
opt: $(PROGS:=.opt)

# need -linkall! otherwise the 'let _ = add_start_hook ...' will not be run.
$(TARGET): $(OBJS)
	$(OCAMLC) -linkall -cclib -L/opt/X11/lib  $(BYTECODE_STATIC) -o $@ \
      $(LIBS) $(GTKLOOP) $(OBJS) $(EXTRA)

$(TARGET).opt: $(OPTOBJS)
	$(OCAMLOPT) $(STATIC) -cclib -L/opt/X11/lib -o $@ \
      $(LIBS:.cma=.cmxa) $(GTKLOOP:.cmo=.cmx) $(OPTOBJS)

#clean::
#	@rm -f $(OBJS) $(OBJS:.cmo=.cmi) $(OBJS:.cmo=.cmx) $(OBJS:.cmo=.o) \
#       $(OBJS:.cmo=.annot) $(OBJS:.cmo=.cmt) $(OBJS:.cmo=.cmti)
clean::
	rm -f */*.cm[ioxa] */*.[oa] */*.cmxa */*.annot */*.cmt*
	rm -f $(PROGS) *.opt


efuns_client: ipc/efuns_client.cmo
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(LIBS) $^

efuns_client.opt: ipc/efuns_client.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(LIBS:.cma=.cmxa) $^


depend::
	$(OCAMLDEP) $(OCAMLDEPS) */*.ml*  $(BACKENDDIR)/*.ml* >> .depend

MODES= \
 prog_modes/ocaml_mode.ml prog_modes/c_mode.ml prog_modes/lisp_mode.ml \
 text_modes/tex_mode.ml text_modes/html_mode.ml

beforedepend:: $(MODES)
prog_modes/ocaml_mode.ml: prog_modes/ocaml_mode.mll
	ocamllex $^
prog_modes/c_mode.ml: prog_modes/c_mode.mll
	ocamllex $^
prog_modes/lisp_mode.ml: prog_modes/lisp_mode.mll
	ocamllex $^
text_modes/tex_mode.ml: text_modes/tex_mode.mll
	ocamllex $^
text_modes/html_mode.ml: text_modes/html_mode.mll
	ocamllex $^

clean:: 
	rm -f $(MODES)

##############################################################################
# Developer rules
##############################################################################

visual:
	~/pfff/codemap -no_legend -screen_size 2 -filter pfff .

graph:
	~/pfff/codegraph -derived_data -lang cmt -build .

check:
	~/pfff/scheck -with_graph_code graph_code.marshall -filter 3 . 2>&1 | grep -v stdlib | grep -v commons/ | grep Function
