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
#Library dependencies
#------------------------------------------------------------------------------

# Basic dependencies 
EXTERNALDIRS= external/commons external/h_visualization
EXTERNALCMAS= external/commons/lib.cma external/h_visualization/lib.cma

# Jsonwheel dependencies 
# (I now use jsonwheel not only for pfff_modes but also for ocaml_merlin)
# this is for -I and for finding dlls (e.g., deps-netsys)
EXTERNALDIRS+=external/json-wheel external/deps-netsys 
EXTERNALCMAS+=\
 external/deps-netsys/netsys_oothr.cma external/deps-netsys/netsys.cma \
 external/deps-netstring/netstring.cma \
 external/json-wheel/jsonwheel.cma

# Pfff dependencies
ifeq ($(USE_PFFF),1)
PFFF_MODES=\
 pfff_modes/pfff_modes.ml\
 pfff_modes/caml_mode.ml\
 pfff_modes/cpp_mode.ml\
 pfff_modes/noweb_mode.ml\

# for compilation (for -I)
PFFF_LIBS1=\
 h_program-lang \
 matcher \
 graph_code \
 lang_ml  lang_ml-visual \
 lang_cpp lang_cpp-analyze \
 lang_nw  lang_nw-analyze \

# some of those dirs are here just because of -linkall
PFFF_LIBS0=\
 config\
 commons-graph \
 h_files-format \

PFFFDIRS=$(PFFF_LIBS1:%=external/pfff-%/)
PFFFCMAS=\
 external/pfff-deps-ocamlgraph/graph.cma \
 external/pfff-deps-commons_core/lib.cma \
 $(PFFF_LIBS0:%=external/pfff-%/lib.cma) \
 $(PFFF_LIBS1:%=external/pfff-%/lib.cma)
endif

ifeq ($(USE_GTKCAIRO2), 1)
BACKENDDIR=graphics/gtk_cairo2
GRAPHICSDIRS=external/lablgtk2 \
 external/cairo2 external/cairo2-gtk external/cairo2-pango
GRAPHICSLIBS=external/lablgtk2/lablgtk.cma external/cairo2/cairo.cma\
  external/cairo2-gtk/cairo_gtk.cma external/cairo2-pango/cairo_pango.cma
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
 commons/utils.ml\
 commons/str2.ml\
 commons/log.ml\
 commons/concur.ml\
 commons/options.ml\
 commons/store.ml\
 \
 graphics/xtypes.ml\
 graphics/xdraw.ml\
 graphics/xK.ml\
 \
 core/text.ml\
 core/efuns.ml\
 core/globals.ml\
 core/var.ml\
 core/parameter.ml\
 core/attr.ml\
 core/action.ml\
 core/hook.ml\
 core/keymap.ml\
 core/ebuffer.ml\
 core/window.ml\
 core/frame.ml\
 core/top_window.ml\
 \
 features/message.ml\
 features/move.ml\
 features/edit.ml\
 features/copy_paste.ml\
 features/scroll.ml\
 features/mouse.ml\
 features/highlight.ml\
 features/color.ml\
 features/indent.ml\
 features/structure.ml\
 features/minibuffer.ml\
 features/multi_frames.ml\
 features/select.ml\
 features/interactive.ml\
 features/multi_buffers.ml\
 features/abbrevs.ml\
 features/system.ml\
 features/dircolors.ml\
 features/compil.ml\
 features/search.ml\
 features/electric.ml\
 features/transform.ml\
 features/rectangle.ml\
 features/macros.ml\
 features/misc.ml\
 \
 minor_modes/minor_modes.ml\
 minor_modes/minor_mode_sample.ml\
 minor_modes/paren_mode.ml\
 minor_modes/abbrevs_mode.ml\
 minor_modes/fill_mode.ml\
 minor_modes/tab_mode.ml\
 \
 major_modes/major_modes.ml\
 major_modes/dired.ml\
 major_modes/buffer_menu.ml\
 major_modes/shell.ml\
 major_modes/outline_mode.ml\
 \
 prog_modes/pl_colors.ml\
 prog_modes/common_lexer.ml\
 prog_modes/common_indenter.ml\
 prog_modes/makefile_mode.ml\
 prog_modes/ocaml_lexer.ml\
 prog_modes/c_lexer.ml\
 prog_modes/lisp_lexer.ml\
 prog_modes/ocaml_mode.ml\
 prog_modes/ocaml_merlin.ml\
 prog_modes/c_mode.ml\
 prog_modes/lisp_mode.ml\
 \
 $(PFFF_MODES)\
 \
 text_modes/tex_mode.ml\
 text_modes/html_mode.ml\
 text_modes/org_mode.ml\
 \
 ipc/server.ml \
 \
 config.ml\
 pad.ml\
 $(BACKENDDIR)/graphics_efuns.ml \
 main.ml \

#todo:
# minor_modes/accents_mode.ml\
# prog_modes/*.mll
# ipc/efuns_client.ml ipc/server.ml
# dynamic/eval.ml
# misc/efuns_xxx.ml

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

# do not put external dirs here! otherwise make depend might modify OPAM files
DEPEND_INCLUDES=$(DIRS:%=-I %)

LIBS=$(EXTERNALCMAS) $(PFFFCMAS) $(GRAPHICSLIBS)
# bigarray is used by cairo
SYSLIBS=unix.cma str.cma threads.cma bigarray.cma

PPX=-ppx $(TOP)/ppx/ppx_interactive

##############################################################################
# Generic variables
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
.PHONY:: all all.opt opt top clean distclean

all:: ppx/ppx_interactive $(PROGS)
opt: ppx/ppx_interactive $(PROGS:=.opt)

# need -linkall! otherwise the 'let _ = add_start_hook ...' will not be run.
$(TARGET): $(OBJS) $(LIBS)
	$(OCAMLC) -linkall -cclib -L/opt/X11/lib  $(BYTECODE_STATIC) -o $@ \
      $(SYSLIBS) $(LIBS) $(GTKLOOP) $(OBJS) $(EXTRA)

$(TARGET).opt: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) $(STATIC) -cclib -L/opt/X11/lib -o $@ \
     $(SYSLIBS:.cma=.cmxa) $(LIBS:.cma=.cmxa) $(GTKLOOP:.cmo=.cmx) $(OPTOBJS)

#clean::
#	@rm -f $(OBJS) $(OBJS:.cmo=.cmi) $(OBJS:.cmo=.cmx) $(OBJS:.cmo=.o) \
#       $(OBJS:.cmo=.annot) $(OBJS:.cmo=.cmt) $(OBJS:.cmo=.cmti)
clean::
	rm -f */*.cm[ioxa] */*.[oa] */*.cmxa */*.annot */*.cmt*
	rm -f $(PROGS) *.opt

#TODO: should recompile if opam switch
ppx/ppx_interactive:
	cd ppx; make
clean::
	rm -f ppx/ppx_interactive

efuns_client: ipc/efuns_client.cmo
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $(LIBS) $^
efuns_client.opt: ipc/efuns_client.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $(LIBS:.cma=.cmxa) $^

depend::
	$(OCAMLDEP) $(DEPEND_INCLUDES) *.ml* */*.ml*  $(BACKENDDIR)/*.ml* > .depend

LEXERS= \
 prog_modes/ocaml_lexer.ml prog_modes/c_lexer.ml prog_modes/lisp_lexer.ml \
 text_modes/tex_mode.ml text_modes/html_mode.ml

beforedepend:: $(LEXERS)
prog_modes/ocaml_lexer.ml: prog_modes/ocaml_lexer.mll
	ocamllex $^
prog_modes/c_lexer.ml: prog_modes/c_lexer.mll
	ocamllex $^
prog_modes/lisp_lexer.ml: prog_modes/lisp_lexer.mll
	ocamllex $^

text_modes/tex_mode.ml: text_modes/tex_mode.mll
	ocamllex $^
text_modes/html_mode.ml: text_modes/html_mode.mll
	ocamllex $^

clean:: 
	rm -f $(LEXERS)

##############################################################################
# Developer rules
##############################################################################

.PHONY:: .merlin

.merlin:
	@echo '# -*- sh -*-' > .merlin
	@echo '# Auto-generated by make .merlin' >> .merlin
	@set -e; for i in $(INCLUDEDIRS); do echo "S $$i" >> .merlin || exit 1; done
	@set -e; for i in $(INCLUDEDIRS); do echo "B $$i" >> .merlin || exit 1; done
	@echo 'B external/FOR_MERLIN/**' >> .merlin
	@echo 'S external/FOR_MERLIN/**' >> .merlin

test:
	./efuns -height 45 -font "UbuntuMono Bold 22" $*

visual:
	~/pfff/codemap -no_legend -screen_size 3 -efuns_client ./efuns_client -emacs_client /dev/null -filter pfff .

graph:
	~/pfff/codegraph_build -symlinks -lang cmt -derived_data -verbose .

check:
	~/pfff/scheck -with_graph_code graph_code.marshall -filter 3 . 2>&1 | grep -v stdlib | grep -v commons/ | grep Function
