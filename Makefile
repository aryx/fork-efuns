#############################################################################
# Configuration section
#############################################################################

#############################################################################
# Variables
#############################################################################
TOP=$(shell pwd)

TARGET=efuns

BACKENDDIR=graphics/gtk_cairo

SRC=\
 commons/simple_color.ml\
 \
 commons/utils.ml commons/str2.ml\
 commons/log.ml\
 commons/options.ml\
 commons/local.ml\
 \
 graphics/xtypes.ml\
 graphics/xdraw.ml\
 graphics/xK.ml\
 \
 core/text.ml\
 core/efuns.ml\
 core/keymap.ml\
 core/ebuffer.ml\
 core/window.ml\
 core/frame.ml\
 core/top_window.ml\
 \
 features/simple.ml\
 features/minibuffer.ml\
 features/multi_frames.ml\
 features/select.ml\
 features/interactive.ml\
 features/multi_buffers.ml\
 features/complexe.ml\
 features/abbrevs.ml\
 features/system.ml\
 features/compil.ml\
 features/search.ml\
 features/dircolors.ml\
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
 \
 prog_modes/pl_colors.ml\
 prog_modes/makefile_mode.ml\
 prog_modes/ocaml_mode.ml\
 prog_modes/c_mode.ml\
 prog_modes/lisp_mode.ml\
 \
 pfff_modes/caml_mode.ml\
 pfff_modes/cpp_mode.ml\
 pfff_modes/noweb_mode.ml\
 \
 text_modes/tex_mode.ml\
 text_modes/html_mode.ml\
 \
 std_efunsrc.ml\
 $(BACKENDDIR)/graphics_efuns.ml \
 main.ml \

# minor_modes/accents_mode.ml\
# prog_modes/*.mll
# ipc/efuns_client.ml ipc/server.ml
# dynamic/eval.ml
# misc/efuns_xxx.ml

CMIS=\
 commons/simple_color.cmi\
 commons/utils.cmi\
 commons/options.cmi\
 commons/local.cmi\
 core/text.cmi\
 core/ebuffer.cmi\
 core/frame.cmi\
 features/simple.cmi\
 features/select.cmi\
 features/search.cmi\

#------------------------------------------------------------------------------
#package dependencies
#------------------------------------------------------------------------------

LIBROOT=/Users/pad/.opam/4.01.0/lib/

GRAPHICSDIRS=$(LIBROOT)/lablgtk2 $(LIBROOT)/cairo
GRAPHICSLIBS=lablgtk.cma cairo.cma   cairo_lablgtk.cma pango_cairo.cma
GTKLOOP=gtkThread.cmo
#alt:
#BACKENDDIR=graphics/ocamlgraphics
#OTHERSYSLIBS=graphics.cma
#$(shell ocamlfind query cairo)



COMMONDIR=$(LIBROOT)/pfff-commons
COMMONCMA=$(LIBROOT)/pfff-commons/lib.cma


# many dirs are here just because of -linkall
PFFF_LIBS=\
 config\
 external-jsonwheel\
 h_files-format\
 h_program-lang \
 matcher\
 lang_ml lang_ml-visual \
 lang_cpp lang_cpp-analyze \
 lang_nw lang_nw-analyze \

PFFFDIRS=$(PFFF_LIBS:%=$(LIBROOT)/pfff-%/)
PFFFCMAS=$(PFFFDIRS:%=%/lib.cma)


#------------------------------------------------------------------------------
# Main variables
#------------------------------------------------------------------------------

SYSLIBS=unix.cma str.cma threads.cma nums.cma bigarray.cma

LIBS=$(SYSLIBS) $(COMMONCMA) $(PFFFCMAS) $(GRAPHICSLIBS) 

INCLUDEDIRS=\
  $(COMMONDIR) \
  commons\
  core features\
  graphics $(BACKENDDIR) $(GRAPHICSDIRS) $(PFFFDIRS) \
  major_modes minor_modes prog_modes text_modes

##############################################################################
# Generic variables
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
.PHONY:: all all.opt opt top clean distclean

all:: $(CMIS)
	$(MAKE) $(TARGET) 

opt:
	$(MAKE) $(TARGET).opt

# need -linkall!
$(TARGET): $(OBJS)
	$(OCAMLC) -linkall -cclib -L/opt/X11/lib  $(BYTECODE_STATIC) -o $@ \
      $(LIBS) $(GTKLOOP) $(OBJS)

$(TARGET).opt: $(OPTOBJS) 
	$(OCAMLOPT) $(STATIC) -cclib -L/opt/X11/lib -o $@ \
      $(LIBS:.cma=.cmxa) $(GTKLOOP:.cmo=.cmx) $(OPTOBJS)

#clean::
#	@rm -f $(OBJS) $(OBJS:.cmo=.cmi) $(OBJS:.cmo=.cmx) $(OBJS:.cmo=.o) \
#       $(OBJS:.cmo=.annot) $(OBJS:.cmo=.cmt) $(OBJS:.cmo=.cmti)
clean::
	rm -f */*.cm[ioxa] */*.[oa] */*.cmxa */*.annot */*.cmt*


depend::
	$(OCAMLDEP) */*.ml*  $(BACKENDDIR)/*.ml* >> .depend

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
	cm -filter pfff .

##############################################################################
# Literate Programming rules
##############################################################################

include $(TOP)/docs/latex/Makefile.common

TEXMAIN=Efuns.nw
TEX=Efuns.tex

SRC_ORIG=Efuns.nw Efuns_extra.nw

# dircolors.ml, prog_modes/*

SRC_VIEWS= \
  commons/local.ml\
  commons/local.mli\
  core/efuns.ml\
  core/text.ml\
  core/ebuffer.ml\
  core/keymap.ml\
  core/window.ml\
  core/frame.ml\
  core/top_window.ml\
  features/simple.ml\
  features/minibuffer.ml\
  features/complexe.ml\
  features/system.ml\
  features/select.ml\
  features/search.ml\
  features/interactive.ml\
  features/multi_buffers.ml\
  features/multi_frames.ml\
  features/abbrevs.ml\
  features/compil.ml\
  std_efunsrc.ml\
  main.ml\
  major_modes/dired.ml\
  minor_modes/minor_mode_sample.ml\
  minor_modes/abbrevs_mode.ml\
  minor_modes/accents_mode.ml\
  minor_modes/fill_mode.ml\
  minor_modes/paren_mode.ml\
  minor_modes/tab_mode.ml\
  prog_modes/makefile_mode.ml\
  prog_modes/ocaml_toplevel.ml\
  ipc/server.ml\
  ipc/efuns_client.ml\
  dynamic/eval.ml\
  misc/efuns_filebrowser.ml\
  misc/efuns_texbrowser.ml\
