#############################################################################
# Configuration section
#############################################################################

##############################################################################
# Variables
##############################################################################
TOP=$(shell pwd)

TARGET=efuns


#BACKENDDIR=graphics/ocamlgraphics
#OTHERSYSLIBS=graphics.cma

BACKENDDIR=graphics/gtk_cairo
GRAPHICSDIR=$(shell ocamlfind query lablgtk2) $(shell ocamlfind query cairo)
OTHERSYSLIBS=lablgtk.cma cairo.cma cairo_lablgtk.cma 
GTKLOOP=gtkThread.cmo

#todo: put top_window.ml in core and minibuffer in features/ instead

SRC=\
 commons/common.ml\
 commons/simple_color.ml\
 \
 commons/utils.ml\
 commons/log.ml\
 commons/str2.ml\
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
 \
 graphics/top_window.ml\
 features/simple.ml\
 core/minibuffer.ml\
 features/multi_frames.ml\
 features/select.ml\
 features/interactive.ml\
 features/complexe.ml\
 features/abbrevs.ml\
 features/system.ml\
 features/compil.ml\
 features/search.ml\
 minor_modes/minor_mode_sample.ml\
 minor_modes/paren_mode.ml\
 minor_modes/abbrevs_mode.ml\
 minor_modes/fill_mode.ml\
 minor_modes/tab_mode.ml\
 major_modes/dired.ml\
 prog_modes/makefile_mode.ml\
 std_efunsrc.ml\
 $(BACKENDDIR)/graphics_efuns.ml \
 main.ml \

# minor_modes/accents_mode.ml\
# prog_modes/*.mll
# ipc/efuns_client.ml ipc/server.ml
# dynamic/eval.ml
# misc/efuns_xxx.ml

CMIS=\
 commons/common.cmi\
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

SYSLIBS=unix.cma str.cma threads.cma 

INCLUDEDIRS=\
  commons\
  core features\
  graphics $(BACKENDDIR) $(GRAPHICSDIR)

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

$(TARGET): $(LIBS) $(OBJS)
	$(OCAMLC) -cclib -L/opt/X11/lib  $(BYTECODE_STATIC) -o $@ $(OTHERSYSLIBS) $(SYSLIBS) $(GTKLOOP) $^

$(TARGET).opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) 
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)  $^

clean::
	rm -f $(OBJS) $(OBJS:.cmo=.cmi)

depend::
	$(OCAMLDEP) */*.ml*  $(BACKENDDIR)/*.ml* >> .depend


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

SRC_VIEWS= \
  commons/local.ml\
  commons/local.mli\
  core/efuns.ml\
  core/text.ml\
  core/ebuffer.ml\
  core/keymap.ml\
  core/window.ml\
  core/frame.ml\
  core/minibuffer.ml\
  features/simple.ml\
  features/complexe.ml\
  features/system.ml\
  features/select.ml\
  features/search.ml\
  features/interactive.ml\
  features/multi_frames.ml\
  features/abbrevs.ml\
  features/compil.ml\
  graphics/top_window.ml\
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
