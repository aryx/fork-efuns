#############################################################################
# Configuration section
#############################################################################

##############################################################################
# Variables
##############################################################################
TOP=$(shell pwd)

CMIS=\
 commons2/utils.cmi\
 commons2/options.cmi\
 misc/local.cmi\
 core/text.cmi\
 core/ebuffer.cmi\
 core/frame.cmi\
 features/simple.cmi\
 features/select.cmi\
 features/search.cmi\

SRC=\
 commons2/utils.ml\
 commons2/log.ml\
 commons2/str2.ml\
 commons2/options.ml\
 misc/local.ml\
 core/text.ml\
 core/efuns.ml\
 core/keymap.ml\
 core/window.ml\
 core/ebuffer.ml\
 core/frame.ml\
 graphics/xtypes.ml\
 graphics/xK.ml\
 graphics/top_window.ml\
 features/simple.ml\
 core/minibuffer.ml\
 features/multi_frames.ml\
 features/select.ml\
 features/interactive.ml\
 features/complex.ml\
 features/abbrevs.ml\
 features/system.ml\
 features/compil.ml\
 features/search.ml\
 minor_modes/minor_mode_sample.ml\
 minor_modes/abbrevs_mode.ml\
 minor_modes/fill_mode.ml\
 minor_modes/paren_mode.ml\
 minor_modes/tab_mode.ml\
 major_modes/dired.ml\
 prog_modes/makefile_mode.ml\
 std_efunsrc.ml\
 main.ml

# minor_modes/accents_mode.ml\
# client/efuns_client.ml server/server.ml
# dynamic/eval.ml
# misc/efuns_xxx.ml
# prog_modes/*.mll

TARGET=efuns

SYSLIBS=unix.cma str.cma

INCLUDEDIRS=commons2 core misc graphics features

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
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $^

$(TARGET).opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) 
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)  $^

clean::
	rm -f $(CMIS) $(OBJS)

depend::
	$(OCAMLDEP) */*.mli */*.ml > .depend


##############################################################################
# Developer rules
##############################################################################

##############################################################################
# Literate Programming rules
##############################################################################

include $(TOP)/docs/latex/Makefile.common


TEXMAIN=Efuns.nw
TEX=Efuns.tex

SRC_ORIG=Efuns.nw Efuns_extra.nw

SRC_VIEWS= \
  core/efuns.ml\
  core/text.ml\
  core/ebuffer.ml\
  core/keymap.ml\
  core/minibuffer.ml\
  core/window.ml\
  core/frame.ml\
  features/simple.ml\
  features/complex.ml\
  features/system.ml\
  features/select.ml\
  features/search.ml\
  features/interactive.ml\
  features/abbrevs.ml\
  features/multi_frames.ml\
  features/compil.ml\
  server/server.ml\
  client/efuns_client.ml\
  dynamic/eval.ml\
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
  misc/efuns_filebrowser.ml\
  misc/efuns_texbrowser.ml\
  misc/local.ml\
  prog_modes/makefile_mode.ml\
  prog_modes/ocaml_toplevel.ml\
