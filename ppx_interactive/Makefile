TOP=..

##############################################################################
# Variables
##############################################################################
TARGET=ppx_interactive

SRC=ppx_interactive.ml

INCLUDEDIRS=\
  $(TOP)/external/compiler-libs\
  $(TOP)/external/ocaml-migrate-parsetree

LIBS=\
  $(TOP)/external/compiler-libs/ocamlcommon.cma \
  $(TOP)/external/ocaml-migrate-parsetree/migrate_parsetree.cma \

SYSLIBS=

##############################################################################
# Generic variables
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################

all:: $(TARGET)
all.opt:: $(TARGET).opt

$(TARGET): $(OBJS)
	$(OCAMLC) -o $(TARGET) $(SYSLIBS) $(LIBS) $(OBJS)
$(TARGET).opt: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -o $(TARGET).opt $(SYSLIBS:.cma=.cmxa) $(LIBS:.cma=.cmxa) $(OPTOBJS)

clean::
	rm -f $(TARGET) $(TARGET.opt)
