# Helper included next to src/makefile to export its variables to tests/Makefile

# the include flags of the sources, as the main build uses them (the build
# no longer defines them in one variable since it computes the dependencies
# with -MMD)
tm_test_incl = $(call incl_flags,System System/Boot System/Classes \
  System/Files System/Link System/Misc System/Language \
  Kernel/Abstractions Kernel/Containers Kernel/Types Data/Convert \
  Data/Document Data/Drd Data/History Data/Parser Data/Observers \
  Data/String Data/Tmfs Data/Tree Graphics Plugins Plugins/Pdf/LibAesgm \
  Plugins/$(QT_PLUGIN_DIR) Style/Environment Style/Evaluate Style/Memorizer \
  Typeset Edit Texmacs Texmacs/Data Scheme Graphics/Bitmap_fonts \
  Graphics/Colors Graphics/Fonts Graphics/Gui Graphics/Mathematics \
  Graphics/Renderer Graphics/Pictures Graphics/Handwriting Graphics/Types \
  Graphics/Spacial) \
 $(CPPFLAGS) $(CXXAXEL) $(CXXCAIRO) $(CXXIMLIB2) $(CXXSQLITE3) $(CXXFREETYPE) \
 $(CXXICONV) $(CXXGUILE) $(CXXGNUTLS) $(CXXRESVG) -I$(tmsrc)/include $(CXXGUI)

print-flags:
	@echo 'TM_CXX := $(CXX)'
	@echo 'TM_LD := $(LD)'
	@echo 'TM_INCL := $(tm_test_incl)'
	@echo 'TM_CXXFLAGS := $(CXXFLAGS)'
	@echo 'TM_LDFLAGS := $(LDFLAGS)'
	@echo 'TM_LIBS := $(LIBS)'
	@echo 'TM_LINK_OPTIONS := $(link_options)'
	@echo 'TM_MOC := $(MOC)'
	@echo 'TM_MOCFLAGS := $(MOCFLAGS)'
