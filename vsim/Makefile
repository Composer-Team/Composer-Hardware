#=======================================================================
# Makefile for Verilog simulation w/ VCS
#-----------------------------------------------------------------------
# Chris Kjellqvist (cmk91@cs.duke.edu)
#

default: all

base_dir = $(abspath ..)
generated_dir = $(abspath ./generated-src)
sim_dir = .
output_dir = $(sim_dir)/output

bootrom_img = $(base_dir)/rocket-chip/bootrom/bootrom.img

BACKEND ?= v
TB ?= TestDriver
CXX ?= g++
CXXFLAGS := -O1
JVM_MEMORY ?= 2G

# Extracting this information from SBT would be more robust
# api-config-chipsalliance does not use standard SBT src/main/scala, but has no resources
scala_srcs := $(shell find $(base_dir) -name "*.scala" -o -name "*.sbt")
resource_dirs := $(shell find $(base_dir) -type d -path "*/src/main/resources")
resources := $(foreach d,$(resource_dirs),$(shell find $(d) -type f))
all_srcs := $(scala_srcs) $(resources)


JAVA ?= java -Xmx$(JVM_MEMORY) -Xss8M
FIRRTL ?= $(JAVA) -cp $(ROCKET_CHIP_JAR) firrtl.stage.FirrtlMain
GENERATOR ?= $(JAVA) -cp $(ROCKET_CHIP_JAR) freechips.rocketchip.system.Generator
SBT ?= java -Xmx$(JVM_MEMORY) -Xss8M -jar $(base_dir)/rocket-chip/sbt-launch.jar
SHELL := /bin/bash

ROCKET_CHIP_JAR := $(base_dir)/rocketchip.jar
$(ROCKET_CHIP_JAR): $(all_srcs)
	cd $(base_dir) && $(SBT) assembly

# These define the configuration that we want
# ?= means set if not already set
MODEL ?= TestHarness
PROJECT ?= freechips.rocketchip.system
CFG_PROJECT ?= composer
CONFIG ?= $(CFG_PROJECT).MyTemplateConfig

# TODO: For now must match rocketchip.Generator
comma := ,
space := $() $()
splitConfigs := $(subst $(comma), ,$(CONFIG))
configBases := $(foreach config,$(splitConfigs),$(lastword $(subst ., ,$(config))))
CONFIG_STR := $(subst $(space),_,$(configBases))


#--------------------------------------------------------------------
# Verilog Generation
#--------------------------------------------------------------------

# If I don't mark these as .SECONDARY then make will delete these internal
# files.
.SECONDARY: $(generated_dir)/composer.v $(generated_dir)/composer.fir

$(generated_dir)/composer.fir $(generated_dir)/%.d: $(ROCKET_CHIP_JAR) $(bootrom_img)
	rm -f $@
	mkdir -p $(dir $@)
	cd $(base_dir) && $(GENERATOR) -td $(generated_dir) -T composer.$(MODEL) -C $(CONFIG)
	mv $(generated_dir)/*.fir $@

$(generated_dir)/composer.v $(generated_dir)/%.conf: $(generated_dir)/composer.fir $(ROCKET_CHIP_JAR)
	mkdir -p $(dir $@)
	$(FIRRTL) -i $< \
    -o $(generated_dir)/composer.v \
    -X verilog

verilog: $(generated_dir)/composer.v
# End Verilog Generation

vsim: verilog ${COMPOSER_ROOT}/Composer_Verilator/*.cc ${COMPOSER_ROOT}/Composer_Verilator/*.h
	composer-make

all: vsim

clean:
	rm -rf $(junk) simv* csrc *.key DVE* *.h *.a *.daidir $(generated_dir) $(ROCKET_CHIP_JAR)



.PHONY: default all debug clean
