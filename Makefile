all: aws

aws:
	sbt run
	cd vsim && composer-config
	. $(COMPOSER_ROOT)/aws-fpga/hdk_setup.sh && export CL_DIR=$(COMPOSER_ROOT)/Composer-Hardware/vsim/ && cd vsim/build/scripts/ && ./aws_build_dcp_from_cl.sh

kria:
	sbt run
	# TODO fill this in better
	scp vsim/generated-src/composer_allocator_declaration.h petalinux@kria-fpga.cs.duke.edu:~/Composer/Composer-Hardware/vsim/generated-src/

.PHONY: aws kria