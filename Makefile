all:
	sbt run
	cd vsim && composer-config
	. $(COMPOSER_ROOT)/aws-fpga/hdk_setup.sh && cd vsim/build/scripts/ && ./aws_build_dcp_from_cl.sh
