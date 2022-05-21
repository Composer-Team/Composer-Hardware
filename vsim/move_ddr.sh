#!/bin/bash
export AWS_MODELS=/scratch/bruns-smith/adamacc-catapult-top/aws-fpga-genomeacc/hdk/common/verif/models
cp generated-src/adamaccaws.TestHarness.DefaultAWSConfig.v ~/aws_adamacc/design/adamacc_generated_mem.sv
cat ~/aws_adamacc/design/aws_wrappers/SIMULATION_HEADER.sv ~/aws_adamacc/design/adamacc_generated_mem.sv > ~/aws_adamacc/design/adamacc_generated.sv
cp ~/aws_adamacc/design/tl2_ddr/adamacc_aws.sv ./
cp ~/aws_adamacc/design/aws_wrappers/with_ddr/sh_bfm.sv $AWS_MODELS/sh_bfm
cp ~/aws_adamacc/design/aws_wrappers/with_ddr/fpga_ddr.svh $AWS_MODELS/fpga
