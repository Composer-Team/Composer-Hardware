#!/bin/bash
export CONFIG=ReducerTestConfig
export AWS_DESIGN_DIR=${CL_DIR}/design
cp generated-src/adamaccaws.TestHarness.${CONFIG}.v ${AWS_DESIGN_DIR}/adamacc_generated_mem.sv
cat ${AWS_DESIGN_DIR}/aws_wrappers/SIMULATION_HEADER.sv ${AWS_DESIGN_DIR}/adamacc_generated_mem.sv > ${AWS_DESIGN_DIR}/adamacc_generated.sv
cp ${AWS_DESIGN_DIR}/aws_wrappers/no_ddr/adamacc_aws.sv.TL2 ${AWS_DESIGN_DIR}/adamacc_aws.sv
