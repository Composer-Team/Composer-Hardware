#!/bin/bash
export CONFIG=TestConfig2
export AWS_DESIGN_DIR=${CL_DIR}/design
cp generated-src/adamaccaws.TestHarness.${CONFIG}.v ${AWS_DESIGN_DIR}/adamacc_generated.sv
cp ${AWS_DESIGN_DIR}/aws_wrappers/with_ddr/adamacc_aws.sv.1DDR ${AWS_DESIGN_DIR}/adamacc_aws.sv
echo "current built config = ${CONFIG}" > currentconfig.txt
