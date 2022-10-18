# How to Run

These examples are meant to show how to use Composer and serve loosely
as unit tests.
Simply build and run alongside the Composer runtime to use these.
These examples currently use `fpga_handle_sim_t` so they expect a
simulator backend, but they can be easily exchanged for any handle type.

To build and run, enter the following commands while inside the `vsim`
directory.
```shell
# Build the verilog for the exampleConfig
make verilog CONFIG=design.exampleConfig
cd examples
# Build the verilator for our design inside the examples directory
composer-make
# Build the user programs to test the hardware
mkdir build
cd build
cmake ..
make
# Run verilator and pipe output. Optionally, run this in another window.
../verilator/Composer_Verilator &> /dev/null &
# Run the 'vector' unit test
./vector
```
