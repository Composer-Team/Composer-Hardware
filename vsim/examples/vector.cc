//
// Created by Chris Kjellqvist on 10/13/22.
//

#include <rocc.h>
#include <fpga_handle.h>
#include <iostream>
#include <composer_allocator_declaration.h>

using namespace composer;
using dtype=uint16_t;
int main() {
  fpga_handle_sim_t fpga;
  int n_numbers = 1024;
  auto fpga_read = fpga.malloc(sizeof(dtype) * n_numbers);
  auto fpga_write = fpga.malloc(sizeof(dtype) * n_numbers);
  auto my_array = new dtype[n_numbers];
  for (int i = 0; i < n_numbers; ++i) {
    my_array[i] = i;
  }
  fpga.copy_to_fpga(fpga_read, my_array);

  auto load_addr = rocc_cmd::addr_cmd(VectorSystem_ID, 0, 0, fpga_read);
  auto write_addr = rocc_cmd::addr_cmd(VectorSystem_ID, 0, 1, fpga_write);
  printf("load: %16llx\nwrite: %16llx\n", fpga_read.getFpgaAddr(), fpga_write.getFpgaAddr());
  std::cout << "load: " << load_addr << "\nstore: " << write_addr << std::endl;
  auto h1 = fpga.send(load_addr);
  auto h2 = fpga.send(write_addr);
  auto start_cmd = rocc_cmd::start_cmd(VectorSystem_ID, 0, 0, true, composer::RD::R0, 0, 0, 0, n_numbers, 15);
  auto h3 = fpga.send(start_cmd);
  std::cout << h3.get() << std::endl;
  fpga.copy_from_fpga(my_array, fpga_write);
  for (int i = 0; i < n_numbers; ++i) {
    printf("Was: %d\tExpect: %d\tGot: %d\n", i, i+15, my_array[i]);
  }
}