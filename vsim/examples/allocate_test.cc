//
// Created by Chris Kjellqvist on 10/17/22.
//
#include <rocc.h>
#include <fpga_handle.h>
#include <iostream>
int main() {
  composer::fpga_handle_sim_t handle;
  auto q = handle.malloc(512);
  std::cout << q.getFpgaAddr() << std::endl;
}