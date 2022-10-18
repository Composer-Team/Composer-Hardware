#include <rocc.h>
#include <fpga_handle.h>
#include <iostream>
#include <composer_allocator_declaration.h>

using namespace composer;
int main() {
  auto start_cmd = rocc_cmd::start_cmd(LFSRSystem_ID,0, 0, true, RD::R0, 0, 0, 0, 0, 0);
  fpga_handle_sim_t handle;
  auto start_id = handle.send(start_cmd);
  auto resp = start_id.get();
  printf("System ID: %d\nCore ID: %d\nNumber: %llu\n",
         resp.system_id, resp.core_id, resp.data);
  return 0;
}
