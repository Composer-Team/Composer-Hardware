//
// Created by Chris Kjellqvist on 10/13/22.
//

#include <rocc.h>
#include <fpga_handle.h>
#include <composer_allocator_declaration.h>
using namespace composer;

int main() {
  fpga_handle_sim_t vsim;
  // test add 32 + 64 = 96
  auto add_comm = rocc_cmd::start_cmd(ALUSystem_ID,0,0,true,RD::R0, 0, 0, 0, 32, 64);
  auto add_resp = vsim.send(add_comm).get();
  if (add_resp.data != 96) {
    printf("Did not receive expected response (96), got %llu instead!\n", add_resp.data);
    exit(1);
  } else {
    printf("Passed add\n");
  }
  // test subs
  auto sub_comm = rocc_cmd::start_cmd(ALUSystem_ID,1,0,true,RD::R0, 0, 0, 0, 135, 35);
  auto sub_resp = vsim.send(sub_comm).get();
  if (sub_resp.data != 100) {
    printf("Did not receive expected response (100), got %llu instead!\n", sub_resp.data);
    exit(1);
  } else {
    printf("Passed sub\n");
  }

  // test mult
  auto mult_comm = rocc_cmd::start_cmd(ALUSystem_ID, 2, 0, true, RD::R0, 0, 0, 0, 7, 9);
  auto mult_resp = vsim.send(mult_comm).get();
  if (mult_resp.data != 63) {
    printf("Did not receive expected response (100), got %llu instead!\n", mult_resp.data);
    exit(1);
  } else {
    printf("Passed mult\n");
  }
}